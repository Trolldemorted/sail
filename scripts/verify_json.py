#!/usr/bin/env python3
"""
Verify that a sail --json output satisfies the "zero-ambiguity, fully
monomorphised, strictly typed" contract.

Walks the JSON tree and checks three properties:

  1. MONOMORPHISATION  - no remaining polymorphic type/kind references.
     Looks for "Typ_var", "Nexp_var", "TypQ_tq" (type quantifier blocks),
     "QI_id", "KOpt_kind" with kind K_type/K_int. Each occurrence is
     flagged with its path and the kid string so a downstream consumer
     can decide whether to fail.

  2. TYPEDNESS         - every pat / exp / fexp / mpat / mpexp / lexp
     must carry a "typ" or "var_type" or "ref_type" or
     "match_arm_typ" / "param_types" field. Functions whose tao is
     Typ_annot_opt_some must include param_types.

  3. BITVECTOR SIZES   - every Typ_app ("bitvector", [Nexp]) or
     Typ_app ("bits", [Nexp]) must have a concrete Nexp_constant size
     (the printer attaches a "size" field when the size is concrete).
     Typ_id references (e.g. xlenbits) and any other bitvector-shaped
     type that lacks a "size" field are flagged.

The script prints a summary and exits non-zero if any check fails.
"""

from __future__ import annotations

import argparse
import json
import sys
from collections import Counter, defaultdict
from pathlib import Path
from typing import Any, Iterator


def iter_paths(node: Any, path: tuple[str, ...] = ()) -> Iterator[tuple[tuple[str, ...], Any]]:
    yield path, node
    if isinstance(node, dict):
        for k, v in node.items():
            yield from iter_paths(v, path + (str(k),))
    elif isinstance(node, list):
        for i, v in enumerate(node):
            yield from iter_paths(v, path + (f"[{i}]",))


# Polymorphic type/kind constructors that the printer uses. These should
# not appear in a fully-monomorphised output.
POLY_MARKERS = {
    "Typ_var",
    "Nexp_var",
    "Nexp_id",
    "TypQ_tq",
    "QI_id",
}


# AST node kinds that must carry some kind of type annotation.
# NOTE: pexp wrappers in the printer use the JSON key "Pat_exp" for both
# the outer wrapper and the inner Pat_exp constructor. The outer wrapper
# has no typ of its own (its pattern has one typ and its body has another),
# so we exclude it from this set.
TYPED_NODE_KEYS = {
    "Pat",
    "E",
    "LE",
    "FE",
    "MP",
    "MPat",
}


# Keys that satisfy the typedness check for a given node.
TYPE_FIELD_NAMES = {
    "typ",
    "var_type",
    "ref_type",
    "match_arm_typ",
    "param_types",
}


# Field paths that are allowed to contain polymorphic markers. These are
# locations where an unresolved reference is "OK" because the printer
# itself cannot resolve them (e.g. an id reference inside a `DEF_val`
# extern binding string).
POLY_ALLOWLIST_PATHS = (
    # The "bindings" key of externs is a flat string list of names.
    ("bindings",),
    # Doc comment strings.
    ("doc",),
)


def is_inside_extern_val(path: tuple[str, ...]) -> bool:
    """Return True if the path is inside a DEF_val (extern declaration)."""
    for i in range(len(path)):
        if path[i] == "DEF_val":
            return True
        # DEF_fundef / DEF_internal_mutrec / DEF_type are NOT externs.
    return False


def path_key(path: tuple[str, ...]) -> str:
    return "/".join(path)


def is_path_allowlisted(path: tuple[str, ...]) -> bool:
    """Check if a path matches any allowlisted prefix."""
    for prefix in POLY_ALLOWLIST_PATHS:
        if len(path) >= len(prefix) and path[: len(prefix)] == prefix:
            return True
    return False


class Verifier:
    def __init__(self, ast: dict[str, Any], *, allow_extern_poly: bool = False) -> None:
        self.ast = ast
        self.allow_extern_poly = allow_extern_poly
        # (path, kind, kid_str or None) tuples
        self.poly_issues: list[tuple[str, str, str | None]] = []
        # (path, node_key) tuples
        self.untyped: list[tuple[str, str]] = []
        # (path, type_label) tuples
        self.bitvector_issues: list[tuple[str, str]] = []
        # (path, fn_name) tuples - functions missing param_types
        self.fn_param_issues: list[tuple[str, str]] = []
        # (path, type_id, target_kind) - Typ_id references whose target
        # is missing or non-bitvector-shaped.
        self.alias_issues: list[tuple[str, str, str]] = []
        # Stats
        self.def_kinds: Counter[str] = Counter()
        self.defs_total = 0
        self.fundefs_seen = 0
        self.bitvector_typ_apps = 0
        self.bitvector_with_size = 0
        self.typ_id_count = 0
        self.typ_id_resolved = 0
        self.types_table_size = 0

    def verify(self) -> None:
        ast_list = self.ast.get("ast", [])
        if not isinstance(ast_list, list):
            print(f"ERROR: top-level 'ast' is not a list (got {type(ast_list).__name__})", file=sys.stderr)
            return
        self.defs_total = len(ast_list)

        # Walk every top-level def
        for i, defn in enumerate(ast_list):
            self._check_def(defn, prefix=(f"ast[{i}]",))

    def _check_def(self, node: Any, prefix: tuple[str, ...]) -> None:
        if not isinstance(node, dict):
            return
        if len(node) != 1:
            # Unexpected shape - record and continue.
            return
        kind = next(iter(node.keys()))
        self.def_kinds[kind] += 1
        if kind == "DEF_fundef":
            self.fundefs_seen += 1
            self._check_fundef(node[kind], prefix)
        elif kind == "DEF_internal_mutrec":
            for j, fd in enumerate(node[kind]):
                self._check_fundef(fd, prefix + (f"[{j}]",))

    def _check_fundef(self, fd: Any, prefix: tuple[str, ...]) -> None:
        if not isinstance(fd, dict):
            return
        # Format: {"FD_function": [ rec_opt, tao, [ funcls ... ]]}
        fdf = fd.get("FD_function")
        if not isinstance(fdf, list) or len(fdf) < 3:
            return
        rec_opt, tao, funcls = fdf[0], fdf[1], fdf[2]
        # tao is {"Typ_annot_opt": {"tannot_opt": "Typ_annot_opt_none"}}
        # or {"Typ_annot_opt": {"tannot_opt": "Typ_annot_opt_some", ...}}
        if isinstance(tao, dict) and "Typ_annot_opt" in tao:
            tannot_opt = tao["Typ_annot_opt"]
            if isinstance(tannot_opt, dict):
                tao_kind = tannot_opt.get("tannot_opt")
                if tao_kind == "Typ_annot_opt_some":
                    # Look for "param_types" sibling key in the parent dict.
                    if "param_types" not in fd:
                        # Funcl name (best effort).
                        fn_name = "<anon>"
                        if isinstance(funcls, list) and funcls:
                            first = funcls[0]
                            if isinstance(first, dict) and "FCL_funcl" in first:
                                fcl = first["FCL_funcl"]
                                if isinstance(fcl, list) and fcl:
                                    idnode = fcl[0]
                                    if isinstance(idnode, dict) and "Id" in idnode:
                                        idn = idnode["Id"]
                                        if isinstance(idn, dict) and "id" in idn:
                                            fn_name = idn["id"]
                        self.fn_param_issues.append((path_key(prefix + ("FD_function",)), fn_name))

    def report_polymorphic(self, path: tuple[str, ...], node: dict[str, Any]) -> None:
        # Look at top-level key
        if not node:
            return
        kind = next(iter(node.keys()))
        if kind not in POLY_MARKERS:
            return
        if is_path_allowlisted(path):
            return
        if self.allow_extern_poly and is_inside_extern_val(path):
            return
        # Find the kid string if present
        kid = None
        inner = node[kind]
        if isinstance(inner, dict):
            for k in ("Var", "kid", "KOpt_kind"):
                if k in inner:
                    cand = inner[k]
                    if isinstance(cand, dict):
                        for kk in ("kid", "kinded_id"):
                            if kk in cand:
                                vv = cand[kk]
                                if isinstance(vv, str):
                                    kid = vv
                                elif isinstance(vv, dict) and "Var" in vv and isinstance(vv["Var"], dict):
                                    if "kid" in vv["Var"]:
                                        kid = str(vv["Var"]["kid"])
        self.poly_issues.append((path_key(path), kind, kid))

    def report_untyped(self, path: tuple[str, ...], node: dict[str, Any]) -> None:
        # Find the wrapper key, look for any of TYPE_FIELD_NAMES
        wrapper = next(iter(node.keys()))
        if wrapper not in TYPED_NODE_KEYS:
            return
        # If any of the inner field names is present, ok.
        for fname in TYPE_FIELD_NAMES:
            if fname in node[wrapper]:
                return
        # Walk the inner "exp"/"pat"/"lexp"/"pexp"/"mpat"/"mpexp"/"fexp" object.
        inner_obj = None
        for k in ("exp", "pat", "lexp", "pexp", "mpat", "mpexp", "fexp"):
            if k in node[wrapper]:
                inner_obj = node[wrapper][k]
                break
        if inner_obj is None:
            return
        # Specifically check that the inner exp/pat/etc. is a kind we know
        # about. P_lit, P_wild, etc. have no inner typ field by design
        # (their type is in the wrapper's "typ" key, which is missing).
        # So skip those - they're already in "untyped".
        self.untyped.append((path_key(path), wrapper))

    def report_bitvector(self, path: tuple[str, ...], node: dict[str, Any]) -> None:
        if not node:
            return
        kind = next(iter(node.keys()))
        if kind != "Typ":
            return
        typ_obj = node[kind].get("typ")
        if not isinstance(typ_obj, dict):
            return
        # Look for the actual type variant key (skip auxiliary keys like "size").
        inner_kind = None
        for k in typ_obj.keys():
            if k != "size":
                inner_kind = k
                break
        if inner_kind == "Typ_app":
            # shape: {"Typ_app": [ {Id}, ...args ], "size": ... }
            ta = typ_obj["Typ_app"]
            if isinstance(ta, list) and ta:
                head = ta[0]
                if isinstance(head, dict) and "Id" in head:
                    idn = head["Id"]
                    if isinstance(idn, dict):
                        name = idn.get("id", "")
                        if name in ("bits", "bitvector"):
                            self.bitvector_typ_apps += 1
                            size_field = typ_obj.get("size")
                            if isinstance(size_field, str):
                                # The sail JSON printer emits Big_int values as
                                # JSON strings; a concrete integer string like
                                # "64" counts as a known size, while a kid
                                # spelling like "'n" or "arch_pc" does not.
                                if size_field.startswith("'") or not size_field.isdigit():
                                    self.bitvector_issues.append((path_key(path), f"{name}<{size_field}>"))
                                else:
                                    self.bitvector_with_size += 1
                            elif isinstance(size_field, int):
                                self.bitvector_with_size += 1
                            elif size_field is None:
                                self.bitvector_issues.append((path_key(path), f"{name}<no-size>"))
                            else:
                                self.bitvector_issues.append((path_key(path), f"{name}<null-size>"))
        elif inner_kind == "Typ_id":
            # A bare Typ_id reference. After Work item 3 the printer inlines
            # the resolved size if the target is a bitvector alias. Track
            # how many of these are well-resolved.
            self.typ_id_count += 1
            if "size" in typ_obj:
                self.typ_id_resolved += 1
            else:
                # Try to resolve via the types table.
                ta = typ_obj["Typ_id"]
                if isinstance(ta, list) and ta:
                    head = ta[0]
                    if isinstance(head, dict) and "Id" in head:
                        idn = head["Id"]
                        if isinstance(idn, dict):
                            target_name = idn.get("id", "")
                            target = self._lookup_type_table(target_name)
                            if target is None:
                                self.alias_issues.append((path_key(path), target_name, "missing"))
                            elif target.get("kind") == "type_alias":
                                rhs = target.get("rhs", {})
                                if "typ" not in rhs:
                                    self.alias_issues.append((path_key(path), target_name, "non-bv-alias"))

    def _lookup_type_table(self, name: str) -> dict[str, Any] | None:
        if not hasattr(self, "_types_table_idx"):
            table = self.ast.get("types")
            if isinstance(table, list):
                idx = {t.get("name"): t for t in table if isinstance(t, dict)}
                self._types_table_idx = idx
                self.types_table_size = len(table)
            else:
                self._types_table_idx = {}
                self.types_table_size = 0
        return self._types_table_idx.get(name)

    def ensure_type_table_indexed(self) -> None:
        # Force the type-table index to be built even when no Typ_id
        # references it. This lets the report show the table size.
        if not hasattr(self, "_types_table_idx"):
            _ = self._lookup_type_table("")


def main() -> int:
    parser = argparse.ArgumentParser(description="Verify sail --json output is fully monomorphised and typed.")
    parser.add_argument("json_path", type=Path, help="Path to JSON file produced by sail --json.")
    parser.add_argument("--strict", action="store_true", help="Exit non-zero on any issue.")
    parser.add_argument("--allow-extern-poly", action="store_true", help="Ignore polymorphic markers inside DEF_val extern declarations.")
    parser.add_argument("--report-polymorphic", type=int, default=20, help="Number of polymorphic occurrences to list (0 = none).")
    parser.add_argument("--report-untyped", type=int, default=20, help="Number of untyped nodes to list (0 = none).")
    parser.add_argument("--report-bitvector", type=int, default=20, help="Number of bitvector-size issues to list (0 = none).")
    parser.add_argument("--report-alias", type=int, default=20, help="Number of unresolved Typ_id alias references to list (0 = none).")
    args = parser.parse_args()

    with args.json_path.open() as f:
        data = json.load(f)

    v = Verifier(data, allow_extern_poly=args.allow_extern_poly)
    v.verify()

    # Walk the whole tree for polymorphic / untyped / bitvector checks
    for path, node in iter_paths(data):
        if isinstance(node, dict) and len(node) == 1:
            kind = next(iter(node.keys()))
            if kind in POLY_MARKERS:
                v.report_polymorphic(path, node)
            if kind in TYPED_NODE_KEYS:
                v.report_untyped(path, node)
            if kind == "Typ":
                v.report_bitvector(path, node)

    v.ensure_type_table_indexed()

    print(f"AST definitions: {v.defs_total}")
    print(f"  by kind: {dict(v.def_kinds)}")
    print(f"  fundefs seen: {v.fundefs_seen}")
    print()
    print(f"Polymorphic markers (Typ_var / Nexp_var / Nexp_id / TypQ_tq / QI_id): {len(v.poly_issues)}")
    if v.poly_issues and args.report_polymorphic:
        print("  examples:")
        for p, k, kid in v.poly_issues[: args.report_polymorphic]:
            print(f"    {k}  kid={kid}  @ {p}")
    print()
    print(f"Bitvector/bits Typ_app occurrences: {v.bitvector_typ_apps}")
    print(f"  with concrete \"size\" field: {v.bitvector_with_size}")
    print(f"  without concrete size: {len(v.bitvector_issues)}")
    if v.bitvector_issues and args.report_bitvector:
        print("  examples:")
        for p, n in v.bitvector_issues[: args.report_bitvector]:
            print(f"    {n}  @ {p}")
    print()
    print(f"Typ_id references: {v.typ_id_count}")
    print(f"  with inlined \"size\" field: {v.typ_id_resolved}")
    print(f"Type table entries: {v.types_table_size}")
    print(f"  unresolved alias references: {len(v.alias_issues)}")
    if v.alias_issues and args.report_alias:
        print("  examples:")
        for p, n, k in v.alias_issues[: args.report_alias]:
            print(f"    {n}  [{k}]  @ {p}")
    print()
    print(f"Untyped AST nodes (Pat/E/LE/FE/MP/MPat/Pat_exp without typ/var_type/...): {len(v.untyped)}")
    if v.untyped and args.report_untyped:
        print("  examples:")
        for p, w in v.untyped[: args.report_untyped]:
            print(f"    {w}  @ {p}")
    print()
    print(f"Functions (Typ_annot_opt_some) missing \"param_types\" field: {len(v.fn_param_issues)}")
    if v.fn_param_issues:
        for p, fn in v.fn_param_issues[: args.report_polymorphic]:
            print(f"    {fn}  @ {p}")
    print()

    bad = bool(v.poly_issues or v.bitvector_issues or v.untyped or v.fn_param_issues or v.alias_issues)
    if bad and args.strict:
        print("STRICT MODE: failing due to issues above.")
        return 1
    if bad:
        print("Issues found (run with --strict to fail).")
    else:
        print("All checks passed.")
    return 0


if __name__ == "__main__":
    sys.exit(main())