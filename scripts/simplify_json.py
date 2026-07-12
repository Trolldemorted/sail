#!/usr/bin/env python3
"""Convert raw `sail --json` output into a developer-friendly form.

The raw JSON preserves the full Sail AST, including polymorphic type
quantifiers, scattered unions, attribute annotations, the various
interned `Pat_*` / `E_*` / `LE_*` / `Nexp_*` constructors, location
ranges, and so on. That is useful for compilers, but it's a lot to
sift through when you're trying to write a Rust or C transpiler that
just needs:

  - the structs (records) the model defines
  - the enums (variants) the model defines
  - the type aliases (mostly bitvector shorthand)
  - the public functions, with their parameter types, return type,
    and a flat list of "instructions" describing the body

This script reads the raw JSON and emits that subset as
`simplified.json`. The output is described by the pydantic classes in
`model.py` (sibling file) and can be loaded back into Python with
`SimplifiedModel.model_validate_json(...)`.

Source locations are preserved on every emitted object so a generated
Rust/C file can include `// sail/lib/foo.sail:42` comments pointing
back at the original Sail source."""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path
from typing import Any

from pydantic import BaseModel, Field as PydanticField


# ---------------------------------------------------------------------------
# Source-location helpers
# ---------------------------------------------------------------------------

# Sail emits locations like:
#   Range(/path/to/foo.sail,42,1234,56->/path/to/foo.sail,42,1234,89)
# or:
#   Generated(Range(/path/to/foo.sail,...))
_LOC_RE = re.compile(r"^(?:Generated\()?Range\((?P<file>[^,]+),(?P<rest>.+)\)?$")


class SourceLoc(BaseModel):
    """A reference back to the original Sail source file."""

    file: str
    line: int
    col: int

    @classmethod
    def parse(cls, raw: str | None) -> "SourceLoc | None":
        if not isinstance(raw, str):
            return None
        # Pull out the file and the first line/col out of the comma-separated
        # rest. Format is `start_line,start_col,start_offset->end_line,end_col,end_offset`.
        m = _LOC_RE.match(raw)
        if not m:
            return None
        try:
            parts = m.group("rest").split("->")[0].split(",")
            line = int(parts[0])
            col = int(parts[1])
        except (IndexError, ValueError):
            return None
        return cls(file=m.group("file"), line=line, col=col)


def _loc(obj: Any) -> SourceLoc | None:
    """Pull the `loc` field out of a Sail JSON object and parse it.

    The Sail printer attaches the location at the inner payload level,
    so if the dict has a single key whose value is itself a dict
    containing `loc`, we use that. Otherwise we fall back to the
    top-level `loc`."""
    if isinstance(obj, dict):
        raw = obj.get("loc")
        if isinstance(raw, str):
            return SourceLoc.parse(raw)
        # If we only have a single-key wrapper, the actual loc is on
        # the inner dict.
        if len(obj) == 1:
            inner = next(iter(obj.values()))
            if isinstance(inner, dict):
                raw = inner.get("loc")
                if isinstance(raw, str):
                    return SourceLoc.parse(raw)
    return None


# ---------------------------------------------------------------------------
# Simplified type representation
# ---------------------------------------------------------------------------


class Type(BaseModel):
    """A simplified type. One of:

    - {"kind": "primitive", "name": "bool"}
    - {"kind": "bitvector", "size": "64"}    (size as string to preserve Big_int)
    - {"kind": "alias", "name": "xlenbits"}  (caller must resolve via the type table)
    - {"kind": "named", "name": "MyStruct"}  (struct/enum reference)
    - {"kind": "unknown"}                    (we could not decode)
    """

    kind: str
    name: str | None = None
    size: str | None = None
    loc: SourceLoc | None = None


class Field(BaseModel):
    """A field in a record or a constructor argument in a variant."""

    name: str
    type: Type | None = None
    loc: SourceLoc | None = None


class StructDef(BaseModel):
    """A `type x = { f: T, g: U }` record."""

    kind: str = "struct"
    name: str
    fields: list[Field] = PydanticField(default_factory=list)
    loc: SourceLoc | None = None
    source: str | None = None  # filled by the simplifier if it knows the file


class EnumVariant(BaseModel):
    """A constructor of a `type x = A of T | B` variant."""

    name: str
    arg: Type | None = None
    loc: SourceLoc | None = None


class EnumDef(BaseModel):
    """A `type x = A of T | B | C of U` variant."""

    kind: str = "enum"
    name: str
    variants: list[EnumVariant] = PydanticField(default_factory=list)
    loc: SourceLoc | None = None
    source: str | None = None


class TypeAlias(BaseModel):
    """A `type x = bits(64)` style alias. We carry the RHS string form
    so a transpiler that wants to inline the alias can do so without
    re-implementing the type-resolver."""

    kind: str = "alias"
    name: str
    rhs: str
    loc: SourceLoc | None = None
    source: str | None = None


class AnyTypeDef(BaseModel):
    """A `type x` (abstract) declaration."""

    kind: str = "abstract"
    name: str
    loc: SourceLoc | None = None
    source: str | None = None


# ---------------------------------------------------------------------------
# Simplified instruction / expression representation
# ---------------------------------------------------------------------------


class Instruction(BaseModel):
    """One statement in a function body.

    The `op` field names the operation (e.g. "assign", "call", "if",
    "match", "return"). The `args` field carries the operands. The
    exact shape of `args` depends on `op`; the `src` field on every
    instruction lets a generated comment point back at the original
    Sail source line.
    """

    op: str
    args: dict[str, Any] = PydanticField(default_factory=dict)
    loc: SourceLoc | None = None


class Param(BaseModel):
    name: str
    type: Type | None = None


class Function(BaseModel):
    """A Sail function definition.

    `instructions` is the body as a flat list. Multi-clause functions
    (`function foo 0 = ... | _ = ...`) become a single Function with
    `match` instructions at the top."""

    name: str
    params: list[Param] = PydanticField(default_factory=list)
    return_type: Type | None = None
    is_recursive: bool = False
    instructions: list[Instruction] = PydanticField(default_factory=list)
    loc: SourceLoc | None = None
    source: str | None = None
    extern: bool = False  # True for DEF_val declarations


class ExternDecl(BaseModel):
    """A `val foo = "..." : T` extern declaration. We keep these so
    downstream transpilers can map them onto the runtime ABI."""

    name: str
    extern_target: str | None = None  # the C symbol, e.g. "eq_string"
    extern_type: str | None = None  # "ocaml" / "c" / "asm" etc.
    type: Type | None = None
    loc: SourceLoc | None = None
    source: str | None = None


class SimplifiedModel(BaseModel):
    """Top-level output of simplify_json."""

    structs: list[StructDef] = PydanticField(default_factory=list)
    enums: list[EnumDef] = PydanticField(default_factory=list)
    aliases: list[TypeAlias] = PydanticField(default_factory=list)
    abstracts: list[AnyTypeDef] = PydanticField(default_factory=list)
    functions: list[Function] = PydanticField(default_factory=list)
    externs: list[ExternDecl] = PydanticField(default_factory=list)
    source_files: list[str] = PydanticField(default_factory=list)


# ---------------------------------------------------------------------------
# Sail-JSON AST walking helpers
# ---------------------------------------------------------------------------


def _node_kind(node: Any) -> str | None:
    """The pattern / expression kind.

    Most Sail JSON nodes are a single-key dict (e.g. `{"P_id": {...}}`).
    But patterns and other constructors sometimes carry extra
    metadata (`{"P_id": {...}, "var_type": {...}}`) — in that case
    the kind is whichever key starts with a known constructor
    prefix."""
    if not isinstance(node, dict):
        return None
    if len(node) == 1:
        return next(iter(node.keys()))
    # Multi-key dict: pick the first key that's a recognised
    # constructor.
    for k in node.keys():
        if k.startswith(("P_", "E_", "LE_", "L_", "Typ_", "Nexp_", "A_", "K_", "FCL_", "FD_", "DEF_", "VS_")):
            return k
    return None


def _node_payload(node: Any) -> Any:
    """The value behind the kind key, even when the dict has extra metadata."""
    k = _node_kind(node)
    if k is not None and isinstance(node, dict) and k in node:
        return node[k]
    if isinstance(node, dict) and len(node) == 1:
        return next(iter(node.values()))
    return None


def _typ_size(node: Any) -> str | None:
    """The `size` field that the JSON printer attaches to bitvector
    types. Returns the raw size string (Big_int printed) or None."""
    if isinstance(node, dict):
        s = node.get("size")
        if isinstance(s, str):
            return s
        if isinstance(s, int):
            return str(s)
    return None


def _typ_kind(node: Any) -> str | None:
    """The inner type-variant key (Typ_app / Typ_id / Typ_var / Typ_fn / ...)."""
    if not isinstance(node, dict):
        return None
    for k in node.keys():
        if k != "size":
            return k
    return None


# The Sail JSON printer wraps payload objects in a single-key dict
# (`{"E": {...}}`, `{"Pat": {...}}`, `{"LE": {...}}`, `{"Lit": {...}}`,
# `{"Typ": {...}}`, `{"FE": {...}}`, `{"MPat": {...}}`, `{"MP": {...}}`).
# These wrappers carry the loc/typ metadata, and the actual kind is
# one key deeper. Most of the simplifier helpers used to look at
# `node.get("exp")` / `node.get("pat")` on the wrapper directly; that
# silently missed everything. The helpers below unwrap first.
PAYLOAD_WRAPPERS = {
    "E", "Pat", "LE", "Lit", "Typ", "FE", "MP", "MPat",
    "Typ_annot_opt", "TypSchm", "Rec", "Pat_exp",
}


def _unwrap(node: Any) -> Any:
    """If `node` is a single-key dict whose key is one of the
    PAYLOAD_WRAPPERS, return the inner value. Otherwise return node."""
    if isinstance(node, dict) and len(node) == 1:
        k = next(iter(node.keys()))
        if k in PAYLOAD_WRAPPERS:
            return node[k]
    return node


def _simplify_typ(node: Any) -> Type | None:
    """Turn a Sail JSON Typ object into our simplified Type."""
    if not isinstance(node, dict):
        return None
    node = _unwrap(node)
    if not isinstance(node, dict):
        return None
    loc = _loc(node)
    typ_obj = node.get("typ")
    if not isinstance(typ_obj, dict):
        return None
    kind = _typ_kind(typ_obj)
    if kind == "Typ_app":
        arr = typ_obj.get("Typ_app")
        head = None
        if isinstance(arr, list) and arr:
            head = arr[0]
        elif isinstance(arr, dict) and "Id" in arr:
            head = arr
        if isinstance(head, dict) and "Id" in head:
            idn = head["Id"]
            if isinstance(idn, dict):
                name = idn.get("id", "")
                if name in ("bits", "bitvector"):
                    size = _typ_size(typ_obj)
                    return Type(kind="bitvector", size=size or "?", loc=loc)
                if name == "bool":
                    return Type(kind="primitive", name="bool", loc=loc)
                if name == "unit":
                    return Type(kind="primitive", name="unit", loc=loc)
                if name == "int":
                    return Type(kind="primitive", name="int", loc=loc)
                if name == "real":
                    return Type(kind="primitive", name="real", loc=loc)
                if name == "string":
                    return Type(kind="primitive", name="string", loc=loc)
                if name == "atom":
                    return Type(kind="primitive", name="atom", loc=loc)
                if name == "list":
                    return Type(kind="primitive", name="list", loc=loc)
                if name == "vector":
                    return Type(kind="primitive", name="vector", loc=loc)
                # User-defined constructor application
                return Type(kind="named", name=name, loc=loc)
    if kind == "Typ_id":
        tid = typ_obj.get("Typ_id")
        # The printer emits Typ_id in two shapes:
        #   - {"Typ_id": [ {Id: ...}, ...optional quantifier args ]}
        #   - {"Typ_id": { "Id": {...} }}
        # Normalise to a single head dict.
        head = None
        if isinstance(tid, list) and tid:
            head = tid[0]
        elif isinstance(tid, dict) and "Id" in tid:
            head = tid
        if isinstance(head, dict) and "Id" in head:
            name = head["Id"].get("id", "")
            # Could be primitive, struct, enum, or alias - we don't know.
            if name in ("bool", "unit", "int", "real", "string", "nat", "bit"):
                return Type(kind="primitive", name=name, loc=loc)
            return Type(kind="alias", name=name, loc=loc)
    if kind == "Typ_var":
        # Polymorphic marker - shouldn't appear after --json-strict-poly.
        arr = typ_obj.get("Typ_var")
        if isinstance(arr, dict) and "Var" in arr:
            kid = arr["Var"].get("kid", "?")
            return Type(kind="unknown", name=f"'{kid}", loc=loc)
        return Type(kind="unknown", loc=loc)
    if kind == "Typ_fn":
        return Type(kind="unknown", name="fn", loc=loc)
    if kind == "Typ_tuple":
        return Type(kind="unknown", name="tuple", loc=loc)
    if kind == "Typ_exist":
        return Type(kind="unknown", name="existential", loc=loc)
    if kind == "Typ_internal_unknown":
        return Type(kind="unknown", loc=loc)
    return Type(kind="unknown", loc=loc)


# ---------------------------------------------------------------------------
# Type-table -> StructDef / EnumDef / TypeAlias
# ---------------------------------------------------------------------------


def _source_for(loc: SourceLoc | None, type_tables: list[str]) -> str | None:
    if loc is None:
        return None
    for prefix in type_tables:
        if loc.file.startswith(prefix):
            return loc.file
    return loc.file


def _simplify_types(types_table: list[dict], type_tables: list[str]) -> tuple[
    list[StructDef], list[EnumDef], list[TypeAlias], list[AnyTypeDef]
]:
    structs: list[StructDef] = []
    enums: list[EnumDef] = []
    aliases: list[TypeAlias] = []
    abstracts: list[AnyTypeDef] = []
    for t in types_table:
        if not isinstance(t, dict):
            continue
        name = t.get("name")
        kind = t.get("kind")
        loc = _loc(t)
        source = _source_for(loc, type_tables)
        if kind == "type_alias":
            rhs_obj = t.get("rhs", {})
            # Serialize the RHS back to a one-liner-ish form.
            rhs = _typ_to_string(rhs_obj) or "?"
            aliases.append(TypeAlias(name=name or "?", rhs=rhs, loc=loc, source=source))
        elif kind == "record":
            fields: list[Field] = []
            for f in t.get("fields", []):
                if not isinstance(f, dict):
                    continue
                ft = f.get("typ")
                ftype = _simplify_typ(ft) if isinstance(ft, dict) else None
                fields.append(
                    Field(name=f.get("name", "?"), type=ftype, loc=_loc(f.get("name") if isinstance(f.get("name"), dict) else None))
                )
            structs.append(StructDef(name=name or "?", fields=fields, loc=loc, source=source))
        elif kind == "variant":
            variants: list[EnumVariant] = []
            for c in t.get("constructors", []):
                if not isinstance(c, dict):
                    continue
                arg_typ = None
                if "arg" in c:
                    arg_typ = _simplify_typ(c["arg"])
                variants.append(EnumVariant(name=c.get("name", "?"), arg=arg_typ, loc=loc))
            enums.append(EnumDef(name=name or "?", variants=variants, loc=loc, source=source))
        elif kind == "enum":
            variants = [EnumVariant(name=str(v), loc=loc) for v in t.get("constructors", [])]
            enums.append(EnumDef(name=name or "?", variants=variants, loc=loc, source=source))
        elif kind == "abstract":
            abstracts.append(AnyTypeDef(name=name or "?", loc=loc, source=source))
    return structs, enums, aliases, abstracts


def _typ_to_string(rhs_obj: Any) -> str | None:
    """Best-effort string rendering for a type-table rhs entry."""
    if not isinstance(rhs_obj, dict):
        return None
    if "typ" in rhs_obj:
        t = _simplify_typ(rhs_obj["typ"])
        if t is None:
            return None
        if t.kind == "primitive":
            return t.name or "?"
        if t.kind == "bitvector":
            return f"bits({t.size or '?'})"
        if t.kind == "named":
            return t.name or "?"
        if t.kind == "alias":
            return t.name or "?"
        return "?"
    if "nexp" in rhs_obj:
        n = rhs_obj["nexp"]
        if isinstance(n, dict):
            nc = n.get("nexp") if "nexp" in n else n
            if isinstance(nc, dict):
                if "Nexp_constant" in nc:
                    return str(nc["Nexp_constant"])
                if "Nexp_var" in nc:
                    var = nc["Nexp_var"]
                    if isinstance(var, dict) and "Var" in var:
                        return str(var["Var"].get("kid", "?"))
        return "?"
    return None


# ---------------------------------------------------------------------------
# Function-body simplification
# ---------------------------------------------------------------------------


def _pat_to_var_name(node: Any) -> str | None:
    """Pull the variable name out of a P_id pattern."""
    if isinstance(node, dict):
        if "P_id" in node:
            pid = node["P_id"]
            if isinstance(pid, dict) and "Id" in pid:
                return pid["Id"].get("id")
    return None


def _simplify_lit(node: Any) -> Any:
    """Render a Sail literal as a Python value.

    The `lit` field is sometimes a string shorthand (e.g. `"L_true"`),
    sometimes a dict like `{"L_bin": [...]}`, and sometimes a dict
    with `L_num`. Handle all three."""
    if not isinstance(node, dict):
        return None
    lit_obj = node.get("lit")
    # Bare-string shorthand.
    if isinstance(lit_obj, str):
        return {"L_unit": "()", "L_zero": "0", "L_one": "1",
                "L_true": "true", "L_false": "false",
                "L_num_zero": "0", "L_num_one": "1"}.get(lit_obj, lit_obj)
    if isinstance(lit_obj, dict):
        if "L_unit" in lit_obj:
            return "()"
        if "L_zero" in lit_obj:
            return "0"
        if "L_one" in lit_obj:
            return "1"
        if "L_true" in lit_obj:
            return "true"
        if "L_false" in lit_obj:
            return "false"
        if "L_num" in lit_obj:
            # L_num is keyed by a string-encoded Big_int or just a value.
            v = lit_obj["L_num"]
            if isinstance(v, str):
                return v
            if isinstance(v, dict):
                # Some printer forms put the value under a key.
                for kk in ("int", "n", "value"):
                    if kk in v:
                        return str(v[kk])
            return str(v)
        if "L_int" in lit_obj:
            v = lit_obj["L_int"]
            if isinstance(v, list) and v:
                return v[0]
            if isinstance(v, dict):
                return v.get("value") or v.get("int") or v.get("n") or str(v)
            return str(v)
        if "L_hex" in lit_obj:
            v = lit_obj["L_hex"]
            if isinstance(v, list) and v:
                digits = "".join(
                    str(d).split("_")[-1]
                    for outer in v
                    for d in (outer if isinstance(outer, list) else [outer])
                )
                return f"0x{digits}"
        if "L_bin" in lit_obj:
            v = lit_obj["L_bin"]
            if isinstance(v, list) and v:
                # Each outer element is itself a list of digit names.
                digits = "".join(
                    str(d).split("_")[-1]
                    for outer in v
                    for d in (outer if isinstance(outer, list) else [outer])
                )
                return f"0b{digits}"
        if "L_string" in lit_obj:
            return str(lit_obj["L_string"])
        if "L_real" in lit_obj:
            v = lit_obj["L_real"]
            if isinstance(v, list) and v:
                return v[0]
            return str(v)
        if "L_undefined" in lit_obj:
            return "<undefined>"
    return None


def _simplify_exp(exp: Any) -> Instruction | None:
    """Turn a Sail E_* node into an Instruction (statement form).

    For expressions that fit naturally as statements (assignments,
    function calls, control flow, blocks) we emit one Instruction.
    For pure expressions (E_id, E_lit, E_app of a function that returns
    a value) we emit a wrapping `value` instruction so a Rust/C
    consumer can still see the call."""
    if not isinstance(exp, dict):
        return None
    # Unwrap {"E": {...}} -> the inner dict carries exp/loc/typ.
    exp = _unwrap(exp)
    if not isinstance(exp, dict):
        return None
    loc = _loc(exp)
    inner = exp.get("exp")
    if not isinstance(inner, dict):
        return None
    kind = _node_kind(inner)
    payload = _node_payload(inner)

    if kind == "E_block":
        return Instruction(op="block", args={"body": [_simplify_exp(x) for x in payload if x]}, loc=loc)

    if kind == "E_id":
        if isinstance(payload, dict) and "Id" in payload:
            return Instruction(op="value", args={"name": payload["Id"].get("id", "?")}, loc=loc)

    if kind == "E_lit":
        # E_lit payload is {"Lit": {"lit": {"L_*": ...}}}. Drill down two
        # levels before handing off to the literal renderer.
        lit_inner = payload.get("Lit", {}).get("lit") if isinstance(payload, dict) else None
        return Instruction(op="value", args={"lit": _simplify_lit({"lit": lit_inner})}, loc=loc)

    if kind == "E_app":
        # E_app shape: [ {Id: name}, arg1, arg2, ... ]
        if isinstance(payload, list) and payload:
            head = payload[0]
            name = None
            if isinstance(head, dict) and "Id" in head:
                name = head["Id"].get("id", "?")
            args = []
            for a in payload[1:]:
                si = _simplify_exp(a)
                args.append(si.args if si else None)
            return Instruction(op="call", args={"name": name or "?", "args": args}, loc=loc)

    if kind == "E_assign":
        # E_assign: [ LE, E ]
        if isinstance(payload, list) and len(payload) == 2:
            return Instruction(
                op="assign",
                args={
                    "lhs": _simplify_lexp(payload[0]),
                    "rhs": _simplify_exp(payload[1]).args if _simplify_exp(payload[1]) else None,
                },
                loc=loc,
            )

    if kind == "E_if":
        if isinstance(payload, list) and len(payload) == 3:
            return Instruction(
                op="if",
                args={
                    "cond": _simplify_exp(payload[0]).args if _simplify_exp(payload[0]) else None,
                    "then": _simplify_exp(payload[1]).args if _simplify_exp(payload[1]) else None,
                    "else_": _simplify_exp(payload[2]).args if _simplify_exp(payload[2]) else None,
                },
                loc=loc,
            )

    if kind == "E_match":
        if isinstance(payload, list) and len(payload) == 2:
            arms_obj = payload[1]
            # arms_obj is a list of FCL_funcl (one per arm). Each has
            # [Pat, body] structure (simplified form).
            arms = _simplify_match_arms(arms_obj)
            return Instruction(
                op="match",
                args={
                    "scrutinee": _simplify_exp(payload[0]).args if _simplify_exp(payload[0]) else None,
                    "arms": arms,
                },
                loc=loc,
            )

    if kind == "E_let":
        # E_let: [ Pat, binding, body ]
        if isinstance(payload, list) and len(payload) == 3:
            return Instruction(
                op="let",
                args={
                    "pat": _simplify_pat(payload[0]),
                    "value": _simplify_exp(payload[1]).args if _simplify_exp(payload[1]) else None,
                    "body": _simplify_exp(payload[2]).args if _simplify_exp(payload[2]) else None,
                },
                loc=loc,
            )

    if kind == "E_for":
        if isinstance(payload, list) and len(payload) >= 5:
            return Instruction(
                op="for",
                args={
                    "var": _simplify_pat(payload[1]),
                    "lo": _simplify_exp(payload[2]).args if _simplify_exp(payload[2]) else None,
                    "hi": _simplify_exp(payload[3]).args if _simplify_exp(payload[3]) else None,
                    "dir": payload[0] if isinstance(payload[0], str) else None,
                    "body": _simplify_exp(payload[5]).args if len(payload) > 5 and _simplify_exp(payload[5]) else None,
                },
                loc=loc,
            )

    if kind == "E_loop":
        if isinstance(payload, list) and len(payload) == 4:
            return Instruction(
                op="loop",
                args={
                    "cond": _simplify_exp(payload[2]).args if _simplify_exp(payload[2]) else None,
                    "body": _simplify_exp(payload[3]).args if _simplify_exp(payload[3]) else None,
                },
                loc=loc,
            )

    if kind == "E_return":
        if isinstance(payload, list) and len(payload) == 1:
            return Instruction(
                op="return",
                args={"value": _simplify_exp(payload[0]).args if _simplify_exp(payload[0]) else None},
                loc=loc,
            )

    if kind == "E_throw":
        if isinstance(payload, list) and len(payload) == 1:
            return Instruction(
                op="throw",
                args={"value": _simplify_exp(payload[0]).args if _simplify_exp(payload[0]) else None},
                loc=loc,
            )

    if kind == "E_assert":
        if isinstance(payload, list) and len(payload) == 2:
            return Instruction(
                op="assert",
                args={
                    "cond": _simplify_exp(payload[0]).args if _simplify_exp(payload[0]) else None,
                    "msg": _simplify_exp(payload[1]).args if _simplify_exp(payload[1]) else None,
                },
                loc=loc,
            )

    if kind == "E_typ":
        if isinstance(payload, list) and len(payload) == 2:
            return Instruction(
                op="annotation",
                args={"type": _simplify_typ(payload[0]).model_dump() if _simplify_typ(payload[0]) else None},
                loc=loc,
            )

    if kind == "E_sizeof":
        if isinstance(payload, list) and len(payload) == 1:
            return Instruction(op="sizeof", args={"value": _typ_to_string({"nexp": payload[0]})}, loc=loc)

    if kind == "E_internal_plet":
        if isinstance(payload, list) and len(payload) == 3:
            return Instruction(
                op="internal_plet",
                args={
                    "pat": _simplify_pat(payload[0]),
                    "value": _simplify_exp(payload[1]).args if _simplify_exp(payload[1]) else None,
                    "body": _simplify_exp(payload[2]).args if _simplify_exp(payload[2]) else None,
                },
                loc=loc,
            )

    if kind == "E_internal_return":
        if isinstance(payload, list) and len(payload) == 1:
            return Instruction(
                op="internal_return",
                args={"value": _simplify_exp(payload[0]).args if _simplify_exp(payload[0]) else None},
                loc=loc,
            )

    if kind == "E_vector" or kind == "E_vector_append" or kind == "E_cons" or kind == "E_list":
        if isinstance(payload, list):
            return Instruction(op="collection", args={"kind": kind, "items": [_simplify_exp(x).args if _simplify_exp(x) else None for x in payload]}, loc=loc)

    if kind == "E_tuple":
        if isinstance(payload, list):
            return Instruction(op="tuple", args={"items": [_simplify_exp(x).args if _simplify_exp(x) else None for x in payload]}, loc=loc)

    if kind == "E_record":
        # E_record: [ {Id: fname}, E_fexp1, ... ]
        if isinstance(payload, list) and payload:
            head = payload[0]
            base = None
            if isinstance(head, dict) and "Id" in head:
                base = head["Id"].get("id")
            updates = []
            for u in payload[1:]:
                if isinstance(u, dict) and "E_fexp" in u:
                    fexp = u["E_fexp"]
                    if isinstance(fexp, list) and len(fexp) == 2:
                        fname = None
                        if isinstance(fexp[0], dict) and "Id" in fexp[0]:
                            fname = fexp[0]["Id"].get("id")
                        rhs = _simplify_exp(fexp[1])
                        updates.append({"field": fname, "value": rhs.args if rhs else None})
            return Instruction(op="record_update", args={"base": base, "updates": updates}, loc=loc)

    if kind == "E_field":
        if isinstance(payload, list) and len(payload) == 2:
            return Instruction(
                op="field",
                args={
                    "record": _simplify_exp(payload[0]).args if _simplify_exp(payload[0]) else None,
                    "field": payload[1] if isinstance(payload[1], dict) and "Id" in payload[1]
                        else None,
                },
                loc=loc,
            )

    # Fallback: record the unknown op name so the simplifier is
    # transparent about what it could not handle.
    return Instruction(op=f"unknown({kind})", args={}, loc=loc)


def _simplify_lexp(node: Any) -> dict[str, Any]:
    """Simplify a lexp (l-value expression)."""
    if not isinstance(node, dict):
        return {}
    node = _unwrap(node)
    if not isinstance(node, dict):
        return {}
    loc = _loc(node)
    inner = node.get("lexp")
    if not isinstance(inner, dict):
        return {}
    kind = _node_kind(inner)
    payload = _node_payload(inner)
    if kind == "LE_id":
        if isinstance(payload, dict) and "Id" in payload:
            return {"op": "id", "name": payload["Id"].get("id", "?"), "loc": loc.model_dump() if loc else None}
    if kind == "LE_deref":
        if isinstance(payload, list) and payload:
            return {"op": "deref", "expr": _simplify_exp(payload[0]).args if _simplify_exp(payload[0]) else None, "loc": loc.model_dump() if loc else None}
    if kind == "LE_app":
        if isinstance(payload, list) and payload:
            head = payload[0]
            name = head["Id"].get("id", "?") if isinstance(head, dict) and "Id" in head else "?"
            args = [_simplify_exp(a).args if _simplify_exp(a) else None for a in payload[1:]]
            return {"op": "call", "name": name, "args": args, "loc": loc.model_dump() if loc else None}
    return {"op": f"unknown({kind})", "loc": loc.model_dump() if loc else None}


def _simplify_pat(node: Any) -> dict[str, Any]:
    """Simplify a pattern."""
    if not isinstance(node, dict):
        return {}
    # Unwrap {"Pat": {...}} -> the inner dict carries pat/loc/typ.
    node = _unwrap(node)
    if not isinstance(node, dict):
        return {}
    inner = node.get("pat")
    if not isinstance(inner, dict):
        return {}
    kind = _node_kind(inner)
    payload = _node_payload(inner)
    loc = _loc(node)
    if kind == "P_id":
        if isinstance(payload, dict) and "Id" in payload:
            return {"op": "var", "name": payload["Id"].get("id", "?"), "loc": loc.model_dump() if loc else None}
    if kind == "P_lit":
        return {"op": "lit", "value": _simplify_lit(payload), "loc": loc.model_dump() if loc else None}
    if kind == "P_wild":
        return {"op": "wild", "loc": loc.model_dump() if loc else None}
    if kind == "P_tuple":
        if isinstance(payload, list):
            return {"op": "tuple", "items": [_simplify_pat(x) for x in payload], "loc": loc.model_dump() if loc else None}
    if kind == "P_typ":
        if isinstance(payload, list) and len(payload) == 2:
            return {"op": "annotation", "type": _simplify_typ(payload[0]).model_dump() if _simplify_typ(payload[0]) else None, "pat": _simplify_pat(payload[1]), "loc": loc.model_dump() if loc else None}
    if kind == "P_var":
        if isinstance(payload, list) and len(payload) == 2:
            return {"op": "var_typed", "name": payload[1] if isinstance(payload[1], str) else None, "loc": loc.model_dump() if loc else None}
    if kind == "P_app":
        # Constructor application: [Id, arg_pat, ...]. The first element
        # is {Id: "Cons"}, the rest are sub-patterns.
        ctor = None
        args = []
        if isinstance(payload, list) and payload:
            head = payload[0]
            if isinstance(head, dict) and "Id" in head:
                ctor = head["Id"].get("id", "?")
            for a in payload[1:]:
                args.append(_simplify_pat(a))
        return {"op": "ctor", "name": ctor or "?", "args": args, "loc": loc.model_dump() if loc else None}
    return {"op": f"unknown({kind})", "loc": loc.model_dump() if loc else None}


def _simplify_match_arms(arms: Any) -> list[dict[str, Any]]:
    """Simplify a list of match-arm entries into arm dicts.

    The printer emits each arm as either:
      - {"FCL_funcl": [ Id_or_Pat, pat_body ]}    (function clause)
      - {"pexp": {Pat_exp: [Pat, body]}}          (match-arm body)

    In both cases the body lives behind a `Pat_exp` wrapper whose
    inner payload is `[Pat, Exp]` after the pexp unwrap.

    Each output arm is:
        {"pat": <simplified_pat>, "body": <simplified_exp_args>, "loc": <loc>}
    """
    out: list[dict[str, Any]] = []
    if not isinstance(arms, list):
        return out
    for arm in arms:
        if not isinstance(arm, dict):
            continue
        # FCL_funcl form: [Id, Pat_exp_node]
        pe_arr: Any = None
        pat_node: Any = None
        if "FCL_funcl" in arm:
            fcl = arm["FCL_funcl"]
            if isinstance(fcl, list) and len(fcl) >= 2:
                pat_node = fcl[1]
                # FCL_funcl[1] is {Pat_exp: {pexp: {Pat_exp: [pat, body]}}}
                if isinstance(pat_node, dict):
                    pe_outer = pat_node.get("Pat_exp")
                    if isinstance(pe_outer, dict) and "pexp" in pe_outer:
                        inner = pe_outer["pexp"]
                        if isinstance(inner, dict) and "Pat_exp" in inner:
                            pe_arr = inner["Pat_exp"]
        elif "pexp" in arm:
            # Match-arm form: {match_arm_typ, pexp: {Pat_exp: [pat, body]}}
            inner = arm["pexp"]
            if isinstance(inner, dict) and "Pat_exp" in inner:
                pe_arr = inner["Pat_exp"]
        if not isinstance(pe_arr, list) or len(pe_arr) != 2:
            continue
        pat = _simplify_pat(pe_arr[0])
        body_exp = _simplify_exp(pe_arr[1])
        body = body_exp.args if body_exp else None
        out.append({"pat": pat, "body": body, "loc": _loc(pat_node or arm).model_dump() if _loc(pat_node or arm) else None})
    return out


# ---------------------------------------------------------------------------
# Function discovery
# ---------------------------------------------------------------------------


def _simplify_functions(ast: list[dict], type_tables: list[str]) -> tuple[list[Function], list[ExternDecl]]:
    functions: list[Function] = []
    externs: list[ExternDecl] = []
    for entry in ast:
        if not isinstance(entry, dict):
            continue
        kind = _node_kind(entry)
        if kind == "DEF_fundef":
            for fn in _simplify_fundef(entry["DEF_fundef"], type_tables):
                functions.append(fn)
        elif kind == "DEF_internal_mutrec":
            for fd in entry["DEF_internal_mutrec"]:
                if isinstance(fd, dict) and "FD_function" in fd:
                    for fn in _simplify_fundef(fd["FD_function"], type_tables, recursive=True):
                        functions.append(fn)
        elif kind == "DEF_val":
            ext = _simplify_extern(entry["DEF_val"])
            if ext is not None:
                externs.append(ext)
    return functions, externs


def _simplify_fundef(fd: Any, type_tables: list[str], recursive: bool = False) -> list[Function]:
    if not isinstance(fd, dict):
        return []
    fdf = fd.get("FD_function")
    if not isinstance(fdf, list) or len(fdf) < 3:
        return []
    rec_opt_obj, tao, funcls = fdf[0], fdf[1], fdf[2]
    is_recursive = bool(isinstance(rec_opt_obj, dict) and "Rec" in rec_opt_obj
                        and isinstance(rec_opt_obj["Rec"].get("rec_opt"), str)
                        and rec_opt_obj["Rec"]["rec_opt"] != "Rec_nonrec")

    # Type annotation
    ret_type: Type | None = None
    if isinstance(tao, dict) and "Typ_annot_opt" in tao:
        ta = tao["Typ_annot_opt"]
        if isinstance(ta, dict) and ta.get("tannot_opt") == "Typ_annot_opt_some":
            ts = ta.get("Typ_annot_opt_some")
            if isinstance(ts, dict) and "TypSchm" in ts:
                typschm = ts["TypSchm"]
                if isinstance(typschm, dict) and "typschm" in typschm:
                    tso = typschm["typschm"]
                    if isinstance(tso, dict) and "TypSchm_ts" in tso:
                        ts_arr = tso["TypSchm_ts"]
                        if isinstance(ts_arr, list) and ts_arr:
                            last = ts_arr[-1]
                            if isinstance(last, dict) and "Typ" in last:
                                ret_type = _simplify_typ(last["Typ"])

    out: list[Function] = []
    for fc in funcls:
        if not isinstance(fc, dict) or "FCL_funcl" not in fc:
            continue
        funcl = fc["FCL_funcl"]
        if not isinstance(funcl, list) or len(funcl) < 2:
            continue
        id_node = funcl[0]
        name = None
        if isinstance(id_node, dict) and "Id" in id_node:
            name = id_node["Id"].get("id", "?")
        loc = _loc(id_node)
        source = _source_for(loc, type_tables)

        # Extract params from the Pat_exp body
        params: list[Param] = []
        instructions: list[Instruction] = []
        body_ret_type: Type | None = None
        pe = funcl[1]
        if isinstance(pe, dict) and "Pat_exp" in pe:
            inner = pe["Pat_exp"]
            if isinstance(inner, dict) and "pexp" in inner:
                pe2 = inner["pexp"]
                if isinstance(pe2, dict) and "Pat_exp" in pe2:
                    pe3 = pe2["Pat_exp"]
                    if isinstance(pe3, list) and len(pe3) == 2:
                        pat = pe3[0]
                        body = pe3[1]
                        # Extract params: pattern is usually P_tuple -> [P_id, ...]
                        params = _params_from_pat(pat)
                        # Simplify body
                        instr = _simplify_exp(body)
                        if instr is not None:
                            instructions.append(instr)
                        # If the fundef tao was None, the function's return
                        # type is the body's typ.
                        if isinstance(body, dict):
                            body_unwrapped = _unwrap(body)
                            if isinstance(body_unwrapped, dict) and "typ" in body_unwrapped:
                                body_ret_type = _simplify_typ(body_unwrapped["typ"])

        # Apply body-typ fallback for the return type if tao didn't supply one.
        if ret_type is None:
            ret_type = body_ret_type
        out.append(
            Function(
                name=name or "?",
                params=params,
                return_type=ret_type,
                is_recursive=is_recursive or recursive,
                instructions=instructions,
                loc=loc,
                source=source,
            )
        )
    return out


def _params_from_pat(node: Any) -> list[Param]:
    """Pull parameter list out of a function's argument pattern."""
    if not isinstance(node, dict):
        return []
    node = _unwrap(node)
    if not isinstance(node, dict):
        return []
    pat = node.get("pat")
    if not isinstance(pat, dict):
        return []
    kind = _node_kind(pat)
    payload = _node_payload(pat)
    if kind == "P_tuple":
        out: list[Param] = []
        if isinstance(payload, list):
            for x in payload:
                p = _simplify_pat(x)
                if p.get("op") == "var":
                    t = None
                    if "type" in p:
                        t = Type.model_validate(p["type"]) if p["type"] else None
                    elif "var_type" in (x.get("typ") or {} if isinstance(x, dict) else {}):
                        # try to pull var_type from the original Pat wrapper
                        var_type = x.get("typ") if isinstance(x, dict) else None
                        if isinstance(var_type, dict):
                            t = _simplify_typ(var_type)
                    out.append(Param(name=p.get("name", "?"), type=t))
        return out
    if kind == "P_unit":
        return []
    if kind == "P_typ":
        # Type-annotated pattern: [Typ, inner_pat]. Recurse on inner_pat.
        if isinstance(payload, list) and len(payload) == 2:
            return _params_from_pat(payload[1])
        return []
    if kind == "P_id":
        # payload is the dict that holds {"P_id": {"Id": {...}}} and
        # possibly {"var_type": ...}. We need to dive into P_id.
        pid_obj = payload.get("P_id") if isinstance(payload, dict) else None
        if isinstance(pid_obj, dict) and "Id" in pid_obj:
            name = pid_obj["Id"].get("id", "?")
        elif isinstance(payload, dict) and "Id" in payload:
            name = payload["Id"].get("id", "?")
        else:
            name = "?"
        # var_type lives on the inner pat dict (alongside P_id).
        vt = payload.get("var_type") if isinstance(payload, dict) else None
        tt = payload.get("typ") if isinstance(payload, dict) else None
        if vt is None:
            vt = node.get("var_type") if isinstance(node, dict) else None
        if tt is None:
            tt = node.get("typ") if isinstance(node, dict) else None
        t = _simplify_typ(vt) or _simplify_typ(tt)
        return [Param(name=name, type=t)]
    return []


def _simplify_extern(val_spec: Any) -> ExternDecl | None:
    if not isinstance(val_spec, dict):
        return None
    vs = val_spec.get("VS_val_spec")
    if not isinstance(vs, list) or not vs:
        return None
    # The shape is: [{TypSchm}, bindings..., extern_attr]
    head = vs[0]
    if not isinstance(head, dict) or "TypSchm" not in head:
        return None
    typschm = head["TypSchm"]
    name = None
    typ_obj = None
    if isinstance(typschm, dict) and "typschm" in typschm:
        tso = typschm["typschm"]
        if isinstance(tso, dict) and "TypSchm_ts" in tso:
            ts_arr = tso["TypSchm_ts"]
            if isinstance(ts_arr, list) and ts_arr:
                # First is quantifiers, last is the type
                first = ts_arr[0]
                if isinstance(first, dict) and "TypQ_no_forall" not in first:
                    # Has quantifiers; name lives elsewhere
                    pass
                last = ts_arr[-1]
                if isinstance(last, dict) and "Typ" in last:
                    typ_obj = last["Typ"]

    # The name and extern target come from the bindings list (after the TypSchm).
    extern_target = None
    extern_type = None
    for x in vs[1:]:
        if not isinstance(x, dict):
            continue
        if "Id" in x:
            if name is None:
                name = x["Id"].get("id", "?")
            else:
                extern_target = x["Id"].get("id")
        if "pure" in x:
            extern_type = "pure"
        if "c" in x:
            extern_type = "c"
        if "ocaml" in x:
            extern_type = "ocaml"
        if "lem" in x:
            extern_type = "lem"
        if "asm" in x:
            extern_type = "asm"
        if "bindings" in x:
            # Sometimes the extern string is here.
            b = x["bindings"]
            if isinstance(b, list) and b:
                for entry in b:
                    if isinstance(entry, list) and len(entry) == 2:
                        if entry[0] == "_" and extern_target is None:
                            extern_target = entry[1]
                        elif extern_target is None:
                            extern_target = entry[1]

    return ExternDecl(
        name=name or "?",
        extern_target=extern_target,
        extern_type=extern_type,
        type=_simplify_typ(typ_obj),
        loc=_loc(head),
    )


# ---------------------------------------------------------------------------
# Driver
# ---------------------------------------------------------------------------


def simplify(raw: dict, type_tables: list[str]) -> SimplifiedModel:
    structs, enums, aliases, abstracts = _simplify_types(raw.get("types", []), type_tables)
    functions, externs = _simplify_functions(raw.get("ast", []), type_tables)

    # Collect source files referenced anywhere
    files: set[str] = set()

    def walk_files(obj: Any) -> None:
        if isinstance(obj, dict):
            if "file" in obj and isinstance(obj["file"], str):
                files.add(obj["file"])
            for v in obj.values():
                walk_files(v)
        elif isinstance(obj, list):
            for v in obj:
                walk_files(v)

    walk_files(raw)
    return SimplifiedModel(
        structs=structs,
        enums=enums,
        aliases=aliases,
        abstracts=abstracts,
        functions=functions,
        externs=externs,
        source_files=sorted(files),
    )


def main() -> int:
    parser = argparse.ArgumentParser(description="Simplify sail --json output for transpiler authors.")
    parser.add_argument("raw_json", type=Path, help="Path to raw JSON produced by sail --json.")
    parser.add_argument("--out", type=Path, default=None, help="Output path (default: <raw_json>.simplified.json).")
    parser.add_argument("--type-prefix", action="append", default=[], help="Source-path prefix to consider as 'model source' (for source refs).")
    args = parser.parse_args()

    type_tables = list(args.type_prefix)

    with args.raw_json.open() as f:
        raw = json.load(f)

    model = simplify(raw, type_tables)
    out_path = args.out or args.raw_json.with_suffix(".simplified.json")
    with out_path.open("w") as f:
        json.dump(model.model_dump(exclude_none=True), f, indent=2)
    print(f"wrote {out_path} ({len(model.structs)} structs, {len(model.enums)} enums, {len(model.aliases)} aliases, {len(model.functions)} funcs, {len(model.externs)} externs)", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())