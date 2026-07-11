# JSON backend findings

Verification of `sail --json` output against the contract
"recursively monomorphised, strictly typed, bitvectors have concrete sizes".

## What I ran

Two sail-riscv invocations (chosen because the second is the simplest one
that produces any non-trivial JSON at all):

1. `sail --auto-mono --json --json-output-dir /tmp/... -o riscv model/riscv.sail_project`
   — the **only** invocation that completes without segfaulting or hitting
   a monomorphisation error. This loads the `main` module of sail-riscv
   plus its transitive dependencies *as reachable from `main`*. Yields
   `riscv.json` (689 KB, 18 408 lines).

2. `sail --auto-mono --json --json-output-dir /tmp/... -o test test/c/bitvector.sail`
   — a small in-tree test file. Yields `test.json` (37 430 lines, 168
   top-level definitions including 17 fundefs) for sanity-checking the
   verifier on a richer tree.

## Verifier

`scripts/verify_json.py` walks the JSON and reports three classes of issue:

- **Polymorphic markers**: `Typ_var`, `Nexp_var`, `Nexp_id`, `TypQ_tq`,
  `QI_id`. Any of these in the output means the AST still contains
  unresolved type or kind variables.
- **Bitvector sizes**: any `Typ_app ("bits"|"bitvector", [...])` whose
  `"size"` field is not a JSON integer. The printer attaches `"size": null`
  when the size could not be resolved, or `"size": "'n"` when it is still a
  kind reference.
- **Untyped nodes**: any `Pat` / `E` / `LE` / `FE` / `MP` / `MPat` wrapper
  that lacks `typ`, `var_type`, `ref_type`, `match_arm_typ` or
  `param_types`. Also checks that `DEF_fundef` with
  `Typ_annot_opt_some` emits a `param_types` sibling.

## Results

### riscv.json (main-module-only run)

```
AST definitions: 14
  by kind: {'DEF_val': 13, 'DEF_fundef': 1}
  fundefs seen: 1
Polymorphic markers: 26
Bitvector/bits Typ_app occurrences: 2
  with concrete "size" field: 0
  without concrete size: 2  (bitvector<1>, bitvector<'n>)
Untyped AST nodes: 0
Functions (Typ_annot_opt_some) missing "param_types": 0
```

### test/c/bitvector.sail JSON

```
AST definitions: 168
  by kind: {'DEF_val': 94, 'DEF_pragma': 13, 'DEF_default': 1,
            'DEF_type': 2, 'DEF_fundef': 17, 'DEF_overload': 31,
            'DEF_fixity': 1}
  fundefs seen: 17
Polymorphic markers: 793
Bitvector/bits Typ_app occurrences: 2
  with concrete "size" field: 0
  without concrete size: 2  (both bitvector<'n>)
Untyped AST nodes: 0
Functions (Typ_annot_opt_some) missing "param_types": 0
```

## What this means for the contract

### ✅ Pass: strictly typed

Every `Pat`/`E`/`LE`/`FE`/`MP`/`MPat` in both outputs carries a `typ` (or
`var_type`/`ref_type`/`match_arm_typ`/`param_types`) field. Every fundef
with a `Typ_annot_opt_some` carries the sibling `param_types` field. The
printer is faithful here — *given* the typed AST the typechecker hands it,
every node that should carry a type annotation does carry one.

### ❌ Fail: not recursively monomorphised

26 polymorphic markers in the riscv run, 793 in the small test run.
They are **not random** — every single one lives inside a `DEF_val`
extern declaration (e.g. `undefined_bool`, `undefined_int`, `internal_pick`,
`vector_subrange`, `eq_bits`, ...). These come from `lib/prelude.sail`,
`lib/vector.sail`, etc. The `monomorphise` rewriter intentionally leaves
extern declarations alone because they have no body to copy. The output
keeps their `forall`/`'a`/`'n` kind variables verbatim.

In the small `bitvector.sail` run, the polymorphic markers are concentrated
in those same library `val` declarations. The single fundef present
(`TD_record foo`, `bitvector<'n>`) is reachable from a polymorphic
context and was not specialised — that's why we see
`ast/[96]/DEF_fundef/.../P_tuple/[0]/Pat/typ` carrying `bitvector<'n>`.

The path forward is to decide what the contract means for externs:

- **Option A**: externs are *signatures* and may legitimately stay
  polymorphic; consumers must monomorphise them when emitting a
  concrete call. Then the verifier should ignore `DEF_val` externs
  and only flag `Typ_var`/`Nexp_var` inside `DEF_fundef`, `DEF_type`,
  `DEF_internal_mutrec`, etc.
- **Option B**: every binding in the output must be ground. Then the
  rewriter pipeline needs an additional pass that *either* drops
  unreachable polymorphic externs (the existing
  `--json-roots`/`--json-filter-unreachable` is meant for this) *or*
  emits a concrete `bits(0)` placeholder for the polymorphic size.

For Option B, the current `--auto-mono` does not succeed on sail-riscv
with `--all-modules`: the rewriter fails with

```
Unable to monomorphise '_#plat_cache_block_size_exp: Unknown type variable
Unable to monomorphise dependency: Recursive call of pt_walk
Unable to monomorphise (8 * 'bytes): Effects from function application
```

and without `--auto-mono`, the JSON target segfaults during
`Pretty_print_json.pp_ast_json`. So the only working invocation today
is the conservative one above, which does not run monomorphise
without `--auto-mono`, and runs it without `--all-modules`, leaving
externs polymorphic.

### ❌ Fail: bitvector sizes not always concrete

In both outputs every `Typ_app ("bits"|"bitvector", [...])` carries a
`"size"` field, but **none of them is a concrete integer** — they are
either `"'n"` (kid reference) or `null` (unresolved). The printer only
attaches a `"size"` field for the `Typ_app` form; **bitvector aliases**
like `xlenbits` / `flenbits` are emitted as `Typ_id` with no `size`
attribute at all, so a downstream consumer that expects to read
`size` from every bitvector reference will see `null` everywhere.

For downstream Rust/C transpilers this is the most material gap: they
either have to walk `DEF_type` (or a separately-emitted type table) to
resolve `Typ_id` aliases themselves, *or* the JSON printer needs to be
extended to (a) emit a type definition table alongside the AST and
(b) inline the resolved bitvector size next to every `Typ_id` reference
whose target is a bitvector-shaped alias.

## Recommended changes

1. **Make the rewriter pipeline survive the full sail-riscv AST.**
   Today `--all-modules` without `--auto-mono` segfaults in the printer,
   and with `--auto-mono` it errors out on `pt_walk` recursion and
   `Effects from function application`. Either fix those or add a
   `--json-strict-poly` flag that drops un-monomorphisable definitions
   with a warning.
2. **Emit a resolved type table.** A `types: [...]` sibling of `ast:`
   listing every type definition with its expanded bitvector size (for
   aliases, the substituted `Nexp_constant` if known) lets downstream
   consumers resolve `Typ_id` references without re-walking the
   rewrite history.
3. **Always inline `size` on `Typ_id` references** that point at
   bitvector-shaped aliases (after the type table is in place, or by
   passing the env to every `json_of_typ` call).
4. **Add a CI test**: run the JSON backend on a representative ISA
   fragment, run `scripts/verify_json.py --strict`, and fail the build
   when the verifier reports any issue.