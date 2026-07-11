# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

Sail is a language for defining ISA semantics. The `sail` compiler type-checks `.sail` source files (using Z3 for dependent type and bitvector-length constraints) and emits backends such as C, OCaml, Coq/Rocq, Lean, Lem, SMT-LIB, SystemVerilog, LaTeX, JSON, and interactive interpreter/REPL output.

`dune-project` defines the package set: `libsail` (the compiler), `sail_maker` (tarball helper), plus one opam package per backend (`sail_c_backend`, `sail_ocaml_backend`, `sail_smt_backend`, `sail_sv_backend`, `sail_lem_backend`, `sail_coq_backend` / `rocq` target, `sail_lean_backend`, `sail_latex_backend`, `sail_doc_backend`, `sail_json_backend`, `sail_output` plugin), and the `sail` umbrella that depends on them all. The version lives in both `dune-project` (line 7) and `src/bin/sail.ml` (`let version = ...`); keep them in sync.

## Build, install, and toolchain

The project uses OCaml ≥ 4.14 with opam (CI tests 4.14.3 and 5.2.1 on Linux/macOS/Windows). System deps: `build-essential libgmp-dev z3 cvc4 pkg-config` on Ubuntu. `INSTALL.md` covers the full opam flow.

```
make                # dune build --release
make install        # dune install
make clean          # dune clean
make tarball        # build redistributable tarball (uses sail_maker)
make coverage       # dune build with bisect_ppx
make asciidoc       # regenerate doc/manual.html
```

For a single-package build (e.g. working on just the OCaml backend): `dune build src/sail_oc_backend`. The repo root `sail` script is a wrapper that sets `SAIL_DIR` and `DUNE_DIR_LOCATIONS` (plugin lookup) before invoking the installed binary — use it instead of calling `dune exec` directly because of dune's global lock that breaks concurrent test runs.

## Tests

```
make core-tests     # lexing/pattern_completeness/typecheck/ocaml/float
make c-tests        # C backend tests under test/c/
make test           # full suite: core + lem + mono + latex + c + smt + builtins + arm + lean
```

Test drivers live under `test/<group>/run_tests.py` and all import `test/sailtest.py`, which defines `chunks`, `Results`, and reads `SAIL_DIR` / `SAIL` / `TEST_PAR` env vars (parallelism defaults to 16). Useful flags: `--test <name>` to run a single test, `--update-expected` to refresh `.expect` golden files, `--seq` for serial, `--targets c,ocaml` to pick backends.

CI runs two relevant workflows: `.github/workflows/build.yml` (multi-OS multi-OCaml build matrix) and `.github/workflows/test-matrix.yml` (per-suite coverage runs with bisect_ppx). The formatting workflow runs `dune fmt` and fails on any diff — formatting is enforced at PR time.

## Architecture / pipeline

`src/bin/sail.ml` is the entry point. CLI flags populate refs in `Sail_options`, then `run_sail` drives the pipeline. The high-level flow is:

1. **Parsing** — `src/lib/lexer.mll` → `parser.mly` → `infix_parser.mly` → `parse_ast.ml` (also `project_parser.mly` / `project_lexer.mll` for `.sail_project` files).
2. **Frontend** (`src/lib/frontend.ml`) — loads files or project modules, runs `initial_check.ml` (name resolution, scoping), then `effects.ml` (side-effect inference) and `type_check.ml` / `type_env.ml` (the big dependent type checker, which uses `constraint.ml` + Z3 via `smt_gen.ml` / `smt_exp.ml`).
3. **Configuration / instantiation** — `Frontend.instantiate_abstract_types`, `Config.rewrite_ast`, optional `Splice.splice_files`.
4. **Initial rewrites** — `Rewrites.rewrite` / `rewrites.ml` + `initial_rewrite.ml` apply the sequence registered with the chosen target.
5. **Backend action** — each target's registered action produces the final output.

Backends are loaded as plugins via the `-plugin` flag or statically registered in their own `src/sail_<x>_backend/sail_plugin_<x>.ml` via `Target.register ~name:"..."`. A target's `action` is the function called with `{ ctx; ast; effect_info; env; default_sail_dir; config }` after rewrites. See `src/lib/target.mli` for the full target record and hook set (`pre_parse_hook`, `pre_initial_check_hook`, `pre_rewrites_hook`, `rewrites`, `asserts_termination`, `supports_abstract_types`, `supports_runtime_config`, `skip_initial_rewrite`). New backends normally follow the pattern in `src/sail_json_backend/sail_plugin_json.ml`.

The internal SSA-like IR used by the SMT and SystemVerilog backends is `jib_*` (`jib_compile.ml`, `jib_ssa.ml`, `jib_optimize.ml`, `jib_util.ml`, `jib_visitor.ml`). The Lem/Coq backends go through Lem extraction (`src/lib/extraction/`, `src/gen_lib/sail2_*.lem`) — the Lem-side core semantics are now extracted from Rocq (per CHANGELOG 0.20).

The interactive REPL uses `interactive.ml` / `interpreter.ml`; see `src/bin/repl.enabled.ml` and the `linenoise` dependency (Unix only).

The shared Sail standard library (prelude, bitvector/float/vector/exception/elf/mono_rewrites) lives in `lib/*.sail`; `$include <foo.sail>` resolves to `<sail-share>/libsail/`. The C runtime (`lib/rts.c`, `lib/sail.h`, `lib/sail.c`, `lib/sail_config.*`, `lib/elf.*`) is what the C/C++ backend links against.

`sailcov` (Rust + OCaml in `sailcov/` and `lib/coverage/`) is a separate coverage visualizer: compile with `sail -c -c_coverage <out> -c_include sail_coverage.h`, link with `lib/coverage/target/release/libsail_coverage.a -lpthread -ldl`, then run `sailcov -a <branches> -t sail_coverage model.sail`. Build with `make -C lib/coverage` (cargo).

## Conventions

- Code style is governed by `.ocamlformat` (profile `default`, version `0.27.0`, margin `120`); files listed in `.ocamlformat-ignore` (`lib/**`, `src/lib/rocq/**`, `src/lib/extraction/*`) are exempt.
- Each new file in `src/lib/` and the backends must carry the BSD-2-Clause header used throughout `src/lib/*.ml` — see any existing file for the template.
- The build is dune-only; `Makefile` just wraps dune for ergonomics.
- Plugins are discovered via the `libsail` opam `sites` (the root `sail` script sets `DUNE_DIR_LOCATIONS="libsail:share:$SAIL_DIR/_build/install/default/share/libsail"`).
- Z3 is invoked as a subprocess; results can be memoized to `sail_smt_cache` (configurable via `-memo_z3_path`, disable with `-no_memo_z3`).