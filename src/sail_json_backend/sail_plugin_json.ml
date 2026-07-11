(****************************************************************************)
(*     Sail                                                                 *)
(*                                                                          *)
(*  Sail and the Sail architecture models here, comprising all files and    *)
(*  directories except the ASL-derived Sail code in the aarch64 directory,  *)
(*  are subject to the BSD two-clause licence below.                        *)
(*                                                                          *)
(*  The ASL derived parts of the ARMv8.3 specification in                   *)
(*  aarch64/no_vector and aarch64/full are copyright ARM Ltd.               *)
(*                                                                          *)
(*  Copyright (c) 2013-2026                                                 *)
(*    Sail contributors                                                     *)
(*                                                                          *)
(*  SPDX-License-Identifier: BSD-2-Clause                                   *)
(****************************************************************************)

(** Sail plugin that exports the typed AST as JSON.

    Invoked via [`sail -json [-o <prefix>]`]. The plugin writes one file
    per source-file base-name (extension `.json`). With `-o`, files are
    named `<prefix>.json`; otherwise each source `model/foo.sail` becomes
    `foo.json` in the current directory.

    The JSON preserves every identifier's spelling from the source -
    mono-suffixed names such as `read_bits_8` round-trip verbatim because
    we do not unmangle or re-mangle anything. *)

open Libsail

open Interactive.State

let opt_json_output_dir : string option ref = ref None
let opt_json_filter_unreachable : bool ref = ref false

(* Rewriter pipeline applied when --auto-mono (or --mono-split) is passed.
   Without a rewrite pipeline, polymorphic bitvector sizes like 'n
   remain polymorphic in the JSON even when --auto-mono is set, because
   the monomorphise rewriter only runs if the target includes it.
   Users who want a fully-monomorphised AST should pass --auto-mono.

   When --json-roots is set, the [filter_unreachable] pass drops every
   top-level definition that the listed entry points do not reach, so
   un-monomorphisable code never enters the monomorphise pipeline. The
   rewriter reads the roots from [Rewrites.opt_filter_unreachable_roots]
   at run time, so the option must be set before the pipeline runs. *)
let json_rewrites =
  let open Rewrites in
  [
    ("filter_unreachable", [String_arg "json"; If_flag opt_json_filter_unreachable]);
    ("instantiate_outcomes", [String_arg "json"]);
    ("realize_mappings", []);
    ("remove_vector_subrange_pats", []);
    ("toplevel_string_append", []);
    ("pat_string_append", []);
    ("mapping_patterns", []);
    ("truncate_hex_literals", []);
    ("mono_rewrites", [If_flag opt_mono_rewrites]);
    ("recheck_defs", [If_flag opt_mono_rewrites]);
    ("toplevel_nexps", [If_mono_arg]);
    ("monomorphise", [String_arg "json"; If_mono_arg]);
    ("atoms_to_singletons", [String_arg "json"; If_mono_arg]);
    ("recheck_defs", [If_mono_arg]);
    ("add_bitvector_casts", [If_mono_arg]);
    ("undefined", [Bool_arg false]);
    ("remove_not_pats", []);
    ("drop_uninstantiated_polymorphic", [If_flag Rewrites.opt_drop_uninstantiated_polymorphic]);
  ]

let json_options =
  [
    ( Flag.create ~prefix:[ "json" ] "output_dir",
      Arg.String (fun dir -> opt_json_output_dir := Some dir),
      "<dir> write generated JSON files into this directory"
    );
    ( Flag.create ~prefix:[ "json" ] "roots",
      Arg.String (fun s -> Rewrites.opt_filter_unreachable_roots := s),
      "<id1,id2,...> comma-separated entry-point function ids whose transitive \
       call graph is kept in the output; everything else is dropped. Useful for \
       stripping un-monomorphisable library code from the JSON. Has no effect \
       unless --json-filter_unreachable is also passed."
    );
    ( Flag.create ~prefix:[ "json" ] "filter_unreachable",
      Arg.Set opt_json_filter_unreachable,
      " enable the --json-roots reachability filter (off by default)"
    );
    ( Flag.create ~prefix:[ "json" ] "strict_poly",
      Arg.Set Rewrites.opt_drop_uninstantiated_polymorphic,
      " drop top-level definitions whose body still contains polymorphic \
       markers (Typ_var, TypQ_tq, Nexp_var) after monomorphisation. Useful \
       together with --auto-mono to produce a fully-ground JSON AST. \
       Extern val declarations (DEF_val) are intentionally kept because \
       they are signatures."
    );
  ]

let ensure_dir dir =
  try
    if not (Sys.is_directory dir) then (
      Printf.eprintf "sail-plugin-json: %s exists but is not a directory\n" dir;
      exit 1
    )
  with Sys_error _ ->
    let cmd = Printf.sprintf "mkdir -p %s" dir in
    if Sys.command cmd <> 0 then (
      Printf.eprintf "sail-plugin-json: failed to create directory %s\n" dir;
      exit 1
    )

let write_json outfile json =
  let oc = open_out outfile in
  output_string oc (Yojson.Safe.pretty_to_string json);
  close_out oc;
  Printf.eprintf "sail-plugin-json: wrote %s\n" outfile

let json_target out_file { ast; env; ctx = _; default_sail_dir = _; _ } =
  let prefix =
    match out_file with
    | Some f -> f
    | None -> "out"
  in
  (match !opt_json_output_dir with
   | Some dir -> ensure_dir dir
   | None -> ());
  let outdir = match !opt_json_output_dir with Some d -> d | None -> "." in
  let outpath = Filename.concat outdir (prefix ^ ".json") in
  write_json outpath (Pretty_print_json.pp_ast_json env ast)

let _ =
  Target.register ~name:"json" ~options:json_options
    ~rewrites:json_rewrites
    ~description:"export the typed AST as a JSON file" json_target
