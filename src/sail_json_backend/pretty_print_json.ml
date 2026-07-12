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

(** JSON pretty-printer for Sail ASTs.

    The output schema keys each AST node by its polymorphic-variant
    constructor name (e.g. `"E_id"`, `"DEF_type"`, `"TD_record"`), which
    makes the JSON zero-ambiguity for downstream Rust/C transpilers.

    Names defined in the source are preserved verbatim. Polymorphic
    identifiers keep their raw string form so a name like
    [`'n : Int`] round-trips as `"'n"` (with the leading tick) and a
    mono-suffixed name like `foo_bits_8` round-trips as `"foo_bits_8"`.
    No re-mangling is performed by this printer - monomorphisation
    produces a renamed definition, and we faithfully emit whatever name
    it carries. *)

open Libsail

open Ast
open Ast_defs
open Ast_util

module Big_int = Nat_big_num

(* ------------------------------------------------------------------ *)
(* Helpers                                                            *)
(* ------------------------------------------------------------------ *)

let loc_to_string l = `String (simple_string_of_loc l)

let with_loc_opt l fields =
  match l with
  | Parse_ast.Unknown -> `Assoc fields
  | _ -> `Assoc (("loc", loc_to_string l) :: fields)

let jl xs = `List xs

let pl f xs = List.map f xs

let json_of_bool b = `Bool b

let json_of_int i = `Int i

let json_of_big_int n = `String (Big_int.to_string n)

let json_of_string s = `String s

let json_of_option f = function None -> `Null | Some x -> f x

let json_of_visibility = function
  | Public -> `String "Public"
  | Private _ -> `String "Private"

let json_of_prec = function
  | Infix -> `String "Infix"
  | InfixL -> `String "InfixL"
  | InfixR -> `String "InfixR"

let json_of_hex_digit = function
  | Hex_0 -> `String "Hex_0"
  | Hex_1 -> `String "Hex_1"
  | Hex_2 -> `String "Hex_2"
  | Hex_3 -> `String "Hex_3"
  | Hex_4 -> `String "Hex_4"
  | Hex_5 -> `String "Hex_5"
  | Hex_6 -> `String "Hex_6"
  | Hex_7 -> `String "Hex_7"
  | Hex_8 -> `String "Hex_8"
  | Hex_9 -> `String "Hex_9"
  | Hex_A -> `String "Hex_A"
  | Hex_B -> `String "Hex_B"
  | Hex_C -> `String "Hex_C"
  | Hex_D -> `String "Hex_D"
  | Hex_E -> `String "Hex_E"
  | Hex_F -> `String "Hex_F"

let json_of_bin_digit = function
  | Bin_0 -> `String "Bin_0"
  | Bin_1 -> `String "Bin_1"

let json_of_non_empty f (Non_empty (hd, tl)) = `List (List.map f (hd :: tl))

(* ------------------------------------------------------------------ *)
(* Polymorphic recursive printers                                     *)
(*                                                                    *)
(* Each printer is annotated `type a b. ...` so it abstracts over    *)
(* the polymorphic AST type variables. OCaml 5.3 honours this across  *)
(* mutually-recursive `let rec ... and ...` blocks.                  *)
(* ------------------------------------------------------------------ *)

(* Tracks whether [tannot_of_annot] has ever fallen back to the empty
   tannot because the input annot was untyped. The flag lives outside the
   mutually-recursive printer block because OCaml's [let rec ... and ...]
   binding form does not let us share ordinary [let] declarations across
   siblings - only [let rec]/[let] with the same shape. *)
let tannot_untyped_warned = ref false

let rec json_of_id_aux : Ast.id_aux -> Yojson.Safe.t = function
  | Id s -> `String s
  | Operator s -> `Assoc [ ("Operator", `String s) ]
  | And_bool -> `String "And_bool"
  | Or_bool -> `String "Or_bool"

and json_of_id (Id_aux (aux, l)) =
  `Assoc [ ("Id", with_loc_opt l [ ("id", json_of_id_aux aux) ]) ]

and json_of_kid_aux : Ast.kid_aux -> Yojson.Safe.t = function
  | Var s -> `String s

and json_of_kid (Kid_aux (aux, l)) =
  `Assoc [ ("Var", with_loc_opt l [ ("kid", json_of_kid_aux aux) ]) ]

and json_of_kind_aux : Ast.kind_aux -> Yojson.Safe.t = function
  | K_type -> `String "K_type"
  | K_int -> `String "K_int"
  | K_bool -> `String "K_bool"

and json_of_kind (K_aux (aux, l)) =
  `Assoc [ ("Kind", with_loc_opt l [ ("kind", json_of_kind_aux aux) ]) ]

and json_of_kinded_id (KOpt_aux (aux, l)) =
  let payload =
    match aux with
    | KOpt_kind (k, kid) ->
        `Assoc [ ("KOpt_kind", `List [ json_of_kind k; json_of_kid kid ]) ]
  in
  `Assoc [ ("KOpt_kind", with_loc_opt l [ ("kinded_id", payload) ]) ]

and json_of_struct_name = function
  | SN_id id -> `Assoc [ ("SN_id", json_of_id id) ]
  | SN_anon -> `String "SN_anon"

and json_of_field_pat_wildcard = function
  | FP_wild _ -> `String "FP_wild"
  | FP_no_wild -> `String "FP_no_wild"

and json_of_loop = function
  | While -> `String "While"
  | Until -> `String "Until"

and json_of_order_aux = function
  | Ord_inc -> `String "Ord_inc"
  | Ord_dec -> `String "Ord_dec"

and json_of_order (Ord_aux (aux, l)) =
  `Assoc [ ("Ord", with_loc_opt l [ ("order", json_of_order_aux aux) ]) ]

and json_of_nexp_aux n =
  match n with
  | Nexp_id id -> `Assoc [ ("Nexp_id", json_of_id id) ]
  | Nexp_var kid -> `Assoc [ ("Nexp_var", json_of_kid kid) ]
  | Nexp_constant n -> `Assoc [ ("Nexp_constant", json_of_big_int n) ]
  | Nexp_app (id, args) ->
      `Assoc [ ("Nexp_app", `List (json_of_id id :: pl json_of_nexp args)) ]
  | Nexp_if (c, t, e) ->
      `Assoc
        [ ( "Nexp_if",
            `List [ json_of_n_constraint c; json_of_nexp t; json_of_nexp e ] )
        ]
  | Nexp_times (a, b) ->
      `Assoc [ ("Nexp_times", `List [ json_of_nexp a; json_of_nexp b ]) ]
  | Nexp_sum (a, b) ->
      `Assoc [ ("Nexp_sum", `List [ json_of_nexp a; json_of_nexp b ]) ]
  | Nexp_minus (a, b) ->
      `Assoc [ ("Nexp_minus", `List [ json_of_nexp a; json_of_nexp b ]) ]
  | Nexp_exp n -> `Assoc [ ("Nexp_exp", json_of_nexp n) ]
  | Nexp_neg n -> `Assoc [ ("Nexp_neg", json_of_nexp n) ]

and json_of_nexp (Nexp_aux (aux, l)) =
  `Assoc [ ("Nexp", with_loc_opt l [ ("nexp", json_of_nexp_aux aux) ]) ]

and json_of_n_constraint_aux c =
  match c with
  | NC_equal (a, b) ->
      `Assoc [ ("NC_equal", `List [ json_of_typ_arg a; json_of_typ_arg b ]) ]
  | NC_not_equal (a, b) ->
      `Assoc
        [ ("NC_not_equal", `List [ json_of_typ_arg a; json_of_typ_arg b ]) ]
  | NC_ge (a, b) -> `Assoc [ ("NC_ge", `List [ json_of_nexp a; json_of_nexp b ]) ]
  | NC_gt (a, b) -> `Assoc [ ("NC_gt", `List [ json_of_nexp a; json_of_nexp b ]) ]
  | NC_le (a, b) -> `Assoc [ ("NC_le", `List [ json_of_nexp a; json_of_nexp b ]) ]
  | NC_lt (a, b) -> `Assoc [ ("NC_lt", `List [ json_of_nexp a; json_of_nexp b ]) ]
  | NC_set (n, ns) ->
      `Assoc [ ("NC_set", `List [ json_of_nexp n; jl (pl json_of_big_int ns) ]) ]
  | NC_and (a, b) ->
      `Assoc
        [ ("NC_and", `List [ json_of_n_constraint a; json_of_n_constraint b ]) ]
  | NC_or (a, b) ->
      `Assoc
        [ ("NC_or", `List [ json_of_n_constraint a; json_of_n_constraint b ]) ]
  | NC_app (id, args) ->
      `Assoc [ ("NC_app", `List (json_of_id id :: pl json_of_typ_arg args)) ]
  | NC_id id -> `Assoc [ ("NC_id", json_of_id id) ]
  | NC_var kid -> `Assoc [ ("NC_var", json_of_kid kid) ]
  | NC_true -> `String "NC_true"
  | NC_false -> `String "NC_false"

and json_of_n_constraint (NC_aux (aux, l)) =
  `Assoc [ ("NC", with_loc_opt l [ ("n_constraint", json_of_n_constraint_aux aux) ]) ]

and json_of_typ_arg ta =
  match ta with
  | A_aux (A_nexp n, l) ->
      `Assoc [ ("A_nexp", with_loc_opt l [ ("typ_arg", json_of_nexp n) ]) ]
  | A_aux (A_typ t, l) ->
      `Assoc [ ("A_typ", with_loc_opt l [ ("typ_arg", json_of_typ t) ]) ]
  | A_aux (A_bool c, l) ->
      `Assoc [ ("A_bool", with_loc_opt l [ ("typ_arg", json_of_n_constraint c) ]) ]

and resolve_kid_size (env : Type_check.Env.t) (kid : Ast.kid) : Yojson.Safe.t option =
  let open Ast in
  let nexp = Nexp_aux (Nexp_var kid, Parse_ast.Unknown) in
  match Type_check.solve_unique env nexp with
  | Some n -> Some (json_of_big_int n)
  | None -> None

(* Walk a type and return Some (size-in-json) if the type resolves,
   directly or via a chain of type-alias [type x = ...] definitions,
   to a bitvector of known size. Returns None otherwise. *)
and resolve_bitvector_size (env : Type_check.Env.t) (t : typ) : Yojson.Safe.t option =
  let open Ast in
  let rec follow (t : typ) =
    match t with
    | Typ_aux (Typ_app (id, [ A_aux (A_nexp n, _) ]), _)
      when string_of_id id = "bitvector" || string_of_id id = "bits" ->
        size_of_nexp n
    | Typ_aux (Typ_id id, _) -> (
        match Ast_compare.Bindings.find_opt id (Type_check.Env.get_typ_synonyms env) with
        | Some (_, A_aux (A_typ t', _)) -> follow t'
        | _ -> None)
    | _ -> None
  and size_of_nexp = function
    | Nexp_aux (Nexp_constant n, _) -> Some (json_of_big_int n)
    | Nexp_aux (Nexp_var kid, _) -> resolve_kid_size env kid
    | _ -> None
  in
  follow t

and json_of_typ_aux ?(env = Type_check.Env.empty) t =
  let bitvector_size =
    match t with
    | Typ_app (id, [ A_aux (A_nexp n, _) ])
      when string_of_id id = "bitvector" || string_of_id id = "bits" ->
        Some
          (match n with
           | Nexp_aux (Nexp_constant n, _) -> json_of_big_int n
           | Nexp_aux (Nexp_var kid, _) -> begin
               match resolve_kid_size env kid with
               | Some s -> s
               | None -> `String (Ast_util.string_of_kid kid)
             end
           | _ -> `Null)
    | Typ_id _ -> resolve_bitvector_size env (Typ_aux (t, Parse_ast.Unknown))
    | _ -> None
  in
  let base_assoc =
    match t with
    | Typ_internal_unknown -> `String "Typ_internal_unknown"
    | Typ_id id -> `Assoc [ ("Typ_id", json_of_id id) ]
    | Typ_var kid -> `Assoc [ ("Typ_var", json_of_kid kid) ]
    | Typ_fn (args, ret) ->
        `Assoc
          [ ( "Typ_fn",
              `List [ jl (pl (json_of_typ ~env) args); json_of_typ ~env ret ] )
          ]
    | Typ_bidir (a, b) ->
        `Assoc [ ("Typ_bidir", `List [ json_of_typ ~env a; json_of_typ ~env b ]) ]
    | Typ_tuple ts -> `Assoc [ ("Typ_tuple", jl (pl (json_of_typ ~env) ts)) ]
    | Typ_app (id, args) ->
        `Assoc [ ("Typ_app", `List (json_of_id id :: pl json_of_typ_arg args)) ]
    | Typ_exist (kids, c, t) ->
        `Assoc
          [ ( "Typ_exist",
              `List
                [ jl (pl json_of_kinded_id kids);
                  json_of_n_constraint c;
                  json_of_typ ~env t
                ] )
          ]
  in
  match bitvector_size with
  | Some s -> (
      match base_assoc with
      | `Assoc fields -> `Assoc (("size", s) :: fields)
      | other -> other)
  | None -> base_assoc

and json_of_typ ?env (Typ_aux (aux, l)) =
  let env = match env with None -> Type_check.Env.empty | Some e -> e in
  (* Defensive: if the printer fails on this typ (the underlying AST
     contains a typ_aux shape we did not anticipate, or the env is in
     a state the printer cannot resolve), emit a stub rather than
     propagate a Match_failure that would abort the whole JSON
     emission. The stub uses the same outer wrapper so downstream
     consumers can still parse the file. *)
  try `Assoc [ ("Typ", with_loc_opt l [ ("typ", json_of_typ_aux ~env aux) ]) ]
  with _ ->
    `Assoc
      [ ("Typ",
         with_loc_opt l
           [ ("typ", `String "Typ_printer_failure");
             ("typ_debug", `String (string_of_typ (Typ_aux (aux, l)))) ]
        )
      ]

(* ------------------------------------------------------------------ *)
(* Typed-annot helpers                                                *)
(*                                                                    *)
(* The Sail typechecker attaches a [tannot] (containing [env], [typ], *)
(* etc.) to every typed AST node via [E_aux (aux, (l, tannot))].      *)
(* The inferred type and binding env are needed to satisfy the policy *)
(* "EVERY TYPE OF EVERY PARAMETER AND VARIABLE MUST BE DEFINED. For   *)
(* bitvectors, the size must be defined too."                         *)
(*                                                                    *)
(* The JSON printer is only ever called on the typed AST, so [a] in   *)
(* ['a annot] is always [Type_check.tannot]. The [tannot_of_annot]    *)
(* helper pulls out the [tannot] (the second component of an annot).  *)
(* ------------------------------------------------------------------ *)

(* Reinterpret the polymorphic annot's second component as a [tannot] and
   only return it if [destruct_tannot] confirms it carries type
   information. For untyped annots (where the second component is a bare
   [uannot] record), the first word of that record — the [attrs] list
   head pointer — has tag 0, which [destruct_tannot] reads as [None], so
   we detect the untyped case without ever dereferencing a bogus
   [tannot'] payload. We log the first such encounter so the user knows
   the printer is silently dropping typ data. *)
and tannot_of_annot : type a. a Ast.annot -> Type_check.tannot =
 fun (_, t) ->
  let t' : Type_check.tannot = Obj.magic t in
  match Type_check.destruct_tannot t' with
  | Some _ -> t'
  | None ->
      if not !tannot_untyped_warned then begin
        Printf.eprintf
          "sail-plugin-json: warning: untyped annot encountered; downstream \
           consumers will see null typ fields for some nodes\n%!";
        tannot_untyped_warned := true
      end;
      Type_check.empty_tannot

and typ_of_annot tannot =
  match Type_check.destruct_tannot tannot with
  | Some (_env, t) -> Some t
  | None -> None

and env_of_annot tannot =
  match Type_check.destruct_tannot tannot with
  | Some (env, _) -> Some env
  | None -> None

and json_of_typ_opt ?env = function
  | None -> `Null
  | Some t -> json_of_typ ?env t

(* Look up the type of an identifier from an env. Returns [None] for
   unbound names (e.g. constructor references that the env does not
   know about, since they live in the type, not the value env). *)
and lookup_id_typ (env : Type_check.Env.t) (id : Ast.id) : typ option =
  try Some (Ast_util.lvar_typ (Type_check.Env.lookup_id id env)) with _ -> None

(* Pull the size out of a bitvector/bits typ. Returns:
   - [Some (`String n)] when the size is a concrete [Nexp_constant n]
   - [Some (`String kid)] when the size is [Nexp_var kid] (e.g. "'n")
   - [None] when the typ is not bitvector-shaped. *)
and bitvector_size_of_typ (t : typ) : Yojson.Safe.t option =
  match Type_check.destruct_bitvector Type_check.Env.empty t with
  | Some (Nexp_aux (Nexp_constant n, _)) -> Some (json_of_big_int n)
  | Some (Nexp_aux (Nexp_var (Kid_aux (Var kid, _)), _)) -> Some (`String kid)
  | _ -> None

and json_of_typquant = function
  | TypQ_aux (TypQ_tq items, l) ->
      `Assoc
        [ ( "TypQ_tq",
            with_loc_opt l [ ("typquant", jl (pl json_of_quant_item items)) ] )
        ]
  | TypQ_aux (TypQ_no_forall, l) ->
      `Assoc [ ("TypQ_no_forall", with_loc_opt l []) ]

(* Destruct a function type into its argument types. Recurses through
   curried [Typ_fn (args, ret)] applications. *)
and param_types_of_typ t =
  match t with
  | Typ_aux (Typ_fn (args, ret), _) -> args @ param_types_of_typ ret
  | _ -> []

and json_of_quant_item = function
  | QI_aux (QI_id kid, l) ->
      `Assoc [ ("QI_id", with_loc_opt l [ ("quant_item", json_of_kinded_id kid) ]) ]
  | QI_aux (QI_constraint c, l) ->
      `Assoc
        [ ("QI_constraint", with_loc_opt l [ ("quant_item", json_of_n_constraint c) ])
        ]

and json_of_lit (L_aux (aux, l)) =
  let payload =
    match aux with
    | L_unit -> `String "L_unit"
    | L_true -> `String "L_true"
    | L_false -> `String "L_false"
    | L_num n -> `Assoc [ ("L_num", json_of_big_int n) ]
    | L_hex hex ->
        `Assoc
          [ ("L_hex",
             `List
               (List.map
                  (fun ne -> json_of_non_empty json_of_hex_digit ne)
                  hex))
          ]
    | L_bin bin ->
        `Assoc
          [ ("L_bin",
             `List
               (List.map
                  (fun ne -> json_of_non_empty json_of_bin_digit ne)
                  bin))
          ]
    | L_string s -> `Assoc [ ("L_string", `String s) ]
    | L_real r ->
        let q = Util.Rational.from_rocq r in
        `Assoc
          [ ( "L_real",
              `List [ json_of_big_int q.Q.num; json_of_big_int q.Q.den ] )
          ]
  in
  `Assoc [ ("Lit", with_loc_opt l [ ("lit", payload) ]) ]

and json_of_typ_pat_aux = function
  | TP_wild -> `String "TP_wild"
  | TP_var kid -> `Assoc [ ("TP_var", json_of_kid kid) ]
  | TP_app (id, args) ->
      `Assoc [ ("TP_app", `List (json_of_id id :: pl json_of_typ_pat args)) ]

and json_of_typ_pat (TP_aux (aux, l)) =
  `Assoc [ ("TP", with_loc_opt l [ ("typ_pat", json_of_typ_pat_aux aux) ]) ]

and json_of_pat_aux : type a. a Ast.pat_aux -> Type_check.tannot -> Yojson.Safe.t
    =
 fun pat tannot ->
  let pat_typ = json_of_typ_opt ?env:(env_of_annot tannot) (typ_of_annot tannot) in
  let binding_id_type id = json_of_typ_opt ?env:(env_of_annot tannot) (typ_of_annot tannot) in
  match pat with
  | P_lit l -> `Assoc [ ("P_lit", json_of_lit l) ]
  | P_wild -> `String "P_wild"
  | P_or (a, b) -> `Assoc [ ("P_or", `List [ json_of_pat a; json_of_pat b ]) ]
  | P_not p -> `Assoc [ ("P_not", json_of_pat p) ]
  | P_as (p, id) ->
      `Assoc
        [ ("P_as", `List [ json_of_pat p; json_of_id id ]);
          ("var_type", binding_id_type id) ]
  | P_typ (t, p) -> `Assoc [ ("P_typ", `List [ json_of_typ t; json_of_pat p ]) ]
  | P_id id ->
      `Assoc [ ("P_id", json_of_id id); ("var_type", binding_id_type id) ]
  | P_var (p, tp) -> `Assoc [ ("P_var", `List [ json_of_pat p; json_of_typ_pat tp ]) ]
  | P_app (id, args) ->
      `Assoc [ ("P_app", `List (json_of_id id :: pl json_of_pat args)) ]
  | P_vector ps -> `Assoc [ ("P_vector", jl (pl json_of_pat ps)) ]
  | P_vector_concat ps -> `Assoc [ ("P_vector_concat", jl (pl json_of_pat ps)) ]
  | P_vector_subrange (id, a, b) ->
      `Assoc
        [ ( "P_vector_subrange",
            `List [ json_of_id id; json_of_big_int a; json_of_big_int b ] )
        ]
  | P_tuple ps -> `Assoc [ ("P_tuple", jl (pl json_of_pat ps)) ]
  | P_list ps -> `Assoc [ ("P_list", jl (pl json_of_pat ps)) ]
  | P_cons (a, b) -> `Assoc [ ("P_cons", `List [ json_of_pat a; json_of_pat b ]) ]
  | P_string_append ps -> `Assoc [ ("P_string_append", jl (pl json_of_pat ps)) ]
  | P_struct (sn, fields, fpw) ->
      `Assoc
        [ ( "P_struct",
            `List
              [ json_of_struct_name sn;
                jl
                  (List.map
                     (fun (id, p) -> `Assoc [ (string_of_id id, json_of_pat p) ])
                     fields);
                json_of_field_pat_wildcard fpw
              ] )
        ]

and json_of_pat :
    type a. a Ast.pat -> Yojson.Safe.t
    = fun (P_aux (aux, ((l, _) as annot))) ->
      let tannot = tannot_of_annot annot in
      let base = [ ("pat", json_of_pat_aux aux tannot) ] in
      let extras =
        match typ_of_annot tannot, env_of_annot tannot with
        | Some t, Some env -> [ ("typ", json_of_typ ~env t) ]
        | Some t, None -> [ ("typ", json_of_typ t) ]
        | _ -> []
      in
      `Assoc [ ("Pat", with_loc_opt l (base @ extras)) ]

and json_of_lexp_aux : type a. a Ast.lexp_aux -> Type_check.tannot -> Yojson.Safe.t
    =
 fun le tannot ->
  let ref_type id =
    match env_of_annot tannot with
    | Some env -> json_of_typ_opt (lookup_id_typ env id)
    | None -> `Null
  in
  match le with
  | LE_id id -> `Assoc [ ("LE_id", json_of_id id); ("ref_type", ref_type id) ]
  | LE_deref e -> `Assoc [ ("LE_deref", json_of_exp e) ]
  | LE_app (id, args) ->
      `Assoc [ ("LE_app", `List (json_of_id id :: pl json_of_exp args)) ]
  | LE_typ (t, id) ->
      `Assoc
        [ ("LE_typ", `List [ json_of_typ ?env:(env_of_annot tannot) t; json_of_id id ]) ]
  | LE_tuple les -> `Assoc [ ("LE_tuple", jl (pl json_of_lexp les)) ]
  | LE_vector_concat les -> `Assoc [ ("LE_vector_concat", jl (pl json_of_lexp les)) ]
  | LE_vector (le, e) -> `Assoc [ ("LE_vector", `List [ json_of_lexp le; json_of_exp e ]) ]
  | LE_vector_range (le, a, b) ->
      `Assoc
        [ ("LE_vector_range", `List [ json_of_lexp le; json_of_exp a; json_of_exp b ]) ]
  | LE_field (le, id) -> `Assoc [ ("LE_field", `List [ json_of_lexp le; json_of_id id ]) ]

and json_of_lexp :
    type a. a Ast.lexp -> Yojson.Safe.t
    = fun (LE_aux (aux, ((l, _) as annot))) ->
      let tannot = tannot_of_annot annot in
      let base = [ ("lexp", json_of_lexp_aux aux tannot) ] in
      let extras =
        match typ_of_annot tannot, env_of_annot tannot with
        | Some t, Some env -> [ ("typ", json_of_typ ~env t) ]
        | Some t, None -> [ ("typ", json_of_typ t) ]
        | _ -> []
      in
      `Assoc [ ("LE", with_loc_opt l (base @ extras)) ]

and json_of_fexp :
    type a. a Ast.fexp -> Yojson.Safe.t
    = fun (FE_aux (FE_fexp (id, e), ((l, _) as annot))) ->
      let tannot = tannot_of_annot annot in
      let base =
        [ ( "fexp",
            `Assoc [ ("FE_fexp", `List [ json_of_id id; json_of_exp e ]) ] )
        ]
      in
      let extras =
        match typ_of_annot tannot, env_of_annot tannot with
        | Some t, Some env -> [ ("typ", json_of_typ ~env t) ]
        | Some t, None -> [ ("typ", json_of_typ t) ]
        | _ -> []
      in
      `Assoc [ ("FE", with_loc_opt l (base @ extras)) ]

and json_of_pexp_aux : type a. a Ast.pexp_aux -> Type_check.tannot -> Yojson.Safe.t
    = fun pe _tannot ->
  match pe with
  | Pat_exp (p, e) -> `Assoc [ ("Pat_exp", `List [ json_of_pat p; json_of_exp e ]) ]
  | Pat_when (p, c, e) ->
      `Assoc [ ("Pat_when", `List [ json_of_pat p; json_of_exp c; json_of_exp e ]) ]

and json_of_pexp :
    type a. a Ast.pexp -> Yojson.Safe.t
    = fun (Pat_aux (aux, ((l, _) as annot))) ->
      let tannot = tannot_of_annot annot in
      let base = [ ("pexp", json_of_pexp_aux aux tannot) ] in
      let extras =
        match typ_of_annot tannot, env_of_annot tannot with
        | Some t, Some env -> [ ("typ", json_of_typ ~env t) ]
        | Some t, None -> [ ("typ", json_of_typ t) ]
        | _ -> []
      in
      `Assoc [ ("Pat_exp", with_loc_opt l (base @ extras)) ]

and json_of_mpat_aux : type a. a Ast.mpat_aux -> Type_check.tannot -> Yojson.Safe.t
    =
 fun mp tannot ->
  let binding_id_type id =
    json_of_typ_opt ?env:(env_of_annot tannot) (typ_of_annot tannot)
  in
  match mp with
  | MP_lit l -> `Assoc [ ("MP_lit", json_of_lit l) ]
  | MP_id id ->
      `Assoc [ ("MP_id", json_of_id id); ("var_type", binding_id_type id) ]
  | MP_app (id, args) -> `Assoc [ ("MP_app", `List (json_of_id id :: pl json_of_mpat args)) ]
  | MP_vector mps -> `Assoc [ ("MP_vector", jl (pl json_of_mpat mps)) ]
  | MP_vector_concat mps -> `Assoc [ ("MP_vector_concat", jl (pl json_of_mpat mps)) ]
  | MP_vector_subrange (id, a, b) ->
      `Assoc
        [ ( "MP_vector_subrange",
            `List [ json_of_id id; json_of_big_int a; json_of_big_int b ] )
        ]
  | MP_tuple mps -> `Assoc [ ("MP_tuple", jl (pl json_of_mpat mps)) ]
  | MP_list mps -> `Assoc [ ("MP_list", jl (pl json_of_mpat mps)) ]
  | MP_cons (a, b) -> `Assoc [ ("MP_cons", `List [ json_of_mpat a; json_of_mpat b ]) ]
  | MP_string_append mps -> `Assoc [ ("MP_string_append", jl (pl json_of_mpat mps)) ]
  | MP_typ (mp, t) -> `Assoc [ ("MP_typ", `List [ json_of_mpat mp; json_of_typ t ]) ]
  | MP_as (mp, id) ->
      `Assoc
        [ ("MP_as", `List [ json_of_mpat mp; json_of_id id ]);
          ("var_type", binding_id_type id) ]
  | MP_struct (sn, fields) ->
      `Assoc
        [ ( "MP_struct",
            `List
              [ json_of_struct_name sn;
                jl
                  (List.map
                     (fun (id, mp) -> `Assoc [ (string_of_id id, json_of_mpat mp) ])
                     fields)
              ] )
        ]

and json_of_mpat :
    type a. a Ast.mpat -> Yojson.Safe.t
    = fun (MP_aux (aux, ((l, _) as annot))) ->
      let tannot = tannot_of_annot annot in
      let base = [ ("mpat", json_of_mpat_aux aux tannot) ] in
      let extras =
        match typ_of_annot tannot, env_of_annot tannot with
        | Some t, Some env -> [ ("typ", json_of_typ ~env t) ]
        | Some t, None -> [ ("typ", json_of_typ t) ]
        | _ -> []
      in
      `Assoc [ ("MP", with_loc_opt l (base @ extras)) ]

and json_of_mpexp_aux : type a. a Ast.mpexp_aux -> Type_check.tannot -> Yojson.Safe.t
    = fun mpe _tannot ->
  match mpe with
  | MPat_pat mp -> `Assoc [ ("MPat_pat", json_of_mpat mp) ]
  | MPat_when (mp, e) -> `Assoc [ ("MPat_when", `List [ json_of_mpat mp; json_of_exp e ]) ]

and json_of_mpexp :
    type a. a Ast.mpexp -> Yojson.Safe.t
    = fun (MPat_aux (aux, ((l, _) as annot))) ->
      let tannot = tannot_of_annot annot in
      let base = [ ("mpexp", json_of_mpexp_aux aux tannot) ] in
      let extras =
        match typ_of_annot tannot, env_of_annot tannot with
        | Some t, Some env -> [ ("typ", json_of_typ ~env t) ]
        | Some t, None -> [ ("typ", json_of_typ t) ]
        | _ -> []
      in
      `Assoc [ ("MPat", with_loc_opt l (base @ extras)) ]

and json_of_in_place_loop_measure :
    type a. a Ast.in_place_loop_measure -> Yojson.Safe.t
    = function
  | Measure_aux (Measure_none, l) -> `Assoc [ ("Measure_none", with_loc_opt l []) ]
  | Measure_aux (Measure_some e, l) ->
      `Assoc [ ("Measure_some", with_loc_opt l [ ("exp", json_of_exp e) ]) ]

and json_of_value v =
  let open Value in
  match v with
  | V_bitvector bits ->
      `Assoc
        [ ( "V_bitvector",
            `List (List.map (fun b -> `String (Sail_lib.string_of_bit b)) bits) )
        ]
  | V_vector vs -> `Assoc [ ("V_vector", jl (pl json_of_value vs)) ]
  | V_list vs -> `Assoc [ ("V_list", jl (pl json_of_value vs)) ]
  | V_int n -> `Assoc [ ("V_int", json_of_big_int n) ]
  | V_real r ->
      let q = Util.Rational.from_rocq r in
      `Assoc [ ("V_real", `List [ json_of_big_int q.Q.num; json_of_big_int q.Q.den ]) ]
  | V_bool b -> `Assoc [ ("V_bool", `Bool b) ]
  | V_tuple vs -> `Assoc [ ("V_tuple", jl (pl json_of_value vs)) ]
  | V_unit -> `String "V_unit"
  | V_string s -> `Assoc [ ("V_string", `String s) ]
  | V_ref id -> `Assoc [ ("V_ref", json_of_id id) ]
  | V_member id -> `Assoc [ ("V_member", json_of_id id) ]
  | V_ctor (id, vs) ->
      `Assoc [ ("V_ctor", `List [ json_of_id id; jl (pl json_of_value vs) ]) ]
  | V_record fields ->
      `Assoc
        [ ( "V_record",
            `Assoc
              (List.map (fun (id, v) -> (string_of_id id, json_of_value v)) fields)
        ) ]

and json_of_exp_aux : type a. a Ast.exp_aux -> Type_check.tannot -> Yojson.Safe.t =
 fun e tannot ->
  let ref_type id =
    match env_of_annot tannot with
    | Some env -> json_of_typ_opt (lookup_id_typ env id)
    | None -> `Null
  in
  match e with
  | E_block es -> `Assoc [ ("E_block", jl (pl json_of_exp es)) ]
  | E_id id -> `Assoc [ ("E_id", json_of_id id); ("ref_type", ref_type id) ]
  | E_lit l -> `Assoc [ ("E_lit", json_of_lit l) ]
  | E_typ (t, e) ->
      `Assoc [ ("E_typ", `List [ json_of_typ ?env:(env_of_annot tannot) t; json_of_exp e ]) ]
  | E_app (id, args) -> `Assoc [ ("E_app", `List (json_of_id id :: pl json_of_exp args)) ]
  | E_tuple es -> `Assoc [ ("E_tuple", jl (pl json_of_exp es)) ]
  | E_if (a, b, c) -> `Assoc [ ("E_if", `List [ json_of_exp a; json_of_exp b; json_of_exp c ]) ]
  | E_loop (l, m, c, b) ->
      `Assoc
        [ ( "E_loop",
            `List
              [ json_of_loop l;
                json_of_in_place_loop_measure m;
                json_of_exp c;
                json_of_exp b
              ] )
        ]
  | E_for (id, a, b, c, ord, body) ->
      `Assoc
        [ ( "E_for",
            `List
              [ json_of_id id;
                json_of_exp a;
                json_of_exp b;
                json_of_exp c;
                json_of_order ord;
                json_of_exp body
              ] )
        ]
  | E_vector es -> `Assoc [ ("E_vector", jl (pl json_of_exp es)) ]
  | E_vector_append (a, b) ->
      `Assoc [ ("E_vector_append", `List [ json_of_exp a; json_of_exp b ]) ]
  | E_list es -> `Assoc [ ("E_list", jl (pl json_of_exp es)) ]
  | E_cons (a, b) -> `Assoc [ ("E_cons", `List [ json_of_exp a; json_of_exp b ]) ]
  | E_struct (sn, fields) ->
      `Assoc [ ("E_struct", `List [ json_of_struct_name sn; jl (pl json_of_fexp fields) ]) ]
  | E_struct_update (e, fields) ->
      `Assoc
        [ ("E_struct_update", `List [ json_of_exp e; jl (pl json_of_fexp fields) ]) ]
  | E_field (e, id) -> `Assoc [ ("E_field", `List [ json_of_exp e; json_of_id id ]) ]
  | E_match (e, arms) ->
      let arm_types =
        `List
          (List.map
             (fun (arm : Type_check.tannot Ast.pexp) ->
               match arm with
               | Pat_aux (aux, annot) ->
                   let tannot = tannot_of_annot annot in
                   (* Pull the pattern's typ out of the pexp_aux itself.
                      Defensive: if [p] turns out to be untyped we fall back
                      to [None] rather than crash on [tannot_of_annot]. *)
                   let pat_typ =
                     match aux with
                     | Pat_exp (p, _) | Pat_when (p, _, _) -> (
                         match snd (Obj.magic p) with
                         | (_, inner) -> typ_of_annot (tannot_of_annot inner))
                   in
                   `Assoc
                     [ ("match_arm_typ", json_of_typ_opt pat_typ);
                       ("pexp", json_of_pexp_aux aux tannot) ])
             (Obj.magic arms : (Type_check.tannot Ast.pexp) list))
      in
      `Assoc
        [ ("E_match", `List [ json_of_exp e; arm_types ]) ]
  | E_let (p, a, b) -> `Assoc [ ("E_let", `List [ json_of_pat p; json_of_exp a; json_of_exp b ]) ]
  | E_assign (le, e) -> `Assoc [ ("E_assign", `List [ json_of_lexp le; json_of_exp e ]) ]
  | E_sizeof n -> `Assoc [ ("E_sizeof", json_of_nexp n) ]
  | E_return e -> `Assoc [ ("E_return", json_of_exp e) ]
  | E_exit e -> `Assoc [ ("E_exit", json_of_exp e) ]
  | E_config cs -> `Assoc [ ("E_config", jl (pl json_of_string cs)) ]
  | E_ref id -> `Assoc [ ("E_ref", json_of_id id); ("ref_type", ref_type id) ]
  | E_throw e -> `Assoc [ ("E_throw", json_of_exp e) ]
  | E_try (e, arms) -> `Assoc [ ("E_try", `List [ json_of_exp e; jl (pl json_of_pexp arms) ]) ]
  | E_assert (a, b) -> `Assoc [ ("E_assert", `List [ json_of_exp a; json_of_exp b ]) ]
  | E_var (le, a, b) -> `Assoc [ ("E_var", `List [ json_of_lexp le; json_of_exp a; json_of_exp b ]) ]
  | E_undef -> `String "E_undef"
  | E_internal_plet (p, a, b) ->
      `Assoc
        [ ("E_internal_plet", `List [ json_of_pat p; json_of_exp a; json_of_exp b ]) ]
  | E_internal_return e -> `Assoc [ ("E_internal_return", json_of_exp e) ]
  | E_internal_value v -> `Assoc [ ("E_internal_value", json_of_value v) ]
  | E_internal_assume (c, e) ->
      `Assoc [ ("E_internal_assume", `List [ json_of_n_constraint c; json_of_exp e ]) ]
  | E_constraint c -> `Assoc [ ("E_constraint", json_of_n_constraint c) ]

and json_of_exp :
    type a. a Ast.exp -> Yojson.Safe.t
    = fun (E_aux (aux, ((l, _) as annot))) ->
      let tannot = tannot_of_annot annot in
      let base = [ ("exp", json_of_exp_aux aux tannot) ] in
      let extras =
        match typ_of_annot tannot, env_of_annot tannot with
        | Some t, Some env -> [ ("typ", json_of_typ ~env t) ]
        | Some t, None -> [ ("typ", json_of_typ t) ]
        | _ -> []
      in
      `Assoc [ ("E", with_loc_opt l (base @ extras)) ]

and json_of_rec_opt_aux :
    type a. a Ast.rec_opt_aux -> Yojson.Safe.t
    = function
  | Rec_nonrec -> `String "Rec_nonrec"
  | Rec_rec -> `String "Rec_rec"
  | Rec_measure (p, e) ->
      `Assoc [ ("Rec_measure", `List [ json_of_pat p; json_of_exp e ]) ]

and json_of_rec_opt :
    type a. a Ast.rec_opt -> Yojson.Safe.t
    = fun (Rec_aux (aux, l)) ->
      `Assoc [ ("Rec", with_loc_opt l [ ("rec_opt", json_of_rec_opt_aux aux) ]) ]

and json_of_tannot_opt_aux = function
  | Typ_annot_opt_none -> `String "Typ_annot_opt_none"
  | Typ_annot_opt_some (tq, t) ->
      `Assoc [ ("Typ_annot_opt_some", `List [ json_of_typquant tq; json_of_typ t ]) ]

and json_of_tannot_opt (Typ_annot_opt_aux (aux, l)) =
  `Assoc [ ("Typ_annot_opt", with_loc_opt l [ ("tannot_opt", json_of_tannot_opt_aux aux) ]) ]

and json_of_typschm (TypSchm_aux (TypSchm_ts (tq, t), l)) =
  `Assoc
    [ ( "TypSchm",
        with_loc_opt l
          [ ( "typschm",
              `Assoc
                [ ("TypSchm_ts", `List [ json_of_typquant tq; json_of_typ t ]) ] )
          ] )
    ]

and json_of_funcl :
    type a. a Ast.funcl -> Yojson.Safe.t
    = fun (FCL_aux (FCL_funcl (id, pe), _)) ->
      `Assoc [ ("FCL_funcl", `List [ json_of_id id; json_of_pexp pe ]) ]

and json_of_mapcl :
    type a. a Ast.mapcl -> Yojson.Safe.t
    = function
  | MCL_aux (MCL_bidir (a, b), _) ->
      `Assoc [ ("MCL_bidir", `List [ json_of_mpexp a; json_of_mpexp b ]) ]
  | MCL_aux (MCL_forwards pe, _) -> `Assoc [ ("MCL_forwards", json_of_pexp pe) ]
  | MCL_aux (MCL_backwards pe, _) -> `Assoc [ ("MCL_backwards", json_of_pexp pe) ]

and json_of_index_range_aux = function
  | BF_single n -> `Assoc [ ("BF_single", json_of_nexp n) ]
  | BF_range (a, b) -> `Assoc [ ("BF_range", `List [ json_of_nexp a; json_of_nexp b ]) ]
  | BF_concat (a, b) ->
      `Assoc [ ("BF_concat", `List [ json_of_index_range a; json_of_index_range b ]) ]

and json_of_index_range (BF_aux (aux, l)) =
  `Assoc [ ("BF", with_loc_opt l [ ("index_range", json_of_index_range_aux aux) ]) ]

and json_of_type_union = function
  | Tu_aux (Tu_ty_id (t, id), _) ->
      `Assoc [ ("Tu_ty_id", `List [ json_of_typ t; json_of_id id ]) ]

and json_of_opt_abstract_config = function
  | TDC_key ks -> `Assoc [ ("TDC_key", jl (pl json_of_string ks)) ]
  | TDC_none -> `String "TDC_none"

and json_of_outcome_spec (OV_aux (OV_outcome (id, ts, tq), l)) =
  `Assoc
    [ ( "OV",
        with_loc_opt l
          [ ( "outcome_spec",
              `Assoc
                [ ( "OV_outcome",
                    `List [ json_of_id id; json_of_typschm ts; json_of_typquant tq ] )
                ] )
          ] )
    ]

and json_of_instantiation_spec :
    type a. a Ast.instantiation_spec -> Yojson.Safe.t
    = fun (IN_aux (IN_id id, _)) ->
      `Assoc [ ("IN_id", json_of_id id) ]

and json_of_subst = function
  | IS_aux (IS_typ (kid, ta), _) ->
      `Assoc [ ("IS_typ", `List [ json_of_kid kid; json_of_typ_arg ta ]) ]
  | IS_aux (IS_id (a, b), _) ->
      `Assoc [ ("IS_id", `List [ json_of_id a; json_of_id b ]) ]

and json_of_extern (ext : extern) =
  `Assoc
    [ ("pure", `Bool ext.pure);
      ( "bindings",
        jl
          (List.map (fun (k, v) -> `List [ `String k; `String v ]) ext.bindings) )
    ]

and json_of_val_spec :
    type a. a Ast.val_spec -> Yojson.Safe.t
    = fun (VS_aux (VS_val_spec (ts, id, ext), _)) ->
      `Assoc
        [ ( "VS_val_spec",
            `List
              [ json_of_typschm ts;
                json_of_id id;
                json_of_option json_of_extern ext
              ] )
        ]

and json_of_default_spec (DT_aux (DT_order ord, l)) =
  `Assoc
    [ ( "DT",
        with_loc_opt l [ ("default_spec", `Assoc [ ("DT_order", json_of_order ord) ]) ] )
    ]

and json_of_dec_spec :
    type a. a Ast.dec_spec -> Yojson.Safe.t
    = fun (DEC_aux (DEC_reg (t, id, init), (l, _))) ->
      `Assoc
        [ ( "DEC",
            with_loc_opt l
              [ ( "dec_spec",
                  `Assoc
                    [ ( "DEC_reg",
                        `List
                          [ json_of_typ t;
                            json_of_id id;
                            json_of_option json_of_exp init
                          ] )
                    ] )
              ] )
        ]

and json_of_scattered :
    type a. a Ast.scattered_def -> Yojson.Safe.t
    = function
  | SD_aux (SD_function (id, tao), _) ->
      `Assoc [ ("SD_function", `List [ json_of_id id; json_of_tannot_opt tao ]) ]
  | SD_aux (SD_funcl fcl, _) -> `Assoc [ ("SD_funcl", json_of_funcl fcl) ]
  | SD_aux (SD_variant (id, tq), _) ->
      `Assoc [ ("SD_variant", `List [ json_of_id id; json_of_typquant tq ]) ]
  | SD_aux (SD_unioncl (id, tu), _) ->
      `Assoc [ ("SD_unioncl", `List [ json_of_id id; json_of_type_union tu ]) ]
  | SD_aux (SD_internal_unioncl_record (id1, id2, tq, items), _) ->
      `Assoc
        [ ( "SD_internal_unioncl_record",
            `List
              [ json_of_id id1;
                json_of_id id2;
                json_of_typquant tq;
                jl
                  (List.map
                     (fun ((id, t), _) -> `List [ json_of_id id; json_of_typ t ])
                     items)
              ] )
        ]
  | SD_aux (SD_mapping (id, tao), _) ->
      `Assoc [ ("SD_mapping", `List [ json_of_id id; json_of_tannot_opt tao ]) ]
  | SD_aux (SD_mapcl (id, mcl), _) ->
      `Assoc [ ("SD_mapcl", `List [ json_of_id id; json_of_mapcl mcl ]) ]
  | SD_aux (SD_enum id, _) -> `Assoc [ ("SD_enum", json_of_id id) ]
  | SD_aux (SD_enumcl (a, b), _) ->
      `Assoc [ ("SD_enumcl", `List [ json_of_id a; json_of_id b ]) ]
  | SD_aux (SD_end id, _) -> `Assoc [ ("SD_end", json_of_id id) ]

and json_of_type_def_aux : Ast.type_def_aux -> Yojson.Safe.t = function
  | TD_abbrev (id, tq, ta) ->
      `Assoc
        [ ("TD_abbrev", `List [ json_of_id id; json_of_typquant tq; json_of_typ_arg ta ]) ]
  | TD_record (id, tq, fields, b) ->
      `Assoc
        [ ( "TD_record",
            `List
              [ json_of_id id;
                json_of_typquant tq;
                jl
                  (List.map
                     (fun ((id, t), _) -> `List [ json_of_id id; json_of_typ t ])
                     fields);
                `Bool b
              ] )
        ]
  | TD_variant (id, tq, unions, b) ->
      `Assoc
        [ ( "TD_variant",
            `List
              [ json_of_id id;
                json_of_typquant tq;
                jl (pl json_of_type_union unions);
                `Bool b
              ] )
        ]
  | TD_enum (id, members, b) ->
      `Assoc
        [ ( "TD_enum",
            `List
              [ json_of_id id;
                jl (List.map (fun (id, _) -> json_of_id id) members);
                `Bool b
              ] )
        ]
  | TD_abstract (id, kind, cfg) ->
      `Assoc
        [ ( "TD_abstract",
            `List [ json_of_id id; json_of_kind kind; json_of_opt_abstract_config cfg ]
        ) ]
  | TD_bitfield (id, t, fields) ->
      `Assoc
        [ ( "TD_bitfield",
            `List
              [ json_of_id id;
                json_of_typ t;
                jl
                  (List.map
                     (fun ((id, r), _) -> `List [ json_of_id id; json_of_index_range r ])
                     fields)
              ] )
        ]

and json_of_type_def :
    type a. a Ast.type_def -> Yojson.Safe.t
    = fun (TD_aux (aux, (l, _))) ->
      `Assoc [ ("TD", with_loc_opt l [ ("type_def", json_of_type_def_aux aux) ]) ]

and json_of_fundef :
    type a. a Ast.fundef -> Yojson.Safe.t
    = fun (FD_aux (FD_function (rec_opt, tao, fcls), _)) ->
      let param_types =
        match tao with
        | Typ_annot_opt_aux (Typ_annot_opt_some (_, typ), _) ->
            Some (param_types_of_typ typ)
        | _ -> None
      in
      let param_field = match param_types with
        | Some pts -> [ ("param_types", `List (List.map json_of_typ pts)) ]
        | None -> []
      in
      `Assoc
        ([ ( "FD_function",
              `List
                [ json_of_rec_opt rec_opt;
                  json_of_tannot_opt tao;
                  jl (pl json_of_funcl fcls)
                ] )
        ]
        @ param_field)

and json_of_mapdef :
    type a. a Ast.mapdef -> Yojson.Safe.t
    = fun (MD_aux (MD_mapping (id, tao, mcls), _)) ->
      `Assoc
        [ ( "MD_mapping",
            `List
              [ json_of_id id; json_of_tannot_opt tao; jl (pl json_of_mapcl mcls) ] )
        ]

and json_of_def_aux :
    type a b. (a, b) Ast.def_aux -> Yojson.Safe.t
    = function
  | DEF_type td -> `Assoc [ ("DEF_type", json_of_type_def td) ]
  | DEF_constraint c -> `Assoc [ ("DEF_constraint", json_of_n_constraint c) ]
  | DEF_fundef fd -> `Assoc [ ("DEF_fundef", json_of_fundef fd) ]
  | DEF_mapdef md -> `Assoc [ ("DEF_mapdef", json_of_mapdef md) ]
  | DEF_impl fcl -> `Assoc [ ("DEF_impl", json_of_funcl fcl) ]
  | DEF_let (p, e) ->
      `Assoc
        [ ("DEF_let", `List [ json_of_pat p; json_of_exp e ]);
          ("def_type", pat_bound_names_json (Obj.magic p)) ]
  | DEF_val vs -> `Assoc [ ("DEF_val", json_of_val_spec vs) ]
  | DEF_outcome (os, ds) ->
      `Assoc
        [ ("DEF_outcome", `List [ json_of_outcome_spec os; jl (pl json_of_def ds) ]) ]
  | DEF_instantiation (is, subs) ->
      `Assoc
        [ ( "DEF_instantiation",
            `List [ json_of_instantiation_spec is; jl (pl json_of_subst subs) ] )
        ]
  | DEF_fixity (prec, n, id) ->
      `Assoc [ ("DEF_fixity", `List [ json_of_prec prec; json_of_big_int n; json_of_id id ]) ]
  | DEF_overload (id, ids) ->
      `Assoc [ ("DEF_overload", `List [ json_of_id id; jl (pl json_of_id ids) ]) ]
  | DEF_default ds -> `Assoc [ ("DEF_default", json_of_default_spec ds) ]
  | DEF_scattered sd -> `Assoc [ ("DEF_scattered", json_of_scattered sd) ]
  | DEF_measure (id, p, e) ->
      `Assoc [ ("DEF_measure", `List [ json_of_id id; json_of_pat p; json_of_exp e ]) ]
  | DEF_loop_measures (id, lms) ->
      `Assoc
        [ ( "DEF_loop_measures",
            `List
              [ json_of_id id;
                jl
                  (List.map
                     (fun (l, e) -> `List [ json_of_loop l; json_of_exp e ])
                     lms)
              ] )
        ]
  | DEF_register dec -> `Assoc [ ("DEF_register", json_of_dec_spec dec) ]
  | DEF_internal_mutrec fds ->
      `Assoc [ ("DEF_internal_mutrec", jl (pl json_of_fundef fds)) ]
  | DEF_pragma (name, _) -> `Assoc [ ("DEF_pragma", `String name) ]

and json_of_def :
    type a b. (a, b) Ast.def -> Yojson.Safe.t
    = fun (DEF_aux (aux, annot)) ->
      let json = json_of_def_aux aux in
      match annot.doc_comment with
      | None -> json
      | Some dc ->
          (match json with
           | `Assoc fields -> `Assoc (("doc", `String dc.Parse_ast.contents) :: fields)
           | _ -> json)

(* Walk a pattern, yielding [(id, typ)] pairs for each name bound. The
   [typ] is read from the pattern's own tannot where possible, falling
   back to the enclosing pattern's tannot for nested bindings. *)
and pat_bound_names p =
  let self_typ_of annot = typ_of_annot (tannot_of_annot annot) in
  match p with
  | P_aux (P_id id, annot) -> [ (id, self_typ_of annot) ]
  | P_aux (P_as (p, id), annot) -> (id, self_typ_of annot) :: pat_bound_names p
  | P_aux (P_var (p, _), _) -> pat_bound_names p
  | P_aux (P_typ (_, p), _) -> pat_bound_names p
  | P_aux (P_or (a, b), _) -> pat_bound_names a @ pat_bound_names b
  | P_aux (P_tuple ps, _) -> List.concat (List.map pat_bound_names ps)
  | P_aux (P_struct (_, fields, _), _) ->
      List.concat (List.map (fun (_, p) -> pat_bound_names p) fields)
  | P_aux (P_vector ps, _) -> List.concat (List.map pat_bound_names ps)
  | P_aux (P_vector_concat ps, _) ->
      List.concat (List.map pat_bound_names ps)
  | P_aux (P_cons (a, b), _) -> pat_bound_names a @ pat_bound_names b
  | P_aux (P_string_append ps, _) ->
      List.concat (List.map pat_bound_names ps)
  | P_aux (P_list ps, _) -> List.concat (List.map pat_bound_names ps)
  | P_aux (P_app (_, args), _) -> List.concat (List.map pat_bound_names args)
  | P_aux (P_not p, _) -> pat_bound_names p
  | _ -> []

and pat_bound_names_json p =
 `List
   (List.map
      (fun (id, typ) ->
        `Assoc
          [ ("name", json_of_id id);
            ("var_type", json_of_typ_opt typ) ])
      (pat_bound_names p))

let json_of_comments cs =
  `List
    (List.map
       (fun (c : Lexer.comment) ->
         match c with
         | Lexer.Comment (ct, _p1, _p2, s) ->
             `Assoc
               [ ("contents", `String s);
                 ( "comment_type",
                   `String
                     (match ct with
                      | Comment_block -> "Comment_block"
                      | Comment_line -> "Comment_line") )
               ])
       cs)

(* ------------------------------------------------------------------ *)
(* Type table emission                                                *)
(*                                                                    *)
(* Emit a sibling ["types"] field listing every type the typechecker  *)
(* knows about, with the resolved RHS and (for bitvector aliases)    *)
(* the concrete size. Downstream consumers use this table to resolve *)
(* bare [Typ_id] references that aren't bitvectors, or to validate   *)
(* that a [Typ_id] referring to a bitvector alias has a known size.  *)
(* ------------------------------------------------------------------ *)

let json_of_type_table (env : Type_check.Env.t) : Yojson.Safe.t =
  let entries =
    let open Type_check.Env in
    let open Ast_compare.Bindings in
    let rec name_of_kind k =
      match k with K_aux (aux, _) -> name_of_kind_aux aux
    and name_of_kind_aux = function
      | K_type -> `String "Type"
      | K_int -> `String "Int"
      | K_bool -> `String "Bool"
    in
    (* Type aliases ([type x = ...]) *)
    let synonyms =
      bindings (get_typ_synonyms env)
      |> List.map (fun (id, (tq, ta)) ->
           let rhs =
             match ta with
             | A_aux (A_typ t, _) -> `Assoc [ ("typ", json_of_typ t) ]
             | A_aux (A_nexp n, _) -> `Assoc [ ("nexp", json_of_nexp n) ]
             | A_aux (A_bool c, _) -> `Assoc [ ("n_constraint", json_of_n_constraint c) ]
           in
           `Assoc
             [ ("name", `String (string_of_id id));
               ("kind", `String "type_alias");
               ("typquant", json_of_typquant tq);
               ("rhs", rhs)
             ])
    in
    (* Records ([type x = { f: T, ... }]) *)
    let records =
      bindings (get_records env)
      |> List.map (fun (id, (tq, fields)) ->
           `Assoc
             [ ("name", `String (string_of_id id));
               ("kind", `String "record");
               ("typquant", json_of_typquant tq);
               ( "fields",
                 `List
                   (List.map
                      (fun (t, fid) ->
                        `Assoc
                          [ ("name", `String (string_of_id fid));
                            ("typ", json_of_typ t)
                          ])
                      fields) )
             ])
    in
    (* Variants ([type x = A of T | B of U]) *)
    let variants =
      bindings (get_variants env)
      |> List.map (fun (id, (tq, unions)) ->
           `Assoc
             [ ("name", `String (string_of_id id));
               ("kind", `String "variant");
               ("typquant", json_of_typquant tq);
               ( "constructors",
                 `List
                   (List.map
                      (fun (Tu_aux (Tu_ty_id (_t, cid), _)) ->
                        `Assoc [ ("name", `String (string_of_id cid)) ])
                      unions) )
             ])
    in
    (* Enums ([type x = A | B | C]) *)
    let enums =
      bindings (get_enums env)
      |> List.map (fun (id, ctors) ->
           `Assoc
             [ ("name", `String (string_of_id id));
               ("kind", `String "enum");
               ( "constructors",
                 `List (Ast_compare.IdSet.elements ctors |> List.map (fun c -> `String (string_of_id c)))
               )
             ])
    in
    (* Abstract types ([type x]) *)
    let abstracts =
      bindings (get_abstract_typs env)
      |> List.map (fun (id, k) ->
           `Assoc
             [ ("name", `String (string_of_id id));
               ("kind", `String "abstract");
               ("type_kind", name_of_kind k)
             ])
    in
    synonyms @ records @ variants @ enums @ abstracts
  in
  `List entries

let pp_ast_json (type a b) (env : Type_check.Env.t) (ast : (a, b) Ast_defs.ast) : Yojson.Safe.t =
  `Assoc
    [ ("ast", jl (pl json_of_def ast.defs));
      ("types", json_of_type_table env);
      ( "comments",
        `List
          (List.map
             (fun (file, cs) ->
               `Assoc [ ("file", `String file); ("comments", json_of_comments cs) ])
             ast.comments)
      )
    ]