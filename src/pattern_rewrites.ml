(**************************************************************************)
(*     Sail                                                               *)
(*                                                                        *)
(*  Copyright (c) 2013-2017                                               *)
(*    Kathyrn Gray                                                        *)
(*    Shaked Flur                                                         *)
(*    Stephen Kell                                                        *)
(*    Gabriel Kerneis                                                     *)
(*    Robert Norton-Wright                                                *)
(*    Christopher Pulte                                                   *)
(*    Peter Sewell                                                        *)
(*    Alasdair Armstrong                                                  *)
(*    Brian Campbell                                                      *)
(*    Thomas Bauereiss                                                    *)
(*    Anthony Fox                                                         *)
(*    Jon French                                                          *)
(*    Dominic Mulligan                                                    *)
(*    Stephen Kell                                                        *)
(*    Mark Wassell                                                        *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(*  This software was developed by the University of Cambridge Computer   *)
(*  Laboratory as part of the Rigorous Engineering of Mainstream Systems  *)
(*  (REMS) project, funded by EPSRC grant EP/K008528/1.                   *)
(*                                                                        *)
(*  Redistribution and use in source and binary forms, with or without    *)
(*  modification, are permitted provided that the following conditions    *)
(*  are met:                                                              *)
(*  1. Redistributions of source code must retain the above copyright     *)
(*     notice, this list of conditions and the following disclaimer.      *)
(*  2. Redistributions in binary form must reproduce the above copyright  *)
(*     notice, this list of conditions and the following disclaimer in    *)
(*     the documentation and/or other materials provided with the         *)
(*     distribution.                                                      *)
(*                                                                        *)
(*  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''    *)
(*  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED     *)
(*  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A       *)
(*  PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR   *)
(*  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,          *)
(*  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT      *)
(*  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF      *)
(*  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND   *)
(*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,    *)
(*  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT    *)
(*  OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF    *)
(*  SUCH DAMAGE.                                                          *)
(**************************************************************************)

module Big_int = Nat_big_num

open Ast
open Ast_util
open Rewriter

let rec irrefutable (P_aux (aux, annot)) =
  let open Type_check in
  match aux with
  | P_id id ->
     let open Type_check in
     let env = env_of_annot annot in
     begin match Env.lookup_id id env with
     | Enum (Typ_aux (Typ_id enum_id, _)) ->
        List.compare_length_with (Env.get_enum enum_id env) 1 = 0
     | _ -> true
     end
  | P_app (ctor, args) ->
     Env.is_singleton_union_constructor ctor (env_of_annot annot) && List.for_all irrefutable args
  | P_wild -> true
  | P_lit _ | P_string_append _ | P_cons _ -> false
  | P_as (pat, _) | P_typ (_, pat) | P_var (pat, _) | P_view (pat, _, _) -> irrefutable pat
  | P_vector pats | P_vector_concat pats | P_list pats | P_tup pats -> List.for_all irrefutable pats
  | P_or _ | P_not _ -> Reporting.unreachable (fst annot) __POS__ "Or or not pattern found in replace_views"

(* Check if one pattern subsumes the other, and if so, calculate a
   substitution of variables that are used in the same position.
   TODO: Check somewhere that there are no variable clashes (the same variable
   name used in different positions of the patterns)
 *)
let rec subsumes_pat (P_aux (p1,annot1) as pat1) (P_aux (p2,annot2) as pat2) =
  let open Type_check in
  let rewrap p = P_aux (p,annot1) in
  let subsumes_list s pats1 pats2 =
    if List.length pats1 = List.length pats2
    then
      let subs = List.map2 s pats1 pats2 in
      List.fold_right
        (fun p acc -> match p, acc with
          | Some subst, Some substs -> Some (subst @ substs)
          | _ -> None)
        subs (Some [])
    else None in
  match p1, p2 with
  | P_lit (L_aux (lit1,_)), P_lit (L_aux (lit2,_)) ->
      if lit1 = lit2 then Some [] else None
  | P_or(pat1, pat2), _ -> (* todo: possibly not the right answer *) None
  | _, P_or(pat1, pat2) -> (* todo: possibly not the right answer *) None
  | P_not(pat), _ -> (* todo: possibly not the right answer *) None
  | _, P_not(pat) -> (* todo: possibly not the right answer *) None
  | P_as (pat1,_), _ -> subsumes_pat pat1 pat2
  | _, P_as (pat2,_) -> subsumes_pat pat1 pat2
  | P_typ (_,pat1), _ -> subsumes_pat pat1 pat2
  | _, P_typ (_,pat2) -> subsumes_pat pat1 pat2
  | P_id (Id_aux (id1,_) as aid1), P_id (Id_aux (id2,_) as aid2) ->
    if id1 = id2 then Some []
    else if Env.lookup_id aid1 (env_of_annot annot1) = Unbound
    then if Env.lookup_id aid2 (env_of_annot annot2) = Unbound
      then Some [(id2,id1)]
      else Some []
    else None
  | P_id id1, _ ->
    if Env.lookup_id id1 (env_of_annot annot1) = Unbound then Some [] else None
  | P_var (pat1,_), P_var (pat2,_) -> subsumes_pat pat1 pat2
  | P_wild, _ -> Some []
  | P_app (Id_aux (id1,l1),args1), P_app (Id_aux (id2,_),args2) ->
    if id1 = id2 then subsumes_list subsumes_pat args1 args2 else None
  | P_vector pats1, P_vector pats2
  | P_vector_concat pats1, P_vector_concat pats2
  | P_tup pats1, P_tup pats2
  | P_list pats1, P_list pats2 ->
    subsumes_list subsumes_pat pats1 pats2
  | P_list (pat1 :: pats1), P_cons _ ->
    subsumes_pat (rewrap (P_cons (pat1, rewrap (P_list pats1)))) pat2
  | P_cons _, P_list (pat2 :: pats2)->
    subsumes_pat pat1 (rewrap (P_cons (pat2, rewrap (P_list pats2))))
  | P_cons (pat1, pats1), P_cons (pat2, pats2) ->
    (match subsumes_pat pat1 pat2, subsumes_pat pats1 pats2 with
    | Some substs1, Some substs2 -> Some (substs1 @ substs2)
    | _ -> None)
  | _, P_wild -> if irrefutable pat1 then Some [] else None
  | _ -> None

let id_is_unbound id env =
  match Type_check.Env.lookup_id id env with
  | Unbound -> true
  | _ -> false
       
(* A simple check for pattern disjointness; used for optimisation in the
   guarded pattern rewrite step *)
let rec disjoint_pat env (P_aux (p1,annot1) as pat1) (P_aux (p2,annot2) as pat2) =
  match p1, p2 with
  | P_as (pat1, _), _ -> disjoint_pat env pat1 pat2
  | _, P_as (pat2, _) -> disjoint_pat env pat1 pat2
  | P_typ (_, pat1), _ -> disjoint_pat env pat1 pat2
  | _, P_typ (_, pat2) -> disjoint_pat env pat1 pat2
  | P_var (pat1, _), _ -> disjoint_pat env pat1 pat2
  | _, P_var (pat2, _) -> disjoint_pat env pat1 pat2
  | P_id id, _ when id_is_unbound id env -> false
  | _, P_id id when id_is_unbound id env -> false
  | P_id id1, P_id id2 -> Id.compare id1 id2 <> 0
  | P_app (id1, args1), P_app (id2, args2) ->
     Id.compare id1 id2 <> 0 || List.exists2 (disjoint_pat env) args1 args2
  | P_vector pats1, P_vector pats2
  | P_tup pats1, P_tup pats2
  | P_list pats1, P_list pats2 ->
     List.exists2 (disjoint_pat env) pats1 pats2
  | _ -> false

let equiv_pats pat1 pat2 =
  match subsumes_pat pat1 pat2, subsumes_pat pat2 pat1 with
  | Some _, Some _ -> true
  | _, _ -> false

let subst_id_pat pat (id1,id2) =
  let p_id (Id_aux (id,l)) = (if id = id1 then P_id (Id_aux (id2,l)) else P_id (Id_aux (id,l))) in
  fold_pat { id_algebra with p_id = p_id } pat

let subst_id_exp exp (id1,id2) =
  Ast_util.subst (Id_aux (id1, Parse_ast.Unknown))
    (E_aux (E_id (Id_aux (id2, Parse_ast.Unknown)), (Parse_ast.Unknown, Type_check.empty_tannot)))
    exp

let remove_wildcards pre (P_aux (_,(l,_)) as pat) =
  fold_pat
    { id_algebra with
      p_aux = function
              | (P_wild,(l,annot)) -> P_aux (P_id (mk_id "w__0") ,(l,annot))
              | (p,annot) -> P_aux (p,annot) }
    pat
  
(**************************************************************************)
(* 1. Pattern rewrites                                                    *)
(**************************************************************************)

type action =
  | Subst_id of (id -> unit guard list)
  | No_change

(** The Pattern_rewriter module implements a bottom up traversal of
   all patterns with the AST, applying actions at each pattern. *)
module Pattern_rewriter = struct
  open Type_check

  module type Config = sig
    val id_root : string
    val action : Ast.l -> Type_check.tannot pat -> action
  end

  module Make (C : Config) : sig
    val rewrite : tannot defs -> tannot defs
  end = struct

    let rec rewrite_pat n env (P_aux (aux, annot)) =
      let wrap gs (P_aux (_, annot) as pat) =
        match C.action (gen_loc (fst annot)) pat with
        | No_change -> pat, gs
        | Subst_id to_guards ->
           let typ = typ_of_annot annot in
           let replaced_id = mk_id (C.id_root ^ "__" ^ string_of_int !n) in
           incr n;
           let env = Env.add_local replaced_id (Immutable, typ) env in
           (* Make sure casts don't interfere with re-writing *)
           let gs', _ = check_guards (Env.no_casts env) (to_guards replaced_id) in
           P_aux (P_typ (typ, P_aux (P_id replaced_id, annot)), annot), gs' @ gs
      in
      match aux with
      | P_view (pat, id, args) ->
         let pat, guards = rewrite_pat n env pat in
         wrap guards (P_aux (P_view (pat, id, args), annot))
      | P_lit _ | P_wild | P_id _ ->
         wrap [] (P_aux (aux, annot))
      | P_as (pat, id) ->
         let pat, guards = rewrite_pat n env pat in
         wrap guards (P_aux (P_as (pat, id), annot))
      | P_typ (typ, pat) ->
         let pat, guards = rewrite_pat n env pat in
         wrap guards (P_aux (P_typ (typ, pat), annot))
      | P_app (id, pats) ->
         let rewritten = List.map (rewrite_pat n env) pats in
         wrap (List.concat (List.map snd rewritten)) (P_aux (P_app (id, List.map fst rewritten), annot))
      | P_vector pats ->
         let rewritten = List.map (rewrite_pat n env) pats in
         wrap (List.concat (List.map snd rewritten)) (P_aux (P_vector (List.map fst rewritten), annot))
      | P_vector_concat pats ->
         let rewritten = List.map (rewrite_pat n env) pats in
         wrap (List.concat (List.map snd rewritten)) (P_aux (P_vector_concat (List.map fst rewritten), annot))
      | P_tup pats ->
         let rewritten = List.map (rewrite_pat n env) pats in
         wrap (List.concat (List.map snd rewritten)) (P_aux (P_tup (List.map fst rewritten), annot))
      | P_list pats ->
         let rewritten = List.map (rewrite_pat n env) pats in
         wrap (List.concat (List.map snd rewritten)) (P_aux (P_list (List.map fst rewritten), annot))
      | P_cons (pat1, pat2) ->
         let pat1, guards1 = rewrite_pat n env pat1 in
         let pat2, guards2 = rewrite_pat n env pat2 in
         wrap (guards1 @ guards2) (P_aux (P_cons (pat1, pat2), annot))
      | P_string_append pats ->
         let rewritten = List.map (rewrite_pat n env) pats in
         wrap (List.concat (List.map snd rewritten)) (P_aux (P_string_append (List.map fst rewritten), annot))
      | P_var (pat, tpat) ->
         let pat, guards = rewrite_pat n env pat in
         wrap guards (P_aux (P_var (pat, tpat), annot))
      | P_or _ | P_not _ -> Reporting.unreachable (fst annot) __POS__ "Or and not patterns are currently not implemented"

    and rewrite_guard n env (G_aux (aux, l)) =
      match aux with
      | G_if exp -> G_aux (G_if exp, l), []
      | G_pattern (pat, exp) ->
         let pat, guards = rewrite_pat n env pat in
         G_aux (G_pattern (pat, exp), l), guards

    (* For pattern re-writes that introduce new guards, we need to
       check those guards using the environment that the first
       existing guard was originally checked using, or the expression
       if no guard exists *)
    let first_guard_environment guards exp =
      match guards with
      | [] -> env_of exp
      | G_aux (G_if exp, _) :: _ -> env_of exp
      | G_aux (G_pattern (pat, _), _) :: _ -> env_of_pat pat

    let rewrite_case (pat, guards, exp) =
      let n = ref 0 in
      let pat, guards' = rewrite_pat n (first_guard_environment guards exp) pat in
      let rewritten_guards = List.map (rewrite_guard n (env_of exp)) guards in
      Pat_case (pat, guards' @ List.map fst rewritten_guards @ List.concat (List.map snd rewritten_guards), exp)

    let rewrite_exp = fold_exp { id_algebra with pat_case = rewrite_case }

    let rewrite_funcl (FCL_aux (FCL_Funcl (f, Pat_aux (Pat_case (pat, guards, exp), p_l)), annot)) =
      FCL_aux (FCL_Funcl (f, Pat_aux (rewrite_case (pat, guards, rewrite_exp exp), p_l)), annot)

    let rewrite_fundef (FD_aux (FD_function (rec_opt, tannot_opt, effect_opt, funcls), annot)) =
      FD_aux (FD_function (rec_opt, tannot_opt, effect_opt, List.map rewrite_funcl funcls), annot)

    let rewrite_mapcl (MCL_aux (aux, annot)) =
      match aux with
      | MCL_forwards (Pat_aux (Pat_case (pat, guards, exp), p_l)) ->
         MCL_aux (MCL_forwards (Pat_aux (rewrite_case (pat, guards, rewrite_exp exp), p_l)), annot)
      | MCL_backwards (Pat_aux (Pat_case (pat, guards, exp), p_l)) ->
         MCL_aux (MCL_backwards (Pat_aux (rewrite_case (pat, guards, rewrite_exp exp), p_l)), annot)
      | MCL_bidir _ ->
         Reporting.unreachable (fst annot) __POS__ "Bi-directional mapping clauses should have been removed before pattern rewriting"

    let rewrite_mapdef (MD_aux (MD_mapping (m, args, tannot_opt, mapcls), annot)) =
      MD_aux (MD_mapping (m, args, tannot_opt, List.map rewrite_mapcl mapcls), annot)

    let rewrite_def = function
      | DEF_fundef fdef -> DEF_fundef (rewrite_fundef fdef)
      | DEF_mapdef mdef -> DEF_mapdef (rewrite_mapdef mdef)
      | def -> def

    let rewrite (Defs defs) = Defs (List.map rewrite_def defs)

  end
end

(* Rewrite a view pattern of the form

   p <- f(x, y, z) => ...
   into
   id let p = f(x, y, z, id) => ...

   i.e. it turns view patterns into pattern guards. *)
module View_config = struct
  let id_root = "view"

  let action l = function
    | P_aux (P_view (pat, id, args), (l, _)) ->
       let args = List.map Type_check.strip_exp args in
       Subst_id (fun s ->
           [G_aux (G_pattern (Type_check.strip_pat pat, mk_exp ~loc:l (E_app (id, args @ [mk_exp ~loc:l (E_id s)]))), l)]
         )
    | _ -> No_change
end

module View_rewriter = Pattern_rewriter.Make(View_config)

(* Rewrite a bitvector pattern of the form

   p_1 @ ... @ p_n => ...
   into
   id let p_1 = id[hi_1 .. lo_1], ... , let p_n = id[hi_n .. lo_n] => ... *)
module Bitvector_concat_config = struct
  let id_root = "v"

  let action l = function
    | P_aux (P_vector_concat pats, annot) ->
       let open Type_check in
       let env = env_of_annot annot in
       let typ = typ_of_annot annot in
       let lengths = List.map (fun pat ->
                         match destruct_bitvector env (typ_of_pat pat) with
                         | Some (Nexp_aux (Nexp_constant n, _), _) -> n
                         | _ -> Reporting.unreachable l __POS__ "Non-constant width bitvector concat subpattern found in rewrite"
                       ) pats in
       let _, ranges = List.fold_left (fun (lo, ranges) len -> let hi = Big_int.add lo len in (hi, (Big_int.pred hi, lo) :: ranges)) (Big_int.zero, []) (List.rev lengths) in
       let pats = List.map Type_check.strip_pat pats in
       Subst_id (fun s ->
           List.map2 (fun pat (hi, lo) -> G_aux (G_pattern (pat, mk_exp ~loc:l (E_vector_subrange (mk_exp ~loc:l (E_id s), mk_lit_exp (L_num hi), mk_lit_exp (L_num lo)))), l)) pats ranges
         )
    | _ -> No_change
end

module Bitvector_concat_rewriter = Pattern_rewriter.Make(Bitvector_concat_config)

module Literal_config = struct
  let id_root = "l"

  let action l = function
    | P_aux (P_lit (L_aux (L_unit, _)), annot) -> No_change
    | P_aux (P_lit (L_aux (lit, _)), annot) ->
       Subst_id (fun s ->
           [G_aux (G_if (locate (fun _ -> l) (mk_exp (E_app_infix (mk_exp (E_id s), mk_id "==", mk_lit_exp lit)))), l)]
         )
    | _ -> No_change
end

module Literal_rewriter = Pattern_rewriter.Make(Literal_config)

(* Rewrite a string append pattern of the form

   s_1 ^ ... ^ s_n => ...
   into
   id let (g_1, ... , g_n) = split(), let s_1 = g_1, ... , let s_n = g_n => ...

   where g_1 to g_n are the groups described by the regular expression that splits the string pattern, performed by split() *)
module String_append_config = struct
  let id_root = "s"

  let action _ _ = No_change
end

module String_append_rewriter = Pattern_rewriter.Make(String_append_config)

(**************************************************************************)
(* 2. Guard removal                                                       *)
(**************************************************************************)

let trivially_pure_functions =
  [ "vector_subrange" ]
  |> List.map mk_id
  |> IdSet.of_list

let rec trivially_pure (E_aux (aux, _)) =
  match aux with
  | E_id _ | E_lit _ -> true
  | E_app (f, args) ->
     IdSet.mem f trivially_pure_functions && List.for_all trivially_pure args
  | _ -> false

(* The idea behind this step is if we have a list of guards in a case
   expression

   g_0, ... , g_n => exp

   we want to push any irrefutable pattern guards into exp as
   letbindings. This is done by taking the list of guards with '<=' as
   a special element representing moving a guard from the right to the
   left, i.e. we start with

   <=, g_n, ... , g_0

   and then apply commutativity rules until we have something like

   g_n, g_{n-2}, <=, g_{n-1}, ... , g_0 which then becomes

   g_0 , ... , g_{n-1} => let X in exp

   where X are letbindings equivalent to the irrefutable pattern
   guards g_n and g_{n-2} on the left of '<='.

   The reason to do this is to reduce the amount of work that needs to
   be done by the generic guard removal step. *)

let swap_guards guards =
  let swap = function
    | None, Some (G_aux (G_pattern (pat, exp), l)) when irrefutable pat ->
       Some (Some (G_aux (G_pattern (pat, exp), l)), None)

    | Some (G_aux (G_if cond, l1)), Some (G_aux (G_pattern (pat, exp), l2)) when irrefutable pat && trivially_pure exp ->
       let P_aux (_, annot) = pat in
       let cond = E_aux (E_let (LB_aux (LB_val (pat, exp), annot), cond), annot) in
       Some (Some (G_aux (G_pattern (pat, exp), l2)), Some (G_aux (G_if cond, l1)))

    | _, _ -> None
  in

  let rec apply_swaps guards =
    let swaps = ref 0 in
    let rec swap_list = function
      | x :: y :: zs ->
         begin match swap (x, y) with
         | Some (y, x) ->
            incr swaps;
            y :: swap_list (x :: zs)
         | None -> x :: swap_list (y :: zs)
         end
      | [x] -> [x]
      | [] -> []
    in
    let lhs, rhs = Util.take_drop Util.is_some guards in
    let rhs = swap_list rhs in
    if !swaps > 0 then
      apply_swaps (lhs @ rhs)
    else
      lhs @ rhs
  in

  let guards = None :: List.rev_map (fun x -> Some x) guards in
  List.rev (Util.option_these (apply_swaps guards))

let rewrite_case (pat, guards, exp) =
  Pat_case (pat, swap_guards guards, exp)

let rewrite_exp = fold_exp { id_algebra with pat_case = rewrite_case }

let rewrite_funcl (FCL_aux (FCL_Funcl (f, Pat_aux (Pat_case (pat, guards, exp), p_l)), annot)) =
  FCL_aux (FCL_Funcl (f, Pat_aux (rewrite_case (pat, guards, rewrite_exp exp), p_l)), annot)

let rewrite_fundef (FD_aux (FD_function (rec_opt, tannot_opt, effect_opt, funcls), annot)) =
  FD_aux (FD_function (rec_opt, tannot_opt, effect_opt, List.map rewrite_funcl funcls), annot)

let rewrite_mapcl (MCL_aux (aux, annot)) =
  match aux with
  | MCL_forwards (Pat_aux (Pat_case (pat, guards, exp), p_l)) ->
     MCL_aux (MCL_forwards (Pat_aux (rewrite_case (pat, guards, rewrite_exp exp), p_l)), annot)
  | MCL_backwards (Pat_aux (Pat_case (pat, guards, exp), p_l)) ->
     MCL_aux (MCL_backwards (Pat_aux (rewrite_case (pat, guards, rewrite_exp exp), p_l)), annot)
  | MCL_bidir _ ->
     Reporting.unreachable (fst annot) __POS__ "Bi-directional mapping clauses should have been removed before pattern rewriting"

let rewrite_mapdef (MD_aux (MD_mapping (m, args, tannot_opt, mapcls), annot)) =
  MD_aux (MD_mapping (m, args, tannot_opt, List.map rewrite_mapcl mapcls), annot)

let rewrite_def = function
  | DEF_fundef fdef -> DEF_fundef (rewrite_fundef fdef)
  | DEF_mapdef mdef -> DEF_mapdef (rewrite_mapdef mdef)
  | def -> def

let swap_guards (Defs defs) = Defs (List.map rewrite_def defs)

let get_loc_exp (E_aux (_,(l,_))) = l

let annot_exp_effect e_aux l env typ effect = E_aux (e_aux, (l, Type_check.mk_tannot env typ effect))
let annot_exp e_aux l env typ = annot_exp_effect e_aux l env typ no_effect
                            
let rec pat_to_exp ((P_aux (pat,(l,annot))) as p_aux) =
  let open Type_check in
  let rewrap e = E_aux (e,(l,annot)) in
  let env = env_of_pat p_aux in
  let typ = typ_of_pat p_aux in
  match pat with
  | P_lit lit -> rewrap (E_lit lit)
  | P_wild -> raise (Reporting.err_unreachable l __POS__
      "pat_to_exp given wildcard pattern")
  | P_or(pat1, pat2) -> (* todo: insert boolean or *) pat_to_exp pat1 
  | P_not(pat) -> (* todo: insert boolean not *) pat_to_exp pat
  | P_as (pat,id) -> rewrap (E_id id)
  | P_var (pat, _) -> pat_to_exp pat
  | P_typ (_,pat) -> pat_to_exp pat
  | P_id id -> rewrap (E_id id)
  | P_app (id,pats) -> rewrap (E_app (id, List.map pat_to_exp pats))
  | P_vector pats -> rewrap (E_vector (List.map pat_to_exp pats))
  | P_vector_concat pats -> begin
      let empty_vec = E_aux (E_vector [], (l,())) in
      let concat_vectors vec1 vec2 =
        E_aux (E_vector_append (vec1, vec2), (l, ()))
      in
      check_exp env (List.fold_right concat_vectors (List.map (fun p -> strip_exp (pat_to_exp p)) pats) empty_vec) typ
    end
  | P_tup pats -> rewrap (E_tuple (List.map pat_to_exp pats))
  | P_list pats -> rewrap (E_list (List.map pat_to_exp pats))
  | P_cons (p,ps) -> rewrap (E_cons (pat_to_exp p, pat_to_exp ps))
  | P_string_append (pats) -> begin
      let empty_string = annot_exp (E_lit (L_aux (L_string "", l))) l env string_typ in
      let string_append str1 str2 =
        annot_exp (E_app (mk_id "string_append", [str1; str2])) l env string_typ
      in
      (List.fold_right string_append (List.map pat_to_exp pats) empty_string)
    end
                            
let case_exp e t cs =
  let open Type_check in
  let l = get_loc_exp e in
  let env = env_of e in
  let annot = (get_loc_exp e, Some (env_of e, t, no_effect)) in
  match cs with
  | [(P_aux (P_wild, _), body, _)] ->
     fix_eff_exp body
  | [(P_aux (P_id id, pannot) as pat, body, _)] ->
     fix_eff_exp (annot_exp (E_let (LB_aux (LB_val (pat, e), pannot), body)) l env t)
  | _ ->
     let pexp (pat,body,annot) = Pat_aux (Pat_case (pat,[],body),annot) in
     let ps = List.map pexp cs in
     fix_eff_exp (annot_exp (E_case (e, ps)) l env t)
                            
(* Rewrite guarded patterns into a combination of if-expressions and
   unguarded pattern matches

   Strategy:
   - Split clauses into groups where the first pattern subsumes all the
     following ones
   - Translate the groups in reverse order, using the next group as a
     fall-through target, if there is one
   - Within a group,
     - translate the sequence of clauses to an if-then-else cascade using the
       guards as long as the patterns are equivalent modulo substitution, or
     - recursively translate the remaining clauses to a pattern match if
       there is a difference in the patterns.

  TODO: Compare this more closely with the algorithm in the CPP'18 paper of
  Spector-Zabusky et al, who seem to use the opposite grouping and merging
  strategy to ours: group *mutually exclusive* clauses, and try to merge them
  into a pattern match first instead of an if-then-else cascade.
*)
let rewrite_guarded_clauses l env pat_typ typ cs =
  let open Type_check in
  let rec group fallthrough clauses =
    let add_clause (pat,cls,annot) c = (pat,cls @ [c],annot) in
    let rec group_aux current acc = (function
      | ((pat,guard,body,annot) as c) :: cs ->
          let (current_pat,_,_) = current in
          (match subsumes_pat current_pat pat with
            | Some substs ->
                let pat' = List.fold_left subst_id_pat pat substs in
                let guard' = (match guard with
                  | [G_aux (G_if exp, l)] -> [G_aux (G_if (List.fold_left subst_id_exp exp substs), l)]
                  | _ -> []) in
                let body' = List.fold_left subst_id_exp body substs in
                let c' = (pat',guard',body',annot) in
                group_aux (add_clause current c') acc cs
            | None ->
                let pat = match cs with _::_ -> remove_wildcards "g__" pat | _ -> pat in
                group_aux (pat,[c],annot) (acc @ [current]) cs)
      | [] -> acc @ [current]) in
    let groups = match clauses with
      | [(pat,guard,body,annot) as c] ->
          [(pat, [c], annot)]
      | ((pat,guard,body,annot) as c) :: cs ->
          group_aux (remove_wildcards "g__" pat, [c], annot) [] cs
      | _ ->
          raise (Reporting.err_unreachable l __POS__
            "group given empty list in rewrite_guarded_clauses") in
    let add_group cs groups = (if_pexp (groups @ fallthrough) cs) :: groups in
    List.fold_right add_group groups []
  and if_pexp fallthrough (pat,cs,annot) = (match cs with
    | c :: _ ->
        let body = if_exp fallthrough pat cs in
        (pat, body, annot)
    | [] ->
        raise (Reporting.err_unreachable l __POS__
            "if_pexp given empty list in rewrite_guarded_clauses"))
  and if_exp fallthrough current_pat = (function
    | (pat,guard,body,l) :: ((pat',guard',body',l') as c') :: cs ->
        (match guard with
          | [G_aux (G_if exp, l)] ->
              let else_exp =
                if equiv_pats current_pat pat'
                then if_exp fallthrough current_pat (c' :: cs)
                else case_exp (pat_to_exp current_pat) (typ_of body') (group fallthrough (c' :: cs)) in
              fix_eff_exp (annot_exp (E_if (exp,body,else_exp)) l (env_of exp) (typ_of body))
          | _ -> body)
    | [(pat,guard,body,l)] ->
        (* For singleton clauses with a guard, use fallthrough clauses if the
           guard is not satisfied, but only those fallthrough clauses that are
           not disjoint with the current pattern *)
        let overlapping_clause (pat, _, _) = not (disjoint_pat env current_pat pat) in
        let fallthrough = List.filter overlapping_clause fallthrough in
        (match guard, fallthrough with
          | [G_aux (G_if exp, l)], _ :: _ ->
              let else_exp = case_exp (pat_to_exp current_pat) (typ_of body) fallthrough in
              fix_eff_exp (annot_exp (E_if (exp,body,else_exp)) l (env_of exp) (typ_of body))
          | _, _ -> body)
    | [] ->
        raise (Reporting.err_unreachable l __POS__
            "if_exp given empty list in rewrite_guarded_clauses")) in
  let is_complete = Pattern_completeness.is_complete (Env.pattern_completeness_ctx env) (List.map construct_pexp cs) in
  let fallthrough =
    if not is_complete then
      let p = P_aux (P_wild, (gen_loc l, mk_tannot env pat_typ no_effect)) in
      let msg = "Pattern match failure at " ^ Reporting.short_loc_to_string l in
      let a = mk_exp ~loc:(gen_loc l) (E_assert (mk_lit_exp L_false, mk_lit_exp (L_string msg))) in
      let b = mk_exp ~loc:(gen_loc l) (E_exit (mk_lit_exp L_unit)) in
      let e = check_exp env (mk_exp ~loc:(gen_loc l) (E_block [a; b])) typ in
      [(p,[],e,l)]
    else []
  in
  group [] (cs @ fallthrough)
