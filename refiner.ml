open Core
open Defs

module K = Kernel
module Ctx = Context

exception Refiner_err of string

type goal = sequent
type subgoals = goal list
type validation = K.proof list -> K.proof
type rule = goal -> subgoals * validation

(* Helpers *)

let refiner_assert rule cond =
  if (not cond) then raise (Refiner_err (rule ^ " does not apply"))
  else ()

let as_conj = function
  | Conj (l, r) -> (l, r)
  | _ -> raise (Refiner_err "can't deconstruct as conj")

let as_disj = function
  | Disj (l, r) -> (l, r)
  | _ -> raise (Refiner_err "can't deconstruct as disj")

let as_impl = function
  | Impl (ant, succ) -> (ant, succ)
  | _ -> raise (Refiner_err "can't deconstruct as impl")

(* Module impl *)

let init hyp (Seq (ctx, succ)) =
  let prop = List.nth_exn ctx hyp in
  refiner_assert "init" (prop = succ);
  let valid = fun _ -> K.init (ctx, hyp) in
  ([], valid)

let trueR (Seq (ctx, succ)) =
  refiner_assert "trueR" (succ = True);
  let valid = fun _ -> K.trueR ctx in
  ([], valid)

let falseL hyp (Seq (ctx, succ)) =
  let false_prop = List.nth_exn ctx hyp in
  refiner_assert "falseL" (false_prop = False);
  let valid = fun _ -> K.falseL (ctx, hyp, succ) in
  ([], valid)

let conjR (Seq (ctx, succ)) =
  let (l, r) = as_conj succ in
  let valid =
    fun subgs -> match subgs with
    | [pl; pr] -> K.conjR (pl, pr)
    | _ -> raise (Refiner_err "can't apply conjR: expected two subgoals")
  in
  ([Seq (ctx, l); Seq (ctx, r)], valid)

let conjL1 hyp (Seq (ctx, succ)) =
  let (l, r) = as_conj (List.nth_exn ctx hyp) in
  let ctx1 = Ctx.ctx_add ctx l in
  let loc_conj = Ctx.ctx_find ctx1 (Conj (l, r)) in
  let loc_left = Ctx.ctx_find ctx1 l in
  let valid =
    fun subgs -> match subgs with
    | [p] -> K.conjL1 (p, loc_conj, loc_left)
    | _ -> raise (Refiner_err "can't apply conjL1: expected one subgoal")
  in
  ([Seq (ctx1, succ)], valid)

let conjL2 hyp (Seq (ctx, succ)) =
  let (l, r) = as_conj (List.nth_exn ctx hyp) in
  let ctx1 = Ctx.ctx_add ctx r in
  let loc_conj = Ctx.ctx_find ctx1 (Conj (l, r)) in
  let loc_right = Ctx.ctx_find ctx1 l in
  let valid =
    fun subgs -> match subgs with
    | [p] -> K.conjL2 (p, loc_conj, loc_right)
    | _ -> raise (Refiner_err "can't apply conjL2: expected one subgoal")
  in
  ([Seq (ctx1, succ)], valid)

let disjR1 (Seq (ctx, succ)) =
  let (l, r) = as_disj succ in
  let valid = fun p -> K.disjR1 (p, r) in
  ([Seq (ctx, l)], valid)

let disjR2 (Seq (ctx, succ)) =
  let (l, r) = as_disj succ in
  let valid = fun p -> K.disjR2 (p, l) in
  ([Seq (ctx, r)], valid)

let disjL hyp (Seq (ctx, succ)) =
  let (l, r) = as_disj (List.nth_exn ctx hyp) in
  let ctx_left = Ctx.ctx_add ctx l in
  let ctx_right = Ctx.ctx_add ctx r in
  let loc_left = Ctx.ctx_find ctx_left l in
  let loc_right = Ctx.ctx_find ctx_right r in
  let valid =
    fun subgs -> match subgs with
    | [proof_left, proof_right] -> K.disjL (proof_left, loc_left, proof_right, loc_right)
    | _ -> raise (Refiner_err "can't apply disjL: expected two subgoals")
  in
  ([Seq (ctx_left, succ); Seq (ctx_right, succ)], valid)

(* 
val implR: rule
val implL: hyp -> rule *)