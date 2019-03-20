open Core
open Defs

module Ctx = Context

exception Kernel_err of string

type proof = By of sequent * proof deriv

let infer (By (seq, _)) = seq

let unroll (By (_, der)) = der

let init (ctx, hyp) = List.nth_exn ctx hyp

let trueR ctx = By (Seq (ctx, True), DTrueR)

let falseL (ctx, hyp, prop) =
  match (List.nth_exn ctx hyp) with
  | False -> By (Seq (ctx, prop), DFalseL hyp)
  | _ -> raise (Kernel_err "falseL does not apply")

let kernel_assert rule cond =
  if (not cond) then raise (Kernel_err (rule ^ " does not apply"))
  else ()

let conjR ((By ((Seq (ctx1, succ1)), _)) as der1) ((By ((Seq (ctx2, succ2)), _)) as der2) =
  kernel_assert "conjR" (ctx1 = ctx2); 
  By (Seq (ctx1, Conj (succ1, succ2)), DConjR (der1, der2))

let conjL1 ((By (Seq (ctx, prop), _)) as der, conj_hyp, left_hyp) =
  try
    let Conj (l, _) = List.nth_exn ctx conj_hyp in
    let left = List.nth_exn ctx left_hyp in
    kernel_assert "conjL1" (left = l);
    By (Seq (Ctx.ctx_rem ctx left_hyp, prop), DConjL1 (der, conj_hyp, left_hyp))
   with
   | _ -> raise (Kernel_err "conjL1 does not apply")

let conjL2 ((By (Seq (ctx, prop), _)) as der, conj_hyp, right_hyp) =
  try
    let Conj (_, r) = List.nth_exn ctx conj_hyp in
    let right = List.nth_exn ctx right_hyp in
    kernel_assert "conjL2" (right = r);
    By (Seq (Ctx.ctx_rem ctx right_hyp, prop), DConjL2 (der, conj_hyp, right_hyp))
   with
   | _ -> raise (Kernel_err "conjL2 does not apply")


let disjR1 (By (Seq (ctx, succ), _) as der) prop =
  By (Seq (ctx, Disj (succ, prop)), DDisjR1 der)

let disjR2 (By (Seq (ctx, succ), _) as der) prop =
  By (Seq (ctx, Disj (prop, succ)), DDisjR2 der)

let disjL (proof_left, left_hyp, proof_right, right_hyp) =
  try
    let By (Seq (ctx_left, succ_left), _) = proof_left in
    let prop_left = List.nth_exn ctx_left left_hyp in
    let ctx_left1 = Ctx.ctx_rem ctx_left left_hyp in
    let By (Seq (ctx_right, succ_right), _) = proof_right in
    let prop_right = List.nth_exn ctx_right right_hyp in
    let ctx_right1 = Ctx.ctx_rem ctx_right right_hyp in
    kernel_assert "disjL" (ctx_left1 = ctx_right1 && succ_left = succ_right);
    By (Seq (ctx_left1, Disj (prop_left, prop_right)), DDisjL (proof_left, left_hyp, proof_right, right_hyp))
  with
   | _ -> raise (Kernel_err "disjL does not apply")


(* val implR: proof * hyp -> proof *)

(* val implL: proof * hyp * proof * hyp * hyp -> proof *)
