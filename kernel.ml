open Core
open Defs

module Ctx = Context

exception Kernel_err of string

type proof = By of sequent * proof deriv

let infer (By (seq, _)) = seq

let unroll (By (_, der)) = der

let init (ctx, hyp) = By (Seq (ctx, List.nth_exn ctx hyp), DInit hyp)

let trueR ctx = By (Seq (ctx, True), DTrueR)

let falseL (ctx, hyp, prop) =
  match (List.nth_exn ctx hyp) with
  | False -> By (Seq (ctx, prop), DFalseL hyp)
  | _ -> raise (Kernel_err "falseL does not apply")

let kernel_assert rule cond =
  if (not cond) then raise (Kernel_err (rule ^ " does not apply"))
  else ()

let conjR (((By ((Seq (ctx1, succ1)), _)) as der1), ((By ((Seq (ctx2, succ2)), _)) as der2)) =
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


let disjR1 ((By (Seq (ctx, succ), _) as der), prop) =
  By (Seq (ctx, Disj (succ, prop)), DDisjR1 der)

let disjR2 ((By (Seq (ctx, succ), _) as der), prop) =
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

let implR ((By (Seq (ctx, succ), _) as der), hyp) =
  try
    let prop = List.nth_exn ctx hyp in
    let ctx1 = Ctx.ctx_rem ctx hyp in
    By (Seq (ctx1, Impl (prop, succ)), DImplR (der, hyp))
  with
  | _ -> (raise (Kernel_err "implR does not apply"))

let implL (proof1, impl_hyp1, proof2, impl_hyp2, succ_hyp2) =
  try
    let (By (Seq (ctx1, succ1), _)) = proof1 in
    let (By (Seq (ctx2, succ2), _)) = proof2 in
    let ctx1p = Ctx.ctx_rem ctx1 impl_hyp1 in
    let ctx2p = Ctx.ctx_rem (Ctx.ctx_rem ctx2 impl_hyp2) succ_hyp2 in
    kernel_assert "implL" (ctx1p = ctx2p);
    let Impl (a1, b1) = List.nth_exn ctx1 impl_hyp1 in
    kernel_assert "implL" (a1 = succ1);
    let Impl (a2, b2) = List.nth_exn ctx2 impl_hyp2 in
    kernel_assert "implL" (a1 = a2 && b1 = b2);
    let b = List.nth_exn ctx2 succ_hyp2 in
    kernel_assert "implL" (b2 = b);
    By (Seq (ctx1, succ2), DImplL (proof1, impl_hyp1, proof2, impl_hyp2, succ_hyp2))
  with
  | _ -> (raise (Kernel_err "implL does not apply"))
