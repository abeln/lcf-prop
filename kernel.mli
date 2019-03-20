open Defs
open Defs.Context

exception Kernel_err of string

(* The abstract type of proofs. Remains abstract so that only the kernel can build proofs. *)
type proof

(* The sequent implied by the proof. *)
val infer: proof -> sequent

(* Do a one-step unrolling of the proof. A `proof deriv` is then a read-only view of the proof. *)
val unroll: proof -> proof deriv

(*
----------
G, P => P
*)
val init: context * hyp -> proof

(*
------
G => T
*)
val trueR: context -> proof

(*
----------
G, ⟂ => C
*)
val falseL: context * hyp * prop -> proof

(*
G => A  G => B
--------------
G => A ∧ B
*)
val conjR: proof * proof -> proof

(*
G, A ∧ B, A => C
----------------
G, A ∧ B => C 
*)
val conjL1: proof * hyp * hyp -> proof

(*
G, A ∧ B, B => C
----------------
G, A ∧ B => C 
*)
val conjL2: proof * hyp * hyp -> proof

(*
G => A
----------
G => A ∨ B
*)
val disjR1: proof * prop -> proof

(*
G => B
----------
G => A ∨ B
*)
val disjR2: proof * prop -> proof

(*
G, A => C
G, B => C
-------------
G, A ∨ B => C
*)
val disjL: proof * hyp * proof * hyp -> proof

(*
G, A => B
-----------
G => A ⊃ B
*)
val implR: proof * hyp -> proof

(*
G, A ⊃ B => A
G, A ⊃ B, B => C 
-----------------
G, A ⊃ B => C
*)
val implL: proof * hyp * proof * hyp * hyp -> proof
