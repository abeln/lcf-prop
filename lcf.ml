open Core

type prop = 
    True                    (* T *)
  | False                   (* ⟂ *)
  | Atom of string          (* P *)
  | Conj of prop * prop     (* A ∧ B *)
  | Disj of prop * prop     (* A ∨ B *)
  | Impl of prop * prop     (* A => B *)

type context = prop list

type sequent = Seq of context * prop

let rec pprint = function
    | True -> "T"
    | False -> "⟂"
    | Atom p -> p
    | Conj (l, r) -> "(" ^ (pprint l) ^ " ∧ " ^ (pprint r) ^ ")"
    | Disj (l, r) ->  "(" ^ (pprint l) ^ " ∨ " ^ (pprint r) ^ ")"
    | Impl (l, r) ->  "(" ^ (pprint l) ^ " => " ^ (pprint r) ^ ")"

let a_and_b = Conj (Atom "A", Atom "B")

module type KERNEL = sig
    (* The abstract type of proofs. Remains abstract so that only the kernel can build proofs. *)
    type proof

    (* The sequent implied by the proof. *)
    val infer: proof -> sequent

    (* A hypothesis is just an index into the context. *)
    type hyp = int

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
    val implL: proof * proof * hyp -> proof
end

let () =
    print_endline (pprint a_and_b)