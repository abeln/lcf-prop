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