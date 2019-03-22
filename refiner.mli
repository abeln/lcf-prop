open Defs

module K = Kernel

exception Refiner_err of string

type goal = sequent
type subgoals = goal list
type validation = K.proof list -> K.proof
type rule = goal -> subgoals * validation

val init: hyp -> rule
val trueR: rule
val falseL: hyp -> rule
val conjR: rule
val conjL1: hyp -> rule
val conjL2: hyp -> rule
val disjR1: rule
val disjR2: rule
val disjL: hyp -> rule
val implR: rule
val implL: hyp -> rule