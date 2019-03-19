open Core
open Defs

(* module K = Kernel *)

let a_and_b = Conj (Atom "A", Atom "B")
let b_or_c = Disj (Atom "B", Atom "C")

let () =
    print_endline (pprint_ctx [b_or_c; a_and_b]);
    print_endline (pprint_ctx (ctx_add (ctx_add empty_ctx b_or_c) a_and_b))