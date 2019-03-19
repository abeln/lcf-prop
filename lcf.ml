open Core
open Defs

module K: KERNEL = Kernel

let a_and_b = Conj (Atom "A", Atom "B")

let () =
    print_endline (pprint a_and_b)