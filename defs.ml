open Core

type prop = 
  True                    (* T *)
| False                   (* ⟂ *)
| Atom of string          (* P *)
| Conj of prop * prop     (* A ∧ B *)
| Disj of prop * prop     (* A ∨ B *)
| Impl of prop * prop     (* A => B *)

type context = prop list

let empty_ctx = []

let rec ctx_add ctx prop =
  match ctx with
  | [] -> [prop]
  | p :: ps ->
    if (prop <= p) then prop :: p :: ps
    else p :: (ctx_add ps prop)

let ctx_rm ctx index = List.filteri ctx ~f: (fun i _ -> i <> index)

type sequent = Seq of context * prop

let rec pprint_prop = function
| True -> "T"
| False -> "⟂"
| Atom p -> p
| Conj (l, r) -> "(" ^ (pprint_prop l) ^ " ∧ " ^ (pprint_prop r) ^ ")"
| Disj (l, r) ->  "(" ^ (pprint_prop l) ^ " ∨ " ^ (pprint_prop r) ^ ")"
| Impl (l, r) ->  "(" ^ (pprint_prop l) ^ " => " ^ (pprint_prop r) ^ ")"

let pprint_ctx ctx = "[" ^ String.concat ~sep:"," (List.map ctx ~f:pprint_prop) ^ "]"