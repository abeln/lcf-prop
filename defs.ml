open Core

(* A datatype for propositional logic. *)
type prop = 
    True                    (* T *)
  | False                   (* ⟂ *)
  | Atom of string          (* P *)
  | Conj of prop * prop     (* A ∧ B *)
  | Disj of prop * prop     (* A ∨ B *)
  | Impl of prop * prop     (* A => B *)

(* A context is a list of propositions that we know currently hold.
   Contexts are normalized in that propositions are kept in-order within the context.
   To preserve normalization, all changes to contexts should be done through the context-related methods. *)
module Context = struct
  (* Contexts are normalized by sorting the propositions in them. *)
  type context = prop list

  (* Exception type for context operations. *)
  exception Ctx_err of string

  (* The normalized empty context. *)
  let empty_ctx = []

  (* Add a proposition to a normalized context. *)
  let rec ctx_add ctx prop =
    match ctx with
    | [] -> [prop]
    | p :: ps ->
      if (prop <= p) then prop :: p :: ps
      else p :: (ctx_add ps prop)

  (* Remove a proposition from a normalized context. *)
  let rec ctx_rem ctx index =
    match (ctx, index) with
    | ([], _) -> raise (Ctx_err "removing from empty context")
    | (_ :: ps, 0) -> ps
    | (p :: ps, i) -> p :: (ctx_rem ps (i - 1))
end

type sequent = Seq of Context.context * prop

let rec pprint_prop = function
  | True -> "T"
  | False -> "⟂"
  | Atom p -> p
  | Conj (l, r) -> "(" ^ (pprint_prop l) ^ " ∧ " ^ (pprint_prop r) ^ ")"
  | Disj (l, r) ->  "(" ^ (pprint_prop l) ^ " ∨ " ^ (pprint_prop r) ^ ")"
  | Impl (l, r) ->  "(" ^ (pprint_prop l) ^ " => " ^ (pprint_prop r) ^ ")"

let pprint_ctx ctx = "[" ^ String.concat ~sep:"," (List.map ctx ~f:pprint_prop) ^ "]"