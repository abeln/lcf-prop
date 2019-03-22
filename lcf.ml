open Core
open Defs
open Defs.Context
open List

module K = Kernel
module R = Refiner

let a_and_b = Conj (Atom "A", Atom "B")
let b_or_c = Disj (Atom "B", Atom "C")

(* Operations on proof state *)
module State = 
struct
  type rule =
      Back
    | Rule of R.rule

  (* The state of the interactive proof. *)
  type state =
      Done of K.proof
    | FinalGoal of R.goal
    | Subgoal of R.goal list * K.proof list * R.validation * state

  (* Normalize a state by generating a proof if all subgoals have been proven before. *)
  let rec norm st =
    let use_proof proof st = match st with
    | Done _ -> st
    | FinalGoal _ -> Done proof
    | Subgoal (subgoals, proofs, valid_fn, next_st) -> Subgoal (tl_exn subgoals, proof :: proofs, valid_fn, next_st)
    in
    match st with
    | Subgoal ([], proofs, valid_fn, next_st) -> norm (use_proof (valid_fn (List.rev proofs)) next_st)
    | _ -> st

  (* Apply a rule to a specific goal and state, generating a next state (or an error). *)
  let apply_to_goal rule goal state =
    let next_res =
      try
        Ok (rule goal)
      with
      | K.Kernel_err msg -> Error msg
      | R.Refiner_err msg -> Error msg
    in
    match next_res with
    | Error msg -> Error msg
    | Ok (subgoals, valid_fn) -> Ok (norm (Subgoal (subgoals, [], valid_fn, state)))

  (* Advance a state one or more steps, by applying the rule. Or indicate
    why the rule can't be applied. *)
  let apply rule state = match state with
  | Done _ -> Error "the proof is done"
  | FinalGoal goal -> apply_to_goal rule goal state
  | Subgoal (goals, _, _, _) ->
      (* We maintain the invariant that the list of subgoals is non-empty. *)
      let subgoal = hd_exn goals in
      apply_to_goal rule subgoal state

end

let () =
  print_endline (pprint_ctx [b_or_c; a_and_b]);
  print_endline (pprint_ctx (ctx_add (ctx_add empty_ctx b_or_c) a_and_b))