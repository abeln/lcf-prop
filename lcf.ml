open Core
open Defs
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

  let pprint_goal (Seq (ctx, succ)) =
    List.iter ctx ~f:(fun prop -> print_endline (pprint_prop prop));
    print_endline "===============";
    print_endline (pprint_prop succ)

  let pprint_state st =
    print_endline "\n\n\n";
    match st with
    | Done _ -> print_endline "QED □"
    | FinalGoal goal -> pprint_goal goal
    | Subgoal (goals, _, _, _) -> pprint_goal (hd_exn goals)

end

module St = State

let to_hyp str = int_of_string str

let to_rule cmd = match (String.split cmd ~on:' ') with
| ["init"; hyp] -> R.init (to_hyp hyp)
| ["trueR"] -> R.trueR
| ["falseL"; hyp] -> R.falseL (to_hyp hyp)
| ["conjR"] -> R.conjR
| ["conjL1"; hyp] -> R.conjL1 (to_hyp hyp)
| ["conjL2"; hyp] -> R.conjL2 (to_hyp hyp)
| ["disjR1"] -> R.disjR1
| ["disjR2"] -> R.disjR2
| ["disjL"; hyp] -> R.disjL (to_hyp hyp)
| ["implR"] -> R.implR
| ["implL"; hyp] -> R.implL (to_hyp hyp)
| _ -> raise (Invalid_argument cmd)

let rec repl state =
  St.pprint_state state;
  print_endline "";
  let cmd = In_channel.input_line_exn In_channel.stdin in
  let rule = to_rule cmd in
  match (St.apply rule state) with
  | Error (msg) ->
    print_endline msg;
    repl state
  | Ok (new_st) -> repl new_st

let () =
  let init_st = St.FinalGoal (Seq (Context.empty_ctx, Impl (Conj (Atom "A", Atom "B"), Conj (Atom "B", Atom "A")))) in
  repl init_st