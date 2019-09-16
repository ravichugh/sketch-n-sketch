(*******************************************************************************
 * Information
 *)

let now () =
  Unix.gettimeofday ()

(*******************************************************************************
 * Control-Flow Timing
 *)

type kind =
  | Total
  | Eval

exception Max_time_exceeded of kind

let (total, eval) =
  (ref 0.0, ref 0.0)

let kind_info kind =
  match kind with
    | Total -> (total, Params.max_total_time)
    | Eval -> (eval, Params.max_eval_time)

let start kind =
  let (initial, _) =
    kind_info kind
  in
    initial := now ()

let elapsed kind =
  let (initial, _) =
    kind_info kind
  in
    now () -. !initial

let check_cutoff kind =
  let (_, cutoff) =
    kind_info kind
  in
    if elapsed kind > cutoff then
      raise_notrace (Max_time_exceeded kind)
    else
      ()

let handle kind computation callback =
  start kind;
  let output =
    try
      computation ()
    with
      Max_time_exceeded kind_raised ->
        if kind_raised = kind then
          callback ()
        else
          raise_notrace (Max_time_exceeded kind_raised)
  in
  let time_taken =
    elapsed kind
  in
    (output, time_taken)

(*******************************************************************************
 * Accumulators
 *)

type accumulator =
  | Guess

let guess =
  ref 0.0

let acc_info acc =
  match acc with
    | Guess -> (guess, Params.max_guess_time)

let reset_accumulator acc =
  let (time_taken, _) =
    acc_info acc
  in
    time_taken := 0.0

let accumulate acc computation =
  let initial_time =
    now ()
  in
  let output =
    computation ()
  in
  let final_time =
    now ()
  in
  let (time_taken, _) =
    acc_info acc
  in
    time_taken := !time_taken +. (final_time -. initial_time);
    output

let check_accumulator acc =
  let (time_taken, max_time) =
    acc_info acc
  in
    !time_taken < max_time
