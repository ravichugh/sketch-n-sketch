let now () =
  Unix.gettimeofday ()

module Single = struct
  type t =
    | Total
    | Eval

  let (total, eval) =
    (ref 0.0, ref 0.0)

  let info timer =
    match timer with
      | Total -> (total, Params.max_total_time)
      | Eval -> (eval, Params.max_eval_time)

  let start timer =
    let (initial, _) =
      info timer
    in
      initial := now ()

  let elapsed timer =
    let (initial, _) =
      info timer
    in
      now () -. !initial

  let check timer =
    let (_, cutoff) =
      info timer
    in
      elapsed timer < cutoff
end

module Multi = struct
  type t =
    | Guess

  let guess =
    ref 0.0

  let info timer =
    match timer with
      | Guess -> (guess, Params.max_guess_time)

  let reset timer =
    let (time_taken, _) =
      info timer
    in
      time_taken := 0.0

  let accumulate timer computation =
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
      info timer
    in
      time_taken := !time_taken +. (final_time -. initial_time);
      output

  let check timer =
    let (time_taken, max_time) =
      info timer
    in
      !time_taken < max_time
end
