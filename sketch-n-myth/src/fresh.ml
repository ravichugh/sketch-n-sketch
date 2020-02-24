let hole_counter =
  ref 0

let set_largest_hole h =
  hole_counter := h

let gen_hole () =
  hole_counter := !hole_counter + 1;
  !hole_counter
