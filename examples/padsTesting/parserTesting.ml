open PadsParser

let state1 =
  let _ = print_endline "Calculating state 1" in 
  process_string "hello" { current_line = "hello"; rest = ["hello world"]; loc = start_loc }

let state2 =
  let _ = print_endline "Calculating state 2" in
  process_string "he" (inc_line state1)

let _ = print_endline "Testing state 1" in
assert (state1.loc.line = 1);
print_endline "Testing state 2";
assert (state2.loc.line = 2);
assert (state2.loc.character = 3)
