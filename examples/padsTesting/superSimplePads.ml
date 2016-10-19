open Pads
open PadsParser
open PadsLoader

[%%pads {| 
  ptype parseUntil = Pstring(".")
  ptype testInt = Pint
  ptype testFloat = Pfloat
  ptype stringFixed = Pstring(5)
  ptype constString = "Hello"
|}]

let (x,_) = parseUntil_parse "test.txt"
let () = print_endline x

let (x,_,_) = parseUntil_parse_state {current = "Hello there." ; rest = []; loc = start_loc}
let () = print_endline x

let (x,_,_) = testInt_parse_state {current = "1234Hello there." ; rest = []; loc = start_loc}
let () = print_int x
let () = print_newline ()

let (x,_,_) = testFloat_parse_state {current = "1234.456Hello there." ; rest = []; loc = start_loc}
let () = print_float x
let () = print_newline ()

let (x,_,_) = stringFixed_parse_state {current = "Hello there." ; rest = []; loc = start_loc}
let () = print_endline x

let (_,_,new_state) = stringFixed_parse_state {current = "HelloHello there." ; rest = []; loc = start_loc}
let (x,_,_) = parseUntil_parse_state new_state
let () = print_endline x
