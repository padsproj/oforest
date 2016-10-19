open Pads
open PadsParser
let ws = REd ("[ \t]*", "   ")
let label = REd ("[A-Z_0-9]+", "A")
let nL = REd ("\r?\n","\n")

[%%pads {|

 ptype weird_test = Pstring('\n')

 ptype stringLn = Pstring($nL$)

 ptype sList = stringLn Plist("",EOF)

 ptype hello = "hello"

  ptype regex = $RE "[0-9]+"$

  ptype copy = regex

  ptype int_pads = Pint

  ptype float_pads = Pfloat

  ptype l = int_pads Plist(",",".")

  ptype x = { entry1 : Pint ; "hello"; entry2 : "hello" }

  ptype test_record = { entry3 : Pint; $RE "[0-9]+"$; "hello"; entry4 : Pint }

  pdatatype test_variant =
    | Name of "while"
    | Name2 of Pint

  ptype test_swat = { x : Pint;  $RE "[ \t]+"$; ":"; y : $RE ".*"$ }

  ptype tup_test = Pint * "hello" * $RE "[0-9]+"$

(*
  ptype char_pads = Pchar

  ptype l2 = char_pads Plist("",6)
*)

|}]

let main =
  let (h_rep, h_md, h_state) = hello_parse_state {current="hello0000 world"; rest=[]; loc=start_loc} in
  Printf.printf "At: %d\n" h_state.loc.character;
  let (r_rep, r_md, r_state) = regex_parse_state h_state in
  Printf.printf "At: %d\n" r_state.loc.character;
  Printf.printf "matched: %s\n" r_rep;
  let (c_rep, c_md, c_state) = copy_parse_state {current="0000hello";  rest=[]; loc=start_loc} in
  Printf.printf "At: %d\n" c_state.loc.character;
  Printf.printf "matched: %s\n" c_rep;
  let (test_string_rep, _, test_string_state) = hello_parse_state c_state in
  Printf.printf "At: %d\n" test_string_state.loc.character;
  let (i1_rep, i1_md, i1_state) = int_pads_parse_state {current="3\t3";  rest=[]; loc=start_loc} in
  Printf.printf "matched: %d\n" i1_rep;
  let (i2_rep, i2_md, i2_state) = int_pads_parse_state {current="-3\t3";  rest=[]; loc=start_loc} in
  Printf.printf "matched: %d\n" i2_rep;
  let (f_rep, f_md, f_state) = float_pads_parse_state {current="3.0c";  rest=[]; loc=start_loc} in
  Printf.printf "matched: %f\n" f_rep;
  let (l_rep, l_md, l_state) = l_parse_state {current="3,4,5.";  rest=[]; loc=start_loc} in
  Printf.printf "number of entries in list: %d\n" (List.length l_rep);
  let (x_rep, x_md, x_state) = x_parse_state {current="3hellohello";  rest=[]; loc=start_loc} in
  Printf.printf "entry1 in record: %d\n" x_rep.entry1;
  let (test_rep, test_md, test_state) = 
    test_record_parse_state {current="3x4";  rest=[]; loc=start_loc} in
  List.iter (fun s -> Printf.printf "%s\n" s) test_md.pads_error_msg;
  let (v_rep, v_md, v_state) =
    test_variant_parse_state {current="3";  rest=[]; loc=start_loc} in
  begin
    match v_rep with
    | Name2 x -> Printf.printf "Variant parse int: %d\n" x
    | _ -> failwith "Incorrect"
  end;
  let (swat_rep, swat_md, swat_state) = test_swat_parse_state {current="3   : hello";  rest=[]; loc=start_loc } in
  print_endline (test_swat_to_string (swat_rep,swat_md));
  Printf.printf "to string: %s\n" (x_to_string ({entry1 = 3; entry2 = ()},x_md));
  let dir = Filename.concat (Sys.getcwd ())  "../swatData/TxtInOut_currentnew" in
  let (r1,m1) = stringLn_parse (Filename.concat dir "file.cio") in
  Printf.printf "Errors: %d\n" (m1.pads_num_errors);
  List.iter print_endline m1.pads_error_msg;
  ()
  (*
  let (c_rep, c_md, c_state) = char_pads_parse_state f_state in
  Printf.printf "matched: %c\n" c_rep;
  let (l2_rep, l2_md, l2_state) = l2_parse_state {current="forest"; loc=start_loc} in
  Printf.printf "number of entries in repeated list: %d\n" (List.length l2_rep);
  Printf.printf "to string: %s\n" (l2_to_string (l2_rep,l2_md));
  *)

    (* Old:

  let (r1,m1) = stringLn_parse (Filename.concat dir "file.cio") in
  Printf.printf "StrLn rep: %s\n" r1.str;
  Printf.printf "StrLn Errors: %d\n" (m1.pads_num_errors);
  List.iter print_endline m1.pads_error_msg;
    *)
