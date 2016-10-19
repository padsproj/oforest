open All_types (*
open Core.Std
                  *)
open Lexing
open Printf
open Location

exception LexingError of string

let get_position lexbuf =
  let loc = Location.curr lexbuf in
  let ss,sl,sc = get_pos_info loc.loc_start in
  let es,el,ec = get_pos_info loc.loc_end in
  if sl = el
  then sprintf "File %c%s%c, line %d, characters %d-%d:" '"' ss '"' sl sc ec
  else sprintf "File %c%s%c, lines %d-%d, characters %d-%d:" '"' ss '"' sl el sc ec

let print_position outx lexbuf = fprintf outx "%s" (get_position lexbuf)


let x_parse_with_error parse lex lexbuf =
  try parse lex lexbuf with
  | Forest_lexer.SyntaxError msg ->
     fprintf stderr "\n%a\n" print_position lexbuf;
     fprintf stderr "Error: Lexer error - %s\n\n" msg;
     exit 1
  | Forest_parser.Error ->
     fprintf stderr "\n%a\n" print_position lexbuf;
     fprintf stderr "Error: Parser error\n\n";
     exit 1
       
let x_parse_string (f : lexbuf -> (varname * 'a) list) (loc : Location.t) (str : string) =
  let lexbuf = Lexing.from_string str in
  let lexbuf = {lexbuf with
    lex_start_p = loc.loc_start;
    lex_curr_p = loc.loc_start;
  } in
  f lexbuf
       
let forest_parse_with_error =x_parse_with_error Forest_parser.forest_prog Forest_lexer.forest_read
let skin_parse_with_error = x_parse_with_error Forest_parser.skin_prog Forest_lexer.skin_read 
let pads_parse_with_error = x_parse_with_error Forest_parser.pads_prog Forest_lexer.pads_read       

let forest_parse_string = x_parse_string forest_parse_with_error
let skin_parse_string = x_parse_string skin_parse_with_error
let pads_parse_string = x_parse_string pads_parse_with_error



let forest_lex_string loc str =
  let lexbuf = Lexing.from_string str in
  let rec lexing buf =
    match Forest_lexer.forest_read buf with
    | Forest_parser.EOF -> []
    | x -> x :: (lexing lexbuf) 
  in
  lexing lexbuf

(*
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
    printf "%a\n" Json.output_value value;
    parse_and_print lexbuf
  | None -> ()
*)
