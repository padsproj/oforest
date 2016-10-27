(** Forest_parser_helper is called from Ppx_forest to parse a string into a
    Forest AST *)

exception LexingError of string
    
val get_position : Lexing.lexbuf -> string
val print_position : out_channel -> Lexing.lexbuf -> unit

(** These functions get and print positions from a lexbuf respectively *)
  
val x_parse_with_error :
  ((Lexing.lexbuf -> Forest_parser.token) -> Lexing.lexbuf -> (Forest_types.varname * 'a Forest_types.ast) list) ->
  (Lexing.lexbuf -> Forest_parser.token) ->
  Lexing.lexbuf ->
  (Forest_types.varname * 'a Forest_types.ast) list

(** [x_parse_with_error] takes a parse function, a lexer, and a lexbuf and parses. *)
 
val x_parse_string : (Lexing.lexbuf -> (Forest_types.varname * 'a) list) -> Location.t -> string -> (Forest_types.varname * 'a) list

(** This takes a parser (for example a specialization of [x_parse_with_error]),
    a location, and a string and parses the string. Used by Ppx_forest to parse
    Forest and Skin descriptions. *)

val x_lex_string : (Lexing.lexbuf -> Forest_parser.token) -> string -> Forest_parser.token list

(** This function takes a lexer and a string and turns the string into tokens. Used for testing and debugging purposes. *)

val forest_parse_with_error : Lexing.lexbuf -> (Forest_types.varname * Forest_types.forest_node Forest_types.ast) list
val skin_parse_with_error : Lexing.lexbuf -> (Forest_types.varname * Forest_types.skin_node Forest_types.ast) list
val forest_parse_string : Location.t -> string -> (Forest_types.varname * Forest_types.forest_node Forest_types.ast) list
val skin_parse_string : Location.t -> string -> (Forest_types.varname * Forest_types.skin_node Forest_types.ast) list
val forest_lex_string : string -> Forest_parser.token list
val skin_lex_string : string -> Forest_parser.token list

(** These functions specialize the above functions for the Forest and the
    Skin language respectively *)

