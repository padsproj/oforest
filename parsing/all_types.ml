(*
type forest_token = 
  | WHERE
  | URL
  | STRING of (string)
  | SEMICOLON
  | RPAREN
  | RE
  | RBRACK
  | RBRACE
  | RANGB
  | PADS
  | OPT
  | MATCHES
  | MAP
  | LPAREN
  | LINK
  | LBRACK
  | LBRACE
  | LANGB
  | IS
  | ID of (string)
  | GL
  | FILE
  | EQ
  | EOF
  | DIR
  | DCOLON
  | COMMA
  | BAR
  | AT
  | ARROW
  | AQUOT of (string)

*)
(* Remove post integrating lib *)


(*
Useful for parser testing:
#mod_use "all_types.ml";;
#mod_use "forest_parser.ml";;
#mod_use "forest_lexer.ml";;
let lexbuf = Lexing.from_string "d = directory { test is \"Woo\" ::  file } ";;
Forest_parser.prog Forest_lexer.read lexbuf;;
*)

(*type forest_token = Forest_parser.token
*)


type loc = Location.t

let pp_loc = Location.print_loc 

type filepath = string [@@deriving show]
type varname = string [@@deriving show]
type aquot = string [@@deriving show]
  
(* End remove *)

type pathType =
| Constant of loc * filepath
| Variable of loc * varname
| OC_Path of loc * aquot [@@deriving show]


type forest_regexp_str = string [@@deriving show]

type forest_regex =
| Glob of loc * forest_regexp_str
| Regex of loc * forest_regexp_str [@@deriving show]

type gen =
| Matches of loc * forest_regex
| InList of loc * aquot [@@deriving show]

type predOrGen =
| Guard of loc * aquot
| Generator of loc * varname * gen [@@deriving show]

type compType =
| Map
| List [@@deriving show]

type fPayload =
  | PNone 
  | PVList of varname list 
  | PRec [@@deriving show]

type 'a ast =
  { node : 'a;
    payload : fPayload;
    loc : loc;
  } [@@deriving show]
    
type t_node =  (* TODO: How should we add typeof operator? *)
  | TTop
  | TBot 
  | TFile
  | TLink
  | TPred
  | TRec
  | TPads of varname (* Maybe allow constructs that make this not just Pads *) 
  | TOpt of t_node ast
  | TComp of t_node ast
  | TOr of t_node ast * t_node ast
  | TAnd of t_node ast * t_node ast
  | TDir of t_node ast list 
  | TTypeOf of varname 
  | TDirFun of varname * t_node ast

and skin_node = (*TODO: Do you want fun and app? *)
  | HDelay 
  | HUndelay
  | HNegate
  | HId
  | HVar of varname
  | HOpt of skin_node ast
  | HComp of skin_node ast
  | HType of skin_node ast * t_node ast (* Used to be HTypeF *)
  | HDir of skin_node ast list
  | HSeq of skin_node ast * skin_node ast
  | HAlt of skin_node ast * skin_node ast 
  | HMap of skin_node ast
  | HDirFun of varname * skin_node ast
(*
  | HFun of varname * skin_node ast
  | HApp of skin_node ast * skin_node ast *)[@@deriving show]

and forest_node = 
| SkinApp of forest_node ast * skin_node ast
| Thunked of forest_node ast
| Var of varname
| Pads of varname
| Url of forest_node ast
| File 
| Link 
| Option of forest_node ast 
| Directory of (varname * forest_node ast) list 
| Comprehension of compType * forest_node ast * predOrGen list
| PathExp of pathType * forest_node ast
| Predicate of forest_node ast * aquot [@@deriving show]

(* AST Types for PADS *)

(* Inbuilt / OCaml types *)
type precord_entry =
  | Named of varname * pads_node ast
  | Unnamed of pads_node ast [@@deriving show]

and pads_fixed =
  | PFInt of int
  | PFEOF
  | PFStr of string
  | PFRE of aquot
  
and pads_node =
  | Pint
  | Pfloat
  | Pstring of pads_fixed
  | Pconst of pads_fixed
      
  | Pvar of varname
      
  | Ppred of varname * pads_node ast * aquot
  | Precord of precord_entry list
  | Plist of pads_node ast * pads_fixed * pads_fixed
      
  | Ptuple of pads_node ast * pads_node ast
      
  | Pdatatype of (string * pads_node ast) list
      [@@deriving show]

let mk_ast (loc : loc) (node : 'a) : 'a ast = 
  { node; loc; payload = PNone; }

let mk_p_ast (loc : loc) (payload : fPayload) (node : 'a) : 'a ast = 
  { node; loc; payload}

(*
let forest_print_desc ?name:(name="forest_desc") (e : exp) : string =
  let open Printf in
  let rec print_desc e = 
    match e with
    | URL(_loc,e) -> "URL " ^ (print_desc e)
    | Pads(_loc, name)
    | Var(_loc,name) -> name
    | File(_loc) -> "file"
    | Link(_loc) -> "link"
    | PathExp(_loc,ptype,exp) -> 
      let s = 
          match ptype with 
          | Constant(pathi) -> sprintf "\"%s\" ::" pathi
          | Variable(pathi) -> sprintf "%s ::" pathi
          | OCaml_Expr(e) ->
            let s = 
              let buff = Bytes.create 1024 in
              let oldstdout = Unix.dup Unix.stdout in
              let (inp,newstdout) = Unix.pipe () in 
              Unix.dup2 newstdout Unix.stdout;
              Printers.OCaml.print_implem (Ast.StExp(_loc, e));
              flush stdout;
              let len = Unix.read inp buff 0 1024 in
              Unix.dup2 oldstdout Unix.stdout;
              Bytes.sub_string buff 0 (len-3)
            in
            sprintf "%s ::" s
      in
      String.concat " " [s;print_desc exp]
    | Thunked(_loc,exp) -> String.concat "" ["<";print_desc exp;">"]
    | Option(_loc, exp) -> String.concat " " [print_desc exp;"option"]
    | Directory (_loc, lst) -> 
      let len = (String.length name) + (String.length "directory { ") + 3 in
      let sList = List.map (fun (_loc,vname,exp) -> String.concat " " [sprintf "%s is" vname;print_desc exp]) lst in
      let s = String.concat (sprintf ";\n\t%*s" len "") sList in
      String.concat " " ["directory {";s;"}"]
    | Comprehension(_loc,glist,exp,bool) -> 
      let start = if bool then "Map [" else "[" in
      let sList = List.map (fun (_loc,gl) ->
        match gl with
        | Generator(vname,gen) -> 
          let s = 
            match gen with
            | Matches(_loc,Glob(str)) -> sprintf "GL \"%s\"" str 
            | Matches(_loc,Regex(str)) -> sprintf "RE \"%s\"" str
            | InList(_loc,e) -> (* TODO: Make this work with more bytes *)
              let buff = Bytes.create 1024 in
              let oldstdout = Unix.dup Unix.stdout in
              let (inp,newstdout) = Unix.pipe () in 
              Unix.dup2 newstdout Unix.stdout;
              Printers.OCaml.print_implem (Ast.StExp(_loc, e));
              flush stdout;
              let len = Unix.read inp buff 0 1024 in
              Unix.dup2 oldstdout Unix.stdout;
              Bytes.sub_string buff 0 (len-3)
          in
          String.concat " " [sprintf "%s <-" vname;s]
        | Guard(e) -> (* TODO: Make this work with more bytes *)
          let buff = Bytes.create 1024 in
          let oldstdout = Unix.dup Unix.stdout in
          let (inp,newstdout) = Unix.pipe () in 
          Unix.dup2 newstdout Unix.stdout;
          Printers.OCaml.print_implem (Ast.StExp(_loc, e));
          flush stdout;
          let len = Unix.read inp buff 0 1024 in
          Unix.dup2 oldstdout Unix.stdout;
          Bytes.sub_string buff 0 (len-3)
      ) glist 
      in
      let s = String.concat "," sList in
      String.concat " " [start;print_desc exp; "|"; s;"]"]
    | Predicate(_loc,e,b) -> 
      let buff = Bytes.create 4096 in
      let oldstdout = Unix.dup Unix.stdout in
      let (inp,newstdout) = Unix.pipe () in 
      Unix.dup2 newstdout Unix.stdout;
      Printers.OCaml.print_implem (Ast.StExp(_loc, b));
      flush stdout;
      let len = Unix.read inp buff 0 4096 in
      Unix.dup2 oldstdout Unix.stdout;
      let bstring = Bytes.sub_string buff 0 (len-3) in
      String.concat "" [print_desc e; " where ";bstring]
    | Special(_loc,_) -> failwith "Special constructs can't exist in forest descriptions"
  in
  String.concat "" ["\t";name;" = ";print_desc e]
  *)
