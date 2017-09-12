%{ open Forest_types 
   open Location
   (*type forest_token = token
   *)
   let make_loc p1 p2 =
     { loc_start = p1;
       loc_end = p2;
       loc_ghost = false;
     }
%}

(* General *)
%token <string> ID
%token LBRACE
%token RBRACE
%token LBRACK
%token RBRACK
%token LPAREN
%token RPAREN
%token COMMA
%token EOF
%token SEMICOLON
%token EQ
%token BAR

(* S+F *)
%token LANGB
%token RANGB
%token MAP

(* T+F *)
%token FILE
%token LINK
%token OPT

(* Forest Specific *)
%token <string> STRING
%token <string> AQUOT
%token DIR
%token PADS
%token IS
%token MATCHES
%token RE
%token GL
%token WHERE
%token DCOLON
%token URL
%token AT
%token BARROW

(* Skin Specific *)
%token PLUS
%token ULINE
%token NEG
(*
%token FARROW
%token FUN
*)

(* Type Specific *)
%token PRED
%token REC
%token AND
%token OR
%token TYPEOF

(* Forest *)

%type <Forest_types.forest_node Forest_types.ast> f_ast f_cons f_path_ast f_bast f_cons_ast f_last
%type <Forest_types.pathType> path_typ
%type <Forest_types.compType> comp_type
%type <Forest_types.qualifier> qualifier
%type <Forest_types.forest_regex> match_statement
%type <Forest_types.generator> generator_statement
%type <Forest_types.varname * Forest_types.forest_node Forest_types.ast> direntry

(* Skins *)
    
%type <Forest_types.skin_node Forest_types.ast> s_ast s_semi s_right s_left s_last s_cons

(* Types *)

%type <Forest_types.t_node Forest_types.ast> t_ast t_and t_left t_last t_cons

(* Starts *)
    
%start <(Forest_types.varname * Forest_types.forest_node Forest_types.ast) list> forest_prog
%start <(Forest_types.varname * Forest_types.skin_node Forest_types.ast) list> skin_prog
%%


(* Forest parsing *)
forest_prog: l = nonempty_list(separated_pair(ID,EQ,f_ast)); EOF  { l } ;

f_ast:
  | f = f_path_ast; WHERE; s = AQUOT
   { let l = make_loc $startpos(f) $endpos(s) in
      mk_ast l @@ Predicate (f,s) }
  | f_path_ast { $1 }
  ;

f_path_ast:
  | p = path_typ; DCOLON ; f=f_path_ast
   { let l = make_loc $startpos(p) $endpos(f) in
     mk_ast l @@ PathExp (p,f) }
  | f_bast { $1 }
  ;

f_bast:
  | f = f_bast; OPT
   { let l = make_loc $startpos(f) $endpos($2) in
     mk_ast l @@ Option (f) }
  | f = f_bast; AT; h = s_last
     { let l = make_loc $startpos(f) $endpos(h) in
     mk_ast l @@ SkinApp (f,h) }
  | f_cons_ast { $1 }
  ;

f_cons_ast:
  | f_cons { $1 }
  | f_last { $1 }
  ;
             
f_last: 
  | DIR; LBRACE; list = separated_nonempty_list(SEMICOLON,direntry); RBRACE (*TODO: Figure out a way to allow a final ; *)
     { let l = make_loc $startpos($1) $endpos($4) in
       mk_ast l @@ Directory (list) }
  | c = comp_type ; LBRACK; f = f_ast; BAR ; pl = separated_nonempty_list(COMMA,qualifier); RBRACK
     { let l = make_loc $symbolstartpos $endpos($6) in (*TODO: Make sure you want symbolstartpos here *)
       mk_ast l @@ Comprehension (c,f,pl) }
  | LANGB; f = f_ast; RANGB 
     { let l = make_loc $startpos(f) $endpos(f) in
       mk_ast l @@ Thunked (f) }
  | x = ID 
     { let l = make_loc $startpos(x) $endpos(x) in
       mk_ast l @@ Var (x) }
  | LPAREN; f = f_ast; RPAREN { f }
  ;

f_cons: 
  | FILE 
     { let l = make_loc $startpos $endpos in 
       mk_ast l File }
  | LINK 
     { let l = make_loc $startpos $endpos in 
       mk_ast l Link }
  | PADS; x = ID 
     { let l = make_loc $startpos($1) $endpos(x) in 
       mk_ast l @@ Pads(x) }
  | URL; f = f_cons_ast 
     { let l = make_loc $startpos($1) $endpos(f) in 
       mk_ast l @@ Url(f) }
  ;

path_typ:
  | STRING 
    { let l = make_loc $startpos $endpos in 
      Constant(l,$1) }
  | ID     
     { let l = make_loc $startpos $endpos in 
       Variable(l,$1) }
  | AQUOT  
     { let l = make_loc $startpos $endpos in 
       OC_Path(l,$1) }
  ;

comp_type:
  | MAP { Map }
  | (* empty *) { List }

qualifier:
  | x = ID ; BARROW ; g = generator_statement 
     { let l = make_loc $startpos(x) $endpos(g) in
       Generator (l,x,g) }
  | AQUOT 
      { let l = make_loc $startpos $endpos in
        Guard(l,$1) }
  ;

generator_statement:
  | MATCHES; reg = match_statement 
     { let l = make_loc $startpos($1) $endpos(reg) in
       Matches(l,reg) }
  | AQUOT 
     { let l = make_loc $startpos $endpos in
       InList(l,$1) }
  ;

match_statement:
  | RE; regexp = STRING 
     { let l = make_loc $startpos($1) $endpos(regexp) in
       Regex (l,regexp) }
  | GL; regexp = STRING 
     { let l = make_loc $startpos($1) $endpos(regexp) in
       Glob (l,regexp)  }
  ;

direntry: separated_pair(ID,IS,f_ast) { $1 }

(* Skin Parsing *)

skin_prog: l = nonempty_list(separated_pair(ID,EQ,s_ast)); EOF  { l } ;

s_ast:
  | h1 = s_semi ; PLUS ; h2 = s_ast
   { let l = make_loc $startpos(h1) $endpos(h2) in 
     mk_ast l @@ HAlt(h1,h2) } 
  | s_semi { $1 }

s_semi:
  | h1 = s_left ; SEMICOLON ; h2 = s_semi
   { let l = make_loc $startpos(h1) $endpos(h2) in 
     mk_ast l @@ HSeq(h1,h2) }
  | s_right { $1 }

s_right: (* TODO: Add back functions?
  | FUN ; x = ID ; FARROW ; h = s_right (*Make this comma separated list instead *)
   { let l = make_loc $startpos($1) $endpos(h) in 
     mk_ast l @@ HFun(x,h) } *) 
  | s_left { $1 }

s_left:
(*
  | h1 = s_last ; LPAREN ; h2 = s_ast ; RPAREN (*Make this comma separated list instead *)
   { let l = make_loc $startpos(h1) $endpos($4) in 
     mk_ast l @@ HApp(h1,h2) } *)
  | x = ID ; LPAREN ; h2 = s_ast ; RPAREN
   { let l = make_loc $startpos(x) $endpos($4) in 
     mk_ast l @@ HDirFun(x,h2) } 
  | MAP ; LPAREN ; h = s_ast ; RPAREN (*Make this comma separated list instead *)
   { let l = make_loc $startpos($1) $endpos($4) in 
     mk_ast l @@ HMap(h) } 
  | h = s_last; OPT
   { let l = make_loc $startpos(h) $endpos($2) in
     mk_ast l @@ HOpt h }
  | h = s_left; BAR; t = t_ast
   { let l = make_loc $startpos(h) $endpos(t) in
     mk_ast l @@ HType(h,t) }
  | s_last { $1 }

s_last:
  | LBRACE ; list = separated_nonempty_list(COMMA,s_ast); RBRACE
   { let l = make_loc $startpos($1) $endpos($3) in
     mk_ast l @@ HDir list }
  | LPAREN; h = s_ast; RPAREN { h }
  | x = ID 
   { let l = make_loc $startpos(x) $endpos(x) in
     mk_ast l @@ HVar (x) }
  | LBRACK; h = s_ast; RBRACK 
   { let l = make_loc $startpos($1) $endpos($3) in
     mk_ast l @@ HComp h } 
  | s_cons { $1 }

s_cons: 
  | LANGB ; RANGB
   { let l = make_loc $startpos($1) $endpos($2) in 
     mk_ast l @@ HDelay } 
  | RANGB ; LANGB
   { let l = make_loc $startpos($1) $endpos($2) in 
     mk_ast l @@ HUndelay } 
  | NEG
   { let l = make_loc $startpos $endpos in 
     mk_ast l @@ HNegate } 
  | ULINE
   { let l = make_loc $startpos $endpos in 
     mk_ast l @@ HId } 

(* Type Parsing *)

t_ast: 
  | t1 = t_and ; OR ; t2 = t_ast
   { let l = make_loc $startpos(t1) $endpos(t2) in 
     mk_ast l @@ TOr(t1,t2) }  
  | t_and { $1 }

t_and:
  | t1 = t_left ; AND ; t2 = t_and
   { let l = make_loc $startpos(t1) $endpos(t2) in 
     mk_ast l @@ TAnd(t1,t2) } 
  | t_left { $1 }

t_left:
  | t = t_last; OPT
   { let l = make_loc $startpos(t) $endpos($2) in
     mk_ast l @@ TOpt t }
  | t_last { $1 }

t_last:
  | LBRACE ; list = separated_nonempty_list(COMMA,t_ast); RBRACE
   { let l = make_loc $startpos($1) $endpos($3) in
     mk_ast l @@ TDir list }
  | LPAREN; t = t_ast; RPAREN { t }
  | x = ID 
   { let l = make_loc $startpos(x) $endpos(x) in
     mk_ast l @@ TPads x }
  | LBRACK; t = t_ast; RBRACK 
   { let l = make_loc $startpos($1) $endpos($3) in
     mk_ast l @@ TComp t }
  | TYPEOF ; LPAREN ; f = ID ; RPAREN 
   { let l = make_loc $startpos($1) $endpos($4) in 
     mk_ast l @@ TTypeOf f }
  | TYPEOF ; f = ID
   { let l = make_loc $startpos($1) $endpos(f) in 
     mk_ast l @@ TTypeOf f } 
  | t_cons { $1 }

t_cons: 
  | FILE
   { let l = make_loc $startpos $endpos in 
     mk_ast l @@ TFile } 
  | LINK
   { let l = make_loc $startpos $endpos in 
     mk_ast l @@ TLink } 
  | PRED
   { let l = make_loc $startpos $endpos in 
     mk_ast l @@ TPred } 
  | REC
   { let l = make_loc $startpos $endpos in 
     mk_ast l @@ TRec } 
  | ULINE
   { let l = make_loc $startpos $endpos in 
     mk_ast l @@ TTop } 
