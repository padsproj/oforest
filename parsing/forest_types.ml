type loc = Location.t

let pp_loc = Location.print_loc 

type filepath = string [@@deriving show]
type varname = string [@@deriving show]
type aquot = string [@@deriving show]
  
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
    
type t_node =  
  | TTop
  | TBot 
  | TFile
  | TLink
  | TPred
  | TRec
  | TPads of varname
  | TOpt of t_node ast
  | TComp of t_node ast
  | TOr of t_node ast * t_node ast
  | TAnd of t_node ast * t_node ast
  | TDir of t_node ast list 
  | TTypeOf of varname 
  | TDirFun of varname * t_node ast

and skin_node =
  | HDelay 
  | HUndelay
  | HNegate
  | HId
  | HVar of varname
  | HOpt of skin_node ast
  | HComp of skin_node ast
  | HType of skin_node ast * t_node ast
  | HDir of skin_node ast list
  | HSeq of skin_node ast * skin_node ast
  | HAlt of skin_node ast * skin_node ast 
  | HMap of skin_node ast
  | HDirFun of varname * skin_node ast[@@deriving show]

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

let mk_ast (loc : loc) (node : 'a) : 'a ast = 
  { node; loc; payload = PNone; }

let mk_p_ast (loc : loc) (payload : fPayload) (node : 'a) : 'a ast = 
  { node; loc; payload}
