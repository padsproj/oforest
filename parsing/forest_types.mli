(** Forest_types contains a collection of most if not all of the relevant types
    in oforest *)

type loc = Location.t
val pp_loc : Format.formatter -> Location.t -> unit

(** This is just the location type and a function for printing it given a
    formatter. Mainly this is needed because every other type derives 'show' (see
    ppx_deriving). *)

(** {2 Common types} *)
  
type filepath = string
type varname = string
type aquot = string
type forest_regexp_str = string

(** These are file paths, variable names, quoted OCaml code, and types used to
    generate regular expressions respectively *)
  
type fPayload = PNone | PVList of varname list | PRec

(** This type represents the possible payloads forest AST nodes can
    carry. [PVList] is used to log dependencies while [PRec] marks certain
    recursive ASTs. *)
    
type 'a ast = { node : 'a; payload : fPayload; loc : loc; }

(** This is the type of a general AST. It contains a node (Forest, Skin, or
    Type), a location, and a payload *)

(** {2 Forest/Skin AST types} *)
  
type pathType =
    Constant of loc * filepath
  | Variable of loc * varname
  | OC_Path of loc * aquot

(** The possible ways of specifying paths (either as a constant string, a
    variable, or using an OCaml expression that evaluates to a string) *)
      
type forest_regex =
    Glob of loc * forest_regexp_str
  | Regex of loc * forest_regexp_str
type gen = Matches of loc * forest_regex | InList of loc * aquot
type predOrGen = Guard of loc * aquot | Generator of loc * varname * gen
type compType = Map | List

(** These four types deal with comprehensions. [compType] determines whether
    it's a map or a list, [predOrGen] specifies Guards and Generators (and are
    mostly in a list), gen specifies types of generators, and forest_regex specifies
    what type of regular expressions to use. *)
    
type t_node =
    TTop
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
    HDelay
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
  | HDirFun of varname * skin_node ast
and forest_node =
    SkinApp of forest_node ast * skin_node ast
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
  | Predicate of forest_node ast * aquot

(** These three types are the main AST nodes and specify Type ASTs, Skin ASTs,
    and Forest ASTs respectively. *)
    
val mk_ast : loc -> 'a -> 'a ast
val mk_p_ast : loc -> fPayload -> 'a -> 'a ast

(** These are two helper functions for making AST nodes *)
