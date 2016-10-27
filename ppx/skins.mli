(** Skins defines all the functions related to skins and skinning
    Uses Forest_types and Utility
*)

(** {2 Helper Functions} *)

val rec_replace : Forest_types.varname -> Forest_types.varname -> Forest_types.forest_node Forest_types.ast -> Forest_types.forest_node Forest_types.ast

(** [rec_replace] replaces the first argument with the second argument in every Var node of the third argument *)
  
val rec_check : Forest_types.varname -> Forest_types.forest_node Forest_types.ast -> bool

(** [rec_check] checks if the first argument occurs in any Var node of the second argument *)
  
val fget_name : Forest_types.forest_node -> string
val tget_name : Forest_types.t_node -> string
val hget_name : Forest_types.skin_node -> string

(** These functions make a string for printing from their respective node
    types *)

val get_err : (Forest_types.varname -> string -> 'a, unit, string) format -> Forest_types.t_node -> Forest_types.forest_node -> 'a
val get_std_err : Forest_types.t_node -> Forest_types.forest_node -> string

(** These functions produce error messages when there are type mismatches *)
  
val comp_types : Forest_types.forest_node Forest_types.ast -> Forest_types.forest_node Forest_types.ast -> bool * string

(** [comp_types] compares the types of two Forest nodes returning true if they
    are equal and false with an explanation of where they differ otherwise. *)
  
(** {2 Skinning functions } *)
  
val evalTypeGen : Forest_types.t_node Forest_types.ast -> Forest_types.forest_node Forest_types.ast -> bool * string

(** [evalTypeGen] [t] [f] checks if [f] has type [t] returning true if they do
    and false with an explanation of the failure otherwise *)
  
val evalType : Forest_types.t_node Forest_types.ast -> Forest_types.forest_node Forest_types.ast -> bool
val evalTypeF : Location.t -> Forest_types.t_node Forest_types.ast -> Forest_types.forest_node Forest_types.ast -> bool

(** These functions use evalTypeGen to check if the types match. [evalType]
    ignores the message and just gets the bool, while [evalTypeF] fails and prints
    the message if the types do not match *)
  
val typeofH : Forest_types.skin_node Forest_types.ast -> Forest_types.t_node Forest_types.ast

(** [typeofH] extracts the type of a skin_node *)
  
val evalSkin : Forest_types.loc -> Forest_types.skin_node Forest_types.ast -> Forest_types.forest_node Forest_types.ast -> Forest_types.forest_node Forest_types.ast

(** [evalSkin] applies a skin to a forest_node *)
  
val doSkinning : Forest_types.varname * Forest_types.forest_node Forest_types.ast -> Forest_types.forest_node Forest_types.ast

(** [doSkinning] is the main function which resolves all skinning in the given
    forest_node *)
