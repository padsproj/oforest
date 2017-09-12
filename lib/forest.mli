(** This is the Forest library, which contains a number of functions
    that the OCaml code generated from Forest descriptions calls along
    with some functions that may be helpful to users.

    Uses Core, Core_kernel, Filename, Pads, Re, Sys, and Unix 

*)

(** {2 Types} *)

type fKind =
  | AsciiK
  | BinaryK
  | DirectoryK
  | SymK
  | UnknownK
  
(** [fKind] represents the possible kinds of files.  Ascii, Binaries,
    Directories, Symlinks, and unknown respectively.
*)

type forest_regexp = Str.regexp
type forest_glob = Re.t

(** These describe normal regular expressions and glob style regular
    expressions respectively *)
  
type forest_regexp_str = string
type filepath = Pads.filepath
type varname = string

module OrderedPath : Map.OrderedType with type t = string
module PathMap : Map.S with type key = OrderedPath.t
  
(** These modules are used for map comprehensions *)

  
type manifest_error = 
| Dir_Filename_Overlap
| MD_Missing_Info
| Opt_MD_Rep_Inconsistency
| PadsError of Pads.pads_manifest_error
| PermissionError
| PredicateFail

(** These describe the possible errors that can occur when generating
    a manifest *)
  
type file_info = 
    { full_path : filepath; 
      owner : int;
      group : int;
      size : int;
      permissions : Unix.file_perm;
      kind : fKind;
      access_time : float;
      modify_time : float;
      change_time : float;
    }

(** [file_info] describes a bunch of file properties *)

type 'a forest_md = 
  { num_errors : int;
    error_msg : string list;
    info : file_info option;
    load_time : Core.Time.Span.t;
    data : 'a;
  }

(** A [forest_md] is created anytime Forest loads data from the file
    system. It contains the number of errors encountered, messages
    describing those errors, information about the file at the top
    level specification, the time it took to perform the load as well
    as similar auxiliary data for sub specifications. *)


type 'a manifest = 
  {
    validate: unit -> (filepath * manifest_error) list; (* Validates the correctness of the write *)
    commit: unit -> unit; (* Commits changes to the standard file system *)
    data: 'a (* Stores sub manifests *)
  }

(** Manifests are created prior to storing to determine what errors we
    will run into trying to store the specification. It contains a
    list of such errors, a store function to complete the store and
    the temporary path where the files are currently stored. The store
    function and temporary path are used by other functions and should
    not need to be accessed by a user directly. *)

(** {2 Helper functions} *)
    
val error_to_string : manifest_error -> string

(** [err_to_string] turns a manifest error into a string *)

val print_manifest_errors : (filepath * manifest_error) list -> unit
val print_md_errors : 'a forest_md -> unit
val exit_on_error : 'a forest_md -> unit

(** These functions print manifest and metadata errors respectively.
    The last one exits after printing the errors if there are any.*)

  
val sort_comprehension :
  (('a * 'b forest_md) -> ('a * 'b forest_md) -> int)
  -> 'a list * 'b forest_md list forest_md
  -> 'a list * 'b forest_md list forest_md


(** [sort_comprehension] is a helper function meant to help sort comprehensions
    (since their default order is unspecified). It takes a comparison function,
    comparing one joined (rep,md) pair to another, as well as the comprehension
    rep and md, returning the comprehension rep and md sorted using the compare
    function. *)
  
val sort_comp_path :
     'a list * 'b forest_md list forest_md
  -> 'a list * 'b forest_md list forest_md

(** [sort_comp_path] specializes [sort_comprehension], sorting based on the
    full_path in the metadata.*)

val regexp_from_string : forest_regexp_str -> forest_regexp
val regexp_match : forest_regexp -> string -> bool
val regexp_match_from_string : forest_regexp_str -> string -> bool

val glob_from_string : forest_regexp_str -> forest_glob
val glob_match : forest_glob -> string -> bool
val glob_match_from_string : forest_regexp_str -> string -> bool

(** These functions deal with regular expressions in standard and glob
    format respectively. The first makes regular expressions from
    strings, the second checks if a string matches a regular
    expression and the final composes the two.
*)

val parse_kind : Unix.stats -> filepath -> fKind

(** [parse_kind] parses Unix stats to get a file kind. [parse_kind]
    needs the filepath to figure out if the file is an ascii or a
    binary *)
  
val get_md_info : filepath -> file_info option
  
(** [get_md_info] gets file info from a filepath or returns None if
    there is no file at the path *)
  
val empty_info : filepath -> file_info
  
(** [empty_info] makes a default file info with a given filepath *)
  
val get_att_info : filepath -> file_info
  
(** [get_att_info] invokes [get_md_info] returning [empty_info] if
    None and the retrieved file_info otherwise *)
  
    
val get_kind : file_info -> fKind

(** [get_kind] projects out the kind from a file_info *)

val no_time : Core.Time.Span.t
  
(** A time span that represents no time having passed *)

val base_md : 'a -> filepath -> 'a forest_md
val empty_md : 'a -> filepath -> 'a forest_md
val unit_md : filepath -> unit forest_md

(** These functions make MDs without errors from some path either
    filling it with real file_info ([base_md]) or empty file_info
    ([empty_md]). The 'a is the data from
    sub-specifications. [unit_md] just calls [base_md] with unit. *)

(** {2 File system helpers} *)

  
val check_exists: filepath -> bool
val check_writeable: filepath -> bool
  
(** {2 Primitive Load/Store} *)

val validate : 'a manifest -> (filepath * manifest_error) list
val commit : 'a manifest -> unit

(** These functions take a manifest and execute the store
    function. [store_at] also allows the user to specify a path to store
    the description at. *)

val load_link : filepath -> (filepath * unit forest_md)
  
val load_file : filepath -> (string * unit forest_md)
  
val store_link : (filepath * unit forest_md) ->  filepath -> unit
  
val store_file : (string * unit forest_md) ->  filepath -> unit

(** These functions are for loading and storing the (sym)link and file
    primitives respectively *)
  
(** {2 Cost Monads} *)

module type CostMon = sig
    type cost
    val cost_op : cost -> cost -> cost
    val cost_id : cost
    val cost_file : (string * unit forest_md) -> cost
    val cost_link : (filepath * unit forest_md) -> cost
    val cost_dir : (varname list * 'a forest_md) -> cost
end

(** [CostMon] is the type of Cost Monoids. They have some cost type,
    and five functions specifying how to get costs from primitives, what
    the identity element is and the desired cost operator. *)

module CostTallyMon : CostMon with type cost = int
module CostSizeMon : CostMon with type cost = int
module CostTimeMon : CostMon with type cost = Core.Time.Span.t
module CostNameMon : CostMon with type cost = (string * int) list
module CostUnitMon : CostMon with type cost = unit

(** These are instantiations of [CostMon]. Specifically, they are the
    four seen in the Incremental Forest paper and the unit monoid:
    [CostTallyMon] tallies up the number of files (not links or directories) loaded.
    [CostSizeMon] tallies up the total size of everything loaded.
    [CostTimeMon] returns the total amount of time spent loading.
    [CostNameMon] returns a list of files and links loaded along with how many times they were loaded.
    [CostUnitMon] does not tally costs, returning the unit.
*)
    

(** {2 Cursor Monad} *)
  
module type CursorMonad = sig
  type cost
  type 'a t = cost -> ('a * cost)
  include CostMon with type cost := cost
  include Core_kernel.Monad.S with type 'a t := 'a t 
  val run : ('a t -> ('a * cost))
  val get_cost : ('a t -> cost t)
end


module CursorMonad (M : CostMon) : CursorMonad with type cost = M.cost
  
(** [CursorMonad] is the monad in which cursors live. It is used for
    aggregating costs (based on its input [CostMon]) and providing a
    nice abstraction for effects. *)
