open Unix
open Filename
open Sys
open Pads
open Core
open Core_kernel



(* Rest *)


type fKind = 
| AsciiK
| BinaryK
| DirectoryK
| SymK
| UnknownK

type forest_regexp = Str.regexp
type forest_glob = Re.t

type forest_regexp_str = string
type filepath = Pads.filepath
type varname = string

module OrderedPath = String

module PathMap = Map.Make(OrderedPath)


type manifest_errors = 
| MD_Missing_Info
| Opt_MD_Rep_Inconsistency
| Dir_Filename_Overlap
| PredicateFail
| PadsError of pads_manifest_errors

let err_to_string err =
  match err with
  | MD_Missing_Info -> "Metadata is missing the info 'component'"
  | Opt_MD_Rep_Inconsistency -> "Either the metadata or the rep (but not both) of an option is None"
  | Dir_Filename_Overlap -> "The directory path indicated in the metadata is currently occupied by a file"
  | PredicateFail -> "The predicate failed w.r.t. the chosen rep and md."
  | PadsError (RegexMatchError r) -> Printf.sprintf "Regex %s failed to match" r
  | PadsError( ListLengthError) -> "List length failed to match"
  | PadsError(VariantMismatchError) -> "Variant types for rep and md failed to match."
  | PadsError(ListLengthMismatchError) -> "List length for rep and md failed to match."

type file_info = 
    { full_path : filepath; 
      owner : int;
      group : int;
      size : int;
      permissions : file_perm;
      kind : fKind;
      access_time : float;
      modify_time : float;
      change_time : float;
  }

type 'a forest_md = 
  { num_errors : int;
    error_msg : string list;
    info : file_info option;
    load_time : Time.Span.t;
    data : 'a;
  }

type manifest = 
  { errors : (filepath * manifest_errors) list;
    storeFunc : ?dirname:filepath -> ?basename:filepath -> unit -> unit;
    tmppath : filepath }

(* Implementations *)


let store (mani : manifest) : unit = 
  let _ = mani.storeFunc () in
  try
    Unix.rmdir mani.tmppath
  with _ -> ()
    
let store_at (mani : manifest) (path: filepath) : unit =  
  let _ = mani.storeFunc ~dirname:(Filename.dirname path) ~basename:(Filename.basename path) () in
  try
    Unix.rmdir mani.tmppath
  with _ -> ()

let get_kind (finfo : file_info) : fKind = finfo.kind

let parse_kind (stats : Unix.stats) (path : filepath) : fKind = match stats.st_kind with
    | S_DIR -> DirectoryK
    | S_LNK -> SymK
    | S_REG -> 
      let maxBytes = 1024 in
      let cmd = Printf.sprintf "file -i -L %s" path in
      let ch = Unix.open_process_in cmd in
      let fd = descr_of_in_channel ch in
      let buff = Bytes.make maxBytes '0' in
      let bRead = Unix.read fd buff 0 maxBytes in
      let str = String.sub (String.trim (Bytes.sub_string buff 0 bRead)) (bRead-6) 5 in
      let _ = Unix.close_process_in ch in
      if str = "ascii"
      then AsciiK
      else BinaryK
    | _ -> UnknownK

let print_mani_errors (mani: manifest) =
  List.iter (fun (path,err) -> 
    Printf.printf "%s: %s\n" path (err_to_string err)
  ) mani.errors
    
let print_md_errors (md: 'a forest_md) =
  List.iter (fun err -> 
    Printf.printf "Error: %s\n" err
  ) md.error_msg

let regexp_from_string : string -> forest_regexp = Str.regexp
let regexp_match (reg : forest_regexp) (str : string) : bool =
  try 
    let _ = Str.search_forward reg str 0 in
    true
  with Not_found -> false

let regexp_match_from_string (str : forest_regexp_str) : string -> bool = regexp_match (Str.regexp str)

let glob_from_string : string -> forest_glob = Re_glob.glob
let glob_match (reg : forest_glob) (str : string) : bool =
  try
    let _ =  Re.exec (Re.compile reg) str in
    true
  with Not_found -> false

let glob_match_from_string (str : forest_regexp_str) : string -> bool = glob_match (glob_from_string str)

let get_md_info (path : filepath) : file_info option =
  try 
    let stats = Unix.lstat path in (*TODO(Richard): Maybe we want Unix.stat here?*)
    Some { full_path = path; (* TODO(jnf): fix this *)
           owner = stats.st_uid;
           group = stats.st_gid;
           size = stats.st_size;
           permissions = stats.st_perm;
           kind = parse_kind stats path;
           access_time = stats.st_atime;
           modify_time = stats.st_mtime;
           change_time = stats.st_ctime } 
  with Unix_error(ENOENT,_,_) -> None
  (* TODO: Fails miserably if there are other errors, which is nice for 
   * our debugging but shouldn't be in final system *)

let empty_info (path : filepath) : file_info =
  let time = Unix.time () in
    { full_path = path;
      owner = getuid ();
      group = getgid ();
      size = 0;
      permissions = 0o770;
      kind = UnknownK;
      access_time = time;
      modify_time = time;
      change_time = time; 
    }

let get_att_info (path : filepath) : file_info =
  match get_md_info path with
  | Some x -> x
  | None -> empty_info path
    
let no_time = 
  let curr = Time.now () in
  Time.abs_diff curr curr

let base_md (data : 'a) (path : filepath) : 'a forest_md =
  { num_errors = 0;
    error_msg = [];
    info = get_md_info path;
    load_time = no_time;
    data = data
  }

let empty_md (data : 'a) (path : filepath) : 'a forest_md =
  { num_errors = 0;
    error_msg = [];
    info = Some(empty_info path);
    load_time = no_time;
    data = data
  }

let unit_md : (filepath -> unit forest_md) = base_md ()


let load_link (path:string) : string * unit forest_md =
  let currTime = Time.now () in
  let errrep = "" in
  let errmd = 
      { num_errors = 1;
        error_msg = [];
        info = None;
        load_time = Time.abs_diff currTime (Time.now ());
        data = () } in 
  try 
    let stats = Unix.lstat path in
    match stats.st_kind with
    | S_LNK ->
      let rep = Unix.readlink path in
      let info = get_md_info path in
      let md = {
        num_errors = 0;
        error_msg = [];
        info = info;
        load_time = Time.abs_diff currTime (Time.now ());
        data = ()
      } in
      (rep, md)
    | _ ->
      let info = get_md_info path in
      (errrep,
       {errmd with 
         info = info;
         error_msg = [Printf.sprintf "%s: Not an symlink" path];
         load_time = Time.abs_diff currTime (Time.now ())})
  with 
  | Unix_error (e,_,_) ->
      (errrep,
       {errmd with 
         error_msg =  [Printf.sprintf "Unix_error: %s" (Unix.error_message e)];
         load_time = Time.abs_diff currTime (Time.now ())})
  | _ ->
      (errrep,
       {errmd with 
         error_msg =  [Printf.sprintf "%s: Unknown Error" path];
         load_time = Time.abs_diff currTime (Time.now ())})

let load_file (path:string) : string * unit forest_md =
  let currTime = Time.now () in
  let errrep = "" in
  let errmd = 
      { num_errors = 1;
        error_msg = [];
        info = None;
        load_time = Time.abs_diff currTime (Time.now ());
        data = () } in 
  if not (Sys.file_exists path) then 
    (errrep,
     {errmd with 
       error_msg = [Printf.sprintf "%s: no such file" path];
       load_time = Time.abs_diff currTime (Time.now ())})
  else if Sys.is_directory path then
    (errrep,
     {errmd with 
       error_msg = [Printf.sprintf "%s: is a directory, expecting file" path];
       load_time = Time.abs_diff currTime (Time.now ())})
  else 
    try
      let ch = open_in path in
      let buf = Buffer.create 101 in
      try 
        Buffer.add_string buf (input_line ch);
        while true do
          let new_str = input_line ch in
          Buffer.add_char buf '\n';
          Buffer.add_string buf new_str
        done;
        failwith "Truly, the impossible happened"
      with End_of_file -> 
        close_in ch;
        let rep = Buffer.contents buf in 
        let _ = close_in ch in 
      (* Core function would be great except that it adds a newline to the end :(
      let rep = Core.Std.In_channel.read_all path in *)
      let md = { num_errors = 0;
                 error_msg = [];
                 info =  get_md_info path;
                 load_time = Time.abs_diff currTime (Time.now ());
                 data = () } in 
      (rep,md)
    with Sys_error s ->
      (errrep,
       {errmd with 
         error_msg =  [Printf.sprintf "Sys_error: %s" s];
         load_time = Time.abs_diff currTime (Time.now ())})
        

let store_file ((rep,md) : (string * unit forest_md)) (path:string) : unit = 
  try 
    let ch = open_out path in 
    output_string ch rep;
    close_out ch
  with _ ->
    () (* TODO: Should maybe indicate some failure here? *)

(* TODO : Need to do checking of invariants (predicates, for example).
 * This implementation is rather naive *)
let store_link ((rep, md) : (string * unit forest_md)) (path:string) : unit =
  try
    Unix.symlink rep path
  with _ ->
    try 
      match (Unix.lstat path).st_kind with
      | S_LNK -> 
         Unix.unlink path;
        Unix.symlink rep path
      | _ -> () (* TODO: Need to indicate failure here *)
    with _ ->
      () (* TODO: Need to indicate failure here *)

let get_file_name (path : string) : string =
  try
    let index = String.rindex path '/' + 1 in
    String.sub path index (String.length path - index)
  with Not_found -> path

(* Costs *)


module type CostMon = sig
    type cost
    val cost_op : cost -> cost -> cost
    val cost_id : cost
    val cost_file : (string * unit forest_md) -> cost
    val cost_link : (filepath * unit forest_md) -> cost
    val cost_dir : (varname list * 'a forest_md) -> cost
end

(* Example from paper: Tally number of files (not links etc) loaded *)

module CostTallyMon : CostMon with type cost = int = struct
  type cost = int
  let cost_op = ( + )
  let cost_id = 0
  let cost_file _ = 1
  let cost_link _ = cost_id
  let cost_dir _ = cost_id
end

(* Example from paper: Total file size *)
module CostSizeMon : CostMon with type cost = int = struct
  type cost = int
  let size_check (_,m) = 
    match m.info with
    | None -> 0
    | Some(x) -> x.size
  let cost_op = ( + )
  let cost_id = 0
  let cost_file = size_check
  let cost_link = size_check
  let cost_dir _ = cost_id
end

(* Example from paper: Total load time *)
module CostTimeMon : CostMon with type cost = Time.Span.t = struct
  type cost = Time.Span.t
  let get_time (_,m) = m.load_time
  let add_times t1 t2 =
    let now = Time.now () in
    Time.abs_diff (Time.add (Time.add now t1) t2) now
  let cost_op = add_times
  let cost_id = no_time
  let cost_file = get_time
  let cost_link = get_time
  let cost_dir _ = cost_id
end

(* Example from paper: Multiset of names of files loaded *)
module CostNameMon : CostMon with type cost = (string * int) list = struct
  type cost = (string * int) list
  let get_path (_,m) = 
    match m.info with
    | None -> []
    | Some(x) -> [(x.full_path,1)]
  let mergeF l1 l2 = 
    List.fold_left (fun acc (name,num) ->
      if List.mem_assoc name acc
      then (name,num + (List.assoc name acc)) :: (List.remove_assoc name acc)
      else (name,num) :: acc
    ) l1 l2
  let cost_op = mergeF
  let cost_id = []
  let cost_file = get_path
  let cost_link = get_path
  let cost_dir _ = cost_id
end

(* Unit Cost Monad, to get no costs *)
module CostUnitMon : CostMon with type cost = unit = struct
  type cost = unit
  let cost_op _ _ = () 
  let cost_id = ()
  let cost_file _ = cost_id
  let cost_link _ = cost_id
  let cost_dir _ = cost_id
end

module type CursorMonad = sig
  type cost
  type 'a t = cost -> ('a * cost)
  include CostMon with type cost := cost
  include Monad.S with type 'a t := 'a t 
  val run : ('a t -> ('a * cost))
  val get_cost : ('a t -> cost t)
end

module CursorMonad (M : CostMon) : CursorMonad with type cost = M.cost = struct
  include M
  type 'a t = cost -> ('a * cost)
  include Monad.Make (struct
    type nonrec 'a t = 'a t
    let bind m f = 
      fun c ->
        let x,nc = m c in
        f x nc
    let return x = fun c -> (x,c)
    let map = `Define_using_bind
  end)
  let run (m : 'a t) : ('a * M.cost) = m M.cost_id 
  let get_cost (m : 'a t) : cost t = fun c -> let (_,c) = m c in (c,c)
end

