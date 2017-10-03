open Unix


(* Types *)


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


type manifest_error = 
| ComprehensionUnequalLength
| DirFilenameOverlap
| ForestEmptyManifestError
| MDMissingInfo
| OptMDRepInconsistency
| PadsError of Pads.pads_manifest_error
| PermissionError
| PredicateFail

let error_to_string = function
  | ComprehensionUnequalLength ->
     "The representation and the metadata lists are of different length"
  | DirFilenameOverlap ->
     "The directory path indicated in the metadata is currently occupied by a file"
  | ForestEmptyManifestError -> "Forest Manifest is empty"
  | MDMissingInfo -> "Metadata is missing the info 'component'"
  | OptMDRepInconsistency -> "Either the metadata or the rep (but not both) of an option is None"
  | PadsError(Pads.EmptyManifestError) -> "PADS Manifest is empty"
  | PadsError(Pads.ListLengthError) -> "List length failed to match"
  | PadsError(Pads.ListLengthMismatchError) -> "List length for rep and md failed to match."
  | PadsError(Pads.RegexMatchError r) -> Printf.sprintf "Regex %s failed to match" r
  | PadsError(Pads.VariantMismatchError) -> "Variant types for rep and md failed to match."
  | PermissionError -> "User does not have write permissions to the given path."
  | PredicateFail -> "The predicate failed w.r.t. the chosen rep and md."

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

type 'a forest_md = 
  { num_errors : int;
    error_msg : string list;
    info : file_info option;
    load_time : Core.Time.Span.t;
    data : 'a;
  }

type manifest = 
  {
    validate: unit -> (filepath * manifest_error) list;
    (* TODO: See why you need filepath *)
    commit: unit -> unit; 
  }
(* TODO: Do I still need any of this 
   storeFunc : ?dirname:filepath -> ?basename:filepath -> unit -> unit;
   tmppath : filepath }
*)

(* Some useful user level functions *)

let sort_comprehension (compare : ('a * 'b forest_md) -> ('a * 'b forest_md) -> int)
    ((rep,md) : 'a list * 'b forest_md list forest_md)
    : 'a list * 'b forest_md list forest_md =
  let l = List.combine rep md.data in
  let l = List.fast_sort compare l in
  let (r,m) = List.split l in
  (r,{md with data = m})

let sort_comp_path ((rep,md) : 'a list * 'b forest_md list forest_md)
    : 'a list * 'b forest_md list forest_md =
  let comp (r1,m1) (r2,m2) = match (m1.info,m2.info) with
    | None, None -> 0
    | None, _ -> -1
    | _, None -> 1
    | Some(i1),Some(i2) -> String.compare i1.full_path i2.full_path 
  in
  sort_comprehension comp (rep,md)

let print_manifest_errors (errors : (filepath * manifest_error) list) =
  List.iter (fun (path,err) -> 
    Printf.printf "%s: %s\n" path (error_to_string err)
  ) errors
    
let print_md_errors (md: 'a forest_md) =
  List.iter (fun err -> 
    Printf.printf "Error: %s\n" err
  ) md.error_msg

    
let exit_on_error md =
  if md.num_errors > 0
  then begin
    print_md_errors md;
    exit 1;
  end
  else ()

(* Helper functions *)
    
let regexp_from_string : forest_regexp_str -> forest_regexp = Str.regexp
let regexp_match (reg : forest_regexp) (str : string) : bool =
  try 
    let _ = Str.search_forward reg str 0 in
    true
  with Not_found -> false

let regexp_match_from_string (str : forest_regexp_str) : string -> bool = regexp_match @@ regexp_from_string str

let glob_from_string : forest_regexp_str -> forest_glob = Re_glob.glob
let glob_match (reg : forest_glob) (str : string) : bool =
  try
    let _ =  Re.exec (Re.compile reg) str in
    true
  with Not_found -> false

let glob_match_from_string (str : forest_regexp_str) : string -> bool = glob_match @@ glob_from_string str

let parse_kind (stats : Unix.stats) (path : filepath) : fKind = match stats.st_kind with
    | Unix.S_DIR -> DirectoryK
    | Unix.S_LNK -> SymK
    | Unix.S_REG -> 
      let maxBytes = 1024 in
      let cmd = Printf.sprintf "file -i -L %s" path in
      let ch = Unix.open_process_in cmd in
      let fd = Unix.descr_of_in_channel ch in
      let buff = Bytes.make maxBytes '0' in
      let bRead = Unix.read fd buff 0 maxBytes in
      let str = String.sub (String.trim (Bytes.sub_string buff 0 bRead)) (bRead-6) 5 in
      let _ = Unix.close_process_in ch in
      if str = "ascii"
      then AsciiK
      else BinaryK
    | _ -> UnknownK
       
let get_md_info (path : filepath) : file_info option =
  try 
    let stats = Unix.lstat path in
    let fullpath = if Filename.is_relative path then (Filename.concat (Sys.getcwd ()) path) else path in
    Some { full_path = fullpath;
           owner = stats.st_uid;
           group = stats.st_gid;
           size = stats.st_size;
           permissions = stats.st_perm;
           kind = parse_kind stats path;
           access_time = stats.st_atime;
           modify_time = stats.st_mtime;
           change_time = stats.st_ctime } 
  with Unix_error(ENOENT,_,_) -> None

let empty_info (path : filepath) : file_info =
  let time = Unix.time () in
  let fullpath = if Filename.is_relative path then (Filename.concat (Sys.getcwd ()) path) else path in
    { full_path = fullpath;
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


let get_kind (finfo : file_info) : fKind = finfo.kind
     
let no_time = 
  let curr = Core.Time.now () in
  Core.Time.abs_diff curr curr


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

let make_empty_manifest path =
  {
    commit = (fun () -> ());
    validate = (fun () -> [path,ForestEmptyManifestError])
  }

let empty_manifest = make_empty_manifest ""
   
let fresh_cursor_id cursor_id =
  incr cursor_id;
  !cursor_id
  
(* Safe permission checking and removal *)

let check_exists path =
  match Core.Sys.file_exists ~follow_symlinks:false path with
  | `Yes -> true
  | _ -> false

let check_writeable path =
  match Core.Unix.access path [`Write] with
  | Result.Ok _ -> true
  | _ -> false
  
(* Loadings and Storing primitives *)
    
let validate (manifest : manifest) : (filepath * manifest_error) list = manifest.validate ()
  
let commit (manifest: manifest) : unit = manifest.commit ()
    
let load_link (path: filepath) : filepath * unit forest_md =
  let currTime = Core.Time.now () in
  let errrep = "" in
  let errmd = 
      { num_errors = 1;
        error_msg = [];
        info = None;
        load_time = Core.Time.abs_diff currTime (Core.Time.now ());
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
        load_time = Core.Time.abs_diff currTime (Core.Time.now ());
        data = ()
      } in
      (rep, md)
    | _ ->
      let info = get_md_info path in
      (errrep,
       {errmd with 
         info = info;
         error_msg = [Printf.sprintf "%s: Not an symlink" path];
         load_time = Core.Time.abs_diff currTime (Core.Time.now ())})
  with 
  | Unix_error (e,_,_) ->
      (errrep,
       {errmd with 
         error_msg =  [Printf.sprintf "Unix_error: %s" (Unix.error_message e)];
         load_time = Core.Time.abs_diff currTime (Core.Time.now ())})
  | _ ->
      (errrep,
       {errmd with 
         error_msg =  [Printf.sprintf "%s: Unknown Error" path];
         load_time = Core.Time.abs_diff currTime (Core.Time.now ())})

let load_file (path:string) : string * unit forest_md =
  let currTime = Core.Time.now () in
  let errrep = "" in
  let errmd = 
      { num_errors = 1;
        error_msg = [];
        info = None;
        load_time = Core.Time.abs_diff currTime (Core.Time.now ());
        data = () } in 
  if not (Sys.file_exists path) then 
    (errrep,
     {errmd with 
       error_msg = [Printf.sprintf "%s: no such file" path];
       load_time = Core.Time.abs_diff currTime (Core.Time.now ())})
  else if Sys.is_directory path then
    (errrep,
     {errmd with 
       error_msg = [Printf.sprintf "%s: is a directory, expecting file" path];
       load_time = Core.Time.abs_diff currTime (Core.Time.now ())})
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
      let md = { num_errors = 0;
                 error_msg = [];
                 info =  get_md_info path;
                 load_time = Core.Time.abs_diff currTime (Core.Time.now ());
                 data = () } in 
      (rep,md)
    with Sys_error s ->
      (errrep,
       {errmd with 
         error_msg =  [Printf.sprintf "Sys_error: %s" s];
         load_time = Core.Time.abs_diff currTime (Core.Time.now ())})
        


let store_link ((rep, md) : (filepath * unit forest_md)) (path:string) : unit =
  try
    Unix.symlink rep path
  with _ ->
    try 
      match (Unix.lstat path).st_kind with
      | S_LNK -> 
         Unix.unlink path;
        Unix.symlink rep path
      | _ -> () 
    with _ ->
      ()


let store_file ((rep,md) : (string * unit forest_md)) (path:string) : unit = 
  try 
    let ch = open_out path in 
    output_string ch rep;
    close_out ch
  with _ ->
    () 
      
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
module CostTimeMon : CostMon with type cost = Core.Time.Span.t = struct
  type cost = Core.Time.Span.t
  let get_time (_,m) = m.load_time
  let add_times t1 t2 =
    let now = Core.Time.now () in
    Core.Time.abs_diff (Core.Time.add (Core.Time.add now t1) t2) now
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
  include Core_kernel.Monad.S with type 'a t := 'a t 
  val run : ('a t -> ('a * cost))
  val get_cost : ('a t -> cost t)
end

module CursorMonad (M : CostMon) : CursorMonad with type cost = M.cost = struct
  include M
  type 'a t = cost -> ('a * cost)
  include Core_kernel.Monad.Make (struct
    type nonrec 'a t = 'a t
    let bind m ~f = 
      fun c ->
        let x,nc = m c in
        f x nc
    let return x = fun c -> (x,c)
    let map = `Define_using_bind
  end)
  let run (m : 'a t) : ('a * M.cost) = m M.cost_id 
  let get_cost (m : 'a t) : cost t = fun c -> let (_,c) = m c in (c,c)
end

