open Forest_types
open Forest_utility
open Asttypes
open Parsetree
open Ast_helper

     
let rec cost_function_generator (e : forest_node ast) (vName : string) : Parsetree.expression =
  let e,loc = get_NaL e in
 match e with
  | SkinApp(_,_) -> raise_loc_err loc "cost_function_generator: Skin applications should not exist here."
  | Var(x) -> exp_make_ident loc (cost_name x) 
  | Thunked(_)
  | Pads(_)
  | Url(_) -> [%expr fun (r,m) -> CursorMonad.cost_id ][@metaloc loc]

  | PathExp(_,e) 
  | Predicate(e,_) -> cost_function_generator e vName 

  | File -> [%expr CursorMonad.cost_file ][@metaloc loc]
  | Link -> [%expr CursorMonad.cost_link ][@metaloc loc]

  | Option(e) ->
     [%expr fun (r,m) ->
       match (r,m.data) with 
       | (None,_) -> CursorMonad.cost_id
       | (_,None) -> failwith "Option contained a representation, but no metadata. Should be impossible."
       | (Some(r),Some(m)) -> [%e cost_function_generator e vName] (r,m)
     ][@metaloc loc]
  | Comprehension(Map,e,_) ->
     [%expr fun (r,m) -> 
       PathMap.fold (fun key m acc -> 
         let r = PathMap.find key r in
         let c = [%e cost_function_generator e vName] (r,m) in
         CursorMonad.cost_op acc c
       ) m.data CursorMonad.cost_id
     ][@metaloc loc]
  | Comprehension(List,e,_) ->
     [%expr fun (r,m) -> 
       List.fold_left2 (fun acc r m -> 
         let c = [%e cost_function_generator e vName] (r,m) in
         CursorMonad.cost_op acc c
       )  CursorMonad.cost_id r (m : [%t Typ.var ~loc vName] forest_md).data
     ][@metaloc loc]
  | Directory (entries) -> 
     let field_list = List.fold_left (fun acc (label,e) ->
       let loc = get_loc e in
       [%expr [%e exp_make_string loc label] :: [%e acc]][@metaloc loc]
     ) ([%expr []][@metaloc loc]) entries 
     in
     let sumExp = List.fold_left (fun acc (label,e) -> 
       let loc = get_loc e in
       [%expr CursorMonad.cost_op [%e acc] [%e exp_make_ident loc label]][@metaloc loc]
     ) ([%expr CursorMonad.cost_id][@metaloc loc]) entries
     in
     let last_exp = 
       [%expr let cost1 = [%e sumExp] in 
              CursorMonad.cost_op cost1 (CursorMonad.cost_dir ([%e field_list],m))
       ][@metaloc loc]
     in
     let assignments = List.fold_right (fun (label,expi) acc ->
       let loc = get_loc expi in
       [%expr let  [%p pat_make_var loc label] =
                ([%e cost_function_generator expi label]) 
                  ([%e exp_make_field_n loc "r" label],
                   [%e exp_make_field loc (exp_make_field_n loc "m" "data") (md_name label)])
              in [%e acc]
       ][@metaloc loc]
     ) entries last_exp
     in
     [%expr fun (r,m) -> 
       [%e assignments]
     ][@metaloc loc]

and new_function_generator ?first:(first=false) (e: forest_node ast) (vName : string) : Parsetree.expression =
  let fNode,loc = get_NaL e in
  match fNode with
  | SkinApp(_,_) -> raise_loc_err loc "new_function_generator: Skin applications should not exist here."
  | Var(x) -> exp_make_ident loc (new_nameR x) 
  | Thunked(e) -> new_function_generator e vName
  | File
  | Pads(_)
  | Link 
  | PathExp(_,_)
  | Option(_)
  | Directory (_)
  | Predicate(_,_) 
  | Url(_) 
  | Comprehension(_,_,_) ->
       [%expr
           (fun path ->
             let lfunc () =
               let (rep,md) = 
                 [%e
		  let final_e = if first then exp_make_ident loc (load_name vName) else load_function_generator e vName in  
                     match e.payload with
                     | PVList(deplist) -> 
                        List.fold_right (fun v acc ->
                          let names = [%pat? ([%p pat_make_var loc v],[%p pat_make_var loc (md_name v)])][@metaloc loc] in
                          [%expr let [%p names] = [%e exp_make_ident loc @@ ucur_name v] () in 
                                 [%e acc]][@metaloc loc]
                        ) deplist final_e
		     | _ -> final_e						 
                 ] path
               in
               let cost = 
                 [%e if first then exp_make_ident loc (cost_name vName) else cost_function_generator e vName] (rep,md)
               in
               (rep,md,cost)
             in
             let manif (r,m) = 
               let manifest = 
                 [%e if first then exp_make_ident loc (manifest_name vName) else manifest_function_generator true e vName] (r,m)
               in
               manifest 
             in
             (lfunc,manif))
       ][@metaloc loc]

and load_function_generator (e: forest_node ast) (vName : string) : Parsetree.expression = 
  let e,loc = get_NaL e in
  let add_timing e = 
    [%expr fun path -> let currTime = Core.Time.now () in 
                        let (r,m) = [%e e] in
                        (r, { m with load_time = Core.Time.abs_diff currTime (Core.Time.now ()) } ) ] [@metaloc loc]
  in
  match e with
  | File -> [%expr Forest.load_file ] [@metaloc loc]
  | Link -> [%expr Forest.load_link ] [@metaloc loc]
  | Var(x) -> exp_make_ident loc (load_name x)
  | Pads(s) -> add_timing
    [%expr PadsInterface.load_for_forest
        [%e exp_make_ident loc (pads_parse_name s)] 
        path ][@metaloc loc]
  | Option(e) ->
    add_timing
      [%expr
          match Core.Sys.file_exists path with
          | `Yes ->
             let (r,m) =  ([%e load_function_generator e vName] path) in
             let new_md = { num_errors = m.num_errors;
                            error_msg = m.error_msg;
                            info = Forest.get_md_info path;
                            load_time = no_time;
                            data=Some(m)}
             in
             (Some r,new_md)
          | _ -> 
             (None, Forest.empty_md None path)
      ][@metaloc loc]
  | PathExp(ptype,e) ->
     add_timing
       (match ptype with
       | Constant(loc,pathi) -> 
          [%expr [%e load_function_generator e vName] (Filename.concat path [%e exp_make_string loc pathi])][@metaloc loc]
       | Variable(loc,pathi) ->
          [%expr [%e load_function_generator e vName] (Filename.concat path [%e exp_make_ident loc pathi])][@metaloc loc]
       | OC_Path(loc,pathi) ->
          [%expr [%e load_function_generator e vName] (Filename.concat path [%e exp_make_ocaml loc pathi])][@metaloc loc]
       )
  | Predicate(e,b) ->
    add_timing
    [%expr
        begin [@warning "-26"]
        let (this,this_md) = [%e load_function_generator e vName] path in
        let this_att = match this_md.info with
          | Some(info) -> info
          | None -> Forest.get_att_info path
        in
        if [%e exp_make_ocaml loc b] then (this,this_md) else 
          let new_md = {this_md with 
            error_msg = "Predicate failure" ::  this_md.error_msg;
            num_errors =  this_md.num_errors + 1}
          in
          (this,new_md)
      end][@metaloc loc]
  | Directory (entries) ->
    let ctype = [%type: ([%t typ_make_constr loc (representation_name vName)] * 
                             [%t typ_make_constr loc (md_name vName)])
                   ][@metaloc loc]
    in
    let rep_assgn = List.map (fun (label,_) -> label,label) entries in 
    let md_assgn = List.map (fun (label,_) -> (md_name label),(md_name label)) entries in
    let numErr = List.fold_left (fun acc (label,e) ->
      let loc = get_loc e in
      [%expr [%e exp_make_field_n loc (md_name label) "num_errors"] + [%e acc]][@metaloc loc]
    ) ([%expr 0][@metaloc loc]) entries
    in
    let errMsg = List.fold_left (fun acc (label,e) ->  
      let loc = get_loc e in
      [%expr [%e exp_make_field_n loc (md_name label) "error_msg"] @ [%e acc] ][@metaloc loc]
    ) ([%expr []][@metaloc loc]) entries
    in
    let init_exp = 
      [%expr let (r,m) : [%t ctype] = 
               ([%e exp_make_record_s loc rep_assgn], 
                { num_errors = [%e numErr];
                  error_msg = [%e errMsg];
		  info = Forest.get_md_info path; 
                  load_time = no_time;
		  data = [%e exp_make_record_s loc md_assgn] })
             in (r,m)][@metaloc loc]
    in
    let final_exp = 
      List.fold_right
        (fun (label,expi) acc ->
          let loc = get_loc expi in
          let names = [%pat? ([%p pat_make_var loc label],[%p pat_make_var loc (md_name label)])][@metaloc loc] in
	  let final_e = [%expr ([%e load_function_generator expi vName] path,
				(fun () -> [%e uninc_load_function_generator expi vName] path)) ][@metaloc loc]
	  in
	  let final_e =
	    match expi.payload with
            | PVList(deplist) -> 
               List.fold_right (fun v acc ->
		 let names = [%pat? ([%p pat_make_var loc v],[%p pat_make_var loc (md_name v)])][@metaloc loc] in
		 [%expr let [%p names] = [%e exp_make_ident loc @@ ucur_name v] () in 
			[%e acc]][@metaloc loc]
               ) deplist final_e
            | _ -> final_e
	  in
	  [%expr let ([%p names],[%p pat_make_var loc (ucur_name label)]) = [%e final_e] in 
		 [%e acc]][@metaloc loc]
        )
        entries
        init_exp
    in
    add_timing
    [%expr
        match Core.Sys.is_directory path with
        | `Yes -> [%e final_exp]
        | _ ->  failwith (Printf.sprintf "Path %s is not an accessible directory" path)
      ][@metaloc loc]

  | Comprehension(cType,e,pgList) -> 
     let load_e = load_function_generator e vName in
     let vlist,fListBool = List.fold_right (fun gen (acc,bacc) ->
       match gen with
       | Generator(_,bvar,Matches(_)) -> bvar :: acc, true
       | Generator(_,bvar,InList(_)) -> bvar :: acc, bacc
       | _ -> (acc,bacc)) pgList ([],false)
     in 
     let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1 in
     let make_exp pg (vacc,eacc) =
       match pg with
       | Generator(loc,mvar,Matches(locm,reg)) ->
          let cexpr = [%expr let [%p pat_make_var loc (list_name mvar)] =  List.fold_left (fun acc fname ->
            if regMatch fname 
            then fname :: acc 
            else acc
          ) [] fileList in [%e eacc]][@metaloc loc]
          in
          (mvar::vacc,
            match reg with
            | Regex(loc,reg) -> [%expr let regMatch = Forest.regexp_match_from_string [%e exp_make_string loc reg]
                                       in [%e cexpr] ][@metaloc loc]
            | Glob(loc,reg) ->  [%expr let regMatch = Forest.glob_match_from_string [%e exp_make_string loc reg]
                                       in [%e cexpr]][@metaloc loc]
          )
       | Generator(loc,mvar,InList(locm, lst)) -> (* lst should be an expression that evaluates to a list *)
          (mvar::vacc,
           [%expr let [%p pat_make_var loc (list_name mvar)] = List.rev [%e exp_make_ocaml locm lst] in [%e eacc]][@metaloc loc])
       | Guard(loc,aq) ->         
	  let depVars = List.fold_left (fun acc bvar ->
            if find_ident_in_str (att_name bvar) aq
            then bvar :: acc
            else if find_ident_in_str (md_name bvar) aq 
            then bvar :: acc
            else if find_ident_in_str bvar aq 
            then bvar :: acc
            else acc
	  ) [] (diff vlist vacc) (* Because we fold right, we need to check the ones we haven't seen yet *)
	  in
	  let bigExpr = 
            let pListTuple, emptyAccTuple, pAccTuple, falseTuple, trueTuple =
              (* Are there 0, 1, or more dependent variables? *)
              match depVars with
              | [] -> raise_loc_err loc "Comprehension guards must relate to some generated variable" 
              | id :: [] ->
                 let pListTuple = pat_make_var loc (list_name id) in
                 let emptyAccTuple =  [%expr []][@metaloc loc] in
                 let pAccTuple = pat_make_var loc (acc_name id) in
                 let falseTuple = exp_make_ident loc (acc_name id) in
                 let trueTuple =  
                   let lid = exp_make_ident loc id in
                   let acc = exp_make_ident loc (acc_name id) in
                   [%expr [%e lid] :: [%e acc]][@metaloc loc]
                 in
                 pListTuple, emptyAccTuple, pAccTuple, falseTuple, trueTuple
              | _ ->
                 let pListTuple = Pat.tuple ~loc (List.map (fun id -> pat_make_var loc (list_name id)) depVars) in
                 let emptyAccTuple =  Exp.tuple ~loc (List.map (fun id -> [%expr []][@metaloc loc]) depVars) in
                 let pAccTuple = Pat.tuple ~loc (List.map (fun id -> pat_make_var loc (acc_name id)) depVars) in
                 let falseTuple = Exp.tuple ~loc (List.map (fun id -> exp_make_ident loc (acc_name id)) depVars) in
                 let trueTuple =  Exp.tuple ~loc (List.map (fun id -> 
                   let lid = exp_make_ident loc id in
                   let acc = exp_make_ident loc (acc_name id) in
                   [%expr [%e lid] :: [%e acc]][@metaloc loc]
                 ) depVars)  
                 in
                 pListTuple, emptyAccTuple, pAccTuple, falseTuple, trueTuple
              in
              let bigExpr =
	        List.fold_left (fun accEx bvar ->  
	          let att = find_ident_in_str (att_name bvar) aq in
	          let rep = find_ident_in_str bvar aq in
	          let md = find_ident_in_str (md_name bvar) aq in
	          let newExp = if att
		    then [%expr let [%p pat_make_var loc (att_name bvar)] = 
                                  Forest.get_att_info (Filename.concat path [%e exp_make_ident loc bvar]) 
                                in [%e accEx]][@metaloc loc]
		    else accEx
	          in
                  let names = Pat.tuple ~loc [pat_make_var loc bvar; pat_make_var loc (md_name bvar)] in
	          let newExp = if rep || md
		    then [%expr let [%p names] = [%e load_function_generator e vName] path in [%e newExp]][@metaloc loc]
                    else newExp
	          in
	          [%expr List.fold_left (fun [%p pAccTuple] [%p pat_make_var loc bvar] -> [%e newExp])
                      [%e falseTuple] [%e exp_make_ident loc (list_name bvar)]][@metaloc loc]
	        ) ([%expr if [%e exp_make_ocaml loc aq] then [%e trueTuple] else [%e falseTuple] ][@metaloc loc]) depVars
              in
              [%expr let [%p pAccTuple] = [%e emptyAccTuple] in
                     let [%p pListTuple] = [%e bigExpr] in 
                     [%e eacc]][@metaloc loc]             
	  in
	  vacc, bigExpr
     in
    (* Things dependent on if it's a map or a list:
     * initExp: This is the expression that does the actual loading and makes a single rep and md and adds
     * them to the accumulator
     * initAcc: This is the initial accumulator (empty pathmaps or empty lists)
     * new_rm: This determines how to get the final main md by tallying up errors and such as well as the rep.
     *)
    let (initExp,initAcc,new_rm) = 
      match cType with
      | Map ->
         ([%expr let (r,m) = [%e load_e] path in 
                 match m.info with 
                 | None -> failwith "Loading failed in comprehension"
                 | Some(info) ->  (PathMap.add info.full_path r rep,PathMap.add info.full_path m md) ][@metaloc loc],
          [%expr (PathMap.empty,PathMap.empty) ][@metaloc loc],
          [%expr (rep,{
            num_errors = PathMap.fold (fun _ md a -> md.num_errors + a) md 0; 
            error_msg =  PathMap.fold (fun _ md a -> md.error_msg @ a) md [];
            info = Forest.get_md_info path;
            load_time = no_time;
            data = md;
          }) ][@metaloc loc])
      | List ->
         ([%expr let (r,m) = [%e load_e] path in (r::rep,m::md)][@metaloc loc],
          [%expr ([],[]) ][@metaloc loc],
          [%expr (rep,{
            num_errors = List.fold_left (fun a md -> md.num_errors + a) 0 md; 
            error_msg =  List.fold_left (fun a md -> md.error_msg @ a) [] md;
            info = Forest.get_md_info path;
            load_time = no_time;
            data = md;
          }) ][@metaloc loc])
    in
    (* Creates the folds that does the final accumulation. Does one fold per generator
     * and in the inner most fold it does the loading of a single rep and md (initExp).
     *)
    let get_reps_mds_exp = 
        List.fold_right (fun bv expAcc ->
          [%expr List.fold_left (fun (rep,md) [%p pat_make_var loc bv] ->
            [%e expAcc]
          ) [%e initAcc] [%e exp_make_ident loc (list_name bv)]][@metaloc loc])
          vlist initExp 
    in
    let _,mid_exp = List.fold_right make_exp 
      pgList
      ([],[%expr let (rep,md) = [%e get_reps_mds_exp] in [%e new_rm]][@metaloc loc])
    in
    (* If we need a file list (because we have a matching), we get it first *)
    add_timing 
      (if fListBool
       then [%expr let fileList = Array.to_list (Sys.readdir path) in [%e  mid_exp]][@metaloc loc]
       else mid_exp)
  | Url(e) -> add_timing
     (* Note that it assumes you're getting a directory if it's more than 1 file *)
     [%expr 
      let tmpdir = Filename.temp_file "forest-" "" in
      let _ = Sys.remove tmpdir in
      let _ = Unix.mkdir tmpdir 0o770 in
      let errCode = Sys.command ("wget -P " ^ tmpdir ^ " " ^ path) in
      let fNames = Array.to_list (Sys.readdir tmpdir) in
      let path = if List.length fNames > 1 then tmpdir else Filename.concat tmpdir (List.hd fNames) in 
      let (r,md) = [%e load_function_generator e vName] path in
      let newmd = if errCode = 0 then md
        else {data = md.data;
              info = md.info;
              error_msg = [ (Printf.sprintf "Undetermined wget error with code %d" errCode) :: md.error_msg ];
              load_time = no_time;
              num_errors = md.num_errors + 1}
      in
      (r,newmd) ][@metaloc loc]
  | Thunked(e) -> 
     add_timing [%expr ([%e new_function_generator e vName] path,Forest.unit_md path) ][@metaloc loc]
  | SkinApp(_,_) -> raise_loc_err loc "load_function_generator: Skin applications should not exist here."


and uninc_load_function_generator (e: forest_node ast) (vName : string) : Parsetree.expression = 
  let e,loc = get_NaL e in
  let add_timing e = 
    [%expr fun path -> let currTime = Core.Time.now () in 
                        let (r,m) = [%e e] in
                        (r, { m with load_time = Core.Time.abs_diff currTime (Core.Time.now ()) } ) ] [@metaloc loc]
  in
  match e with
  | File -> [%expr Forest.load_file ] [@metaloc loc]
  | Link -> [%expr Forest.load_link ] [@metaloc loc]
  | Var(x) -> exp_make_ident loc (load_name x)
  | Pads(s) -> add_timing
    [%expr PadsInterface.load_for_forest
        [%e exp_make_ident loc (pads_parse_name s)] 
        path ][@metaloc loc]
  | Option(e) ->
     add_timing
       [%expr
           match Core.Sys.file_exists path with
           | `Yes ->
              let (r,m) =  ([%e uninc_load_function_generator e vName] path) in
              let new_md = { num_errors = m.num_errors;
                             error_msg = m.error_msg;
                             info = Forest.get_md_info path;
                             load_time = no_time;
                             data=Some(m)}
              in
              (Some r,new_md)
           | _ -> 
              (None, Forest.empty_md None path)
       ][@metaloc loc]
  | PathExp(ptype,e) ->
     add_timing
       (match ptype with
       | Constant(loc,pathi) -> 
          [%expr [%e uninc_load_function_generator e vName] (Filename.concat path [%e exp_make_string loc pathi])][@metaloc loc]
       | Variable(loc,pathi) ->
          [%expr [%e uninc_load_function_generator e vName] (Filename.concat path [%e exp_make_ident loc pathi])][@metaloc loc]
       | OC_Path(loc,pathi) ->
          [%expr [%e uninc_load_function_generator e vName] (Filename.concat path [%e exp_make_ocaml loc pathi])][@metaloc loc]
       )
  | Predicate(e,b) ->
    add_timing
    [%expr
        begin [@warning "-26"]
        let (this,this_md) = [%e uninc_load_function_generator e vName] path in
        let this_att = match this_md.info with
          | Some(info) -> info
          | None -> Forest.get_att_info path
        in
        if [%e exp_make_ocaml loc b] then (this,this_md) else 
          let new_md = {this_md with 
            error_msg = "Predicate failure" ::  this_md.error_msg;
            num_errors =  this_md.num_errors + 1}
          in
          (this,new_md)
      end][@metaloc loc]
  | Directory (entries) ->
    let ctype = [%type: ([%t typ_make_constr loc (representation_name vName)] * 
                             [%t typ_make_constr loc (md_name vName)])
                   ][@metaloc loc]
    in
    let rep_assgn = List.map (fun (label,_) -> label,label) entries in 
    let md_assgn = List.map (fun (label,_) -> (md_name label),(md_name label)) entries in
    let numErr = List.fold_left (fun acc (label,e) ->
      let loc = get_loc e in 
      [%expr [%e exp_make_field_n loc (md_name label) "num_errors"] + [%e acc]][@metaloc loc]
    ) ([%expr 0][@metaloc loc]) entries
    in
    let errMsg = List.fold_left (fun acc (label,e) ->  
      let loc = get_loc e in 
      [%expr [%e exp_make_field_n loc (md_name label) "error_msg"] @ [%e acc] ][@metaloc loc]
    ) ([%expr []][@metaloc loc]) entries
    in
    let init_exp = 
      [%expr let (r,m) : [%t ctype] = 
               ([%e exp_make_record_s loc rep_assgn], 
                { num_errors = [%e numErr];
                  error_msg = [%e errMsg];
		  info = Forest.get_md_info path; 
                  load_time = no_time;
		  data = [%e exp_make_record_s loc md_assgn] })
             in (r,m)][@metaloc loc]
    in
    let final_exp = 
      List.fold_right
        (fun (label,expi) acc ->
          let loc = get_loc expi in 
          let names = [%pat? ([%p pat_make_var loc label],[%p pat_make_var loc (md_name label)])][@metaloc loc] in
          [%expr let [%p names] = [%e uninc_load_function_generator expi vName] path in [%e acc]][@metaloc loc]
        )
        entries
        init_exp
    in
    add_timing
    [%expr
        match Core.Sys.is_directory path with
        | `Yes -> [%e final_exp]
        | _ ->  failwith (Printf.sprintf "Path %s is not a directory" path)          
      ][@metaloc loc]

  | Comprehension(cType,e,pgList) -> 
     let load_e = uninc_load_function_generator e vName in
     let vlist,fListBool = List.fold_right (fun gen (acc,bacc) -> match gen with
       | Generator(_,bvar,Matches(_)) -> bvar :: acc, true
       | Generator(_,bvar,InList(_)) -> bvar :: acc, bacc
       | _ -> (acc,bacc)) pgList ([],false)
     in 
     let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1 in
     let make_exp pg (vacc,eacc) =
       match pg with
       | Generator(loc,mvar,Matches(locm,reg)) ->
          let cexpr = [%expr let [%p pat_make_var loc (list_name mvar)] =  List.fold_left (fun acc fname ->
            if regMatch fname 
            then fname :: acc 
            else acc
          ) [] fileList in [%e eacc]][@metaloc loc]
          in
          (mvar::vacc,
            match reg with
            | Regex(loc,reg) -> [%expr let regMatch = Forest.regexp_match_from_string [%e exp_make_string loc reg]
                                       in [%e cexpr] ][@metaloc loc]
            | Glob(loc,reg) ->  [%expr let regMatch = Forest.glob_match_from_string [%e exp_make_string loc reg]
                                       in [%e cexpr]][@metaloc loc]
          )
       | Generator(loc,mvar,InList(locm, lst)) -> (* lst should be an expression that evaluates to a list *)
          (mvar::vacc,
           [%expr let [%p pat_make_var loc (list_name mvar)] = [%e exp_make_ocaml locm lst] in [%e eacc]][@metaloc loc])
       | Guard(loc,aq) ->         
	  let depVars = List.fold_left (fun acc bvar ->
            if find_ident_in_str (att_name bvar) aq
            then bvar :: acc
            else if find_ident_in_str (md_name bvar) aq 
            then bvar :: acc
            else if find_ident_in_str bvar aq 
            then bvar :: acc
            else acc
	  ) [] (diff vlist vacc) (* Because we fold right, we need to check the ones we haven't seen yet *)
	  in
	  let bigExpr = 
            let pListTuple, emptyAccTuple, pAccTuple, falseTuple, trueTuple =
              (* Are there 0, 1, or more dependent variables? *)
              match depVars with
              | [] -> raise_loc_err loc "Comprehension guards must relate to some generated variable" 
              | id :: [] ->
                 let pListTuple = pat_make_var loc (list_name id) in
                 let emptyAccTuple =  [%expr []][@metaloc loc] in
                 let pAccTuple = pat_make_var loc (acc_name id) in
                 let falseTuple = exp_make_ident loc (acc_name id) in
                 let trueTuple =  
                   let lid = exp_make_ident loc id in
                   let acc = exp_make_ident loc (acc_name id) in
                   [%expr [%e lid] :: [%e acc]][@metaloc loc]
                 in
                 pListTuple, emptyAccTuple, pAccTuple, falseTuple, trueTuple
              | _ ->
                 let pListTuple = Pat.tuple ~loc (List.map (fun id -> pat_make_var loc (list_name id)) depVars) in
                 let emptyAccTuple =  Exp.tuple ~loc (List.map (fun id -> [%expr []][@metaloc loc]) depVars) in
                 let pAccTuple = Pat.tuple ~loc (List.map (fun id -> pat_make_var loc (acc_name id)) depVars) in
                 let falseTuple = Exp.tuple ~loc (List.map (fun id -> exp_make_ident loc (acc_name id)) depVars) in
                 let trueTuple =  Exp.tuple ~loc (List.map (fun id -> 
                   let lid = exp_make_ident loc id in
                   let acc = exp_make_ident loc (acc_name id) in
                   [%expr [%e lid] :: [%e acc]][@metaloc loc]
                 ) depVars)  
                 in
                 pListTuple, emptyAccTuple, pAccTuple, falseTuple, trueTuple
              in
              let bigExpr =
	        List.fold_left (fun accEx bvar ->  
	          let att = find_ident_in_str (att_name bvar) aq in
	          let rep = find_ident_in_str bvar aq in
	          let md = find_ident_in_str (md_name bvar) aq in
	          let newExp = if att
		    then [%expr let [%p pat_make_var loc (att_name bvar)] = 
                                  Forest.get_att_info (Filename.concat path [%e exp_make_ident loc bvar]) 
                                in [%e accEx]][@metaloc loc]
		    else accEx
	          in
                  let names = Pat.tuple ~loc [pat_make_var loc bvar; pat_make_var loc (md_name bvar)] in
	          let newExp = if rep || md
		    then [%expr let [%p names] = [%e uninc_load_function_generator e vName] path in [%e newExp]][@metaloc loc]
                    else newExp
	          in
	          [%expr List.fold_left (fun [%p pAccTuple] [%p pat_make_var loc bvar] -> [%e newExp])
                      [%e falseTuple] [%e exp_make_ident loc (list_name bvar)]][@metaloc loc]
	        ) ([%expr if [%e exp_make_ocaml loc aq] then [%e trueTuple] else [%e falseTuple] ][@metaloc loc]) depVars
              in
              [%expr let [%p pAccTuple] = [%e emptyAccTuple] in
                     let [%p pListTuple] = [%e bigExpr] in 
                     [%e eacc]][@metaloc loc]             
	  in
	  vacc, bigExpr
     in
    (* Things dependent on if it's a map or a list:
     * initExp: This is the expression that does the actual loading and makes a single rep and md and adds
     * them to the accumulator
     * initAcc: This is the initial accumulator (empty pathmaps or empty lists)
     * new_rm: This determines how to get the final main md by tallying up errors and such as well as the rep.
     *)
    let (initExp,initAcc,new_rm) = 
      match cType with
      | Map ->
         ([%expr let (r,m) = [%e load_e] path in 
                 match m.info with 
                 | None -> failwith "Loading failed in comprehension"
                 | Some(info) ->  (PathMap.add info.full_path r rep,PathMap.add info.full_path m md) ][@metaloc loc],
          [%expr (PathMap.empty,PathMap.empty) ][@metaloc loc],
          [%expr (rep,{
            num_errors = PathMap.fold (fun _ md a -> md.num_errors + a) md 0; 
            error_msg =  PathMap.fold (fun _ md a -> md.error_msg @ a) md [];
            info = Forest.get_md_info path;
            load_time = no_time;
            data = md;
          }) ][@metaloc loc])
      | List ->
         ([%expr let (r,m) = [%e load_e] path in (r::rep,m::md)][@metaloc loc],
          [%expr ([],[]) ][@metaloc loc],
          [%expr (List.rev rep,{
            num_errors = List.fold_left (fun a md -> md.num_errors + a) 0 md; 
            error_msg =  List.fold_left (fun a md -> md.error_msg @ a) [] md;
            info = Forest.get_md_info path;
            load_time = no_time;
            data = List.rev md;
          }) ][@metaloc loc])
    in
    (* Creates the folds that does the final accumulation. Does one fold per generator
     * and in the inner most fold it does the loading of a single rep and md (initExp).
     *)
    let get_reps_mds_exp = 
        List.fold_right (fun bv expAcc ->
          [%expr List.fold_left (fun (rep,md) [%p pat_make_var loc bv] ->
            [%e expAcc]
          ) [%e initAcc] [%e exp_make_ident loc (list_name bv)]][@metaloc loc])
          vlist initExp 
    in
    let _,mid_exp = List.fold_right make_exp 
      pgList
      ([],[%expr let (rep,md) = [%e get_reps_mds_exp] in [%e new_rm]][@metaloc loc])
    in
    (* If we need a file list (because we have a matching), we get it first *)
    add_timing 
      (if fListBool
       then [%expr let fileList = Array.to_list (Sys.readdir path) in [%e  mid_exp]][@metaloc loc]
       else mid_exp)
  | Url(e) -> add_timing 
     [%expr (* Note that it assumes you're getting a directory if it's more than 1 file *)
      let tmpdir = Filename.temp_file "forest-" "" in
      let _ = Sys.remove tmpdir in
      let _ = Unix.mkdir tmpdir 0o770 in
      let errCode = Sys.command ("wget -P " ^ tmpdir ^ " " ^ path) in
      let fNames = Array.to_list (Sys.readdir tmpdir) in
      let path = if List.length fNames > 1 then tmpdir else Filename.concat tmpdir (List.hd fNames) in
      let (r,md) = [%e uninc_load_function_generator e vName] path in
      let newmd = if errCode = 0 then md
        else {data = md.data;
              info = md.info;
              error_msg = [ (Printf.sprintf "Undetermined wget error with code %d" errCode) :: md.error_msg ];
              load_time = no_time;
              num_errors = md.num_errors + 1}
      in
      (r,newmd) ][@metaloc loc]
    | Thunked(e) ->
     let final_e = uninc_load_function_generator e vName in
     begin
       match e.payload with
       | PVList(deplist) -> 
          List.fold_right (fun v acc ->
	    let names = [%pat? ([%p pat_make_var loc v],[%p pat_make_var loc (md_name v)])][@metaloc loc] in
	    [%expr let [%p names] = [%e exp_make_ident loc @@ ucur_name v] () in 
		   [%e acc]][@metaloc loc]
          ) deplist final_e
       | _ -> final_e
     end
    | SkinApp(_,_) ->
       raise_loc_err loc "load_function_generator: Skin applications should not exist here."

         
and empty_manifest_generator expression =
  let forestAst, location = get_NaL expression in
  match forestAst with
  | File
  | Link ->
     [%expr 
      let commit () = () in
      let validate () = [] in
      { validate; commit; data = () }
     ][@metaloc location]
  | Var(specificationName) ->  exp_make_ident location (empty_manifest_name specificationName)
  | Pads(specificationName) ->
       [%expr
        let baseName = Filename.basename info.full_path in
        let padsManifest =
          [%e exp_make_ident location (pads_empty_manifest_name specificationName)]
        in
        let commit () = () in
        let validate () =
          List.map (fun error -> (baseName, PadsError error)) padsManifest.pads_man_errors
        in
        { validate; commit; data = manifest}
       ][@metaloc location]
  | PathExp(_, forestAst)
  | Predicate(forestAst, _)
  | Url(forestAst)
  | Thunked(forestAst) 
  | Option(forestAst) -> empty_manifest_generator forestAst
  | Directory(entries) ->
     let manifestAssignments =
       List.map (fun (label,_) -> (manifest_name label),(manifest_name label)) entries
     in
     let finalManifest = 
       [%expr
        let validate () =
          [%e List.fold_right
              (fun (label,_) errorList ->
                [%expr
                    [%e exp_make_field_n location (manifest_name label) "validate"]  () @
                    [%e errorList]
                ][@metaloc location]
              )
              entries
              [%expr []][@metaloc location]
          ]
        in
        let commit () =
          [%e List.fold_right
              (fun (label,_) expressionAccumulator ->
                [%expr
                    [%e exp_make_field_n location (manifest_name label) "commit"]  ();
                 [%e expressionAccumulator]
                ][@metaloc location]
              ) entries
              [%expr ()][@metaloc location]
          ]
        in
        let data = [%e exp_make_record_s location manifestAssignments] in
        { validate; commit; data }
       ][@metaloc location]
     in
     List.fold_right
       (fun (label,expression) expressionAccumulator ->
         let location = get_loc expression in
         [%expr
          let [%p pat_make_var location (manifest_name label)] =
            [%e empty_manifest_generator expression] 
          in 
          [%e expressionAccumulator]
         ][@metaloc location]
       ) entries finalManifest
  | Comprehension(comprehensionType, _, _) ->
     begin
       match comprehensionType with
       (* TODO (jdilorenzo): Almost certainly needs to be wrapped in a manifest *)
       | Map ->  [%expr PathMap.empty][@metaloc location]
       | List -> [%expr []][@metaloc location]
     end
       
  | SkinApp(_,_) ->
     raise_loc_err location "empty_manifest_generator: Skin applications should not exist here."
     
and manifest_function_generator inside expression vName = 
  let forestAst, location = get_NaL expression in
  let mainExp =
    match forestAst with
    | Thunked(forestAst) -> empty_manifest_generator forestAst
    | Var(specificationName) ->
       [%expr
           [%e exp_make_ident location (manifest_name specificationName)]
           ~swapDirectory:swapDirectory
           (rep,md)
       ][@metaloc location]
         
    (* TODO (jdilorenzo): This feels wrong... Can I really just rely on the information in the md
       for path expressions? *)
    | PathExp(ptype,e) ->
       [%expr [%e manifest_function_generator true e vName] ~swapDirectory:swapDirectory (rep,md)]
         [@metaloc location] 
    | Url(forestAst) -> 
       [%expr
        let manifest =
          [%e manifest_function_generator true forestAst vName]
            ~swapDirectory:swapDirectory
            (rep,md)
        in
        let commit () = 
          Printf.eprintf "Impossible to store a URL back to its initial path.\nDoing nothing.\n"
        in
        { manifest with commit }
       ][@metaloc location]
    | Pads(x) ->
       [%expr
        let baseName = Filename.basename info.full_path in
        let storePath = info.full_path in
        let padsManifest = [%e exp_make_ident location (pads_manifest_name x)] (rep, md.data) in
        let swapPath = Filename.concat swapDirectory basename in
        PadsInterface.pads_store padsManifest swapPath;
        let commit () =
          if Forest.check_exists storePath then
            Sys.remove storePath;
          Sys.rename swapPath storePath
        in
        let validate () =
          let errors =
            List.map (fun error -> (baseName, PadsError error)) padsManifest.pads_man_errors
          in
          if Forest.check_exists storePath && not Forest.check_writeable storePath then
            (baseName, PermissionError) :: errors
          else
            errors
        in
        { validate; commit; data = manifest}
       ][@metaloc location]
    | Predicate(forestAst, predicate) ->
       [%expr
        let baseName = Filename.basename info.full_path in
        let manifest = 
          [%e manifest_function_generator true forestAst vName]
            ~swapDirectory:swapDirectory
            (rep, md)
        in
        let validate () =
          let errors = manifest.validate () in
          begin [@warning "-26"]
          let (this,this_md) = (rep,md) in
          let this_att = info in
          if [%e exp_make_ocaml location predicate] then
            errors
          else
            [basename, PredicateFail] :: errors
          end
        in
        { manifest with validate }
       ][@metaloc location]
    | File ->
       [%expr
        let baseName = Filename.basename info.full_path in
        let storePath = info.full_path in
        let swapPath = Filename.concat swapDirectory baseName in
        let _ = Forest.store_file (rep,md) swapPath in
        let commit () =
          begin
            match Sys.file_exists storePath with
            | `Yes -> Sys.remove storePath
            | `No -> ()
            | `Unknown -> failwith
               (Printf.sprintf "Failed to store file at `%s` due to unknown causes" storePath)
          end;
          Sys.rename swapPath storePath
        in
        let validate () = [] in
        { validate; commit; data = () }
       ][@metaloc location]
    | Link ->
       [%expr
        let baseName = Filename.basename info.full_path in
        let storePath = info.full_path in
        let swapPath = Filename.concat swapDirectory baseName in
        Forest.store_link (rep,md) swapPath;
        let commit () =
          begin
            match Sys.file_exists storePath with
            | `Yes ->  Unix.unlink storePath
            | `No -> ()
            | `Unknown -> failwith
               (Printf.sprintf "Failed to store link at `%s` due to unknown causes" storePath)
          end;
          Sys.rename swapPath storePath
        in
        let validate () = [] in
        { validate; commit; data = () }
       ][@metaloc location]
    | Option(forestAst) ->
       [%expr
        let baseName = Filename.basename info.full_path in
        let storePath = info.full_path in
        match (rep, md.data) with 
        |  (None, _) ->
           let commit () =
             (* TODO (jdilorenzo): This fails in general. Make it delete ANYTHING *)
             match Sys.file_exists storePath with
             | `Yes ->  Sys.remove storePath
             | _ -> ()
           in
           let validate () = [] in
           let manifest = [%e empty_manifest_generator forestAst] in
           { manifest with validate; commit }
        | (Some r, Some m) ->
           [%e manifest_function_generator true forestAst vName] ~swapDirectory:swapDirectory (r, m)
        | (Some r, None) ->
           let commit () = () in
           let validate () = [(baseName, OptMDRepInconsistency)] in
           let manifest = [%e empty_manifest_generator forestAst] in
           { manifest with validate; commit }
       ][@metaloc location]

    | Directory (entries) ->
       let manifestAssignments =
         List.map (fun (label,_) -> (manifest_name label),(manifest_name label)) entries
       in
       let finalManifest = 
         [%expr
          let validate () =
            let errorList = 
              match Core.Sys.file_exists storePath, Core.Sys.is_directory storePath with
              | `Yes, `No -> [(baseName,DirFilenameOverlap)]
              | _ -> []
            in
            [%e List.fold_right
                (fun (label,_) errorList ->
                  [%expr
                      [%e exp_make_field_n location (manifest_name label) "validate"]  () @
                      [%e errorList]
                  ][@metaloc location]
                )
                entries
                [%expr errorList][@metaloc location]
            ]
          in
          let commit () =
            match Core.Sys.file_exists storePath, Core.Sys.is_directory storePath with
            | `Yes, `Yes ->
               [%e List.fold_right
                   (fun (label,_) expressionAccumulator ->
                     [%expr
                      [%e exp_make_field_n location (manifest_name label) "commit"]  ();
                      [%e expressionAccumulator]
                     ][@metaloc location]
                   ) entries
                   [%expr Unix.rmdir swapPath][@metaloc location]
               ]
            | `No, _ -> Sys.rename swapPath storePath
            | `Yes, `No ->
               failwith (Printf.sprintf "Non-directory file exists at path %s" storePath)
            | _ -> failwith 
               (Printf.sprintf "Failed to store directory at `%s` due to unknown causes" storePath)
          in
          let data = [%e exp_make_record_s location manifestAssignments] in
          { validate; commit; data }
         ][@metaloc location]
       in
       [%expr
        let baseName = Filename.basename info.full_path in  
        let storePath = info.full_path in
        let swapPath = Filename.concat swapDirectory baseName in
        let _ = Core.Unix.mkdir ~perm:info.permissions swapPath in
        [%e List.fold_right
            (fun (label,expression) expressionAccumulator ->
              let location = get_loc expression in
              let names =
                [%pat? (
                  [%p pat_make_var location label],
                  [%p pat_make_var location (md_name label)]
                )][@metaloc location]
              in
              [%expr 
               let [%p names] = 
                 ([%e exp_make_field_n location "rep" label],
                  [%e exp_make_field
                      location
                      (exp_make_field_n location "md" "data")
                      (md_name label)
                  ])
               in
               let [%p pat_make_var location (manifest_name label)] = 
                 [%e manifest_function_generator true expression vName] ~swapDirectory:swapPath 
                   ([%e exp_make_ident location label],
                    [%e exp_make_ident location (md_name label)])
               in 
               [%e expressionAccumulator]
              ][@metaloc location]
            ) entries finalManifest]
       ][@metaloc location]
         
    | Comprehension(comprehensionType, forestAst, _) -> 
       let manifestExpression =
         [%expr 
          let storePath = info.full_path in
          let zipped =
            match List.zip reps mds with
            | Some list -> list
            | None -> []
          in
          let manifests =
            List.map
              ([%e manifest_function_generator true forestAst vName] ~swapDirectory:swapDirectory)
              zipped
          in
          let validate () =
            let errors =
              if List.length reps = List.length mds then
                []
              else
                [(baseName,ComprehensionUnequalLength)]
            in
            List.fold_left
              (fun errorAccumulator manifest -> manifest.validate () @ errorAccumulator)
              errors
              manifests
          in
          let commit () =
            List.iter (fun manifest -> manifest.commit ()) manifests
          in
          { validate; commit; data = manifests }
         ][@metaloc location]
       in
       (* TODO (jdilorenzo): This is gonna need to produce a map too... *)
       if comprehensionType = Map
       then [%expr 
             let reps = List.map snd (PathMap.bindings rep) in
             let mds = List.map snd (PathMap.bindings md.data) in
             [%e manifestExpression]][@metaloc location]
       else [%expr 
             let reps = rep in
             let mds = md.data in
             [%e manifestExpression]][@metaloc location]
    | SkinApp(_,_) ->
       raise_loc_err
         location
         "manifest_function_generator: Skin applications should not exist here."
  in
  match forestAst with
  (* TODO (jdilorenzo): Think I can remove this part now, but let's test more first *)
  | Var(name) -> [%expr fun ?swapDirectory:(swapDirectory="") (rep,md) -> [%e mainExp]][@metaloc location] 
  | _ -> 
     let mainSetup =
       [%expr
           match md.info with
           | None ->
              let manifest = [%e empty_manifest_generator expression] in
              let validate () = manifest.validate () @ [("",MDMissingInfo)] in
              { manifest with validate }
           | Some(info) -> [%e mainExp]
       ][@metaloc location]
     in
     if inside 
     then [%expr (fun ?swapDirectory:(swapDirectory="") (rep,md) -> [%e mainSetup]) ][@metaloc location] 
     else 
       [%expr (fun ?swapDirectory:(swapDirectory="") (rep,md) -> 
         let swapDirectory = if swapDirectory = ""
           then
             let swapDirectory = Filename.temp_file "forest-" "" in
             let _ = Sys.remove swapDirectory in
             let _ = Core.Unix.mkdir ~perm:0o770 swapDirectory in
             swapDirectory
           else
             swapDirectory
         in
         [%e mainSetup])
       ][@metaloc location]
