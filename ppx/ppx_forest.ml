open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Location
open Utility


let forest_mapper argv =
  (* Our getenv_mapper only overrides the handling of expressions in the default mapper. *)
  { default_mapper with
    structure = fun mapper strl ->  
      let rec forest_replace acc str =
        match str with
          (* Processes Forest descriptions *)
        | { pstr_desc = Pstr_extension 
            (({txt = "forest"}, PStr 
              [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string (fstr, Some "")); pexp_loc},_)}]
             ),_)
          ; pstr_loc} -> 
            (* fstr = Forest descriptions
             * pstr_loc = Location of description*)
           let init = 
             if !Utility.firstDef
             then
               let _ = Utility.firstDef := false in
               Ppx_forest_lib.init pstr_loc
             else []
           in
	  let forest_ast = Forest_parser_helper.forest_parse_string pexp_loc fstr in
	  let _ = List.iter (fun (v,s) -> Hashtbl.add Utility.forestTbl v s) forest_ast in
          let ocaml_asts : Parsetree.structure = Ppx_forest_lib.def_generator pstr_loc forest_ast in
          ocaml_asts :: init :: acc
            
          (* Processes Skin descriptions *)
        | { pstr_desc = Pstr_extension 
            (({txt = "skin"}, PStr 
              [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string (fstr, Some "")); pexp_loc},_)}]
             ),_)
          ; pstr_loc} -> 
	   let skin_ast = Forest_parser_helper.skin_parse_string pexp_loc fstr in
	   let _ = List.iter (fun (v,s) -> Hashtbl.add Utility.skinTbl v s) skin_ast in
	   acc
             
         (* Rest is kept the same*)
         | x -> [default_mapper.structure_item mapper x] :: acc

            
      in
      List.flatten (List.rev (List.fold_left forest_replace [] strl))
  }

let () = register "forest" forest_mapper

   
