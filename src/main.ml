(* Copyright (C) 2019 Types Logics Cats

   This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

module Result = struct
  include Stdlib.Result

  let (let+) res f = map f res

  let (let*) = bind
end

let find_template hashtbl name =
  match Hashtbl.find_opt hashtbl name with
  | Some x -> Ok x
  | None -> Error (Error.Unknown_template name)

let parse file =
  let open Result in
  let lexbuf = Lexing.from_channel file in
  match Frontmatter.main lexbuf with
  | None, content -> Ok (`Null, content)
  | Some yaml, content ->
     let+ yaml = Result.map_error Error.to_error (Yaml.of_string yaml) in
     (yaml, content)

let get_doc path =
  Filesystem.with_in (fun file ->
      let open Result in
      match Filename.chop_suffix_opt ~suffix:".html" path with
      | Some name ->
         let* yaml, content = parse file in
         let* yaml =
           Yaml_aux.get_obj yaml
           |> Error.of_option (Error.Expected_type("object", yaml)) in
         Ok (Some Document.{ name; yaml; content })
      | None ->
         match Filename.chop_suffix_opt ~suffix:".md" path with
         | Some name ->
            let* yaml, content = parse file in
            let* yaml =
              Yaml_aux.get_obj yaml
              |> Error.of_option (Error.Expected_type("object", yaml)) in
            let content = Omd.to_html (Omd.of_string content) in
            print_endline content;
            Ok (Some Document.{ name; yaml; content })
         | None -> Ok None
    ) path

let render templates Document.{ yaml; content; _ } =
  let open Result in
  let* template_name = Yaml_aux.find_str_attr "layout" yaml in
  let+ template = find_template templates template_name in
  Mustache.render template (`O (("content", `String content) :: yaml))

let compile templates path =
  let open Result in
  let* doc = get_doc path in
  match doc with
  | None -> Ok ()
  | Some doc ->
     let+ str = render templates doc in
     let output_path = Filename.concat "public" (doc.name ^ ".html") in
     Filesystem.with_out (fun file ->
         output_string file str;
         flush file
       ) output_path

let get_templates dirname =
  let names = Sys.readdir dirname in
  let hashtbl = Hashtbl.create 10 in
  Array.iter (fun name ->
      Filename.concat dirname name
      |> Filesystem.with_in
           (fun file -> Mustache.parse_lx (Lexing.from_channel file))
      |> Hashtbl.add hashtbl name
    ) names;
  hashtbl

let annot_result name = function
  | Ok x -> Ok x
  | Error e -> Error (name, e)

let ignore_file config name =
  name = config.Config.template_dir
  || name = config.output_dir
  || List.fold_left (fun b (regex, _) ->
         b || Re.execp regex name
       ) false config.routes


let rec compile_dir config templates subdir =
  let open Result in
  let dest = Filename.concat config.Config.output_dir subdir in
  Filesystem.mkdir_opt dest;
  Filesystem.fold_dir (fun res name ->
      let* () = res in
      let path = Filename.concat subdir name in
      if ignore_file config name then
        Ok ()
      else if Sys.is_directory path then
        compile_dir config templates path
      else
        annot_result path (compile templates path)
    ) (Ok ()) (Filename.concat "./" subdir)

let () =
  let open Result in
  match
    let* config = annot_result "config.yml" (Config.of_file "config.yml") in
    if Sys.file_exists config.output_dir then
      Filesystem.rm config.output_dir;
    Filesystem.mkdir_opt config.output_dir;
    let templates = get_templates config.Config.template_dir in
    compile_dir config templates ""
  with
  | exception Fun.Finally_raised e -> raise e
  | exception Sys_error s -> print_endline s
  | Error (where, e) -> print_endline @@ Error.show_error where e
  | _ -> ()
