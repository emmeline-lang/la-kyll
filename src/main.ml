(* Copyright (C) 2019 Types Logics Cats

   This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

module Result = struct
  include Stdlib.Result

  let (let+) res f = map f res

  let (let*) = bind
end

type document = {
    name : string;
    yaml : (string * Yaml.value) list;
    content : string;
  }

type error_kind =
  | Exn of exn
  | Expected_type of string * Yaml.value
  | Missing_key of string
  | Unknown_suffix of string
  | Unknown_template of string
  | Other of string

let show_error_kind = function
  | Exn e -> raise e
  | Expected_type(s, yaml) ->
     "Expected type " ^ s ^ ", got " ^ Yaml.to_string_exn yaml
  | Missing_key s -> "Missing key " ^ s
  | Unknown_suffix s -> "Unknown suffix of " ^ s
  | Unknown_template s -> "Unknown template " ^ s
  | Other s -> s

let show_error loc kind =
  "In file " ^ loc ^ ": " ^ (show_error_kind kind)

let to_error (`Msg str) = Other str

let to_result f x =
  try Ok (f x) with
  | e -> Error (Exn e)

let with_in f name =
  let file = open_in name in
  Fun.protect (fun () -> f file) ~finally:(fun () -> close_in file)

let with_out f name =
  let file = open_out name in
  Fun.protect (fun () -> f file) ~finally:(fun () -> close_out file)

let read_file file =
  let approx_size = in_channel_length file in
  let buf = Buffer.create approx_size in
  let rec loop () =
    match input_char file |> Buffer.add_char buf with
    | exception End_of_file -> ()
    | () -> loop ()
  in
  loop ();
  Buffer.contents buf

let mkdir_opt name =
  if not (Sys.file_exists name) then
    Unix.mkdir name 0o777

let rec find_attr f target = function
  | [] -> None
  | (k, v) :: _ when f k target -> Some v
  | _ :: xs -> find_attr f target xs

let find_attr' f target yaml =
  match find_attr f target yaml with
  | Some x -> Ok x
  | None -> Error (Missing_key target)

let get_obj = function
  | (`O x) -> Ok x
  | `Null -> Ok []
  | yaml -> Error (Expected_type("object", yaml))

let find_str_attr target yaml =
  match find_attr (=) target yaml with
  | Some (`String x) -> Ok x
  | Some yaml -> Error (Expected_type("string", yaml))
  | None -> Error (Missing_key target)

let find_template hashtbl name =
  match Hashtbl.find_opt hashtbl name with
  | Some x -> Ok x
  | None -> Error (Unknown_template name)

let parse file =
  let open Result in
  let lexbuf = Lexing.from_channel file in
  match Lexer.main lexbuf with
  | None, content -> Ok (`Null, content)
  | Some yaml, content ->
     let+ yaml = Result.map_error to_error (Yaml.of_string yaml) in
     (yaml, content)

let get_doc path =
  with_in (fun file ->
      let open Result in
      match Filename.chop_suffix_opt ~suffix:".html" path with
      | Some name ->
         let* yaml, content = parse file in
         let* yaml = get_obj yaml in
         Ok (Some { name; yaml; content })
      | None ->
         match Filename.chop_suffix_opt ~suffix:".md" path with
         | Some name ->
            let* yaml, content = parse file in
            let* yaml = get_obj yaml in
            let content = Omd.to_html (Omd.of_string content) in
            print_endline content;
            Ok (Some { name; yaml; content })
         | None -> Ok None
    ) path

let render templates { yaml; content; _ } =
  let open Result in
  let* template_name = find_str_attr "layout" yaml in
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
     with_out (fun file ->
         output_string file str;
         flush file
       ) output_path

let get_templates dirname =
  let names = Sys.readdir dirname in
  let hashtbl = Hashtbl.create 10 in
  Array.iter (fun name ->
      Filename.concat dirname name
      |> with_in (fun file -> Mustache.parse_lx (Lexing.from_channel file))
      |> Hashtbl.add hashtbl name
    ) names;
  hashtbl

let ignore_file name =
  name = "_layouts" || name = "public"

let annot_result name = function
  | Ok x -> Ok x
  | Error e -> Error (name, e)

let rec compile_dir templates subdir =
  let open Result in
  let names = Sys.readdir (Filename.concat "./" subdir) in
  mkdir_opt (Filename.concat "public" subdir);
  Array.fold_left (fun res name ->
      let* () = res in
      let path = Filename.concat subdir name in
      if ignore_file name then
        Ok ()
      else if Sys.is_directory path then
        compile_dir templates path
      else
        annot_result path (compile templates path)
    ) (Ok ()) names

let () =
  mkdir_opt "public";
  match compile_dir (get_templates "_layouts") "" with
  | exception Fun.Finally_raised e -> raise e
  | exception Sys_error s -> print_endline s
  | Error (where, e) -> print_endline @@ show_error where e
  | _ -> ()
