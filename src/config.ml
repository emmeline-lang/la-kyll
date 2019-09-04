(* Copyright (C) 2019 Types Logics Cats

   This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

type t = {
    input_dir : string;
    output_dir : string;
    template_dir : string;
  }

let default =
  { input_dir = "pages"
  ; output_dir = "public"
  ; template_dir = "templates" }

let set_input_dir input_dir t =
  { t with input_dir }

let set_output_dir output_dir t =
  { t with output_dir }

let set_template_dir template_dir t =
  { t with template_dir }

let set_opt f opt t = match opt with
  | Ok x -> Ok (f x t)
  | Error (Error.Missing_key _) -> Ok t
  | Error e -> Error e

let of_yaml yaml =
  let open Yaml_aux in
  let (>>=) = Stdlib.Result.bind in
  match get_obj yaml with
  | Some obj ->
     Ok default
     >>= set_opt set_input_dir (find_str_attr "input_dir" obj)
     >>= set_opt set_output_dir (find_str_attr "output_dir" obj)
     >>= set_opt set_template_dir (find_str_attr "template_dir" obj)
  | None -> Error (Error.Expected_type("object", yaml))

let of_file filename =
  match Yaml.of_string @@ Filesystem.read filename with
  | Ok yaml -> of_yaml yaml
  | Error (`Msg s) -> Error (Error.Other s)
  | exception Sys_error _ -> Ok default

let src t = Filename.concat t.input_dir

let dest t = Filename.concat t.output_dir

let template t = Filename.concat t.template_dir
