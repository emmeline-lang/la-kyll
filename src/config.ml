(* Copyright (C) 2019 Types Logics Cats

   This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

type t = {
    output_dir : string;
    template_dir : string;
    routes : (Re.re * string list) list;
  }

let default =
  { output_dir = "public"
  ; template_dir = "templates"
  ; routes = [] }

let set_routes routes t =
  { t with routes }

let set_output_dir output_dir t =
  { t with output_dir }

let set_template_dir template_dir t =
  { t with template_dir }

let set_opt f opt t = match opt with
  | Ok x -> Ok (f x t)
  | Error (Error.Missing_key _) -> Ok t
  | Error e -> Error e

let read_commands =
  let open Yaml_aux in
  let (>>=) = Stdlib.Result.bind in
  let (>>|) x f = Stdlib.Result.map f x in
  List.fold_left (fun acc (regex, commands) ->
      acc >>= fun list ->
      commands
      |> expect_arr
      >>= expect_typed_arr expect_str
      >>| fun commands ->
      (Re.compile @@ Re.Glob.glob regex, commands) :: list
    ) (Ok [])

let of_yaml yaml =
  let open Yaml_aux in
  let (>>=) = Stdlib.Result.bind in
  match get_obj yaml with
  | Some obj ->
     Ok default
     >>= set_opt set_output_dir (find_str_attr "output_dir" obj)
     >>= set_opt set_template_dir (find_str_attr "template_dir" obj)
     >>= set_opt set_routes
           (expect_attr "routes" obj >>= expect_obj >>= read_commands)
  | None -> Error (Error.Expected_type("object", yaml))

let of_file filename =
  match Yaml.of_string @@ Filesystem.read filename with
  | Ok yaml -> of_yaml yaml
  | Error (`Msg s) -> Error (Error.Other s)
  | exception Sys_error _ -> Ok default
