(* Copyright (C) 2019 Types Logics Cats

   This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

module E = Emmeline

module type Config = sig
  val t : Config.t
end

type 'a value = 'a constraint 'a = [>
  | `Box of 'a E.Eval.box
  | `Char of char
  | `Document of 'a Document.t
  | `Float of float
  | `Foreign of 'a E.Eval.foreign
  | `Partial_app of 'a E.Eval.partial_app
  | `Int of int
  | `Ref of 'a value ref
  | `String of string
  | `Template of Mustache.t
  | `Uninitialized
  | `Unit
  ]

module type S = sig
  val add_get_template : 'a value E.Eval.t -> unit
  val add_render : 'a value E.Eval.t -> unit
  val add_write_doc : 'a value E.Eval.t -> unit
  val init : 'a value E.Eval.t -> unit
end

module Make (C : Config) : S = struct
  let config = C.t

  let add_get_template t =
    let open Util.Result in
    E.Eval.add_foreign_fun t "get_template"
      (E.Eval.foreign ~arity:1 (function
           | [|`String s|] ->
              begin match
                let path = Filename.concat config.Config.template_dir s in
                let+ doc = Document.parse path in
                Document.map (fun s -> `Template (Mustache.of_string s)) doc
              with
              | Ok document -> `Document document
              | Error _ -> failwith "YAML parse error"
              end
           | _ -> failwith "Type error"))

  let add_render t =
    let open Util.Result in
    E.Eval.add_foreign_fun t "render"
      (E.Eval.foreign ~arity:2 (function
           | [|`Template template; `Document document|] ->
              begin match document.Document.content with
              | `String s ->
                 begin match
                   let+ yaml = Yaml_aux.expect_obj document.yaml in
                   Mustache.render template
                     (`O (("content", `String s) :: yaml))
                 with
                 | Ok s -> `String s
                 | Error _ -> failwith "Render error"
                 end
              | _ -> failwith "Type error"
              end
           | _ -> failwith "Type error"))

  let add_write_doc t =
    E.Eval.add_foreign_fun t "write_doc"
      (E.Eval.foreign ~arity:1 (function
           | [|`Document d|] ->
              begin match d.Document.content with
              | `String s ->
                 let path =
                   Filename.concat config.Config.output_dir d.Document.name
                 in
                 Filesystem.with_out (fun file ->
                     output_string file s;
                     flush file
                   ) path;
                 `Unit
              | _ -> failwith "Type error"
              end
             | _ -> failwith "Type error"))

  let init vm =
    add_get_template vm;
    add_render vm;
    add_write_doc vm
end

let create config =
  (module (Make(struct let t = config end)) : S)
