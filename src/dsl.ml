(* Copyright (C) 2019 Types Logics Cats

   This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

module E = Emmeline

module type Config = sig
  val t : Config.t
end

type 'a command =
  | Map of 'a E.Eval.fun_data * 'a command
  | Join of 'a command
  | Return of 'a
  | Apply_template of Mustache.t

type 'a rule =
  | Bin of Re.re * 'a command
  | Text of Re.re * 'a command

type 'a value = 'a constraint 'a = [>
  | `Box of 'a E.Eval.box
  | `Char of char
  | `Command of 'a command
  | `Document of 'a Document.t
  | `Float of float
  | `Foreign of 'a E.Eval.foreign
  | `Partial_app of 'a E.Eval.partial_app
  | `Int of int
  | `Ref of 'a value ref
  | `Rule of 'a rule
  | `String of string
  | `Template of Mustache.t
  | `Uninitialized
  | `Unit
  ]

let rec eval_command
          vm src_file (document : 'a Document.t)
        : 'a value command -> ('a Document.t * 'a) = function
  | Map(f, command) ->
     let document, a = eval_command vm src_file document command in
     ( document
     , E.Eval.apply_function vm src_file f [a])
  | Join command ->
     eval_command vm src_file document command
  | Return a -> (document, a)
  | Apply_template template ->
     begin match document.Document.content with
     | `String s ->
        begin match Yaml_aux.expect_obj document.yaml with
        | Ok yaml ->
           let content =
             `String
               (Mustache.render template (`O (("content", `String s) :: yaml)))
           in { document with content }, `Unit
        | Error _ -> failwith "Malformed yaml"
        end
     | _ -> failwith "Type error, expected string"
     end

let rec eval_list = function
  | `Box { E.Eval.data = [||]; tag = _ } -> []
  | `Box { data = [|`Rule r; xs|]; tag = _ } ->
     let tuple =
       match r with
       | Bin(regex, command) ->
          Filesystem.(read_bin, with_out_bin, regex, command)
       | Text(regex, command) ->
          Filesystem.(read, with_out, regex, command)
     in
     tuple :: eval_list xs
  | _ -> failwith "Type error, expected list"

let rec eval_final vm src_file config filename = function
  | [] -> print_endline filename
  | (read, write, regex, command) :: xs ->
     if Re.execp regex filename then
       let str = read (Config.src config filename) in
       let doc =
         { Document.name = filename; yaml = `Null; content = `String str }
       in
       let doc, _ = eval_command vm src_file doc command in
       let output_path = Filename.concat config.Config.output_dir doc.name in
       write (fun file ->
           output_string file str;
           flush file
         ) output_path
     else
       eval_final vm src_file config filename xs

let rec compile_dir vm src_file config templates subdir list =
  let src = Config.src config subdir in
  let dest = Config.dest config subdir in
  Filesystem.mkdir_opt dest;
  Filesystem.fold_dir (fun () name ->
      let path = Filename.concat subdir name in
      if Filename.concat config.Config.input_dir path
         = config.Config.output_dir then
        ()
      else if Sys.is_directory (Config.src config path) then
        compile_dir vm src_file config templates path list
      else
        eval_final vm src_file config path list
    ) () src

module type S = sig
  val init : 'a value E.Eval.t -> unit
end

module Make (C : Config) : S = struct
  let config = C.t

  let add_to_template t =
    E.Eval.add_foreign_fun t "to_template"
      (E.Eval.foreign ~arity:1 (fun _ args ->
           match args with
           | [|`String s|] -> `Template (Mustache.of_string s)
           | _ -> failwith "Type error"))

  let add_map t =
    E.Eval.add_foreign_fun t "command_map"
      (E.Eval.foreign ~arity:2 (fun file args ->
           match args with
           | [|f; `Command c|] ->
              let f = E.Eval.to_function file f in
              `Command (Map(f, c))
           | _ -> failwith "Type error"))

  let add_join t =
    E.Eval.add_foreign_fun t "command_join"
      (E.Eval.foreign ~arity:1 (fun _ args ->
           match args with
           | [|`Command c|] ->
              `Command (Join c)
           | _ -> failwith "Type error"))

  let add_return t =
    E.Eval.add_foreign_fun t "command_return"
      (E.Eval.foreign ~arity:1 (fun _ args ->
           match args with
           | [|x|] ->
              `Command (Return x)
           | _ -> failwith "Type error"))

  let add_apply_template t =
    E.Eval.add_foreign_fun t "command_apply_template"
      (E.Eval.foreign ~arity:1 (fun _ args ->
           match args with
           | [|`Template t|] -> `Command (Apply_template t)
             | _ -> failwith "Type error"))

  let add_bin_rule t =
    E.Eval.add_foreign_fun t "command_bin_rule"
      (E.Eval.foreign ~arity:2 (fun _ args ->
           match args with
           | [|`String s; `Command c|] ->
              `Rule (Bin (Re.compile (Re.Glob.glob s), c))
           | _ -> failwith "Type error"))

  let add_text_rule t =
    E.Eval.add_foreign_fun t "command_text_rule"
      (E.Eval.foreign ~arity:2 (fun _ args ->
           match args with
           | [|`String s; `Command c|] ->
              `Rule (Text (Re.compile (Re.Glob.glob s), c))
           | _ -> failwith "Type error"))

  let add_eval t =
    E.Eval.add_foreign_fun t "command_eval"
      (E.Eval.foreign ~arity:1 (fun file args ->
           match args with
           | [|arg|] ->
              let list = eval_list arg in
              compile_dir t file config () "" list;
              `Unit
           | _ -> failwith "Type error"))

  let init vm =
    add_map vm;
    add_join vm;
    add_return vm;
    add_apply_template vm;
    add_to_template vm;
    add_bin_rule vm;
    add_text_rule vm;
    add_eval vm
end

let source_code = {|
export (map, join, return, apply_template, eval, bin_rule, text_rule)

import "std" List as L

type Command a =
type Template =
type Rule a =

let map = foreign "command_map" forall a b . (a -> b) -> Command a -> Command b

let join = foreign "command_return" forall a . Command (Command a) -> Command a

let return = foreign "command_return" forall a . a -> Command a

let apply_template = foreign "command_apply_template"
  forall . Template -> Command Unit

let eval = foreign "command_eval"
  forall . L.List (Rule Unit) -> Unit

let bin_rule = foreign "command_bin_rule"
  forall a . String -> Command a -> Rule a

let text_rule = foreign "command_text_rule"
  forall a . String -> Command a -> Rule a
|}

let create config =
  (module (Make(struct let t = config end)) : S)
