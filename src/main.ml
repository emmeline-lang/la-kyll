(* Copyright (C) 2019 Types Logics Cats

   This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

module E = Emmeline

let main_prefix = { E.Qual_id.Prefix.package = "main"; path = [] }

let annot_emmeline_result name = function
  | Ok x -> Ok x
  | Error e -> Error (name, Error.Emmeline_err e)

let () =
  let open Util.Result in
  match
    let* config =
      Util.annot_result "config.yml" (Config.of_file "config.yml")
    in

    let module M = (val (Dsl.create config) : Dsl.S) in
    let packages = E.Pipeline.create_hashtbl () in
    let ctx = E.Eval.create (E.Pipeline.create_hashtbl ()) in
    E.Io.init E.Io.stdio ctx;
    M.init ctx;
    E.Pipeline.create_std packages ctx;
    E.Pipeline.make_module packages
      { E.Qual_id.Prefix.package = "la-kyll"; path = ["Command"] }
      ctx Dsl.source_code;


    let+ file =
      annot_emmeline_result "site.ml"
        (E.Parser.file E.Lexer.expr (Lexing.from_channel (open_in "site.ml"))
         |> E.Pipeline.compile packages main_prefix)
    in
    if Sys.file_exists config.output_dir then
      Filesystem.rm config.output_dir;
    Filesystem.mkdir_opt config.output_dir;

    ignore (E.Eval.eval ctx file)
  with
  | Ok () -> ()
  | Error (where, e) -> print_endline @@ Error.show_error where e
