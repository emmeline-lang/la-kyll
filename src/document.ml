(* Copyright (C) 2019 Types Logics Cats

   This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

type 'a t = {
    name : string;
    yaml : Yaml.value;
    content : 'a;
  }

let parse name =
  Filesystem.with_in (fun file ->
      let open Util.Result in
      let lexbuf = Lexing.from_channel file in
      match Frontmatter.main lexbuf with
      | None, content -> Ok { name; yaml = `Null; content }
      | Some yaml, content ->
         let+ yaml = map_error Error.to_error (Yaml.of_string yaml) in
         { name; yaml; content }
    ) name

let map f x = { x with content = f x.content }
