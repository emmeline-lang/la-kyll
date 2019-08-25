(* Copyright (C) 2019 Types Logics Cats

   This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

module Result = struct
  include Stdlib.Result

  let (let+) res f = map f res

  let (let*) = bind

  let (>>=) = bind
end

let annot_result name = function
  | Ok x -> Ok x
  | Error e -> Error (name, e)
