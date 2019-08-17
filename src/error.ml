(* Copyright (C) 2019 Types Logics Cats

   This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

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

let of_option e = function
  | Some x -> Ok x
  | None -> Error e
