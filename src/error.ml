(* Copyright (C) 2019 Types Logics Cats

   This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

module E = Emmeline

type error_kind =
  | Emmeline_err of (Lexing.position * Lexing.position) Emmeline.Message.t
  | Exn of exn
  | Expected_type of string * Yaml.value
  | Missing_key of string
  | Unknown_suffix of string
  | Unknown_template of string
  | Other of string

let show_error_kind e =
  let rec handle_exn = function
    | Fun.Finally_raised e -> handle_exn e
    | _ -> ""
  in
  match e with
  | Emmeline_err e ->
     let pp = E.Prettyprint.create () in
     E.Prettyprint.print_message E.Prettyprint.print_span pp e;
     E.Prettyprint.to_string pp
  | Exn e -> handle_exn e
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
