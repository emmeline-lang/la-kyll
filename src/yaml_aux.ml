(* Copyright (C) 2019 Types Logics Cats

   This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

open Error

open Util.Result

let rec find_attr' f = function
  | [] -> None
  | (k, v) :: _ when f k -> Some v
  | _ :: xs -> find_attr' f xs

let find_attr k obj =
  find_attr' (fun x -> x = k) obj

let get_obj = function
  | `O x -> Some x
  | `Null -> Some []
  | _ -> None

let get_arr = function
  | `A x -> Some x
  | `Null -> Some []
  | _ -> None

let expect_typed_arr f arr : ('a list, _) result =
  List.fold_right (fun next acc ->
      acc >>= fun list ->
      Stdlib.Result.map (fun typed -> typed :: list) (f next)
    ) arr (Ok [])

let expect_str = function
  | `String s -> Ok s
  | yaml -> Error (Expected_type("string", yaml))

let expect_arr yaml =
  match get_arr yaml with
  | Some a -> Ok a
  | None -> Error (Expected_type("array", yaml))

let expect_attr k obj =
  match find_attr k obj with
  | Some x -> Ok x
  | None -> Error (Missing_key k)

let expect_obj yaml =
  match get_obj yaml with
  | Some o -> Ok o
  | None -> Error (Expected_type("object", yaml))

let find_str_attr target yaml =
  expect_attr target yaml >>= expect_str
