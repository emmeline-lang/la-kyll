(* Copyright (C) 2019 Types Logics Cats

   This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

let with_in f name =
  let file = open_in name in
  Fun.protect (fun () -> f file) ~finally:(fun () -> close_in file)

let with_out f name =
  let file = open_out name in
  Fun.protect (fun () -> f file) ~finally:(fun () -> close_out file)

let read_file file =
  let approx_size = in_channel_length file in
  let buf = Buffer.create approx_size in
  let rec loop () =
    match input_char file |> Buffer.add_char buf with
    | exception End_of_file -> ()
    | () -> loop ()
  in
  loop ();
  Buffer.contents buf

let read name = with_in read_file name

let mkdir_opt name =
  if not (Sys.file_exists name) then
    Unix.mkdir name 0o777

let fold_dir f start dirname =
  let iterator = Unix.opendir dirname in
  Fun.protect (fun () ->
      let rec loop acc =
        match Unix.readdir iterator with
        | exception End_of_file -> acc
        | exception r -> raise r
        | "." | ".." -> loop acc
        | name -> loop (f acc name)
      in loop start
    ) ~finally:(fun () -> Unix.closedir iterator)

let rec rmdir dirname =
  fold_dir (fun () name ->
      let path = Filename.concat dirname name in
      if Sys.is_directory path then
        rmdir path
      else
        Unix.unlink path
    ) () dirname;
  Unix.rmdir dirname

let rm name =
  if Sys.is_directory name then
    rmdir name
  else
    Unix.unlink name
