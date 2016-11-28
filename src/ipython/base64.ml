(*
 * Copyright (c) 2006-2009 Citrix Systems Inc.
 * Copyright (c) 2010 Thomas Gazagnaire <thomas@gazagnaire.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)


(* taken from https://github.com/avsm/ocaml-cohttp/blob/master/cohttp/base64.ml *)

let code = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
let padding = '='

let of_char x = if x = padding then 0 else String.index code x

let to_char x = code.[x]

let decode x : string = 
  let words = String.length x / 4 in
  let padding = 
    if String.length x = 0 then 0 else (
    if x.[String.length x - 2] = padding
    then 2 else (if x.[String.length x - 1] = padding then 1 else 0))
  in
  let output = Bytes.make (words * 3 - padding) '\000' in
  for i = 0 to words - 1 do
    let a = of_char x.[4 * i + 0]
    and b = of_char x.[4 * i + 1]
    and c = of_char x.[4 * i + 2]
    and d = of_char x.[4 * i + 3] in
    let n = (a lsl 18) lor (b lsl 12) lor (c lsl 6) lor d in
    let x = (n lsr 16) land 255
    and y = (n lsr 8) land 255
    and z = n land 255 in
    output.[3 * i + 0] <- char_of_int x;
    if i <> words - 1 || padding < 2 then output.[3 * i + 1] <- char_of_int y;
    if i <> words - 1 || padding < 1 then output.[3 * i + 2] <- char_of_int z;
  done;
  Bytes.to_string output

let encode x : string = 
  let length = String.length x in
  let words = (length + 2) / 3 in (* rounded up *)
  let padding = if length mod 3 = 0 then 0 else 3 - (length mod 3) in
  let output = Bytes.make (words * 4) '\000' in
  let get i = if i >= length then 0 else int_of_char x.[i] in
  for i = 0 to words - 1 do
    let x = get (3 * i + 0)
    and y = get (3 * i + 1)
    and z = get (3 * i + 2) in
    let n = (x lsl 16) lor (y lsl 8) lor z in 
    let a = (n lsr 18) land 63
    and b = (n lsr 12) land 63
    and c = (n lsr 6) land 63
    and d = n land 63 in
    output.[4 * i + 0] <- to_char a;
    output.[4 * i + 1] <- to_char b;
    output.[4 * i + 2] <- to_char c;
    output.[4 * i + 3] <- to_char d;
  done;
  for i = 1 to padding do
    output.[Bytes.length output - i] <- '=';
  done;
  Bytes.to_string output

