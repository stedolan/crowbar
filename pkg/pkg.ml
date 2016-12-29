#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () = Pkg.describe "crowbar" (fun c ->
  Ok [Pkg.mllib "src/crowbar.mllib"])
