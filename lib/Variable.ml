(* $Id: 985c165fe584e6a07b50a0a3c278d1858d97903d $ *)


(*
   Program Variables.
 *)


type t = string
let print = Format.pp_print_string
let compare = compare
let equal = (=)
let hash = Hashtbl.hash
