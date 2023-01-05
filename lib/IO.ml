(* $Id: fc5bb6367f1443fc7b126a7f494d8d1bd287df22 $ *)


(*
   I/O Helper.
 *)


let range_of_lexbuf lexbuf =
  (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)

(* Helper function to format read errors. *)
let print_range fmt (startp, endp) =
  Format.fprintf
    fmt "line@ %d,@ char@ %d-%d"
    startp.Lexing.pos_lnum
    (startp.Lexing.pos_cnum - startp.Lexing.pos_bol)
    (endp.Lexing.pos_cnum - endp.Lexing.pos_bol)

exception Read_error of string

let read_error lexbuf ?(arg = Lexing.lexeme lexbuf) err =
  let msg =
    Format.asprintf
      "@[%a:@ %s:@ `%s'@]"
      print_range (range_of_lexbuf lexbuf) err arg
  in
  raise (Read_error msg)

let in_channel_of_string str =
  let (ifd, ofd) = Unix.pipe ~cloexec:true ()
  in
  let ic = Unix.in_channel_of_descr ifd
  and oc = Unix.out_channel_of_descr ofd
  in
  output_string oc str ;
  close_out oc ;
  ic
