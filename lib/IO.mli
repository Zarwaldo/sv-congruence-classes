(* $Id: 25a6d350d3b30c81fe2da725a136bf196f78d152 $ *)


(**
   I/O Helper.

   This module provides helper functions for input and output.
 *)


exception Read_error of string

(**
   [read_error lexbuf arg err] raises [Read_error msg] where [msg] contains a
   description of the error [err].  The optional argument [arg] defaults to
   [Lexing.lexeme lexbuf].
 *)
val read_error : Lexing.lexbuf -> ?arg:string -> string -> 'a

(**
   [in_channel_of_string str] returns an input channel whose content is supplied
   by the string [str].
 *)
val in_channel_of_string : string -> in_channel
