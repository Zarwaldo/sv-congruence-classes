(* $Id: a5477e77ea266ed3ce486d7ccfb5e636eec197fa $ *)

{
  open AutParser
  exception Error
}

let digit = ['0'-'9']
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '\'' '_']

rule token = parse
  (* Ignore spaces, tabs and carriage returns. *)
  | [' ' '\t' '\r']     { token lexbuf }
  (* Ignore comments. *)
  | '#' ([^'\n']*)      { token lexbuf }
  (* Update line count on new lines. *)
  | '\n'                { Lexing.new_line lexbuf ; token lexbuf }

  (* Keywords. *)
  | "var"               { TK_VAR }
  | "from"              { TK_FROM }
  | "initial"           { TK_INITIAL }
  | "final"             { TK_FINAL }
  | "|"                 { TK_CHOICE }
  | "-->"               { TK_ARROW }
  | ";"                 { TK_SEMICOLON }
  | ","                 { TK_COMMA }
  | ":="                { TK_ASSIGN }
  | "skip"              { TK_SKIP }
  | "+"                 { TK_ADD }
  | "-"                 { TK_SUB }
  | "*"                 { TK_MUL }
  | "/"                 { TK_DIV }
  | "=="                { TK_EQ }
  | "<"                 { TK_LST }
  | ">"                 { TK_GST }
  | "<="                { TK_LEQ }
  | ">="                { TK_GEQ }
  | "!="                { TK_NEQ }
  | '('                 { TK_LPAREN }
  | ')'                 { TK_RPAREN }
  | '{'                 { TK_LBRACE }
  | '}'                 { TK_RBRACE }

  (* Natural numbers. *)
  | (digit)+ as s       { TK_NAT(try int_of_string s with Failure _ -> raise Error) }

  (* End of file. *)
  | eof                 { TK_EOF }

  (* Strings. *)
  | (alphanum)+ as s    { TK_STR(s) }

  (* Fail if nothing else matched. *)
  | _                   { raise Error }
