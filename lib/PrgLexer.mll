(* $Id: a2ef305dd9d76c72cf813f8c641434769850bc6f $ *)

{
  open PrgParser
  exception Error
}

let digit = ['0'-'9']
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '\'' '_']

rule token = parse
  (* Ignore spaces, tabs and carriage returns. *)
  | [' ' '\t' '\r']     { token lexbuf }
  (* Ignore comments. *)
  | "//" ([^'\n']*)     { token lexbuf }
  | "/*"                { comment lexbuf }
  (* Treat pre-processing directives as comments. *)
  | '#' ([^'\n']*)      { token lexbuf }
  (* Update line count on new lines. *)
  | '\n'                { Lexing.new_line lexbuf ; token lexbuf }

  (* Keywords. *)
  | "int"               { TK_VAR }
  | "assert"            { TK_ASSERT }
  | "if"                { TK_IF }
  | "else"              { TK_ELSE }
  | "while"             { TK_WHILE }
  | "for"               { TK_FOR }
  | ";"                 { TK_SEMICOLON }
  | ","                 { TK_COMMA }
  | "="                 { TK_ASSIGN }
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
  | "++"                { TK_INC }
  | "--"                { TK_DEC }
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

and comment = parse
  (* End of comment. *)
  | "*/"                { token lexbuf }
  (* Update line count on new lines. *)
  | '\n'                { Lexing.new_line lexbuf ; comment lexbuf }
  (* Ignore the rest. *)
  | _                   { comment lexbuf }
