(* $Id: 27bc342542df5d72e04ae46319f3746aae505bfc $ *)

%{
  type condition =
    | Guard of Command.Predicate.t
    | NonDet

  type instruction =
    | Assignment of Variable.t * Command.Expression.t
    | IfThenElse of condition * statement * statement option
    | WhileLoop of condition * statement
    | Block of statement list
    | Skip
    | Assertion of condition
   and statement = Lexing.position * instruction

  (* Returns the location of the given position in the source file. *)
  let location_of_position pos =
    let line = pos.Lexing.pos_lnum
    and char = pos.Lexing.pos_cnum - pos.Lexing.pos_bol
    in
    Format.sprintf "l%02d-c%02d" line char

  (* Location for assertion failures. *)
  let assertion_failure_loc = "~assert"

  (*
     process_statement (loc, trans) stmt translates the given statement stmt
     into a program automaton A and returns the program automaton (loc', trans')
     obtained by prepending A to (loc, trans).  So loc' is the start location of
     A and loc is its exit location.

     It is understood that in the context of this function, program automata are
     given by pairs (start location, list of transitions).
   *)
  let rec process_statement (loc, trans) (pos, inst) =
    let startloc = location_of_position pos
    in
    match inst with
    | Assignment (v, e) ->
       (startloc,
        (startloc, [(Command.Assign (v, e), loc)]) :: trans)
    | IfThenElse (cond, stmt_t, stmt_f_opt) ->
       let (cmd_t, cmd_f) =
         match cond with
         | Guard p ->
            (Command.Guard p, Command.Guard (Command.Predicate.negate p))
         | NonDet ->
            (Command.Skip, Command.Skip)
       and (loc_t, trans) = process_statement (loc, trans) stmt_t
       in
       let (loc_f, trans) =
         match stmt_f_opt with
         | None ->
            (loc, trans)
         | Some stmt_f ->
            process_statement (loc, trans) stmt_f
       in
       (startloc,
        (startloc,
         [(cmd_t, loc_t) ;
          (cmd_f, loc_f)]) :: trans)
    | WhileLoop (cond, stmt_t) ->
       let (cmd_t, cmd_f) =
         match cond with
         | Guard p ->
            (Command.Guard p, Command.Guard (Command.Predicate.negate p))
         | NonDet ->
            (Command.Skip, Command.Skip)
       and (loc_t, trans) = process_statement (startloc, trans) stmt_t
       in
       (startloc,
        (startloc,
         [(cmd_t, loc_t) ;
          (cmd_f, loc)]) :: trans)
    | Block stmts ->
       (* The location startloc is not used for blocks. *)
       List.fold_left process_statement (loc, trans) (List.rev stmts)
    | Skip ->
       (startloc,
        (startloc, [(Command.Skip, loc)]) :: trans)
    | Assertion cond ->
       let (cmd_t, cmd_f) =
         match cond with
         | Guard p ->
            (Command.Guard p, Command.Guard (Command.Predicate.negate p))
         | NonDet ->
            (Command.Skip, Command.Skip)
       in
       (startloc,
        (startloc,
         [(cmd_t, loc) ;
          (cmd_f, assertion_failure_loc)]) :: trans)

  let output name vars stmt endpos =
    let endloc = location_of_position endpos
    in
    let (startloc, trans) = process_statement (endloc, []) stmt
    in
    (name, vars, startloc, assertion_failure_loc, trans)
%}

%token TK_VAR
%token TK_ASSERT
%token TK_IF
%token TK_ELSE
%token TK_WHILE
%token TK_FOR
%token TK_SEMICOLON
%token TK_COMMA
%token TK_ASSIGN
%token TK_SKIP
%token TK_ADD
%token TK_SUB
%token TK_MUL
%token TK_DIV
%token TK_EQ
%token TK_LST
%token TK_GST
%token TK_LEQ
%token TK_GEQ
%token TK_NEQ
%token TK_INC
%token TK_DEC
%token TK_LPAREN TK_RPAREN
%token TK_LBRACE TK_RBRACE

%token <int> TK_NAT
%token <string> TK_STR
%token TK_EOF

%nonassoc THEN
%nonassoc TK_ELSE

%left TK_ADD TK_SUB     /* lowest precedence */
%left TK_MUL TK_DIV     /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

(*
   Returns a tuple (name, vars, init, final, trans) where:

   - name is the procedure's name,
   - vars is the list of declared variables,
   - init is the initial location,
   - final is the final location (assertion_failure),
   - trans gives the transitions as a list of pairs (loc, out) where:
     . loc is a location, and
     . out is the list of its outbound transitions.

   The list vars is in the same order as in the input file.
 *)
%start <string * string list * string * string *
          (string * (Command.t * string) list) list> main

%%

main:
| TK_VAR n=TK_STR TK_LPAREN TK_RPAREN
  TK_LBRACE v=list(variables) i=list(statement) TK_RBRACE TK_EOF
  { output n (List.flatten v) ($startpos($5), Block i) $startpos($8) }

variables:
| TK_VAR separated_nonempty_list(TK_COMMA, TK_STR) TK_SEMICOLON
  { $2 }

statement:
| assignments TK_SEMICOLON
  { $1 }
| ifthenelse            { $1 }
| whileloop             { $1 }
| forloop               { $1 }
| block                 { $1 }
| skip                  { $1 }
| assertion             { $1 }

assignments:
| separated_list(TK_COMMA, assignment)
  { $startpos, Block $1 }

assignment:
| TK_STR TK_ASSIGN expression
  { $startpos, Assignment ($1, $3) }
| TK_STR TK_INC
  { $startpos, Assignment ($1,
                           Command.Expression.Op
                             (Command.Expression.Var $1,
                              Command.Expression.Add,
                              Command.Expression.Cst 1)) }
| TK_STR TK_DEC
  { $startpos, Assignment ($1,
                           Command.Expression.Op
                             (Command.Expression.Var $1,
                              Command.Expression.Sub,
                              Command.Expression.Cst 1)) }

ifthenelse:
| TK_IF TK_LPAREN condition TK_RPAREN statement %prec THEN
  { $startpos, IfThenElse ($3, $5, None) }
| TK_IF TK_LPAREN condition TK_RPAREN statement TK_ELSE statement
  { $startpos, IfThenElse ($3, $5, Some $7) }

whileloop:
| TK_WHILE TK_LPAREN condition TK_RPAREN statement
  { $startpos, WhileLoop ($3, $5) }

forloop:
| TK_FOR TK_LPAREN assignments TK_SEMICOLON condition TK_SEMICOLON
  assignments TK_RPAREN statement
  { $startpos, Block [$3 ;
                      $endpos($4), WhileLoop ($5,
                                              ($endpos($8), Block [$9 ; $7]))] }

block:
| TK_LBRACE list(statement) TK_RBRACE
  { $startpos, Block $2 }

skip:
| TK_SKIP TK_SEMICOLON
  { $startpos, Skip }

assertion:
| TK_ASSERT TK_LPAREN condition TK_RPAREN TK_SEMICOLON
  { $startpos, Assertion $3 }

condition:
| expression relop expression
  { Guard ($1, $2, $3) }
| expression
  { Guard ($1, Command.Predicate.Neq, Command.Expression.Cst 0) }
| TK_MUL
  { NonDet }

relop:
| TK_EQ                 { Command.Predicate.Eq }
| TK_LST                { Command.Predicate.Lst }
| TK_GST                { Command.Predicate.Gst }
| TK_LEQ                { Command.Predicate.Leq }
| TK_GEQ                { Command.Predicate.Geq }
| TK_NEQ                { Command.Predicate.Neq }

expression:
| TK_NAT                { Command.Expression.Cst $1 }
| TK_STR                { Command.Expression.Var $1 }
| TK_LPAREN expression TK_RPAREN
  { $2 }
| e=expression o=funop f=expression
  { Command.Expression.Op (e, o, f) }
| TK_SUB expression %prec UMINUS
  { match $2 with
    | Command.Expression.Cst c ->
       Command.Expression.Cst (- c)
    | Command.Expression.Var _
    | Command.Expression.Op (_, _, _) as e ->
       Command.Expression.Op
         (Command.Expression.Cst 0, Command.Expression.Sub, e) }

%inline funop:
| TK_ADD                { Command.Expression.Add }
| TK_SUB                { Command.Expression.Sub }
| TK_MUL                { Command.Expression.Mul }
| TK_DIV                { Command.Expression.Div }
