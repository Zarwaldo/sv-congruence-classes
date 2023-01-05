(* $Id: 26d73d8e348a054171bc117901885ac28969cb73 $ *)


(*
   Automata-based Programs.
 *)


module Location =
struct
  type t = string
  let print = Format.pp_print_string
  let compare = compare
  let equal = (=)
  let hash = Hashtbl.hash
end

module LocMap = Map.Make (Location)

(* The type of program automata. *)
type t = {
  name : string ;
  variables : Variable.t list ;
  locations : Location.t list ;
  initial : Location.t ;
  final : Location.t ;
  succ : ((Command.t * Location.t) list) LocMap.t ;
  pred : ((Command.t * Location.t) list) LocMap.t ;
}

(* Pretty-printer for program automata. *)
let print fmt a =
  let print_succ fmt (cmd, tgt) =
    Format.fprintf
      fmt "@,@[<h>»@ @[%a@ »@;<1 3>%a@]@]"
      Command.print cmd
      Location.print tgt
  in
  Format.fprintf fmt "@[<v>" ;
  Format.fprintf fmt "@[<v 3>Automaton:@,@[%s@]@]@," a.name ;
  Format.fprintf
    fmt "@[<v 3>%d variables:@,@[%a@]@]@,"
    (List.length a.variables)
    (Print.list_printer_from_printer ",@ " Variable.print)
    a.variables ;
  Format.fprintf
    fmt "@[<v 3>%d locations:@,@[%a@]@]@,"
    (List.length a.locations)
    (Print.list_printer_from_printer ",@ " Location.print)
    a.locations ;
  Format.fprintf
    fmt "@[<v 3>Initial location:@,@[%a@]@]@," Location.print a.initial ;
  Format.fprintf
    fmt "@[<v 3>Final location:@,@[%a@]@]@," Location.print a.final ;
  Format.fprintf fmt "@[<v 3>Transitions:" ;
  LocMap.iter
    (fun loc out ->
     Format.fprintf fmt "@,@[<v 3>%a" Location.print loc ;
     List.iter (print_succ fmt) out ;
     Format.fprintf fmt "@]")
    a.succ ;
  Format.fprintf fmt "@]@]"

(* Implementation of the DiGraph.S signature. *)
module Node = Location
module Label = Command
let nodes a = a.locations
let succ a loc = try LocMap.find loc a.succ with Not_found -> []
let pred a loc = try LocMap.find loc a.pred with Not_found -> []

(* Getters. *)
let name a = a.name
let variables a = a.variables
let initial a = a.initial
let final a = a.final

(* Reversal. *)
let reverse a =
  { name = (a.name ^ "~") ;
    variables = a.variables ;
    locations = List.rev a.locations ;
    initial = a.final ;
    final = a.initial ;
    succ = a.pred ;
    pred = a.succ }

(* Helper module to create program automata. *)
module Factory :
sig
  exception Undeclared_variable of Variable.t

  val make : name:string -> vars:string list -> init:string -> final:string ->
    trans:(string * (Command.t * string) list) list -> t
end =
struct
  exception Undeclared_variable of Variable.t

  module VarSet = Set.Make (Variable)
  module LocSet = Set.Make (Location)

  let rec undeclared_variable_in_expression vs =
    function
    | Command.Expression.Cst _ -> ()
    | Command.Expression.Var v ->
       if not (VarSet.mem v vs) then raise (Undeclared_variable v)
    | Command.Expression.Op (e, _, e') ->
       undeclared_variable_in_expression vs e ;
       undeclared_variable_in_expression vs e'

  let undeclared_variable_in_command vs =
    function
    | Command.Assign (v, e) ->
       if not (VarSet.mem v vs) then raise (Undeclared_variable v) ;
       undeclared_variable_in_expression vs e
    | Command.Guard (e, _, e') ->
       undeclared_variable_in_expression vs e ;
       undeclared_variable_in_expression vs e'
    | Command.Skip -> ()

  (* Checks for undeclared variables.  Raises Undeclared_variable if any. *)
  let undeclared_variable vs trans =
    List.iter
      (fun ((*src*) _, out) ->
       List.iter
         (fun (cmd, (*tgt*) _) -> undeclared_variable_in_command vs cmd)
         out)
      trans

  (* Helper function for process_location. *)
  let add_trans locmap src cmd tgt =
    let curr = try LocMap.find src locmap with Not_found -> []
    in
    LocMap.add src ((cmd, tgt) :: curr) locmap

  let process_location (locs, succ, pred) (src, out) =
    let out = List.rev out
    in
    List.fold_left
      (fun (l, s, p) (cmd, tgt) ->
       (LocSet.add tgt l, add_trans s src cmd tgt, add_trans p tgt cmd src))
      (LocSet.add src locs, succ, pred)
      out

  let make ~name ~vars ~init ~final ~trans =
    let vset = List.fold_left (fun vs v -> VarSet.add v vs) VarSet.empty vars
    in
    undeclared_variable vset trans ;
    let (locs, succ, pred) =
      List.fold_left
        process_location
        (LocSet.add final (LocSet.add init LocSet.empty),
         LocMap.empty,
         LocMap.empty)
        trans
    in
    { name = name ;
      variables = vars ;
      locations = LocSet.elements locs ;
      initial = init ;
      final = final ;
      succ = succ ;
      pred = pred }
end

type format =
  | Aut
  | Prg

let read_aut lexbuf =
  let (name, vars, init, final, trans) =
    try
      AutParser.main AutLexer.token lexbuf
    with
    | AutLexer.Error ->
       IO.read_error lexbuf "lexical error"
    | AutParser.Error ->
       IO.read_error lexbuf "syntax error"
  in
  let init = match init with
    | Some x -> x
    | None -> IO.read_error lexbuf "missing declaration" ~arg:"initial"
  and final = match final with
    | Some x -> x
    | None -> IO.read_error lexbuf "missing declaration" ~arg:"final"
  in
  try
    Factory.make ~name ~vars ~init ~final ~trans
  with
  | Factory.Undeclared_variable v ->
     IO.read_error lexbuf "undeclared variable" ~arg:v

let read_prg lexbuf =
  let (name, vars, init, final, trans) =
    try
      PrgParser.main PrgLexer.token lexbuf
    with
    | PrgLexer.Error ->
       IO.read_error lexbuf "lexical error"
    | PrgParser.Error ->
       IO.read_error lexbuf "syntax error"
  in
  try
    Factory.make ~name ~vars ~init ~final ~trans
  with
  | Factory.Undeclared_variable v ->
     IO.read_error lexbuf "undeclared variable" ~arg:v

let read fmt chan =
  let lexbuf = Lexing.from_channel chan
  in
  match fmt with
  | Aut -> read_aut lexbuf
  | Prg -> read_prg lexbuf
