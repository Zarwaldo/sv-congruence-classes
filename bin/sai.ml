(* $Id: 6fd1305428110ffa974cbc4a0daa162dbc49ae2f $ *)


(*
   Main (start of the sai program).
 *)


(* Exception for errors that are local to this module (should not happen). *)
exception Internal_error

(* Module to parse the command-line and store user-specified options. *)
module CommandLine =
struct
  (* Input format. *)
  let format_opt = ref "auto"

  (* Direction of the analysis.  Either "fwd" or "bwd". *)
  let analysis_opt = ref "fwd"

  (* Abstract domain.  Either "constant" or "sign" or "interval" or "congruence". *)
  let domain_opt = ref "congruence"

  (* Widening delay.  Widening is disabled when negative. *)
  let widening_delay_opt = ref 0

  (* Narrowing delay.  Narrowing is disabled when negative. *)
  let narrowing_delay_opt = ref 0

  (* Verbosity switch. *)
  let verbosity_opt = ref false

  (* Specification of command-line options. *)
  let arg_spec_list = [
    ("-format",
     Arg.Symbol
       (["auto" ; "aut" ; "prg"], fun s -> format_opt := s),
     " input format" ^
       " (default: " ^ !format_opt ^ ")")
    ;
    ("-analysis",
     Arg.Symbol
       (["fwd" ; "bwd"], fun s -> analysis_opt := s),
     " direction of the analysis" ^
       " (default: " ^ !analysis_opt ^ ")")
    ;
    ("-domain",
     Arg.Symbol
       (["constant" ; "sign" ; "interval" ; "congruence"],
        fun s -> domain_opt := s),
     " abstract domain" ^
       " (default: " ^ !domain_opt ^ ")")
    ;
    ("-widening-delay",
     Arg.Set_int widening_delay_opt,
     "<int> widening delay; a negative value disables widening" ^
       " (default: " ^ (string_of_int !widening_delay_opt) ^ ")")
    ;
    ("-narrowing-delay",
     Arg.Set_int narrowing_delay_opt,
     "<int> narrowing delay; a negative value disables narrowing" ^
       " (default: " ^ (string_of_int !narrowing_delay_opt) ^ ")")
    ;
    ("-v",
     Arg.Set verbosity_opt,
     " make the fixpoint engine verbose" ^
       " (default: " ^ (string_of_bool !verbosity_opt) ^ ")")
  ]

  let usage_msg = "Usage: " ^ (Sys.argv.(0)) ^ " [option ...] [source-file]\n"

  (* Parses the command line and returns the input file's name, if any. *)
  let parse () =
    let filename = ref None
    in
    Arg.parse
      (Arg.align arg_spec_list)
      (fun a ->
       match !filename with
       | None ->
          filename := Some a
       | Some _ ->
          raise (Arg.Bad ("unexpected argument `" ^ a ^
                            "' (multiple input files are not allowed)")))
      usage_msg ;
    !filename
end

(* Default number of columns for pretty-printing. *)
let () =
  try
    Format.set_margin (int_of_string (Sys.getenv "COLUMNS"))
  with
    Not_found | Failure _ -> Format.set_margin 78

(* Parse the command line. *)
let filename = CommandLine.parse ()

(* Desired format. *)
let format =
  match !CommandLine.format_opt with
  | "aut" -> Automaton.Aut
  | "prg" -> Automaton.Prg
  | "auto" ->
     begin
       let (fn, ext) =
         match filename with
         | None -> ("<stdin>", "")
         | Some f -> (f, Filename.extension f)
       in
       match ext with
       | ".aut" -> Automaton.Aut
       | ".c" | ".prg" -> Automaton.Prg
       | _ ->
          let msg = "unable to detect the input format; use option -format"
          in
          Format.eprintf "@[%s:@ %s@]@." fn msg ;
          exit 1
     end
  | _ -> raise Internal_error

(* Parse the input file. *)
let automaton =
  let (ic, fn) =
    match filename with
    | None -> (stdin, "<stdin>")
    | Some f -> (open_in f, f)
  in
  let a =
    try
      Automaton.read format ic
    with IO.Read_error msg ->
      Format.eprintf "@[%s:@ %s@]@." fn msg ;
      close_in ic ;
      exit 1
  in
  close_in ic ;
  a

(* Create the abstract domain. *)
let abstract_domain : (module AbstractDomain.S) =
  match !CommandLine.domain_opt with
  | "constant"   -> (module PointwiseLifting.Make (DomConstant))
  | "sign"       -> (module PointwiseLifting.Make (DomSign))
  | "interval"   -> (module PointwiseLifting.Make (DomInterval))
  | "congruence" -> (module PointwiseLifting.Make (DomCongruence))
  | _ -> raise Internal_error

module A = (val abstract_domain : AbstractDomain.S)

(* Instantiate the fixpoint engine. *)
module F = RoundRobin.Make (Automaton) (A)

(* Account for the user-specified direction of the analysis. *)
let (automaton, transfer, transfer_name) =
  match !CommandLine.analysis_opt with
  | "fwd" -> (automaton, A.post, "post")
  | "bwd" -> (Automaton.reverse automaton, A.pre, "pre")
  | _ -> raise Internal_error

(* Print the automaton. *)
let () =
  Format.printf "@[%a@]@." Automaton.print automaton

(* Print the selected analysis options. *)
let () =
  Format.printf "@.@[Abstract domain: %s@]@." !CommandLine.domain_opt ;
  Format.printf "@[Transfer function: abstract %s-image@]@." transfer_name

(* Construct the monotonic dataflow problem. *)
let constant loc =
  if Automaton.Node.equal loc (Automaton.initial automaton) then
    A.top
  else
    A.bot

let problem =
  { F.graph = automaton ;
    F.transfer = transfer ;
    F.constant = constant }

(* Define the fixpoint engine's control parameters. *)
let param =
  { F.widening = A.widen ;
    F.widening_delay = !CommandLine.widening_delay_opt ;
    F.narrowing = A.narrow ;
    F.narrowing_delay = !CommandLine.narrowing_delay_opt ;
    F.verbose = !CommandLine.verbosity_opt }

(* Perform the analysis: compute a conservative solution. *)
let tbl = F.Table.create (Automaton.nodes automaton) A.bot
let () = F.solve problem param tbl

(* Display the solution. *)
let () =
  Format.printf
    "@.@[<v 3>Conservative solution:@,%a@]@." F.Table.print tbl

(* Perform the analysis: refine the solution. *)
let () = F.refine problem param tbl

(* Display the solution. *)
let () =
  Format.printf
    "@.@[<v 3>Refined solution:@,%a@]@." F.Table.print tbl

(* Display the reachability status. *)
let () =
  let status =
    if A.empty (F.Table.get tbl (Automaton.final automaton)) then
      "No"
    else
      "Unknown"
  in
  Format.printf
    "@.@[Reachability of final location %a from initial location %a: %s@]@."
    Automaton.Node.print (Automaton.final automaton)
    Automaton.Node.print (Automaton.initial automaton)
    status

(* Exit. *)
let () = exit 0
