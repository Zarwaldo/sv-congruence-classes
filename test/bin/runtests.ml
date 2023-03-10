(* $Id: fa26bce908adce1804be6de4e22fbf34ae911e74 $ *)


(*
   Main (start of the runtests program).
 *)


open TestCore

(* List of all test suites.  Add new test suites here. *)
let allsuites =
  [
    Test_DomConstant.suite ;
    Test_DomSign.suite ;
    Test_DomInterval.suite ;
    Test_DomCongruence.suite ;
    Test_PointwiseLifting_Sign.suite ;
    Test_PointwiseLifting_DummySign.suite ;
    (* ... *)
  ]

(* Specification of command-line options. *)
let arg_spec_list = [
  ("-v",
   Arg.Unit (fun () -> incr verbosity),
   " more verbose output")
  ;
  ("-q",
   Arg.Unit (fun () -> decr verbosity),
   " less verbose output")
]

(* Usage message. *)
let usage_msg = "Usage: " ^ (Sys.argv.(0)) ^ " [option ...] [pattern ...]\n"

(* Parse the command line. *)
let patterns =
  let patlist = ref []
  in
  Arg.parse
    (Arg.align arg_spec_list)
    (fun a ->
     let regexp =
       try
         Str.regexp_case_fold a
       with Failure msg ->
         raise (Arg.Bad ("invalid regular expression `" ^ a ^ "': " ^ msg))
     in
     patlist := regexp :: !patlist)
    usage_msg ;
  List.rev !patlist

(* List of test suites whose name matches one of the specified patterns. *)
let suites =
  if patterns = [] then allsuites
  else
    let keep (name, _) =
      List.exists
        (fun p ->
         try
           Str.search_forward p name 0 >= 0
         with Not_found -> false)
        patterns
    in
    List.filter keep allsuites

(* Wrapper around TestCore.run to run a lazy test suite. *)
let run_lazy (name, lazy_tests) =
  run (name, Lazy.force lazy_tests)

(* Execute all test suites. *)
let pass =
  List.fold_left
    (fun b suite -> run_lazy suite && b)
    true
    suites

(* Exit. *)
let _ = exit (if pass then 0 else 1)
