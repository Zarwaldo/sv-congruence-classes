(* $Id: b131c3b3b56145a9bd63e1528cd04612655d697e $ *)


(*
   Unit tests for the PointwiseLifting module (instantiated with DomSign).
 *)


open TestCore

(* Create the pointwise lifting of the sign domain. *)
module PL = PointwiseLifting.Make (DomSign)
open PL

(* Import utility functions. *)
module TU = TestUtil.OfAbstractDomain (PL)
open TU
open TestUtil.CommandShortcut

module Make () =
struct
let test_print_bot_top () =
  assert_print ~msg:"print bot" "{⊥}" print bot ;
  assert_print ~msg:"print top" "{⊤}" print top

let test_post_bot () =
  (* Shortcuts. *)
  let sequence = sequence post "post" bot
  and x = var "x"
  and y = var "y"
  in
  sequence
    [x := cst (-3) ; x := cst 0 ; x := cst 5 ; y := cst (-7) ; y := cst 0]
    ["{⊥}" ; "{⊥}" ; "{⊥}" ; "{⊥}" ; "{⊥}"] ;
  sequence
    [x := y ; x := x + y ; x := x - y ; x := x * y ; x := x / y]
    ["{⊥}" ; "{⊥}" ; "{⊥}" ; "{⊥}" ; "{⊥}"] ;
  sequence
    [x == cst (-3) ; x <= cst 0 ; x >= cst 5 ; y <= cst 0 ; y > cst 7]
    ["{⊥}" ; "{⊥}" ; "{⊥}" ; "{⊥}" ; "{⊥}"] ;
  sequence
    [x != y ; x <= y ; x > x - y ; y <= x * y ; x / y > cst 0]
    ["{⊥}" ; "{⊥}" ; "{⊥}" ; "{⊥}" ; "{⊥}"]

let test_post_assign_cst () =
  (* Shortcuts. *)
  let sequence = sequence post "post" top
  and x = var "x"
  and y = var "y"
  in
  sequence
    [x := cst (-3) ; x := cst 0]
    ["{⊤, x ↦ -}" ; "{⊤, x ↦ 0}"] ;
  sequence
    [x := cst 0 ; y := cst 0 ; x := cst 5]
    ["{⊤, x ↦ 0}" ; "{⊤, x ↦ 0, y ↦ 0}" ; "{⊤, x ↦ +, y ↦ 0}"] ;
  sequence
    [x := cst (-3) ; y := cst 5 ; x := cst 0]
    ["{⊤, x ↦ -}" ; "{⊤, x ↦ -, y ↦ +}" ; "{⊤, x ↦ 0, y ↦ +}"]

let test_post_assign_exp () =
  (* Shortcuts. *)
  let sequence = sequence post "post" top
  and x = var "x"
  and y = var "y"
  in
  sequence
    [x := cst (-3) ; y := cst 0 ;
     x := x + y ; x := x - y ; x := x * y ; x := x / y]
    ["{⊤, x ↦ -}" ; "{⊤, x ↦ -, y ↦ 0}" ;
     "{⊤, x ↦ -, y ↦ 0}" ; "{⊤, x ↦ -, y ↦ 0}" ; "{⊤, x ↦ 0, y ↦ 0}" ; "{⊥}"] ;
  sequence
    [x := cst 2 ; y := cst 5 ; x := var "z" ; x := (cst (-5)) - y]
    ["{⊤, x ↦ +}" ; "{⊤, x ↦ +, y ↦ +}" ; "{⊤, y ↦ +}" ; "{⊤, x ↦ -, y ↦ +}"] ;
  sequence
    [x := cst (-3) ; y := cst 5 ; x := x + y ; x := x * y]
    ["{⊤, x ↦ -}" ; "{⊤, x ↦ -, y ↦ +}" ; "{⊤, y ↦ +}" ; "{⊤, y ↦ +}"] ;
  sequence
    [y := cst (-3) ; x := y ;
     x := x + y ; x := x * y ; x := x / y]
    ["{⊤, y ↦ -}" ; "{⊤, x ↦ -, y ↦ -}" ;
     "{⊤, x ↦ -, y ↦ -}" ; "{⊤, x ↦ +, y ↦ -}" ; "{⊤, x ↦ -0, y ↦ -}"]

let test_post_guard_cst () =
  (* Shortcuts. *)
  let sequence = sequence post "post" top
  and x = var "x"
  and y = var "y"
  in
  sequence
    [cst 3 == cst 7 ; (cst 3) - (cst 3) == cst 1 ; cst 0 == cst 1]
    ["{⊤}" ; "{⊤}" ; "{⊥}"] ;
  sequence
    [x == cst (-3) ; x <= cst 0]
    ["{⊤, x ↦ -}" ; "{⊤, x ↦ -}"] ;
  sequence
    [x >= cst 0 ; x != cst 0 ; x <= cst 0]
    ["{⊤, x ↦ 0+}" ; "{⊤, x ↦ +}" ; "{⊥}"] ;
  sequence
    [x >= cst (-3) ; x <= cst 0 ; x < cst 0 ; x >= cst 3]
    ["{⊤}" ; "{⊤, x ↦ -0}" ; "{⊤, x ↦ -}" ; "{⊥}"] ;
  sequence
    [x > cst 0 ; y + (cst 4) < cst 0 ; x == cst (-7)]
    ["{⊤, x ↦ +}" ; "{⊤, x ↦ +, y ↦ -}" ; "{⊥}"]

let test_post_guard_exp () =
  (* Shortcuts. *)
  let sequence = sequence post "post" top
  and x = var "x"
  and y = var "y"
  in
  sequence
    [x <= cst 0 ; y >= x ; y >= (cst 4) - x]
    ["{⊤, x ↦ -0}" ; "{⊤, x ↦ -0}" ; "{⊤, x ↦ -0, y ↦ +}"] ;
  sequence
    [y == cst 0 ; x + y == cst 5]
    ["{⊤, y ↦ 0}" ; "{⊤, x ↦ +, y ↦ 0}"] ;
  sequence
    [y != cst 0 ; x + y == cst 0]
    ["{⊤, y ↦ -+}" ; "{⊤, x ↦ -+, y ↦ -+}"] ;
  sequence
    [x == x ; x + (cst 3) == x - (cst 5)]
    ["{⊤}" ; "{⊤, x ↦ -+}"] ;
  sequence
    [(cst 0) * x == cst 0 ; x / (cst 0) == cst 0]
    ["{⊤}" ; "{⊥}"] ;
  sequence
    [y == cst 0 ; x != y ; x * y == cst 5]
    ["{⊤, y ↦ 0}" ; "{⊤, x ↦ -+, y ↦ 0}" ; "{⊥}"] ;
  sequence
    [x >= cst 0 ; x * y >= cst 2 ; x + y < cst 0]
    ["{⊤, x ↦ 0+}" ; "{⊤, x ↦ +, y ↦ +}" ; "{⊥}"] ;
  sequence
    [y >= cst 0 ; x / y >= cst 5 ; x * y >= cst 5 ; x / y < cst 0]
    ["{⊤, y ↦ 0+}" ; "{⊤, y ↦ 0+}" ; "{⊤, x ↦ +, y ↦ +}" ; "{⊥}"]

let test_pre_bot () =
  (* Shortcuts. *)
  let sequence = sequence pre "pre" bot
  and x = var "x"
  and y = var "y"
  in
  sequence
    [x := cst (-3) ; x := cst 0 ; x := cst 5 ; y := cst (-7) ; y := cst 0]
    ["{⊥}" ; "{⊥}" ; "{⊥}" ; "{⊥}" ; "{⊥}"] ;
  sequence
    [x := y ; x := x + y ; x := x - y ; x := x * y ; x := x / y]
    ["{⊥}" ; "{⊥}" ; "{⊥}" ; "{⊥}" ; "{⊥}"] ;
  sequence
    [x == cst (-3) ; x <= cst 0 ; x >= cst 5 ; y <= cst 0 ; y > cst 7]
    ["{⊥}" ; "{⊥}" ; "{⊥}" ; "{⊥}" ; "{⊥}"] ;
  sequence
    [x != y ; x <= y ; x > x - y ; y <= x * y ; x / y > cst 0]
    ["{⊥}" ; "{⊥}" ; "{⊥}" ; "{⊥}" ; "{⊥}"]

let test_pre_cst () =
  (* Shortcuts. *)
  let sequence = sequence pre "pre" top
  and x = var "x"
  and y = var "y"
  in
  sequence
    [x := cst (-3) ; y := cst 0 ; x := cst 5]
    ["{⊤}" ; "{⊤}" ; "{⊤}"] ;
  sequence
    [x == cst 0 ; y > cst 0 ; x := cst 0 ; y := cst (-7)]
    ["{⊤, x ↦ 0}" ; "{⊤, x ↦ 0, y ↦ +}" ; "{⊤, y ↦ +}" ; "{⊥}"] ;
  sequence
    [x <= cst 0 ; x := x + (cst 5) ; x := (cst 2) * x]
    ["{⊤, x ↦ -0}" ; "{⊤, x ↦ -}" ; "{⊤, x ↦ -}"]

let test_pre_exp () =
  (* Shortcuts. *)
  let sequence = sequence pre "pre" top
  and x = var "x"
  and y = var "y"
  in
  sequence
    [y != cst 0 ; y := x ; x >= cst 0]
    ["{⊤, y ↦ -+}" ; "{⊤, x ↦ -+}" ; "{⊤, x ↦ +}"] ;
  sequence
    [x := y ; y == x ; x := x - (cst 2) ; x == x + (cst 3)]
    ["{⊤}" ; "{⊤}" ; "{⊤}" ; "{⊤, x ↦ -+}"] ;
  sequence
    [x == cst 0 ; x := y - (cst 1) ; x > cst 0 ; y := (cst 0) - (x * y)]
    ["{⊤, x ↦ 0}" ; "{⊤, y ↦ +}" ; "{⊤, x ↦ +, y ↦ +}" ; "{⊤, x ↦ +, y ↦ -}"] ;
  sequence
    [x == cst 0 ; x := y + (cst 5) ; x := (cst 2) * x ; y := (cst 2) - y]
    ["{⊤, x ↦ 0}" ; "{⊤, y ↦ -}" ; "{⊤, y ↦ -}" ; "{⊤, y ↦ +}"] ;
  sequence
    [x == cst 1 ; y <= (cst (-2)) / x ; x := y]
    ["{⊤, x ↦ +}" ; "{⊤, x ↦ +, y ↦ -0}" ; "{⊥}"]

(* Define some elements of the pointwise lifting domain (except bot and top). *)
let x = var "x"
and y = var "y"

let x_negative = post (x := cst (-1)) top
and x_zero = post (x := cst 0) top
and x_nonpositive = post (x := (cst (-1)) / (cst 1)) top
and y_zero = post (y := cst 0) top
and y_positive = post (y := cst 1) top
and y_nonnegative = post (y := ((cst 1) / (cst 1))) top

let x_negative_y_zero = post (y := cst 0) x_negative
and x_negative_y_positive = post (y := cst 1) x_negative
and x_negative_y_nonnegative = post (y := ((cst 1) / (cst 1))) x_negative
and x_zero_y_zero = post (y := cst 0) x_zero
and x_zero_y_positive = post (y := cst 1) x_zero
and x_zero_y_nonnegative = post (y := ((cst 1) / (cst 1))) x_zero
and x_nonpositive_y_zero = post (y := cst 0) x_nonpositive
and x_nonpositive_y_positive = post (y := cst 1) x_nonpositive
and x_nonpositive_y_nonnegative = post (y := ((cst 1) / (cst 1))) x_nonpositive

let elements =
  [bot ;
   x_negative_y_zero ; x_negative_y_positive ; x_negative_y_nonnegative ;
   x_zero_y_zero ; x_zero_y_positive ; x_zero_y_nonnegative ;
   x_nonpositive_y_zero ; x_nonpositive_y_positive ; x_nonpositive_y_nonnegative ;
   x_negative ; x_zero ; x_nonpositive ;
   y_zero ; y_positive ; y_nonnegative ;
   top]

let test_print_extra () =
  assert_print ~msg:"print x_nonpositive"         "{⊤, x ↦ -0}"        print x_nonpositive ;
  assert_print ~msg:"print y_zero"                "{⊤, y ↦ 0}"         print y_zero ;
  assert_print ~msg:"print x_negative_y_positive" "{⊤, x ↦ -, y ↦ +}"  print x_negative_y_positive ;
  assert_print ~msg:"print x_zero_y_nonnegative"  "{⊤, x ↦ 0, y ↦ 0+}" print x_zero_y_nonnegative

(* Create the generic lattice test cases. *)
module M =
struct
  module L = PL
  let elements = elements
  let relation =
    [
      (bot, x_negative_y_zero) ;
      (bot, x_negative_y_positive) ;
      (bot, x_zero_y_zero) ;
      (bot, x_zero_y_positive) ;
      (x_negative_y_zero, x_negative_y_nonnegative) ;
      (x_negative_y_zero, x_nonpositive_y_zero) ;
      (x_negative_y_positive, x_negative_y_nonnegative) ;
      (x_negative_y_positive, x_nonpositive_y_positive) ;
      (x_negative_y_nonnegative, x_negative) ;
      (x_negative_y_nonnegative, x_nonpositive_y_nonnegative) ;
      (x_zero_y_zero, x_zero_y_nonnegative) ;
      (x_zero_y_zero, x_nonpositive_y_zero) ;
      (x_zero_y_positive, x_zero_y_nonnegative) ;
      (x_zero_y_positive, x_nonpositive_y_positive) ;
      (x_zero_y_nonnegative, x_zero) ;
      (x_zero_y_nonnegative, x_nonpositive_y_nonnegative) ;
      (x_nonpositive_y_zero, x_nonpositive_y_nonnegative) ;
      (x_nonpositive_y_zero, y_zero) ;
      (x_nonpositive_y_positive, x_nonpositive_y_nonnegative) ;
      (x_nonpositive_y_positive, y_positive) ;
      (x_nonpositive_y_nonnegative, x_nonpositive) ;
      (x_nonpositive_y_nonnegative, y_nonnegative) ;
      (x_negative, x_nonpositive) ;
      (x_zero, x_nonpositive) ;
      (x_nonpositive, top) ;
      (y_zero, y_nonnegative) ;
      (y_positive, y_nonnegative) ;
      (y_nonnegative, top) ;
    ]
end

module T = TestLattice.Make (M)

let test_empty () =
  let aux a =
    assert_bool
      ~msg:(Format.asprintf "@[<h>empty@ %a@]" print a)
      (Stdlib.(==) a bot)
      (empty a)
  in
  List.iter aux elements

(* The widening should coincide with the least upper bound. *)
let test_widen () =
  let aux a b =
    assert_equal ~equal ~print ~msg:(message "widen" a b) (lub a b) (widen a b)
  in
  List.iter (fun a -> List.iter (fun b -> aux a b) elements) elements

(* The narrowing should simply return its second argument. *)
let test_narrow () =
  let aux a b =
    if leq b a then
      assert_equal ~equal ~print ~msg:(message "narrow" a b) b (narrow a b)
  in
  List.iter (fun a -> List.iter (fun b -> aux a b) elements) elements

(* Collection of all tests. *)
let tests prefix =
  [
    prefix ^ ".print.bot+top", test_print_bot_top ;
    prefix ^ ".post.bot", test_post_bot ;
    prefix ^ ".post.assign.cst", test_post_assign_cst ;
    prefix ^ ".post.assign.exp", test_post_assign_exp ;
    prefix ^ ".post.guard.cst", test_post_guard_cst ;
    prefix ^ ".post.guard.exp", test_post_guard_exp ;
    prefix ^ ".pre.bot", test_pre_bot ;
    prefix ^ ".pre.cst", test_pre_cst ;
    prefix ^ ".pre.exp", test_pre_exp ;
    prefix ^ ".print.extra", test_print_extra ;
  ]
  @
  (T.tests prefix)
  @
  [
    prefix ^ ".empty", test_empty ;
    prefix ^ ".widen", test_widen ;
    prefix ^ ".narrow", test_narrow ;
  ]
end

(* This test suite. *)
let suite = ("PointwiseLifting (DomSign)", lazy (let open Make () in tests "pwlifting"))
