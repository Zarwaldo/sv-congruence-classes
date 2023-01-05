(* $Id: 180da8087cc635a6851d3e300efebbcc800cab65 $ *)


(*
   Unit tests for the DomCongruence module.
 *)


open TestCore
open DomCongruence

(* Import utility functions. *)
module TU = TestUtil.OfNumericalDomain (DomCongruence)
open TU

module Make () =
struct
(* Define some elements of the congruence domain (except bot and top). *)
let m7 = abs (-7)
let m2 = abs (-2)
let zero = abs 0
let p2 = abs 2
let p5 = abs 5
let c5_12 = lub m7 p5
let c1_8 = lub m7 (abs 1)
let c1_4 = lub (abs 1) p5
let c2_9 = lub p2 m7
let c5_6 = lub (abs (-1)) p5
let c2_3 = lub p5 p2
let c0_2 = lub zero p2
let elements =
  [bot ; m7 ; m2 ; zero ; p2 ; p5 ; c5_12 ; c1_8 ; c1_4 ; c2_9 ; c5_6 ; c2_3 ;
   c0_2 ; top]

let test_print () =
  assert_print ~msg:"print bot"   "⊥"         print bot ;
  assert_print ~msg:"print m7"    "-7"        print m7 ;
  assert_print ~msg:"print m2"    "-2"        print m2 ;
  assert_print ~msg:"print zero"  "0"         print zero ;
  assert_print ~msg:"print p2"    "2"         print p2 ;
  assert_print ~msg:"print p5"    "5"         print p5 ;
  assert_print ~msg:"print c5_12" "(5 + 12ℤ)" print c5_12 ;
  assert_print ~msg:"print c1_8"  "(1 + 8ℤ)"  print c1_8 ;
  assert_print ~msg:"print c1_4"  "(1 + 4ℤ)"  print c1_4 ;
  assert_print ~msg:"print c2_9"  "(2 + 9ℤ)"  print c2_9 ;
  assert_print ~msg:"print c5_6"  "(5 + 6ℤ)"  print c5_6 ;
  assert_print ~msg:"print c2_3"  "(2 + 3ℤ)"  print c2_3 ;
  assert_print ~msg:"print c0_2"  "2ℤ"        print c0_2 ;
  assert_print ~msg:"print top"   "ℤ"         print top

let test_abs () =
  assert_equal_t ~msg:"abs -13" "-13" (abs (-13)) ;
  assert_equal_t ~msg:"abs 0"   "0"   (abs 0) ;
  assert_equal_t ~msg:"abs 7"   "7"   (abs 7)

let test_empty () =
  assert_bool ~msg:"empty bot"  true  (empty bot) ;
  assert_bool ~msg:"empty m2"   false (empty m2) ;
  assert_bool ~msg:"empty zero" false (empty zero) ;
  assert_bool ~msg:"empty c2_9" false (empty c2_9) ;
  assert_bool ~msg:"empty c0_2" false (empty c0_2) ;
  assert_bool ~msg:"empty top"  false (empty top)

let test_add () =
  let aux a b expected =
    assert_equal_t ~msg:(message "add" a b) expected (Op.add a b)
  in
  List.iter (fun a -> aux bot a "⊥") elements ;
  aux m7 m7 "-14" ;
  aux m7 p5 "-2" ;
  aux m7 c1_4 "(2 + 4ℤ)" ;
  aux m7 top "ℤ" ;
  aux zero m7 "-7" ;
  aux zero zero "0" ;
  aux zero p5 "5" ;
  aux zero c2_9 "(2 + 9ℤ)" ;
  aux zero top "ℤ" ;
  aux p5 m2 "3" ;
  aux p5 zero "5" ;
  aux p5 c1_8 "(6 + 8ℤ)" ;
  aux c5_12 m7 "(10 + 12ℤ)" ;
  aux c5_12 zero "(5 + 12ℤ)" ;
  aux c5_12 p2 "(7 + 12ℤ)" ;
  aux c5_12 c1_4 "(2 + 4ℤ)" ;
  aux c5_12 c2_9 "(1 + 3ℤ)" ;
  aux c0_2 m7 "(1 + 2ℤ)" ;
  aux c0_2 zero "2ℤ" ;
  aux c0_2 p5 "(1 + 2ℤ)" ;
  aux c0_2 c2_3 "ℤ" ;
  aux c1_8 c5_6 "2ℤ" ;
  aux top p5 "ℤ" ;
  aux top c1_8 "ℤ" ;
  aux top top "ℤ"

let test_sub () =
  let aux a b expected =
    assert_equal_t ~msg:(message "sub" a b) expected (Op.sub a b)
  in
  List.iter (fun a -> aux bot a "⊥") elements ;
  aux m7 m7 "0" ;
  aux m7 p5 "-12" ;
  aux m7 c1_4 "4ℤ" ;
  aux m7 top "ℤ" ;
  aux zero m7 "7" ;
  aux zero zero "0" ;
  aux zero p5 "-5" ;
  aux zero c2_9 "(7 + 9ℤ)" ;
  aux zero top "ℤ" ;
  aux p5 m2 "7" ;
  aux p5 zero "5" ;
  aux p5 c1_8 "(4 + 8ℤ)" ;
  aux c5_12 m7 "12ℤ" ;
  aux c5_12 zero "(5 + 12ℤ)" ;
  aux c5_12 p2 "(3 + 12ℤ)" ;
  aux c5_12 c1_4 "4ℤ" ;
  aux c5_12 c2_9 "3ℤ" ;
  aux c0_2 m7 "(1 + 2ℤ)" ;
  aux c0_2 zero "2ℤ" ;
  aux c0_2 p5 "(1 + 2ℤ)" ;
  aux c0_2 c2_3 "ℤ" ;
  aux c1_8 c5_6 "2ℤ" ;
  aux top p5 "ℤ" ;
  aux top c1_8 "ℤ" ;
  aux top top "ℤ"

(* Additional elements to test multiplication and division. *)
let p3 = abs 3
let c3_6 = lub (abs 3) (abs 9)
let c10_35 = lub (abs 10) (abs 45)

let test_mul () =
  let aux a b expected =
    assert_equal_t ~msg:(message "mul" a b) expected (Op.mul a b)
  in
  List.iter (fun a -> aux bot a "⊥") elements ;
  aux m7 m7 "49" ;
  aux m7 p5 "-35" ;
  aux m7 c1_4 "(21 + 28ℤ)" ;
  aux m7 top "7ℤ" ;
  aux zero m7 "0" ;
  aux zero zero "0" ;
  aux zero p5 "0" ;
  aux zero c2_9 "0" ;
  aux zero top "0" ;
  aux p5 m2 "-10" ;
  aux p5 zero "0" ;
  aux p5 c1_8 "(5 + 40ℤ)" ;
  aux c5_12 m7 "(49 + 84ℤ)" ;
  aux c5_12 zero "0" ;
  aux c5_12 p2 "(10 + 24ℤ)" ;
  aux c5_12 c1_4 "(1 + 4ℤ)" ;
  aux c5_12 c2_9 "(1 + 3ℤ)" ;
  aux c0_2 m7 "14ℤ" ;
  aux c0_2 zero "0" ;
  aux c0_2 p5 "10ℤ" ;
  aux c0_2 c2_3 "2ℤ" ;
  aux c1_8 c5_6 "(1 + 2ℤ)" ;
  aux top p5 "5ℤ" ;
  aux top c1_8 "ℤ" ;
  aux top c3_6 "3ℤ" ;
  aux top c10_35 "5ℤ" ;
  aux top top "ℤ"

let test_div () =
  let aux a b expected =
    assert_equal_t ~msg:(message "div" a b) expected (Op.div a b)
  in
  List.iter (fun a -> aux bot a "⊥") elements ;
  aux m7 m7 "1" ;
  aux m7 p5 "-1" ;
  aux m7 c1_4 "ℤ" ;
  aux m7 c3_6 "2ℤ" ;
  aux m7 c10_35 "0" ;
  aux m7 top "ℤ" ;
  aux zero m7 "0" ;
  aux zero zero "⊥" ;
  aux zero p5 "0" ;
  aux zero c2_9 "0" ;
  aux zero top "0" ;
  aux p5 m2 "-2" ;
  aux p5 zero "⊥" ;
  aux p5 c1_8 "5ℤ" ;
  aux p5 c3_6 "ℤ" ;
  aux p5 c10_35 "0" ;
  aux c5_12 m7 "ℤ" ;
  aux c5_12 zero "⊥" ;
  aux c5_12 p2 "ℤ" ;
  aux c5_12 c1_4 "ℤ" ;
  aux c5_12 c2_9 "ℤ" ;
  aux c0_2 m7 "ℤ" ;
  aux c0_2 zero "⊥" ;
  aux c0_2 p5 "ℤ" ;
  aux c0_2 c2_3 "ℤ" ;
  aux c3_6 p2 "ℤ" ;
  aux c3_6 p3 "(1 + 2ℤ)" ;
  aux c10_35 p5 "(2 + 7ℤ)" ;
  aux c10_35 m7 "ℤ" ;
  aux top p5 "ℤ" ;
  aux top c1_8 "ℤ" ;
  aux top top "ℤ"

let test_equality () =
  let aux a b c expected =
    assert_equal_t ~msg:(message' "equality" a b c) expected (Op.equality a b c)
  and aux' a b c expected =
    assert_equal
      ~equal ~print
      ~msg:(message' "equality" a b c) expected (Op.equality a b c)
  in
  List.iter (fun a -> aux bot top a "⊥" ; aux bot a top "⊥") elements ;
  List.iter (fun a -> aux top bot a "⊥" ; aux a bot top "⊥") elements ;
  List.iter (fun a -> aux top a bot "⊥" ; aux a top bot "⊥") elements ;
  List.iter (fun a -> if not (empty a) then aux a top top "ℤ") elements ;
  List.iter (fun a -> aux' top top a a) elements ;
  aux m7 m7 top "-1" ;
  aux m7 zero top "0" ;
  aux m7 p5 top "⊥" ;
  aux m7 c1_4 top "(3 + 4ℤ)" ;
  aux m7 c5_12 top "(11 + 12ℤ)" ;
  aux m7 c0_2 top "2ℤ" ;
  aux m7 c10_35 top "⊥" ;
  aux zero m7 top "⊥" ;
  aux zero zero top "ℤ" ;
  aux zero p5 top "⊥" ;
  aux zero c2_9 top "⊥" ;
  aux zero c0_2 top "ℤ" ;
  aux p5 m2 top "⊥" ;
  aux p5 zero top "0" ;
  aux p5 c1_8 top "(3 + 8ℤ)" ;
  aux p5 c3_6 top "(3 + 6ℤ)" ;
  aux p5 c10_35 top "(5 + 7ℤ)" ;
  aux c5_12 m7 top "(11 + 12ℤ)" ;
  aux c5_12 zero top "12ℤ" ;            (* Ideally 0 *)
  aux c5_12 p2 top "(2 + 12ℤ)" ;        (* Ideally ⊥ *)
  aux c5_12 c1_4 top "(3 + 4ℤ)" ;
  aux c5_12 c2_9 top "(2 + 3ℤ)" ;
  aux c0_2 m7 top "⊥" ;
  aux c0_2 zero top "ℤ" ;
  aux c0_2 p5 top "⊥" ;
  aux c0_2 c2_3 top "ℤ" ;
  aux c2_3 p5 top "(2 + 3ℤ)" ;          (* Ideally 5 + 6ℤ = ∨ {-5, 1} *)
  aux c2_9 m2 top "(1 + 9ℤ)" ;          (* Ideally 1 *)
  aux c3_6 p2 top "⊥" ;
  aux c3_6 p3 top "(1 + 2ℤ)" ;
  aux c10_35 p5 top "(3 + 7ℤ)" ;        (* Ideally ⊥ *)
  aux c10_35 m7 top "⊥" ;
  aux top m2 p2 "2" ;
  aux top p2 m2 "-2" ;
  aux top p5 top "ℤ" ;                  (* Ideally 1 + 2ℤ = ∨ {-5, -1, 1, 5} *)
  aux top c1_8 top "ℤ" ;                (* Ideally 1 + 2ℤ *)
  aux c5_12 zero zero "0" ;
  aux p3 c3_6 p5 "5" ;
  (*
     Examples where (equality a b c) is more precise than (equality a b top)
     intersected with c.
   *)
  aux c2_3 p5 m7 "⊥" ;
  aux top p5 p2 "⊥"

let test_inequality () =
  let aux a b c expected =
    assert_equal_t
      ~msg:(message' "inequality" a b c) expected (Op.inequality a b c)
  and aux' a b c expected =
    assert_equal
      ~equal ~print
      ~msg:(message' "inequality" a b c) expected (Op.inequality a b c)
  in
  List.iter (fun a -> aux bot top a "⊥" ; aux bot a top "⊥") elements ;
  List.iter (fun a -> aux top bot a "⊥" ; aux a bot top "⊥") elements ;
  List.iter (fun a -> aux top a bot "⊥" ; aux a top bot "⊥") elements ;
  List.iter (fun a -> if not (empty a) then aux a top top "ℤ") elements ;
  List.iter (fun a -> aux' top top a a) elements ;
  aux m7 m2 zero "0" ;
  aux m7 m2 p2 "2" ;
  aux m7 m2 c5_12 "(5 + 12ℤ)" ;
  aux m7 m2 top "ℤ" ;
  aux m7 p5 zero "⊥" ;
  aux m7 p5 p2 "2" ;
  aux m7 p5 c5_12 "(5 + 12ℤ)" ;
  aux m7 p5 top "ℤ" ;
  aux zero zero zero "0" ;
  aux zero zero p2 "2" ;
  aux zero zero c5_12 "(5 + 12ℤ)" ;
  aux zero zero top "ℤ" ;
  aux zero p5 zero "⊥" ;
  aux zero p5 p2 "⊥" ;
  aux zero p5 c5_12 "⊥" ;
  aux zero p5 top "⊥" ;
  aux p5 m2 zero "0" ;
  aux p5 m2 p2 "⊥" ;
  aux p5 m2 c5_12 "(5 + 12ℤ)" ;
  aux p5 m2 top "ℤ" ;
  aux p5 p5 zero "⊥" ;
  aux p5 p5 p2 "⊥" ;
  aux p5 p5 c5_12 "(5 + 12ℤ)" ;
  aux p5 p5 top "ℤ" ;
  aux c2_9 m2 zero "0" ;
  aux c2_9 m2 p2 "2" ;
  aux c2_9 m2 c5_12 "(5 + 12ℤ)" ;
  aux c2_9 m2 top "ℤ" ;
  aux c2_9 p5 zero "⊥" ;
  aux c2_9 p5 p2 "2" ;
  aux c2_9 p5 c5_12 "(5 + 12ℤ)" ;
  aux c2_9 p5 top "ℤ" ;
  aux top m2 zero "0" ;
  aux top m2 p2 "2" ;
  aux top m2 c5_12 "(5 + 12ℤ)" ;
  aux top m2 top "ℤ" ;
  aux top p5 zero "⊥" ;
  aux top p5 p2 "2" ;
  aux top p5 c5_12 "(5 + 12ℤ)" ;
  aux top p5 top "ℤ" ;
  aux m7 c1_4 zero "0" ;
  aux zero c1_4 p2 "2" ;
  aux p5 c1_4 c5_12 "(5 + 12ℤ)" ;
  aux c2_9 c1_4 top "ℤ" ;
  aux top c1_4 c2_9 "(2 + 9ℤ)"

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

(* Create the generic lattice test cases. *)
module M =
struct
  module L = DomCongruence
  let elements = elements
  let relation =
    [
      (bot, m7) ;
      (bot, m2) ;
      (bot, zero) ;
      (bot, p2) ;
      (bot, p5) ;
      (m7, c5_12) ;
      (p5, c5_12) ;
      (m7, c1_8) ;
      (m7, c2_9) ;
      (p2, c2_9) ;
      (m7, c5_6) ;
      (p5, c5_6) ;
      (m2, c0_2) ;
      (zero, c0_2) ;
      (p2, c0_2) ;
      (c5_12, c1_4) ;
      (c5_12, c5_6) ;
      (c1_8, c1_4) ;
      (c2_9, c2_3) ;
      (c5_6, c2_3) ;
      (c1_4, top) ;
      (c2_3, top) ;
      (c0_2, top) ;
    ]
end

module T = TestLattice.Make (M)

(* Collection of all tests. *)
let tests prefix =
  [
    prefix ^ ".print", test_print ;
  ]
  @
  (T.tests prefix)
  @
  [
    prefix ^ ".abs", test_abs ;
    prefix ^ ".empty", test_empty ;
    prefix ^ ".add", test_add ;
    prefix ^ ".sub", test_sub ;
    prefix ^ ".mul", test_mul ;
    prefix ^ ".div", test_div ;
    prefix ^ ".equality", test_equality ;
    prefix ^ ".inequality", test_inequality ;
    prefix ^ ".widen", test_widen ;
    prefix ^ ".narrow", test_narrow ;
  ]
end

(* This test suite. *)
let suite = ("DomCongruence", lazy (let open Make () in tests "domcongruence"))
