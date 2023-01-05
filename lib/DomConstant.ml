(* $Id: a6004c5d119c438825eddc71535620de3fede035 $ *)


(*
   Numerical domain for constant propagation analysis.
 *)


(*
   An abstract element is either Bot, or (Val n) where n is an integer, or Top.
   The concretization function γ is defined as follows:

                  Abstract element                Concretization
                         Bot                            ∅
                        Val n                          {n}
                         Top                            ℤ

   The abstraction function α is defined by α(∅) = Bot, α({n}) = n for every
   integer n, and α(S) = Top for every subset S ⊆ ℤ of cardinal at least two.
 *)
type t =
  | Bot
  | Val of Z.t
  | Top

open Z
open Z.Compare

let print fmt =
  function
  | Bot -> Format.pp_print_string fmt "⊥"
  | Val x -> pp_print fmt x
  | Top -> Format.pp_print_string fmt "⊤"

let bot = Bot
let top = Top

let equal a b =
  match (a, b) with
  | (Bot, Bot)
  | (Top, Top) -> true
  | (Val x, Val y) -> x = y
  | ((Bot | Val _ | Top), (Bot | Val _ | Top)) -> false

let leq a b =
  match (a, b) with
  | (Bot, _)
  | (_, Top) -> true
  | (_, Bot)
  | (Top, _) -> false
  | (Val x, Val y) -> x = y

let glb a b =
  match (a, b) with
  | (Bot, _)
  | (_, Bot) -> Bot
  | (Top, c)
  | (c, Top) -> c
  | (Val x, Val y) -> if x = y then a else Bot

let lub a b =
  match (a, b) with
  | (Top, _)
  | (_, Top) -> Top
  | (Bot, c)
  | (c, Bot) -> c
  | (Val x, Val y) -> if x = y then a else Top

let abs x = Val (of_int x)

let empty a = equal a bot

module Op =
struct
  let add a b =
    match (a, b) with
    | (Bot, _)
    | (_, Bot) -> Bot
    | (Top, _)
    | (_, Top) -> Top
    | (Val x, Val y) -> Val (x + y)

  let sub a b =
    match (a, b) with
    | (Bot, _)
    | (_, Bot) -> Bot
    | (Top, _)
    | (_, Top) -> Top
    | (Val x, Val y) -> Val (x - y)

  let mul a b =
    match (a, b) with
    | (Bot, _)
    | (_, Bot) -> Bot
    | (Val x, _) when x = zero -> Val zero
    | (_, Val y) when y = zero -> Val zero
    | (Top, _)
    | (_, Top) -> Top
    | (Val x, Val y) -> Val (x * y)

  let div a b =
    match (a, b) with
    | (Bot, _)
    | (_, Bot) -> Bot
    | (_, Val y) when y = zero -> Bot
    | (Val x, _) when x = zero -> Val zero
    | (Top, _)
    | (_, Top) -> Top
    | (Val x, Val y) -> Val (x / y)

  (*
     This implementation of equality uses the function Z.divisible.  Recall that
     its type is:

     val Z.divisible : Z.t -> Z.t -> bool

     Z.divisible a b returns true if, and only if, ∃ k ∈ ℤ : a = kb.
   *)
  let equality a b c =
    match (a, b, c) with
    | (Bot, _, _)
    | (_, Bot, _)
    | (_, _, Bot) -> Bot
    | (_, Top, _)
    | (Top, Val _, Top) -> c
    | (Top, Val s, Val x) ->
       if divisible s x then
         c
       else
         Bot
    | (Val r, Val s, Top) ->
       if divisible s r then
         if r = zero then
           (* s = 0 *)
           c
         else
           Val (-s / r)
       else
         Bot
    | (Val r, Val s, Val x) ->
       if (r * x) + s = zero then
         c
       else
         Bot

  let inequality a b c =
    match (a, b, c) with
    | (Bot, _, _)
    | (_, Bot, _)
    | (_, _, Bot) -> Bot
    | (_, Top, _)
    | (Top, Val _, Top) -> c
    | (Top, Val s, Val x) ->
       if s <= zero || x <> zero then
         c
       else
         Bot
    | (Val r, Val s, Top) ->
       if s <= zero || r <> zero then
         c
       else
         Bot
    | (Val r, Val s, Val x) ->
       if (r * x) + s <= zero then
         c
       else
         Bot
end

let widen = lub

let narrow a b =
  assert (leq b a) ;
  b
