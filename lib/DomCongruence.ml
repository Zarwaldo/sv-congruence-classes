
type t =
  | Bot
  | Val of Z.t * Z.t

open Z
open Z.Compare

let print fmt =
  function
  | Bot -> Format.pp_print_string fmt "⊥"
  | Val (r, m) -> 
    if m = Z.zero then
      Format.fprintf fmt "%d" (Z.to_int r)
    else
      if r = Z.zero then
        if m = Z.one then Format.pp_print_string fmt "ℤ"
        else Format.fprintf fmt "%dℤ" (Z.to_int m)
      else Format.fprintf fmt "(%d + %dℤ)" (Z.to_int r) (Z.to_int m)

let bot = Bot
let top = Val(Z.zero, Z.one)

let equal a b =
  match (a, b) with
  | (Bot, Bot) -> true
  | (Bot, _) -> false
  | (_, Bot) -> false
  | (Val(r1, m1), Val(r2, m2)) -> (Z.equal r1 r2) && (Z.equal m1 m2)

let leq a b =
  match (a, b) with
  | (Bot, Bot)
  | (Bot, _) -> true
  | (_, Bot) -> false
  | (Val(r1, m1), Val(r2, m2)) when (Z.equal m1 Z.zero) && (Z.equal r1 r2) -> true
  | (Val(r1, m1), Val(r2, m2)) when (Z.equal r1 r2) -> (Z.equal (Z.rem m2 m1) Z.zero)
  | (Val(r1, m1), Val(r2, m2)) when (Z.equal m1 Z.zero) -> (Z.equal (Z.rem m2 (Z.sub r1 r2)) Z.zero)
  | (Val(r1, m1), Val(r2, m2)) -> (Z.equal (Z.rem m2 m1) Z.zero) && (Z.equal (Z.rem m2 (Z.sub r1 r2)) Z.zero)

let glb a b =
    match (a, b) with
   | (Val(r1, m1), Val(r2, m2)) -> 
    let (pgcd, u, _) = Z.gcdext m1 m2 in
    if Z.divisible (Z.sub r2 r1) pgcd then
      let k = Z.div (Z.sub r2 r1) pgcd in
      let k1 = Z.mul u k in
      let m = Z.mul m1 (Z.div m2 pgcd) in
      let r = Z.erem (Z.add (Z.mul m1 k1) r1) m in
      Val(r, m)
    else
      Bot
   | (Val(_, _), Bot)
   | (Bot, Val(_, _))
   | (Bot, Bot) -> Bot

let lub a b =
  match (a, b) with
 | (Val(r1, m1), Val(r2, m2)) ->
  let m = Z.gcd m1 (Z.gcd m2 (Z.sub r2 r1)) in
  let r = Z.erem r1 m in
  Val(r, m)
 | (Val(r1, m1), Bot) -> Val(r1, m1)
 | (Bot, Val(r2, m2)) -> Val(r2, m2)
 | (Bot, Bot) -> Bot

let abs x =
  Val(Z.of_int x, Z.zero)

let empty a =
  match a with
  | Bot -> true
  | Val(_,_) -> false

module Op =
struct
  let add a b =
    match (a, b) with
    | (Bot, _) -> Bot
    | (_, Bot) -> Bot
    | (Val(a, b), Val(u, v)) when (Z.equal b Z.zero) && (Z.equal v Z.zero) -> Val(Z.add a u, Z.zero)
    | (Val(a, b), Val(u, v)) when (Z.equal b Z.zero) -> Val(Z.rem (Z.add a u) v, v)
    | (Val(a, b), Val(u, v)) when (Z.equal v Z.zero) -> Val(Z.rem (Z.add a u) b, b)
    | (Val(a, b), Val(u, v)) -> let d = Z.gcd b v in Val(Z.rem (Z.add a u) d, d)

  let sub a b =
    match (a, b) with
    | (Bot, _) -> Bot
    | (_, Bot) -> Bot
    | (Val(a, b), Val(u, v)) when (Z.equal b Z.zero) && (Z.equal v Z.zero) -> Val(Z.sub a u, Z.zero)
    | (Val(a, b), Val(u, v)) when (Z.equal b Z.zero) -> Val(Z.rem (Z.sub a u) v, v)
    | (Val(a, b), Val(u, v)) when (Z.equal v Z.zero) -> Val(Z.rem (Z.sub a u) b, b)
    | (Val(a, b), Val(u, v)) -> let d = Z.gcd b v in Val(Z.rem (Z.sub a u) d, d)

  let mul a b =
    match (a, b) with
    | (Bot, _) -> Bot
    | (_, Bot) -> Bot
    | (Val(a, b), Val(u, v)) -> add (Val(Z.mul u v, Z.zero)) (add (Val(Z.zero, Z.mul a v)) (add (Val(Z.zero, Z.mul u b)) (Val(Z.zero, Z.mul b v))))

  let div a b =
    match (a, b) with
    | (Bot, _) -> Bot
    | (_, Bot) -> Bot
    | (Val(_, _), Val(_, v)) when not (Z.equal v Z.zero) -> top
    | (Val(_, _), Val(u, _)) when (Z.equal u Z.zero) -> bot
    | (Val(a, _), Val(u, _)) when not (Z.equal (Z.rem a u) Z.zero) -> top
    | (Val(_, b), Val(u, _)) when not (Z.equal (Z.rem b u) Z.zero) -> top
    | (Val(a, b), Val(u, _)) -> Val(Z.div a u, Z.div b u)

  let equality a b c =
    failwith "DomCongruence.equality not implemented."

  let inequality a b c =
    failwith "DomCongruence.inequality not implemented."
end

let widen a b =
  match (a, b) with
    | (Bot, Val(r2, m2)) -> Val(r2, m2)
    | (Val(r1, m1), Bot) -> Val(r1, m1)
    | (Bot, Bot) -> Bot
    | (Val(r1, m1), Val(r2, m2)) -> if m1 > m2 then top else Val(r2, m2)

let narrow a b =
  match (a, b) with
    | (Bot, Val(_, _))
    | (Val(_, _), Bot)
    | (Bot, Bot) -> Bot
    | (Val(r1, m1), Val(_, _)) -> Val(r1, m1)
    | (top, Val(r2, m2)) -> Val(r2, m2)
