
type t =
  | Bot
  | Val of Z.t * Z.t

open Z
open Z.Compare

let print fmt =
  function
  | Bot -> Format.pp_print_string fmt "⊥"
  | Val (r, m) -> Format.fprintf fmt "%d + %dℤ" (Z.to_int r) (Z.to_int m)

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
  | (Bot, Bot) -> true
  | (Bot, _) -> false
  | (_, Bot) -> false
  | (Val(r1, m1), Val(r2, m2)) -> (Z.equal (Z.rem m2 m1) Z.zero) && (Z.equal (Z.rem m2 (Z.sub r1 r2)) Z.zero)

let glb a b =
  failwith "DomCongruence.glb not implemented."

let lub a b =
  failwith "DomCongruence.lub not implemented."

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
    | (Val(a, b), Val(u, v)) -> let d = Z.gcd b v in Val(Z.rem (Z.add a u) d, d)

  let sub a b =
    match (a, b) with
    | (Bot, _) -> Bot
    | (_, Bot) -> Bot
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
    | (Val(a, _), Val(u, _)) when not (Z.equal (Z.rem a u) Z.zero) -> top
    | (Val(_, b), Val(u, _)) when not (Z.equal (Z.rem b u) Z.zero) -> top
    | (Val(a, b), Val(u, _)) -> Val(Z.div a u, Z.div b u)

  let equality a b c =
    failwith "DomCongruence.equality not implemented."

  let inequality a b c =
    failwith "DomCongruence.inequality not implemented."
end

let widen =
    failwith "DomCongruence.widen not implemented."

let narrow a b =
    failwith "DomCongruence.narrow not implemented."
