
type t =
  | Bot
  | Val of Z.t * Z.t

let print fmt =
  function
  | Bot -> Format.pp_print_string fmt "⊥"
  | Val (r, m) -> 
    if m = Z.zero then
      Format.fprintf fmt "%d" (Z.to_int r)
    else
      if r = Z.zero then
        if m = Z.one then Format.pp_print_string fmt "ℤ"
        else Format.fprintf fmt "%dℤ" (Z.to_int (Z.abs m))
      else Format.fprintf fmt "(%d + %dℤ)" (Z.to_int r) (Z.to_int (Z.abs m))

let bot = Bot
let top = Val(Z.zero, Z.one)

let equal a b =
  match (a, b) with
  | (Bot, Bot) -> true
  | (Bot, _) -> false
  | (_, Bot) -> false
  | (Val(a, b), Val(u, v)) -> (Z.equal a u) && (Z.equal b v)

let leq a b =
  match (a, b) with
  | (Bot, Bot)
  | (Bot, _) -> true
  | (_, Bot) -> false
  | (Val(a, b), Val(u, v)) when (Z.equal v Z.zero) -> (Z.equal a u) && (Z.equal b Z.zero)
  | (Val(a, b), Val(u, v)) -> (Z.divisible b v) && (Z.divisible (Z.sub a u) v)

let glb a b =
  match (a, b) with
  | (_, Bot) -> Bot
  | (Bot, _) -> Bot
  | (Val(r1, m1), Val(r2, m2)) when (Z.equal m2 Z.zero) && (Z.equal m1 Z.zero) && r1 == r2 -> Val(r1, Z.zero)
  | (Val(_, m1), Val(_, m2)) when (Z.equal m2 Z.zero) && (Z.equal m1 Z.zero) -> Bot
  | (Val(r1, m1), Val(r2, m2)) when (Z.equal m1 Z.zero) && (Z.divisible (Z.sub r1 r2) m2) -> Val(r1, Z.zero)
  | (Val(_, m1), Val(_, _)) when (Z.equal m1 Z.zero) -> Bot
  | (Val(r1, m1), Val(r2, m2)) when (Z.equal m2 Z.zero) && (Z.divisible (Z.sub r2 r1) m1) -> Val(r2, Z.zero)
  | (Val(_, _), Val(_, m2)) when (Z.equal m2 Z.zero) -> Bot
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

let lub a b =
  match (a, b) with
  | (Bot, Bot) -> Bot
  | (Val(r1, m1), Bot) -> Val(r1, m1)
  | (Bot, Val(r2, m2)) -> Val(r2, m2)
  | (Val(r1, m1), Val(r2, m2)) when (Z.equal m1 Z.zero) && (Z.equal m2 Z.zero) && (Z.equal r1 r2) ->
    Val(r1, Z.zero)
  | (Val(r1, m1), Val(r2, m2)) when (Z.equal m1 Z.zero) && (Z.equal m2 Z.zero) ->
    let m = Z.sub r1 r2 in
    let r = Z.erem r1 m in
    Val(r, m)
  | (Val(r1, m1), Val(r2, m2)) when (Z.equal m2 Z.zero) ->
    let m = Z.gcd m1 (Z.sub r1 r2) in
    let r = Z.erem r2 m in
    Val(r, m)
  | (Val(r1, m1), Val(r2, m2)) when (Z.equal m1 Z.zero) ->
    let m = Z.gcd m2 (Z.sub r2 r1) in
    let r = Z.erem r1 m in
    Val(r, m)
  | (Val(r1, m1), Val(r2, m2)) ->
    let m = Z.gcd m1 (Z.gcd m2 (Z.sub r2 r1)) in
    let r = Z.erem r1 m in
    Val(r, m)

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
    | (Val(a, b), Val(u, v)) when (Z.equal b Z.zero) -> Val(Z.erem (Z.add a u) v, Z.abs v)
    | (Val(a, b), Val(u, v)) when (Z.equal v Z.zero) -> Val(Z.erem (Z.add a u) b, Z.abs b)
    | (Val(a, b), Val(u, v)) -> let d = Z.gcd b v in Val(Z.erem (Z.add a u) d, d)

  let sub a b =
    match (a, b) with
    | (Bot, _) -> Bot
    | (_, Bot) -> Bot
    | (Val(a, b), Val(u, v)) when (Z.equal b Z.zero) && (Z.equal v Z.zero) -> Val(Z.sub a u, Z.zero)
    | (Val(a, b), Val(u, v)) when (Z.equal b Z.zero) -> Val(Z.erem (Z.sub a u) v, Z.abs v)
    | (Val(a, b), Val(u, v)) when (Z.equal v Z.zero) -> Val(Z.erem (Z.sub a u) b, Z.abs b)
    | (Val(a, b), Val(u, v)) -> let d = Z.gcd b v in Val(Z.erem (Z.sub a u) d, d)

  let mul a b =
    match (a, b) with
    | (Bot, _) -> Bot
    | (_, Bot) -> Bot
    | (Val(a, b), Val(u, v)) -> add (Val(Z.mul a u, Z.zero)) (add (Val(Z.zero, Z.mul a v)) (add (Val(Z.zero, Z.mul b u)) (Val(Z.zero, Z.mul b v))))

  let div a b =
    match (a, b) with
    | (Bot, _) -> Bot
    | (_, Bot) -> Bot
    | (Val(_, _), Val(_, v)) when not (Z.equal v Z.zero) -> top
    | (Val(_, _), Val(u, _)) when (Z.equal u Z.zero) -> bot
    | (Val(a, _), Val(u, _)) when not (Z.divisible a u) -> top
    | (Val(_, b), Val(u, _)) when not (Z.divisible b u) -> top
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
    | (Val(_, m1), Val(r2, m2)) -> if m1 > m2 then top else Val(r2, m2)

let narrow a b =
  match (a, b) with
    | (Bot, Val(_, _))
    | (Val(_, _), Bot)
    | (Bot, Bot) -> Bot
    | (t, Val(r2, m2)) when equal t top -> Val(r2, m2)
    | (Val(r1, m1), Val(_, _)) -> Val(r1, m1)
