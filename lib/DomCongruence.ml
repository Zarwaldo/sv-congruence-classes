
type t =
  | Bot
  | Val of Z.t * Z.t

(* Checks that a value of type t is a legal abstract congruence domain. *)
let legal =
  function
  | Bot -> true
  | Val (r1, m1) -> if m1 = Z.zero then true else (r1 < m1) && (r1 >= Z.zero)

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

  let rec div a b =
    match (a, b) with
    | (Bot, _) -> Bot
    | (_, Bot) -> Bot
    | (Val(_, _), Val(u, v)) when (Z.equal v Z.zero) && (Z.equal u Z.zero) -> Bot
    | (Val(a, b), Val(u, v)) when (Z.equal v Z.zero) && (Z.equal b Z.zero) -> Val(Z.div a u, Z.zero)
    | (Val(a, b), Val(u, v)) when (Z.equal v Z.zero) && (Z.divisible a u) && (Z.divisible b u) -> Val(Z.div a u, Z.div b u)
    | (Val(_, b), Val(u, v)) when (Z.equal v Z.zero) && (Z.divisible b u) -> top
    | (Val(_, _), Val(_, v)) when (Z.equal v Z.zero) -> top
    | (Val(a, b), Val(_, _)) when (Z.equal a Z.zero) && (Z.equal b Z.zero) -> Val(Z.zero, Z.zero)
    | (Val(a, b), Val(u, v)) when (Z.equal b Z.zero) && (Z.gt a Z.zero) && (Z.gt u Z.zero) ->
      let n = Z.add (Z.mul v (Z.div (Z.sub a u) v)) u in
      if (Z.leq n Z.zero) then Val(Z.zero, Z.zero) else Val(Z.zero, Z.div a n)
    | (Val(a, b), Val(u, v)) when (Z.equal b Z.zero) && (Z.lt a Z.zero) && (Z.gt u Z.zero) ->
      sub (Val(Z.zero, Z.zero)) (div (Val(Z.neg a, Z.neg b)) (Val(u, v)))
    | (Val(a, b), Val(u, v)) when (Z.equal b Z.zero) && (Z.gt a Z.zero) && (Z.lt u Z.zero) ->
      sub (Val(Z.zero, Z.zero)) (div (Val(a, b)) (Val(Z.neg u, Z.neg v)))
    | (Val(a, b), Val(u, v)) when (Z.equal b Z.zero) && (Z.lt a Z.zero) && (Z.lt u Z.zero) ->
      div (Val(Z.neg a, Z.neg b)) (Val(Z.neg u, Z.neg v))
    | (Val(_, _), Val(_, _)) -> top

  let equality a b c =
    match (a, b, c) with
     | (_, _, _) -> Bot

  let inequality a b c =
    match (a, b, c) with
     | (_, _, _) -> Bot

end

let widen a b = lub a b

let narrow a b = b
