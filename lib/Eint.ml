(* $Id: 8f45af4fb0f5917ea08ac0aaa1db4f174d7474ea $ *)


(*
   Extended arbitrary-precision integers.
 *)


type eint =
  | Integer of Z.t
  | Neg_infty
  | Pos_infty

open Z
open Z.Compare

let pp_print_eint fmt =
  function
  | Neg_infty -> Format.pp_print_string fmt "-∞"
  | Pos_infty -> Format.pp_print_string fmt "+∞"
  | Integer x -> pp_print fmt x

let ( = ) x y =
  match (x, y) with
  | (Neg_infty, Neg_infty)
  | (Pos_infty, Pos_infty) -> true
  | (Integer x, Integer y) -> x = y
  | ((Neg_infty | Pos_infty | Integer _),
     (Neg_infty | Pos_infty | Integer _)) -> false

let ( < ) x y =
  match (x, y) with
  | (_, Neg_infty) -> false
  | (Neg_infty, _) -> true
  | (Pos_infty, _) -> false
  | (_, Pos_infty) -> true
  | (Integer x, Integer y) -> x < y

let ( <= ) x y =
  match (x, y) with
  | (Neg_infty, _) -> true
  | (_, Neg_infty) -> false
  | (_, Pos_infty) -> true
  | (Pos_infty, _) -> false
  | (Integer x, Integer y) -> x <= y

let ( > ) x y = (<) y x
let ( >= ) x y = (<=) y x

let min x y =
  match (x, y) with
  | (Neg_infty, _)
  | (_, Neg_infty) -> Neg_infty
  | (Pos_infty, z)
  | (z, Pos_infty) -> z
  | (Integer x, Integer y) -> Integer (min x y)

let max x y =
  match (x, y) with
  | (Pos_infty, _)
  | (_, Pos_infty) -> Pos_infty
  | (Neg_infty, z)
  | (z, Neg_infty) -> z
  | (Integer x, Integer y) -> Integer (max x y)

let ( + ) x y =
  match (x, y) with
  | (Neg_infty, Pos_infty)
  | (Pos_infty, Neg_infty) -> invalid_arg "Eint.(+)"
  | (Neg_infty, _)
  | (_, Neg_infty) -> Neg_infty
  | (Pos_infty, _)
  | (_, Pos_infty) -> Pos_infty
  | (Integer x, Integer y) -> Integer (x + y)

let ( - ) x y =
  match (x, y) with
  | (Neg_infty, Neg_infty)
  | (Pos_infty, Pos_infty) -> invalid_arg "Eint.(-)"
  | (Neg_infty, _)
  | (_, Pos_infty) -> Neg_infty
  | (Pos_infty, _)
  | (_, Neg_infty) -> Pos_infty
  | (Integer x, Integer y) -> Integer (x - y)

let ( * ) x y =
  match (x, y) with
  | (Integer x, _) when Z.equal x zero -> Integer zero
  | (_, Integer y) when Z.equal y zero -> Integer zero
  | (Neg_infty, z)
  | (z, Neg_infty) -> if z < Integer zero then Pos_infty else Neg_infty
  | (Pos_infty, z)
  | (z, Pos_infty) -> if z < Integer zero then Neg_infty else Pos_infty
  | (Integer x, Integer y) -> Integer (x * y)

let ( / ) x y =
  match (x, y) with
  | (Integer _, (Neg_infty | Pos_infty)) -> Integer zero
  | (_, (Neg_infty | Pos_infty)) -> invalid_arg "Eint.(/)"
  | (_, Integer y) when Z.equal y zero -> raise Division_by_zero
  | (Neg_infty, Integer y) -> if Z.lt y zero then Pos_infty else Neg_infty
  | (Pos_infty, Integer y) -> if Z.gt y zero then Pos_infty else Neg_infty
  | (Integer x, Integer y) -> Integer (x / y)

let ( /< ) x y =
  match (x, y) with
  | (Integer x, Neg_infty) -> if Z.leq x zero then Integer zero else Integer minus_one
  | (Integer x, Pos_infty) -> if Z.geq x zero then Integer zero else Integer minus_one
  | (_, (Neg_infty | Pos_infty)) -> invalid_arg "Eint.(/<)"
  | (_, Integer y) when Z.equal y zero -> raise Division_by_zero
  | (Neg_infty, Integer y) -> if Z.lt y zero then Pos_infty else Neg_infty
  | (Pos_infty, Integer y) -> if Z.gt y zero then Pos_infty else Neg_infty
  | (Integer x, Integer y) -> Integer (x /< y)

let ( /> ) x y =
  match (x, y) with
  | (Integer x, Neg_infty) -> if Z.geq x zero then Integer zero else Integer one
  | (Integer x, Pos_infty) -> if Z.leq x zero then Integer zero else Integer one
  | (_, (Neg_infty | Pos_infty)) -> invalid_arg "Eint.(/>)"
  | (_, Integer y) when Z.equal y zero -> raise Division_by_zero
  | (Neg_infty, Integer y) -> if Z.lt y zero then Pos_infty else Neg_infty
  | (Pos_infty, Integer y) -> if Z.gt y zero then Pos_infty else Neg_infty
  | (Integer x, Integer y) -> Integer (x /> y)

let ( ~+ ) x = x
let ( ~- ) x = (Integer zero) - x
