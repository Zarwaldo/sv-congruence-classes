
type t =
  | Bot
  | Top

open Z
open Z.Compare

let print fmt =
    failwith "DomCongruence.print not implemented."

let bot = Bot
let top = Top

let equal a b =
    failwith "DomCongruence.equal not implemented."

let leq a b =
    failwith "DomCongruence.leq not implemented."

let glb a b =
    failwith "DomCongruence.glb not implemented."

let lub a b =
    failwith "DomCongruence.lub not implemented."

let abs x =
    failwith "DomCongruence.abs not implemented."

let empty a =
    failwith "DomCongruence.empty not implemented."

module Op =
struct
  let add a b =
    failwith "DomCongruence.add not implemented."

  let sub a b =
    failwith "DomCongruence.sub not implemented."

  let mul a b =
    failwith "DomCongruence.mul not implemented."

  let div a b =
    failwith "DomCongruence.div not implemented."

  let equality a b c =
    failwith "DomCongruence.equality not implemented."

  let inequality a b c =
    failwith "DomCongruence.inequality not implemented."
end

let widen =
    failwith "DomCongruence.widen not implemented."

let narrow a b =
    failwith "DomCongruence.narrow not implemented."
