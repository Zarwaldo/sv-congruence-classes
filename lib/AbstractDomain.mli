(* $Id: 2df05a7c652f78b01b0c2880ace2a619911f1e65 $ *)


(**
   Numerical Relational Abstract Domains.

   A numerical relational abstract domain is an abstract lattice that is in
   Galois connection with the concrete lattice (℘(X → ℤ), ⊆), where X is a set
   of variables.  So an abstract element represents (i.e., concretizes to) a set
   of concrete environments.  The domain comes with abstract semantics operators
   (post-image and pre-image), and is equipped with convergence acceleration
   operators (widening and narrowing).

   The underlying set X of variables is the set of all values of type
   [Variable.t].

   {L {b Implemented by:} {! PointwiseLifting}.}
 *)


(**
   Common interface to all abstract domain implementations.
 *)
module type S =
sig
  (**
     Underlying lattice (A, ⊑).
   *)
  include Lattice.S

  (**
     [empty a] tests whether the concretization of [a] is the empty set.
     {L {b Ensures:} γ([a]) = ∅ if [empty a] returns [true].}
   *)
  val empty : t -> bool

  (**
     [post cmd a] returns (an over-approximation of) the abstraction of
     \{ρ' | ∃ ρ ∈ γ([a]) : ρ ↝ ρ' via [cmd]\}.
   *)
  val post : Command.t -> t -> t

  (**
     [pre cmd a] returns (an over-approximation of) the abstraction of
     \{ρ | ∃ ρ' ∈ γ([a]) : ρ ↝ ρ' via [cmd]\}.
   *)
  val pre : Command.t -> t -> t

  (**
     [widen a b] returns the widening of [a] with [b] (i.e., [a] ∇ [b]).
     {L {b Ensures:} [a] ⊑ ([widen a b]) and [b] ⊑ ([widen a b]).}
   *)
  val widen : t -> t -> t

  (**
     [narrow a b] returns the narrowing of [a] with [b] (i.e., [a] ∆ [b]).
     {L {b Requires:} [b] ⊑ [a].}
     {L {b Ensures:} [b] ⊑ ([narrow a b]) ⊑ [a].}
   *)
  val narrow : t -> t -> t
end
