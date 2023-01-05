(* $Id: d63fa41f63a823eca1b7582c3aa516fb63e799ad $ *)


(**
   Numerical domain for arithmetical congruence analysis.

   The underlying abstract lattice contains the element ⊥ as well as all pairs
   ({i r}, {i m}) in ℤ × ℕ such that {i m} = 0 or 0 ≤ {i r} < {i m}.  Such a
   pair ({i r}, {i m}) denotes the congruence class {i r} + {i m}ℤ.

   The function [Op.equality] is optimal in the special case where the first
   argument [a] or the third argument [c] is empty or a singleton.  But it is
   not optimal in general (see below).  The function [Op.inequality] is optimal.

   Consider for instance the “linear equality” (1 + 5ℤ)*x + (-9 + 0ℤ) = 0, with
   no condition on x.  It is easily seen that the set \{x ∈ ℤ | ∃ y ∈ (1 + 5ℤ) :
   y*x = 9\} is the set \{-1, 9\}, whose abstraction is the congruence class
   9 + 10ℤ.  But [Op.equality] returns the larger congruence class 4 + 5ℤ.  This
   congruence class is obtained by solving the given linear equality modulo 5.

   Integers used in this module are arbitrary-precision integers from the Zarith
   library, i.e., values of type [Z.t].

   @see <http://antoinemine.github.io/Zarith/doc/latest/Z.html> Z
 *)


include NumericalDomain.S
