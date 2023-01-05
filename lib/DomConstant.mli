(* $Id: 095c297eea3910f4c68b560e1d86b33a0985a02d $ *)


(**
   Numerical domain for constant propagation analysis.

   The underlying abstract lattice contains the following elements: ⊥, all
   integers, and ⊤.

   The functions [Op.equality] and [Op.inequality] are optimal.

   Integers used in this module are arbitrary-precision integers from the Zarith
   library, i.e., values of type [Z.t].

   @see <http://antoinemine.github.io/Zarith/doc/latest/Z.html> Z
 *)


include NumericalDomain.S
