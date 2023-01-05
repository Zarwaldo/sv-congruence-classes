(* $Id: db9348ea36ba52acd27113b80363f6d980650c05 $ *)


(**
   Numerical domain for sign analysis.

   The underlying abstract lattice contains the following elements: ⊥, negative,
   zero, positive, nonpositive, nonzero, nonnegative, and ⊤.

   The functions [Op.equality] and [Op.inequality] are optimal.
 *)


include NumericalDomain.S
