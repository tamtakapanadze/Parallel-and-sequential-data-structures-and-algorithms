(* Write anything you want in the structure below. To use it at the REPL, do
 * - CM.make "sandbox.cm"; open Sandbox; *)
structure Sandbox =
struct
  structure Seq = ArraySequence

  (* Your code *)
  structure RangeSum = RangeSum

  (* fun rangeSum ptSeq (lowerLeft, upperRight) =
    RangeSum.findSum (RangeSum.makeSumTable ptSeq) (lowerLeft, upperRight)
  *)

  fun ptWToString ((x, y), w) = String.concat ["((", Int.toString (x), ",",
    Int.toString y, "),", Int.toString w, ")"]

  fun ptToString (x, y) =
    String.concat [ "(", Int.toString x, ",", Int.toString y, ")" ]

  fun ptsWToString pts = Seq.toString ptWToString pts

  fun example () =
    let
      val pts = Seq.% [((0,0),2),((1,2),5),((3,3),7),((4,4),3),((5,1),9)]
      val (lower, upper) = ((1,3),(5,1))
      val _ = print (ptsWToString pts ^ "\n")
      val _ = print (ptToString lower ^ "," ^ ptToString upper ^ "\n")
      val _ = print (Int.toString (RangeSum.rangeSum pts (lower, upper)) ^ "\n")
    in
      ()
    end
end
