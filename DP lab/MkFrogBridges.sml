functor MkFrogBridges(structure Seq : SEQUENCE) : FROGBRIDGES =
struct
  structure Seq = Seq
  open Seq

  type color = int
  type village = int * color

  (* Begin Cost Justification *)
  (* End Cost Justification *)



  (* maxBridges (int Seq * int Seq) -> int
   * You are given 2n villages as two sequences of villages, one for the left side of 
   the riverbank and one for the right side. Each sequence is of length n, where each 
   village is represented as a pair of its x-coordinate and its color, which are both ints. 
   Each village on a particular bank has a unique color, which is shared with exactly one 
   village on the opposite bank. The colors on either side of the bank range from 0 to n - 1. 
   You must build bridges between villages of the same color so that no two bridges cross
   each other.
   *)
  fun maxBridges (L, R) = 
    let
      val sortL = Seq.sort (fn (a, b) => Int.compare(#1 a, #1 b)) L
      val sortR = Seq.sort (fn (a, b) => Int.compare(#1 a, #1 b)) R
      fun maxBridges' (L, R) =
        if Seq.length L = 0 orelse Seq.length R = 0 then 0
        else 
            let
              val (v, c) = Seq.nth R 0
              val s = Seq.filter (fn (_, (_, b)) => c = b) (Seq.enum L)
              val (i, (_, c')) = if Seq.length s = 0 then (~1,(0,~1)) 
                                 else Seq.nth s 0
              val leftoverL = Seq.drop L (i + 1)
              (* for case not considering first bridge*)
              val nofirstR = Seq.drop R 1
              val remove = maxBridges'(L, nofirstR)
            in
              if c' = ~1 then remove else
              Int.max(remove, 1 + maxBridges'(leftoverL, nofirstR))
            end    
    in
      maxBridges'(sortL, sortR)
    end

end
