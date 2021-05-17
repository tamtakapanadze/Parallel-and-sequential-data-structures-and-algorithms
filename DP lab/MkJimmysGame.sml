functor MkJimmysGame(structure Seq : SEQUENCE) : JIMMYSGAME =
struct
  structure Seq = Seq
  open Seq

  (* Remove when you're done! *)
  exception NotYetImplemented

  type rules = (int * (int * int)) seq
  type target = int seq

  (* Begin Cost Justification *)
  (* End Cost Justification *)
  


  (* val minSteps : int * target * rules -> int option
   * such that minSteps (sigma1, S, R) calculates the minimum number of steps it takes 
   to make S of length n, starting from “sigma1” using the rules in R. If S cannot be 
   formed from “sigma1” using the provided rules, your algorithm should return NONE. 
   Your solution should have O(n3mk) work when the subproblems are represented as a DAG.
   *)
  fun minSteps (init, S, R) =
    case Seq.length S = 1 of
       true => if Seq.nth S 0 = init then SOME 0 
               else NONE
       |false => 
         let
           val n = Seq.length S
           val whereToSplit = Seq.tabulate(fn k => k + 1)(n - 1)
           val subSeqPairs = Seq.map (fn i => (Seq.take S i, Seq.drop S i)) 
                                           whereToSplit
           val m = Seq.length subSeqPairs
           (*two letters as pairs that could be produced from init one 
           and available rules*)                                
           val twoLetters = Seq.map (fn (a, (b, c)) => (b, c)) 
                     (Seq.filter (fn (a, b) => a = init) R)
           val k = Seq.length twoLetters
           val whatTowhat = Seq.tabulate (fn i => Seq.tabulate (fn j => 
                (Seq.nth twoLetters i, Seq.nth subSeqPairs j)) m) k
           val whatTowhat' = Seq.flatten whatTowhat
           (*Helpers*)
           fun max (x, y) = case (x, y) of
              (NONE, _) => NONE
              |(_, NONE) => NONE
              |(SOME v, SOME v') => SOME (Int.max(v, v') + 1)
           fun mins (x, y) = case (x, y) of
              (NONE, NONE) => NONE
              |(NONE, SOME v) => SOME v
              |(SOME v, NONE) => SOME v
              |(SOME v, SOME v') => SOME (Int.min(v, v')) 
           (*minimum step numbers*)          
           val steps = Seq.map (fn ((a, b), (s1, s2)) => 
            max(minSteps(a, s1, R), minSteps(b, s2, R))) whatTowhat'
           val min = Seq.reduce mins NONE steps
         in
           min
         end
end
