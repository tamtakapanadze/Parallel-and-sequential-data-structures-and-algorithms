(* Write anything you want in the structure below. To use it at the REPL, do
 * - CM.make "sandbox.cm"; open Sandbox; *)
structure Sandbox =
struct
  structure STSeq : ST_SEQUENCE = MkSTSequence (structure Seq = ArraySequence)
  structure Seq = STSeq.Seq

  (* your code, from MkBridges.sml *)
  structure Bridges : BRIDGES = MkBridges (structure STSeq = STSeq)

  (* For your convenience... *)
  fun eToS (x, y) =
    String.concat [ "(", Int.toString x, ",", Int.toString y, ")" ]
  val edgesToString = Seq.toString eToS

  (* An example test *)
  fun example () =
    let
      val tree = Seq.%[(0, 1), (1, 2), (1, 3), (0, 4), (4, 5), (4, 6)]
      val _ = print (edgesToString tree ^ "\n")

      val tree_bridges = Bridges.findBridges (Bridges.makeGraph tree)
      val _ = print (edgesToString tree_bridges ^ "\n")
    in
      ()
    end
end