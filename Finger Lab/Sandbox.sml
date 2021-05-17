(* Write anything you want in the structure below. To use it at the REPL, do
 * - CM.make "sandbox.cm"; open Sandbox; *)
structure Sandbox =
struct
  structure Seq = ArraySequence
  structure Key = IntElt (* see lib/structures/elements *)
  structure Tree = MkTreap (structure Key = Key)

  (* your code, from MkFinger.sml *)
  structure Finger = MkFinger (structure Tree = Tree)

  (* For your convenience... *)
  fun fromSeq (s : Key.t Seq.t) : unit Tree.t =
    let val s' = Seq.sort Tree.Key.compare s
        val singletons = Seq.map (fn k => Tree.$ (k, ())) s'
    in Seq.reduce Tree.join (Tree.empty ()) singletons
    end

  fun treeToString (t : unit Tree.t) : string =
    case Tree.expose t of
      Tree.LEAF => "-"
    | Tree.NODE {key, value, left, right} =>
        String.concat [ "(", treeToString left, " "
                      , Tree.Key.toString key, " "
                      , treeToString right, ")" ]

  fun optToString (tos : 'a -> string) (xopt : 'a option) : string =
    case xopt of
      NONE => "NONE"
    | SOME x => "SOME " ^ tos x

  fun lift' f = #1 (Finger.lift f)
  fun root' t = Option.valOf (Finger.root t)
  fun searchFrom' f k = Option.valOf (Finger.searchFrom f k)
  fun first' t = Option.valOf (Finger.first t)
  fun next' f = Option.valOf (Finger.next f)

  (* An example of playing with your code... *)
  fun example () =
    let
      val t = fromSeq (Seq.tabulate (fn i => i) 10)
      val _ = print (treeToString t ^ "\n")

      val froot = root' t
      val _ = print (Key.toString (lift' froot) ^ "\n")

      val f5 = searchFrom' froot 5
      val _ = print (Key.toString (lift' f5) ^ "\n")
    in
      ()
    end
end
