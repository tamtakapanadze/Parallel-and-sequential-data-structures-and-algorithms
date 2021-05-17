structure Thesaurus =
struct
  structure StringTable = MkTreapTable (structure Key = StringElt)
  structure IntPQ = MkLeftistHeapPQ (structure Key = IntElt)
  structure Seq = StringTable.Seq
  structure Util = MkThesaurusUtil (structure Seq = Seq)
  structure UASP = MkUnweightedASP (structure Table = StringTable)
  structure WASP = MkWeightedASP (structure Table = StringTable
                                  structure PQ = IntPQ)

  val wEdges = Util.fromFile "thesaurus/thesaurus.txt"
  val uEdges = Seq.map (fn (x,y,_) => (x,y)) wEdges

  fun concatWith str s = String.concatWith str (Seq.toList s)

  fun unweightedQuery x =
    let
      val aspx = UASP.makeASP (UASP.makeGraph uEdges) x
      fun report y =
        print ("UNWEIGHTED PATHS FROM " ^ x ^ " TO " ^ y ^":\n" ^ concatWith "\n" (Seq.map (concatWith ", ") (UASP.report aspx y)) ^ "\n")
    in report
    end

  fun weightedQuery x =
    let
      val aspx = WASP.makeASP (WASP.makeGraph wEdges) x
      fun report y =
        print ("WEIGHTED PATHS FROM " ^ x ^ " TO " ^ y ^":\n" ^ concatWith "\n" (Seq.map (concatWith ", ") (WASP.report aspx y)) ^ "\n")
    in report
    end
end
