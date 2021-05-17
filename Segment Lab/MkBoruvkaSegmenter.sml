functor MkBoruvkaSegmenter
  (structure Seq : SEQUENCE
   structure Rand : RANDOM210)
  :> SEGMENTER where Seq = Seq =
struct
  structure Seq = Seq

  (*Credits*)

  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight
  type e = vertex * vertex * weight * (vertex * vertex)

  (* Remove this exception when you're done! *)
  exception NotYetImplemented

  val MAX = valOf(Int.maxInt)


  (* fun vertexBridges e ->  vertex * (vertex, weight, (vertex * vertex))
   * The function vertexBridge(G) finds the minimum edge out of each vertex v and 
   maps v to the pair consisting of the neighbor along the edge and the edge label. 
   To this end, the function makes a singleton table for each edge and then merge 
   all the tables with a function to resolve collisions, which favors lighter edge
   *)
  fun vertexBridges E = 
    let
      val eT = Seq.map(fn (u, v, w, l) => (u,(v, w, l)) ) E 
      fun reduceSelect s = 
      let
        fun select ((v1, w1, l1), (v2, w2, l2)) = 
        if (w1 <= w2) then (v1, w1, l1) else (v2, w2, l2)
      in
        Seq.reduce select (0, MAX, (0, 0, 0)) s
      end
    in
      Seq.map (fn (a, b) => (a, reduceSelect b)) (Seq.collect Int.compare eT)
    end 
  


  (*FlipCoins function*)
  fun flipCoins n r =
    let 
      val (r', chooseSeed) = Rand.splitTab (r, n)
    in 
      (r', Seq.tabulate (Rand.bool o chooseSeed) n)
    end



  (* fun bridgeStarPartition (G as (V, E), i) ->
   * The function bridgeStarPartition performs star contraction on the subgraph 
   induced by the bridges. It starts by selecting the bridges and then it picks 
   from bridges the edges that go from a tail to a head. It then generates a mapping 
   from tails to heads along minimum edges, creating stars. Finally it removes all 
   vertices that are in this mapping to star centers.
   *)
  fun bridgeStarPartition (G as (V, E), C, i) = 
    let
      val eB = vertexBridges E
      val (r, HT) = flipCoins (Seq.length V) i
      (*Combined bools*)
      fun flipAnd (u, (v, w, l)) = 
        let
          val f = Seq.nth HT u
          val t = not (Seq.nth HT v)
        in
          f andalso t
        end
      (*difference between sequences*)
        fun seqdif (V, p) = 
          let
            val optp = Seq.map (fn (u, _) => (u, NONE)) p
          in
            Seq.inject (V, optp)
          end  
      (*Cred function*)  
      fun minmsum (v, seq) = 
        let
          val vertex = Seq.map (fn (u, w, l) => Seq.nth C u) seq
          val weight = Seq.map (fn (u, w, l) => w) seq
          val min =Seq.reduce Int.min (Seq.nth C v) vertex
          val edgeSum = Seq.reduce op+ 0 weight
        in
          (v, min - edgeSum)
        end
      (*Code clone from book*)
      val p = Seq.filter flipAnd eB
      val V' = seqdif(V, p)
      (*Credits part*)
      val revP = Seq.map (fn (u, (v, w, l)) => (v, (u, w, l))) p
      val N = Seq.collect Int.compare revP
      val cred = Seq.map minmsum N
      val C' = Seq.inject (C, cred)  
    in
      (V', p, C', r)
    end
  
    

  (* fun MST((V, E), T, i) ->
   * The function bridgeStarPartition is ready to be used in the MST code, except we 
   return the set of labels for the MST edges instead of the remaining vertices. 
   The code is given below. The MST algorithm is called by running MST(G,∅,0). As 
   an aside, we know that T is a spanning forest on the contracted nodes.
   *)
  fun MST((V, E), T, C, i) = 
    if Seq.length E = 0 then T
    else
      let
        (*function to update edges*)
        fun update (P, u, v) =
          let
            val u' = Seq.nth P u
            val v' = Seq.nth P v
          in
            case (u', v') of
              (SOME pU, SOME pV) => if pU = pV then false 
                                    else true
              | _ => false
          end
        (*Code from  Boruvka's algorithm*)
        val (V', PT, C', r) = bridgeStarPartition((V, E), C, i)
        val PT' = Seq.map (fn (u, (v, w, l)) => (u, SOME v)) PT
        val P = Seq.inject (V', PT')
        val T' = Seq.map(fn (u,(v, w, l)) => l) PT
        val E' = Seq.filter (fn (u, v, w, l) => update(P, u, v)) E
        val E'' = Seq.map (fn (u,v,w,l) => 
          (valOf(Seq.nth P u), valOf(Seq.nth P v), w, l)) E'
        val Enew = Seq.filter (fn (u,v,w,l) => 
                    if Int.min(Seq.nth C' u, Seq.nth C' v) >= w 
                    then true else false )E''
        val T'' = Seq.append (T, T')
      in
        MST((V', Enew), T'', C', r)
      end
 

  fun segment initialCredit (E, n) =
    let
      val T = Seq.empty()
      val C = Seq.tabulate (fn i => initialCredit) n
      val E' = Seq.map (fn (u, v, w) => (u, v, w, (u, v, w)) ) E
      val V = Seq.tabulate (fn i => SOME i) n 
    in
      MST((V, E'), T, C, Rand.fromInt 0)
    end

end
