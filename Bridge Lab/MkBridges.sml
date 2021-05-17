functor MkBridges(structure STSeq : ST_SEQUENCE)
  :> BRIDGES where type 'a Seq.t = 'a STSeq.Seq.t and type vertex = int =
struct
  structure Seq = STSeq.Seq

  type vertex = int
  type edge = vertex * vertex
  type edges = edge Seq.t

  (* Remove the following line before submitting. *)
  exception NotYetImplemented

  type ugraph = int Seq.t Seq.t

  (* val makeGraph : edges -> ugraph
   * Given a sequence E representing the edges of a graph G, makeGraph E 
   returns that same graph under your ugraph representation
   *)
  fun makeGraph (E : edges) : ugraph = 
    let
      (*number of edges*)
      val n = Seq.length(E)
      fun reverse (a, b) = (b, a)
      (*generate the reversed edge names*)
      val revPar = Seq.tabulate(fn i => reverse(Seq.nth E i)) n
      (*make a sequence containing both (u, v) and (v, u)*)
      val allPar = Seq.append(E, revPar)
      (*collect the corresponding neigbour vertex labels*)
      val graph = Seq.collect (Int.compare) allPar
    in
      (*Make the ugraph*)
      Seq.map (fn (a, b) => b) graph
    end
  


  fun changeV(state, v) = 
    let
      val (t, V, par, P, E, B) = state
    in
      (t, V, v, P, E, B)
    end


  fun update state v = 
    let
      val (t, V, par, P, E, B) = state
      val e = STSeq.update(E,(par, Int.min(STSeq.nth E v,
                                 STSeq.nth E par)))
    in
      (t, V, par, P, e, B)
    end


  fun visit state v = 
    let
      val (t, V, par, P, E, B) = state
    in
      (t+1, 
       STSeq.update (V, (v, t)), 
       v, 
       STSeq.update (P, (v, par)), 
       STSeq.update (E, (v, t)), 
       B)
    end


  fun revisit state v = 
    let
      val (t, V, par, P, E, B) = state
    in
      if STSeq.nth(P)(par) <> v then 
        let
          val e = STSeq.update(E,(par, Int.min(STSeq.nth E par,
                                 STSeq.nth E v)))
        in
          (t, V, par, P, e, B) 
        end      
      else state
    end


  (*Using t*)
  fun DFS (G : ugraph) (state, v) = 
    let
      val (t, V, par, P, E, B) = state
    in
      if STSeq.nth(V)(v) > ~1 then revisit state v
      else 
        let
          val state' = visit state v
          val state = Seq.iterate (DFS G) state' (Seq.nth G v)
          val (t, V, _, P, E, B) = update (changeV(state, par)) v
          val b = if STSeq.nth(E)(v) > STSeq.nth(V)(par) 
                   then (v, par)::B else B
        in
          (t+1, V, par, P, E, b)
        end         
    end


  
  (* val findBridges : ugraph -> edges
   * Given an undirected graph G, findBridges G returns a sequence containing 
   exactly the edges which are bridges of G
   *)
  fun findBridges (G : ugraph) : edges = 
    let
      val size = Seq.length(G)
      val Vs = Seq.tabulate (fn i => i) size
      val V = STSeq.fromSeq (Seq.tabulate(fn i => ~1) size)
      val P = STSeq.fromSeq (Seq.tabulate(fn i => ~1) size)
      val E = STSeq.fromSeq (Seq.tabulate(fn i => ~1) size)
      val B = []
      val state = (0, V, 0, P, E, B)
      val (_, _, _, _, _, b) = Seq.iterate(fn ((t, V, _, P, E, B), v) => 
                      DFS G ((t, V, v, P, E, B), v)) state Vs
    in
      Seq.fromList b
    end


end
