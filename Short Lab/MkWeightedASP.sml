functor MkWeightedASP
  (structure Table : ORDTABLE
   structure PQ : PQ where type Key.t = int)
  :> WEIGHTED_ASP where type vertex = Table.Key.t and Seq = Table.Seq =
struct
  structure Seq = Table.Seq

  exception NotYetImplemented

  type vertex = Table.Key.t
  type edge = vertex * vertex * int

  (* You must define the following two types *)
  type graph = (vertex * int) Seq.t Table.t
  type asp = Table.Set.t Table.t
  

  (* val makeGraph : edge Seq.t -> graph
   * Given a sequence of directed edges E, (makeGraph E) should produce 
   the graph induced by E.
   *)
  fun makeGraph (E : edge Seq.t) : graph =
    let
      val t = Seq.map (fn (u, v, w) => (u, (v, w))) E
    in
      Table.collect t
    end

  
  fun updateQ(Q', G, v, d) = 
    let
      fun relax (Q, (u, w)) = PQ.insert(Q, (d + w, (u, v)))
    in
      case Table.find G v of
        NONE => Q'
        | SOME k => Seq.iterate relax Q' k
    end
      


  (* val makeASP : graph -> vertex -> asp
   * Given a graph G, (makeASP G s) should return an asp containing 
   information about all shortest paths in G beginning at the source s. 
   Assuming G represents the graph (V, E) and that all vertices are 
   reachable from the source.
   *)
  fun makeASP (G : graph) (v : vertex) : asp =
    let
      val Que = PQ.insert (PQ.empty(), (0, (v, v)))
      val acc = Table.empty()
      val X = Table.empty()
      fun dijkstra X Q acc = 
        case PQ.deleteMin Q of
          (NONE, _) => acc
          |(SOME (d, (v, p)), Q') =>
            (case Table.find X v of
              NONE => (let
                          val X' = Table.insert (X, (v, d))
                          val acc = Table.union Table.Set.union
                              (acc, Table.singleton (v, Table.Set.singleton p))
                          val Q'' = updateQ(Q', G, v, d)
                        in  
                          dijkstra X' Q'' acc 
                        end )
              |SOME d' => if d > d' then dijkstra X Q' acc 
                          else 
                            (let
                          val X' = Table.insert (X, (v, d))
                          val acc = Table.union(Table.Set.union)
                              (acc, Table.singleton (v, Table.Set.singleton p))
                          val Q'' = updateQ(Q', G, v, d)
                        in  
                          dijkstra X Q'' acc 
                        end ))
    in
      dijkstra X Que acc
    end



  (* val report : asp -> vertex -> vertex Seq.t Seq.t
   * Given an asp A which was constructed for the graph (V, E) with 
   source s, (report A t) should return all shortest paths from s to t. 
   Each path is a sequence of vertices beginning at s and ending at t. 
   Your output doesn’t have to be ordered in any particular way.
   *)
  fun report (A : asp) (v : vertex) : vertex Seq.t Seq.t =
    let      
      fun report' v = 
        let
          val N = Table.Set.toSeq (valOf(Table.find A v))
        in
          if Seq.length N = 1 andalso Table.Key.compare (Seq.nth N 0, v) = EQUAL
            then Seq.singleton([v])
            else 
              let
                val allButV = Seq.map report' N
                val res = Seq.flatten allButV  
              in
                Seq.map (fn L => v::L) res
              end
        end           
      fun format v = 
         let
           val f = Seq.map List.rev (report' v)
         in
           Seq.map Seq.fromList f
         end   
    in
      format v
    end

end
