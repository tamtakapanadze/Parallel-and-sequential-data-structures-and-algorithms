functor MkUnweightedASP
  (structure Table : ORDTABLE)
  :> UNWEIGHTED_ASP where type vertex = Table.Key.t and Seq = Table.Seq =
struct
  structure Seq = Table.Seq

  exception NotYetImplemented

  type vertex = Table.Key.t
  type edge = vertex * vertex

  (* You must define the following two types *)
  type graph = (Table.Set.t Table.t * Table.Set.t Table.t)
  type asp = Table.Set.t Table.t

  
  (* val makeGraph : edge Seq.t -> graph
   * Given a sequence of directed edges E, (makeGraph E) should produce 
   the graph induced by E.
   *)
  fun makeGraph (E : edge Seq.t) : graph = 
    let
      fun rev ((u, v): edge): edge = (v, u)
      val inn = Seq.map(rev)(E)
      val outT = Table.collect(E)
      val outSetT = Table.map(Table.Set.fromSeq)(outT)
      val inT = Table.collect(inn)
      val inSetT = Table.map(Table.Set.fromSeq)(inT)
    in
      (inSetT, outSetT)
    end



  (* val makeASP : graph -> vertex -> asp
   * Given a graph G, (makeASP G s) should return an asp containing 
   information about all shortest paths in G beginning at the source s. 
   Assuming G represents the graph (V, E) and that all vertices are 
   reachable from the source.
   *)
  fun makeASP (G: graph) (v: vertex) : asp = 
    let 
      val x = Table.empty()
      val xdom = Table.Set.empty()
      val fdom = Table.Set.singleton v
      val f = Table.singleton(v, Table.Set.empty())
      fun explore (X, F, Xdom) = 
        if Table.size(F) = 0 then X
        else 
          let 
            val Fdom = Table.domain(F)
            val Xdom' = Table.Set.union(Xdom, Fdom)                            
            val X' = Table.union Table.Set.union (X, F)
            (* Stolen from Lecture notes *)
            val Fdom' = Table.reduce Table.Set.union (Table.Set.empty()) 
                    (Table.restrict(#2 G, Fdom))
            val F' = Table.tabulate(fn k => 
              Table.Set.intersection(valOf(Table.find(#1 G) k), Xdom'))(Fdom')
            val F'' = Table.difference (F', X')
          in
            explore(X', F'', Xdom')
          end
      in
        explore(x, f, xdom)
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
          case Seq.length N of
            0 => Seq.singleton([v])
            |_ => 
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
