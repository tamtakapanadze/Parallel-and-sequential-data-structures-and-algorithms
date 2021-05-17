structure RangeSum :> RANGE_SUM where Seq = ArraySequence =
struct
  structure Seq = ArraySequence

structure MyVal =
struct
	type t = int
	val f : t * t -> t = op+
	val I : t = 0
	val toString : t -> string = Int.toString
end

  structure Ytable =
	MkTreapAugTable (structure Key = IntElt
	                 structure Val = MyVal)


  structure Xtable = MkTreapTable (structure Key = IntElt)

  exception NotYetImplemented

  type point = int * int
  
  (* fun makeTable Seq.seq -> Xtable.table
   * builds two dimensional table for the given sequence
   *)
  fun makeTable S = 
  	let
  		(*Sort the points based on x coordinates*)
  		val s = Seq.sort (fn (((x1, _), _), ((x2, _), _)) => 
  			                     Int.compare(x1, x2)) S
  		(*To build a table, separate x coordinates and y + weight*)
  		val xOnly = Seq.map (fn ((x, y), w) => (x, (y, w))) s
  		(*Create a sequence x -> sweepline sliding from x_min to x*)
  		val seqTable = Seq.iteratePrefixesIncl(fn ((key, tab), (k, t)) => 
  			        (k, Ytable.insert(tab, t)))(0, Ytable.empty())(xOnly)
  	    (*build seqTable into a table*)
  	    val table = Xtable.fromSeq seqTable
  	in
  		table
  	end


  (* fun getInfo Xtable.table * int * int * int -> int
   * get the weight of the root
   *)
  fun getInfo(T, coordinate, yHi, yLo) = 
  	case Xtable.split(T, coordinate) of
            (_, SOME table, _) => 
                     Ytable.reduceVal (Ytable.getRange table (yLo, yHi))
            |(L, NONE, _) => (
                case Xtable.last L of
                  NONE => 0
                | SOME (_, table) => 
                     Ytable.reduceVal (Ytable.getRange table (yLo, yHi)))



  
  (* fun rangeSum Seq.seq (point, point) -> fun rangesum' (point, point)
   * return the sum of weights of points which lie within the rectangle 
   given by top-left coordinate (a, b) and bottom-right coordinate (c, d) 
   in STAGED way
   *)
  fun rangeSum S ((xLeft, yHi), (xRight, yLo)) = 
  	let
  		val T = makeTable S
  		fun rangeSum' ((xLeft, yHi), (xRight, yLo)) = 
  			let
  				val l = getInfo(T, xLeft - 1, yHi, yLo)
  			    val r = getInfo(T, xRight, yHi, yLo)
  			in
  				r-l
  			end
  			
  	in
  		rangeSum' ((xLeft, yHi), (xRight, yLo))
  	end


end
