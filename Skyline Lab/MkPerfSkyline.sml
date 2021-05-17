functor MkPerfSkyline (structure Seq : SEQUENCE)
  :> PERF_SKYLINE where type 'a seq = 'a Seq.t
                  where type skyline = (int * int) Seq.t =
struct
  type 'a seq = 'a Seq.t
  type skyline = (int * int) Seq.t  

(*___________________________________________________________________________________*)

  fun singleton ((l, h, r): (int * int * int)) : skyline = 
  	Seq.append(Seq.singleton(l, h), Seq.singleton(r, 0))

   
 (* f : α option × α option → α option
  * Function from the lecture slides
  *)
  fun copy(a, b) =
 	case b of 
 		SOME(_) => b
 		| NONE => a


 (* combine : skyline × skyline → skyline
  * Combines two skylines into one
  *)
  fun combine (sky1, sky2) =
  	let
(*marking up left and right skyline points to distinguish them easily*)
  		val skyL = Seq.map(fn(a, b) => (a, b, "L"))(sky1)
  		val skyR = Seq.map(fn(a, b) => (a, b, "R"))(sky2)

(*Merge the sequences based on the x coordinates*)
  		val sky = Seq.merge(fn(a, b) => Int.compare(#1 a, #1 b))
  		                                                 (skyL, skyR)

(*If we reach a point in merge where we shift from R to L or vice versa
We want to compare this point to all the ucoming points before another switch.
So we populate the last R point over L's and L points over R creating
two sequences.
*)
  		val skySL = Seq.map(fn(a) => if (#3 a) = "L" then SOME (#1 a, #2 a) 
  		                             else NONE)(sky)
  		val skySR = Seq.map(fn(a) => if (#3 a) = "R" then SOME (#1 a, #2 a)  
  		                             else NONE)(sky)
  	    val scanL = Seq.scanIncl(copy)(NONE)(skySL)
  	    val scanR = Seq.scanIncl(copy)(NONE)(skySR)

(*Analyze those two sequences together, comparing L point to the 
current R ones and vise versa*)
  		val redundant = Seq.tabulate(fn i => 
            case (Seq.nth(scanL)(i), Seq.nth(scanR)(i)) of
            	(NONE, SOME(xR, yR)) => (xR, yR)
            	|(SOME(xL, yL), NONE) => (xL, yL)
            	|(NONE, NONE) => (~1, ~1)
            	|(SOME(xL, yL), SOME(xR, yR)) => 
				  	(case Int.compare(xR, xL) of
				  		GREATER => if yL > yR then (xR, yL) else (xR, yR)
				  		|_ => if yL > yR then (xL, yL) else (xL, yR))
            	  )
            (Seq.length(sky))
(* mark the points that are redundant and filter them out *)
        val toFilter = Seq.mapIdx(fn (i, a) => if i = 0 then a
        		else if (#2 a) = (#2 (Seq.nth(redundant)(i-1))) then (~1, ~1)
        		else a)(redundant)
    in
    	Seq.filter(fn a => not( a = (~1, ~1)))(toFilter)
        			
  	end

(*___________________________________________________________________________________*)


fun skyline_prim B =
	case Seq.splitMid B of
		Seq.EMPTY => Seq.empty ()
		|Seq.ONE x => singleton x
		|Seq.PAIR (L,R) => 
             combine (Primitives.par (fn () => skyline_prim L,
                                      fn () => skyline_prim R))

fun skyline_seq B =
	case Seq.splitMid B of
		Seq.EMPTY => Seq.empty ()
		|Seq.ONE x => singleton x
		|Seq.PAIR (L,R) => 
             combine (skyline_seq L, skyline_seq R)

  fun skyline (g : int) (bs : (int * int * int) Seq.t) : skyline =
  	if g < Seq.length(bs) then skyline_prim(bs)
  	else skyline_seq(bs)
    
end

