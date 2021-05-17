functor MkBruteForcePD (structure Seq : SEQUENCE)
  :> PAREN_DIST where type 'a seq = 'a Seq.t =
struct
  type 'a seq = 'a Seq.t

  exception NotYetImplemented


(* fun allSub : Paren.t Seq.t -> Paren.t Seq
 * returns sequence containing a superset of the sequence
 *) 
  fun allSub (parens: Paren.t Seq.t) : Paren.t Seq.t Seq.t = 
  	case Seq.length(parens) of
	  	0 => Seq.empty()
		|_ =>   let
			        val len = Seq.length(parens)
			  		val subs = Seq.tabulate(fn i => 
			  			Seq.tabulate(fn j => Seq.subseq(parens)(i, j))
			      	        (len - i + 1))(len)
			  	in
			  		Seq.flatten(subs)
			  	end

			  	      
(* fun parenMatch : Paren.t Seq.t -> bool
 * returns true if parentheses are matched false otherwise
 *)  	

  fun parenMatch (ps: Paren.t Seq.t): bool =
	let
		fun parenMatch' ps =
			case Seq.splitMid ps of
				Seq.EMPTY => (0,0)
				| Seq.ONE Paren.L => (0,1)
				| Seq.ONE Paren.R => (1,0)
				| Seq.PAIR (a, b) =>
				    let val ((i,j),(k,l)) =
				    	Primitives.par (fn () => parenMatch' a,
				    		            fn () => parenMatch' b)
				    in
				    	if j <= k then (i+k-j,l) 
				    	else (i,l+j-k) 
				    end
	    in parenMatch' ps = (0,0)
	end 


(* fun parenDist : Paren.t seq -> int option
 * returns the maximum parenthesis distance of S if 
 * S is a matched, non-empty sequence. Otherwise, it returns NONE.
 *)
  fun parenDist (parens : Paren.t Seq.t) : int option =
  	if not (parenMatch parens) then NONE
	else 
		let
	    	val parts = allSub parens
	    	val filter = Seq.filter(fn x => parenMatch x 
	    		                   andalso Seq.length(x) > 0)(parts)
	    	val filtered = Seq.filter(fn x => 
	    		parenMatch(Seq.subseq(x)(1, Seq.length(x) - 2)))
	    	                                      (filter)                                     
	    	val max = Seq.reduce(fn (x, y) => 
	    		if Seq.length(x) < Seq.length(y) then y else x)
	    		                  (Seq.empty())(filtered)
	    in
	    	SOME (Seq.length max - 2)
	    end

end

(*
val _ = print(Seq.toString (fn sq => Seq.toString(fn p => Paren.toString(p)) sq) (parts))
*)