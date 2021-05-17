functor MkSeamFind(structure Seq : SEQUENCE) : SEAMFIND =
struct
  structure Seq = Seq
  open Seq

  (* Remove when you're done! *)
  exception NotYetImplemented

  val max = Real.maxFinite


  type pixel = { r : real, g : real, b : real }
  type image = { width : int, height : int, data : pixel seq seq }
  type gradient = real
  
 (* row =  int * list seq  [where each element has it's chapest path]*) 
  
  fun minCost i row G = 
  	if i = Seq.length G then row
  	else 
	  	let
	  		val m = Seq.length row
	  		fun get j = Seq.nth row j
	  		fun minVal j = 
	  		  let
		  		val left as (l, l1) = if j <> 0 then get (j-1) (*\*)
		  		           else (max, [])
		  		val right as (r, l2) = if j <> (m - 1) then get (j+1) (*/*)
		  			        else (max, [])
		  		val below as (b, r3) = get j (*|*)
		  		val min = Real.min(b, Real.min(l, r))
		  	  in
		  	  	if Real.compare (min, l) = EQUAL then left
		  	    else if Real.compare (min, b) = EQUAL then below
		  	    else right
		  	  end
		  	  val newRow = Seq.tabulate(fn k => 
	  			let
		  			val (min, path) = minVal k
		  			val w = Seq.nth (Seq.nth G i) k
		  		in
		  			(w + min, k::path)
		  		end) m
	  	in
	  		minCost (i + 1) newRow G
	  	end


  fun findSeam G = 
  	let
  		val firstRow = Seq.nth G 0
  		val rowSize = Seq.length firstRow

  		val row = Seq.tabulate (fn i => let
								  			val value = Seq.nth firstRow i
								  		in
								  			(value, [i])
								  		end) rowSize
  		val finalRow = minCost 1 row G
  		val (min, path) = Seq.reduce(fn (a as (v, _), b as (v', _)) => 
  			    if v < v' then a else b) (max, []) finalRow
  	in
  		Seq.fromList (List.rev path)
  	end
end


