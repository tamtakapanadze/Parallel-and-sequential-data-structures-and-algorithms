functor MkBignumAdd
  (structure Seq : SEQUENCE
   structure Bignum : BIGNUM where type t = Bit.t Seq.t) :>
sig
  val ++ : Bignum.t * Bignum.t -> Bignum.t
end =
struct

  infix 6 ++

  exception NotYetImplemented


(* fun abs (int, int) -> int
 * from given numbers substracts smaller from the bigger
 *)
  fun abs (a, b) = 
  	if a > b then a - b 
  	else b - a


 (* f : α option × α option → α option
  * Function from the lecture slides
  *)
  fun copy(a, b) =
 	case b of 
 		SOME(_) => b
 		|NONE => a
 

(* fun addzero('a seq * 'a seq * 'a seq) -> 'a seq
 * appends third sequence to the smaller from (x, y) sequences 
 *)
 fun addzero (x, y, zero) = 
	case (Seq.length(x) > Seq.length(y)) of
		true => (x, Seq.append(y, zero))
		|false => (Seq.append(x, zero), y)


(* fun getval(int) -> int
 * returns the value of the int option, NONE -> 0
 *)
fun getval (x) = 
	case x of 
		NONE => 0
		|SOME(x) => x


(* val ++ : Bignum.t * Bignum.t -> Bignum.t 
 * adds two binary numbers in Bignum.t representation
 *)
  fun x ++ y = 
  	let
  		(*produce as many zeroes as needed to make numbers equal in size*)
  		val zero = Seq.tabulate(fn x => Bit.ZERO)
  		        (abs(Seq.length(x), Seq.length(y)))
  		(*append zeroes to the smaller sequence*)
  		val (xzero, yzero) = addzero(x, y, zero)
  		(*pair the numbers that we would have added normally*)
  		val pairs = Seq.tabulate 
  		(fn i => (Seq.nth xzero i, Seq.nth yzero i))
  		                  (Seq.length(xzero))
  		(*We always have carried bit i case of (1, 1) and never in (0, 0)
  			other cases depend on the previous bit*)
  		val carry = Seq.tabulate(fn i => 
  			let 
  				val (num, nam) = Seq.nth pairs i
  		    in
  				case (num, nam) of
  				 (Bit.ZERO, Bit.ZERO) => SOME 0
  				 |(Bit.ONE, Bit.ONE) => SOME 1
  				 |_ => NONE
            end)(Seq.length(pairs))
        (*copy previous carried bits to figure out (1, 0) and (0, 1)
        	case carried bits*)
  	    val (add, extend) = Seq.scan copy NONE carry
  	    (*map the carry options to integers*)
  	    val toadd = Seq.map(fn x => 
  	    	if x = NONE orelse x = SOME 0 then 0
  	        else 1) add
  	    (*map the bit numbers to integers*)
  	    val numbers = Seq.map(fn (a, b) => 
  	    	case (a, b) of
  	    		(Bit.ZERO, Bit.ZERO) => (0, 0)
  	    		|(Bit.ZERO, Bit.ONE) => (0, 1)
  	    		|(Bit.ONE, Bit.ZERO) => (1, 0)
  	    		|(Bit.ONE, Bit.ONE) => (1, 1)
  	    	)pairs
  	    (*add numbers and carried bits*)
  	    val rawOut = Seq.tabulate(fn i => 
  	    	let
  	    		val (a, b) = Seq.nth numbers i
  	    	in
  	    		(a + b + Seq.nth toadd i) mod 2
  	    	end
            ) (Seq.length numbers)
  	    (*if addition gives us additional digit append at the end*)
  	    val output = Seq.append(rawOut, Seq.singleton(getval extend))
        (*Map the numbers back to Bignum.t type*)
        val bignum = Seq.map(fn i => if i = 0 then Bit.ZERO
  			                         else Bit.ONE ) output 
  	in
  		(*trim down any unnecessary zeroes*)
  		Bignum.trim(bignum)
  	end

end


