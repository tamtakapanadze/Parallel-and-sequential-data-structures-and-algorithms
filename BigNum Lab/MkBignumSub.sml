functor MkBignumSub
  (structure Seq : SEQUENCE
   structure Bignum : BIGNUM where type t = Bit.t Seq.t
   val ++ : Bignum.t * Bignum.t -> Bignum.t) :>
sig
  val -- : Bignum.t * Bignum.t -> Bignum.t
end =
struct

  infix 6 ++ --

  exception NotYetImplemented


(* val -- : Bignum.t * Bignum.t -> Bignum.t
 * substracts two binary numbers in Bignum.t representation
 *)
  fun x -- y = 

  	let
  		(*produce as many zeroes as needed to make numbers equal in size*)
  		val zero = Seq.tabulate(fn x => Bit.ZERO)
  		        (Seq.length(x) - Seq.length(y))
  		(*append zeroes to the smaller sequence*)
  		val yzero = Seq.append(y, zero)
  		(* ~y *)
  		val flipY = Seq.map(fn x => case x of
  			                       Bit.ONE => Bit.ZERO
  			                       |Bit.ZERO => Bit.ONE) yzero 
  		(* -y = ~y + 1*)
  		val add1 = Bignum.trim(flipY) ++ Seq.singleton(Bit.ONE)
  		(* x - y = x + ~y + 1*)
  		val add = x ++ add1
  		val len = Seq.length add
  		(*val xlen = Seq.length x*)
  	in
  		(*remove the carried bit and trim the zeroes*)
  		Bignum.trim(Seq.take(add)(len - 1))
  	end

end