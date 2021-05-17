functor MkBignumMul
  (structure Seq : SEQUENCE
   structure Bignum : BIGNUM where type t = Bit.t Seq.t
   val ++ : Bignum.t * Bignum.t -> Bignum.t
   val -- : Bignum.t * Bignum.t -> Bignum.t) :>
sig
  val ** : Bignum.t * Bignum.t -> Bignum.t
end =
struct

  infix 6 ++ --
  infix 7 **

  exception NotYetImplemented
  
  (* fun timesPow2 (int, Bignum.t) -> Bignum.t
   * Takes a bignum and multipliees it by integer (2^int)
   *)
  fun timesPow2 (n, num) = 
    let
      val zero = Seq.tabulate(fn x => Bit.ZERO)(n)
    in
      Bignum.trim(Seq.append(zero, num))
    end
  

  (* fun smallBig ('a Seq * 'a Seq) -> ('a Seq * 'a Seq)
   * Takes two sequences and sorts them based on their length
   *)
  fun smallBig (a, b) = 
    let
      val aSize = Seq.length(a)
      val bSize = Seq.length(b)
    in
      if aSize > bSize then (b, a)
      else (a, b)
    end
    

  (* val ** : Bignum.t * Bignum.t -> Bignum.t 
   * Takes two bignums and returns their product as a bignum
   *)
  fun x ** y = 
  if Seq.length(x) = 0 orelse Seq.length(y) = 0 then Seq.empty()
  else if Seq.length(x) = 1 then Bignum.trim(y)
  else if Seq.length(y) = 1 then Bignum.trim(x)
  else (
  let
     val (sml, big) = smallBig(x, y)
     (*Make the umbers equal in size*)
     val zero = Seq.tabulate(fn x => Bit.ZERO)
              (Seq.length(big) - Seq.length(sml))
     val sml = Seq.append(sml, zero)
     (*Store the n value, as the length of the bigger seqeunce*)
     val n = Seq.length(big)
   in
     case (Seq.splitMid sml, Seq.splitMid big) of
      (Seq.EMPTY, Seq.EMPTY) => Seq.empty() 
      |(Seq.ONE _, Seq.ONE _) => Seq.singleton(Bit.ONE)
      |(Seq.PAIR (Q, P), Seq.PAIR (S,R)) => (
        let
          val (p, r) = Primitives.par (fn () => Bignum.trim(P),
                                       fn () => Bignum.trim(R))
          val (q, s) = Primitives.par (fn () => Bignum.trim(Q),
                                       fn () => Bignum.trim(S))
          (* Follow the Karatsuba algorithm in bignums
          x = q | p  = 2^(n div 2)*p + q
          y = s | r  = 2^(n div 2)*r + s
          xy = 2^(2*(n div 2))* pr + 
          2^(n div 2)*((p+q)*(r+s)-pr -qs) + qs *)
          val (pr, pqrs, qs) = Primitives.par3 
                      (fn () => p ** r,
                       fn () => (p ++ q) ** (r ++ s),
                       fn () => q ** s)
          val mid = (pqrs -- pr) -- qs
        in
          (timesPow2(2*(n div 2), pr) ++ timesPow2(n div 2, mid)) ++ qs
        end)
   end 
   )
end

