signature BIGNUM =
sig
  type t
  type bignum = t

  val trim : bignum -> bignum
  val toString : bignum -> string

  exception Negative
  val fromIntInf : IntInf.int -> bignum

  exception TrailingZeros
  val toIntInf : bignum -> IntInf.int

  val equal : bignum * bignum -> bool
end
