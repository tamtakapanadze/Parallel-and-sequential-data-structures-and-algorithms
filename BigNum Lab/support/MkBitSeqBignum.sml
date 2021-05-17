functor MkBitSeqBignum (structure Seq : SEQUENCE)
  :> BIGNUM where type t = Bit.t Seq.t =
struct
  type bignum = Bit.t Seq.t
  type t = bignum

  exception TrailingZeros
  exception Negative

  fun properlyFormatted x =
    Seq.length x = 0 orelse Seq.nth x (Seq.length x - 1) = Bit.ONE

  fun fromIntInf (x : IntInf.int) : bignum =
    if x < 0 then raise Negative else
    let
      fun toList (x : IntInf.int) =
        if x = 0 then []
        else Bit.fromInt (IntInf.toInt (x mod 2)) :: toList (x div 2)
    in
      Seq.fromList (toList x)
    end

  fun toIntInf (n : bignum) : IntInf.int =
    if not (properlyFormatted n) then raise TrailingZeros else
    let val n' = Seq.map (IntInf.fromInt o Bit.toInt) n
    in Seq.iterate (fn (x, d) => 2 * x + d) (0 : IntInf.int) (Seq.rev n')
    end

  fun equal (n, m) = Seq.equal op= (n, m)

  fun trim x =
    let fun flag (i, b) = case b of Bit.ONE => i+1 | _ => 0
    in Seq.take x (Seq.reduce Int.max 0 (Seq.mapIdx flag x))
    end

  fun toString n = Seq.toString Bit.toString n
end
