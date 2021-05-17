functor MkBitPackSeqBignum (structure Seq : SEQUENCE)
  :> BIGNUM where type t = Bit.Pack.t Seq.t =
struct
  type bignum = Bit.Pack.t Seq.t
  type t = bignum

  exception TrailingZeros
  exception Negative

  fun properlyFormatted x =
    Seq.length x = 0 orelse
    let val lastPack = Seq.nth x (Seq.length x - 1)
    in Bit.Pack.length lastPack > 0 andalso
       (Bit.Pack.nth lastPack (Bit.Pack.length lastPack - 1)) = Bit.ONE
    end

  val C = Bit.Pack.capacity

  fun pack (s : Bit.t Seq.t) : bignum =
    let
      fun iSize i = Int.min (C, Seq.length s - (i * C))
      fun iPack i = Bit.Pack.tabulate (fn j => Seq.nth s (i * C + j)) (iSize i)
    in
      Seq.tabulate iPack (1 + (Seq.length s - 1) div C)
    end

  fun fromIntInf (x : IntInf.int) : bignum =
    if x < 0 then raise Negative else
    let
      fun toList (x : IntInf.int) =
        if x = 0 then []
        else Bit.fromInt (IntInf.toInt (x mod 2)) :: toList (x div 2)
    in
      pack (Seq.fromList (toList x))
    end

  fun toString (b : bignum) =
    Seq.toString (Bit.Pack.iterate (fn (s, b) => s ^ Bit.toString b) "") b

  fun unpack (s : bignum) : Bit.t Seq.t =
    let
      val numBits =
        case Seq.length s of
          0 => 0
        | m => ((m - 1) * C) + (Bit.Pack.length (Seq.nth s (m - 1)))

      fun selectBit i =
        Bit.Pack.nth (Seq.nth s (i div C)) (i mod C)
    in
      Seq.tabulate selectBit numBits
    end

  fun toIntInf (n : bignum) : IntInf.int =
    let
      fun extendBit ((x : IntInf.int, m : IntInf.int), b) =
        (x + m * (case b of Bit.ZERO => 0 | Bit.ONE => 1), 2 * m)
      (*fun extendBit (x : IntInf.int, b) =
        2 * x + (case b of Bit.ZERO => 0 | Bit.ONE => 1)*)
      fun extendPack (x, bs) = Bit.Pack.iterate extendBit x bs
    in
      if not (properlyFormatted n) then raise TrailingZeros else
      (*Seq.iterate extendPack (0 : IntInf.int) (Seq.rev n)*)
      #1 (Seq.iterate extendPack (0 : IntInf.int, 1 : IntInf.int) n)
    end

  fun equal (n, m) = Seq.equal Bit.Pack.equal (n, m)

  fun trim x =
    let
      fun packContainsOne p =
        Bit.Pack.iterate (fn (x, b) => x orelse b = Bit.ONE) false p
      fun flag (i, p) = if packContainsOne p then i+1 else 0
      val trimmedSeq = Seq.take x (Seq.reduce Int.max 0 (Seq.mapIdx flag x))

      fun trimPack p =
        let fun f ((i, max), b) =
              case b of Bit.ZERO => (i+1, max) | Bit.ONE => (i+1, i+1)
        in Bit.Pack.take p (#2 (Bit.Pack.iterate f (0, 0) p))
        end
      fun last s = Seq.nth s (Seq.length s - 1)
    in
      case Seq.length trimmedSeq of
        0 => trimmedSeq
      | n => Seq.update (trimmedSeq, (n-1, trimPack (last trimmedSeq)))
    end

end
