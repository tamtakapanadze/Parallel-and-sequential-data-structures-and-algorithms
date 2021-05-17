functor MkPerfBignumAdd
  (structure Seq : SEQUENCE
   structure Bignum : BIGNUM where type t = Bit.Pack.t Seq.t) :>
sig
  val ++ : int -> Bignum.t * Bignum.t -> Bignum.t
end =
struct
  exception NotYetImplemented

  val C = Bit.Pack.capacity

  fun packSeq (s : Bit.t Seq.t) : Bit.Pack.t Seq.t =
    let
      fun iSize i = Int.min (C, Seq.length s - (i * C))
      fun iPack i = Bit.Pack.tabulate (fn j => Seq.nth s (i * C + j)) (iSize i)
    in
      Seq.tabulate iPack (1 + (Seq.length s - 1) div C)
    end

  fun unpackSeq (s : Bit.Pack.t Seq.t) : Bit.t Seq.t =
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

  (* Note that the bignum type is now `Bit.Pack.t Seq.t`. Make sure you return
   * a properly formatted bit-packed bignum! *)
  fun ++ (g : int) (x : Bit.Pack.t Seq.t, y : Bit.Pack.t Seq.t) =
    raise NotYetImplemented

end
