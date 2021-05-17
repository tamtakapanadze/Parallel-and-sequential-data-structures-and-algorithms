functor MkBitSeqGen (structure Seq : SEQUENCE) :>
sig
  (* (gen n x) produces a random bit sequence of length n. It guarantees the
   * last bit in the sequence is a 1 such that it is a valid BitSeqBignum.
   * x is a seed for random generation; any integer will do. *)
  val gen : int -> int -> Bit.t Seq.t
end =
struct

  fun gen n x =
    if n = 0 then Seq.empty () else
    let
      val r = DotMix.fromInt x
      val (r', rs) = DotMix.splitTab (r, n-1)
      fun genBit i =
        if i = n-1 orelse DotMix.bool (rs i) then Bit.ONE else Bit.ZERO
    in
      Seq.tabulate genBit n
    end

end
