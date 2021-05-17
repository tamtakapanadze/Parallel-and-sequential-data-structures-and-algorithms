structure Bit =
struct
  datatype t = ZERO | ONE
  type bit = t

  fun fromInt 0 = ZERO
    | fromInt 1 = ONE
    | fromInt x = raise Fail ("Cannot represent " ^ Int.toString x ^ " as bit")

  fun toInt ZERO = 0
    | toInt ONE = 1

  fun toString ZERO = "0"
    | toString ONE = "1"

  structure Pack :>
  sig
    type t
    val capacity : int
    val length : t -> int
    val nth : t -> int -> bit
    val tabulate : (int -> bit) -> int -> t
    val iterate : ('a * bit -> 'a) -> 'a -> t -> 'a
    val take : t -> int -> t
    val drop : t -> int -> t
    val update : t * (int * bit) -> t
    val equal : t * t -> bool
    val toString : t -> string
  end =
  struct
    structure W = LargeWord
    type t = W.word

    fun determineCapacity cap maxLen =
      if maxLen >= cap then cap else determineCapacity (cap-1) (2*(maxLen+1) - 1)

    val capacity : int = determineCapacity (W.wordSize-1) 1
    val lbits : int = W.wordSize - capacity

    val wzero = W.fromInt 0
    val wone = W.fromInt 1

    val wfi = Word.fromInt

    val lengthMask = W.- (W.<< (wone, wfi lbits), wone)
    fun length ps = W.toInt (W.andb (ps, lengthMask))

    fun nth (ps : t) i =
      if i < 0 orelse i > length ps then raise Fail ("Attempting to extract " ^ Int.toString i ^ "th bit from pack of size " ^ Int.toString (length ps)) else
      if W.andb (W.>> (ps, wfi lbits + wfi i), wone) = wzero then ZERO else ONE

    fun tabulate f n =
      if n < 0 orelse n > capacity then raise Fail ("Bit pack capacity is " ^ Int.toString capacity ^ ", cannot create one of size " ^ Int.toString n) else
      let
        fun loop ps i =
          if i = n then ps else
          case f i of
            ZERO => loop ps (i+1) (* ps is initialized to all 0's *)
          | ONE => loop (W.orb (ps, W.<< (wone, wfi lbits + wfi i))) (i+1)
      in
        loop (W.fromInt n) 0
      end

    fun iterate f b ps =
      let
        val n = length ps
        fun loop x ps i =
          if i = n then x else
          let
            val x' = f (x, if W.andb (ps, wone) = wzero then ZERO else ONE)
            val ps' = W.>> (ps, 0w1)
          in
            loop x' ps' (i+1)
          end
      in
        loop b (W.>> (ps, wfi lbits)) 0
      end

    fun equal (ps1, ps2) = (ps1 = ps2)

    fun take ps i =
      if i < 0 orelse i > length ps then raise Fail ("Cannot take " ^ Int.toString i ^ " from pack of size " ^ Int.toString (length ps)) else
      W.orb (W.fromInt i, W.andb (ps, W.<< (W.- (W.<< (wone, wfi i), wone), wfi lbits)))

    fun drop ps i =
      if i < 0 orelse i > length ps then raise Fail ("Cannot drop " ^ Int.toString i ^ " from pack of size " ^ Int.toString (length ps)) else
      W.orb (W.fromInt (length ps - i), W.>> (W.andb (ps, W.<< (W.- (W.<< (wone, wfi (length ps - i)), wone), wfi lbits + wfi i)), wfi i))

    fun update (ps, (i, b)) =
      if i < 0 orelse i >= length ps then raise Fail ("Cannot update at " ^ Int.toString i ^ " in pack of size " ^ Int.toString (length ps)) else
      case b of
        ZERO => W.andb (ps, W.notb (W.<< (wone, wfi lbits + wfi i)))
      | ONE  => W.orb (ps, W.<< (wone, wfi lbits + wfi i))

    (* the toString used below is Bit.toString... sneaky. *)
    val toString = iterate (fn (s, b) => s ^ toString b) ""

  end (* Bit.Pack *)

end
