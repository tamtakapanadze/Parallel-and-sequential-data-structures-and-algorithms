structure Bit =
struct
  datatype t = ZERO | ONE

  fun fromInt 0 = ZERO
    | fromInt 1 = ONE
    | fromInt x =
        raise Fail ("Cannot represent " ^ Int.toString x ^ " as bit.")

  fun toInt ZERO = 0
    | toInt ONE = 1

  fun toString ZERO = "0"
    | toString ONE = "1"
end
