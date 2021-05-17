structure Primitives =
struct
    val rand = Random.rand (15, 210)
    fun fork (f, g) =
      if Random.randRange (0, 1) rand = 0 then (f (), g ())
      else let
        val g' = g ()
        val f' = f ()
      in
        (f', g')
      end

    val par = fork

    fun par3 (f, g, h) =
        case fork (f, fn () => fork (g, h))
         of (rf, (rg, rh)) => (rf, rg, rh)

    fun par4 (f, g, h, i) =
        case fork (fn () => fork (f, g), fn () => fork (h, i))
          of ((rf, rg), (rh, ri)) => (rf, rg, rh, ri)

    (* RAM_NOTE: Still sequential! *)
    fun parTab (n, f) = let val v = Vector.tabulate (n, f)
                        in fn i => Vector.sub (v, i) end

    fun for (i, j) f =
      if i = j then () else (f i; for (i+1, j) f)

    fun loop (lo, hi) b f =
      if (lo >= hi) then b else loop (lo+1, hi) (f (b, lo)) f

    fun parfor grain (i, j) f =
      let val n = j - i
      in if n <= grain
         then for (i, j) f
         else ( par ( fn _ => parfor grain (i, i + n div 2) f
                    , fn _ => parfor grain (i + n div 2, j) f
                    )
              ; ()
              )
      end
end
