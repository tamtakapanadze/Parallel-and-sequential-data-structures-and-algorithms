structure Main =
struct
  structure CLA = CommandLineArgs

  structure Seq = ArraySequence
  structure SeqG = SequenceGranularity

  structure Sky = MkPerfSkyline (structure Seq = Seq)
  structure Gen = MkCityGen (structure Seq = Seq)

  fun run args =
      let
        (* parse command line *)
        val () = CLA.init ()
        val size = CLA.parseOrDefaultInt ("size", 10000)
        val granularity = CLA.parseOrDefaultInt ("granularity", 1000)
        val seed = CLA.parseOrDefaultInt ("seed", 15210)

        val () = print (String.concat ["Running with parameters",
                                       "\n",

                                       "  size = ",
                                       Int.toString size,
                                       "\n",

                                       "  granularity = ",
                                       Int.toString granularity,
                                       "\n"])

        val bs = Gen.city size seed

        val (result, statsString) = StatRunner.run (fn () => Sky.skyline granularity bs)
      in
        print (String.concat ["First element of result is ",
                              "(",
                              Int.toString (#1 (Seq.nth result 0)),
                              ", ",
                              Int.toString (#2 (Seq.nth result 0)),
                              ")",
                              "\n",
                              statsString,
                              "\n"])
      end
end

val () = Main.run (CommandLine.arguments ())
