structure Main =
struct
  structure CLA = CommandLineArgs

  structure Seq = ArraySequence
  structure SeqG = SequenceGranularity
  structure Bignum = MkBitPackSeqBignum (structure Seq = Seq)
  structure Bit = Bit

  structure Add = MkPerfBignumAdd (structure Seq = Seq
                                   structure Bignum = Bignum)
  structure Gen = MkBitSeqGen (structure Seq = Seq)

  val C = Bit.Pack.capacity

  fun packSeq (s : Bit.t Seq.t) : Bit.Pack.t Seq.t =
    let
      fun iSize i = Int.min (C, Seq.length s - (i * C))
      fun iPack i = Bit.Pack.tabulate (fn j => Seq.nth s (i * C + j)) (iSize i)
    in
      Seq.tabulate iPack (1 + (Seq.length s - 1) div C)
    end

  fun run args =
    let
      (* parse command line *)
      val () = CLA.init ()
      val size = CLA.parseOrDefaultInt ("size", 1000000)
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

      val bs1 = Gen.gen size seed
      val bs2 = Gen.gen size (seed+1)
      val in1 = packSeq bs1
      val in2 = packSeq bs2

      val (result, statsString) =
        StatRunner.run (fn () => Add.++ granularity (in1, in2))
    in
      print (String.concat ["First element of result is ",
                            Bit.Pack.iterate (fn (s, b) => s ^ Bit.toString b) "" (Seq.nth result 0),
                            "\n",
                            statsString,
                            "\n"])
    end
end

val () = Main.run (CommandLine.arguments ())
