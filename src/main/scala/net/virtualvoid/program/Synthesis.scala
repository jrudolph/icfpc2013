package net.virtualvoid.program

case class Example(input: Long, output: Long)

object Synthesis {
  case class SynthesisResult(result: Expr, remainingSize: Int)
  trait Context {
    def vars: Seq[Ident]
    def synthesize(remainingSize: Int): Seq[SynthesisResult]
  }
  abstract class Synthesizer(val minSize: Int, val maxSize: Int) {
    def generate(ctx: Context, remainingSize: Int): Seq[SynthesisResult]

    def canGenerate(remainingSize: Int): Boolean = remainingSize >= minSize //&& remainingSize <= maxSize
  }

  object ZeroSynthesizer extends Synthesizer(1, 1) {
    def generate(ctx: Context, remainingSize: Int): Seq[SynthesisResult] = Seq(SynthesisResult(Zero, remainingSize - 1))
  }
  object OneSynthesizer extends Synthesizer(1, 1) {
    def generate(ctx: Context, remainingSize: Int): Seq[SynthesisResult] = Seq(SynthesisResult(One, remainingSize - 1))
  }
  object IdentSynthesizer extends Synthesizer(1, 1) {
    def generate(ctx: Context, remainingSize: Int): Seq[SynthesisResult] = ctx.vars.map(x ⇒ SynthesisResult(x, remainingSize - 1))
  }
  def binOpSynthesizer(binOp: BinaryOp): Synthesizer =
    new Synthesizer(3, Int.MaxValue) {
      def generate(ctx: Context, remainingSize: Int): Seq[SynthesisResult] =
        for {
          SynthesisResult(arg1, rem1) ← ctx.synthesize(remainingSize - 1)
          SynthesisResult(arg2, rem2) ← ctx.synthesize(rem1)
        } yield SynthesisResult(BinOpApply(binOp, arg1, arg2), rem2)
    }
  object If0Synthesizer extends Synthesizer(4, Int.MaxValue) {
    def generate(ctx: Context, remainingSize: Int): Seq[SynthesisResult] =
      for {
        SynthesisResult(arg1, rem1) ← ctx.synthesize(remainingSize - 1)
        SynthesisResult(arg2, rem2) ← ctx.synthesize(rem1)
        SynthesisResult(arg3, rem3) ← ctx.synthesize(rem2)
      } yield SynthesisResult(If0(arg1, arg2, arg3), rem3)
  }

  def synthesizer(op: String): Synthesizer = op match {
    case "if0"  ⇒ If0Synthesizer
    case "and"  ⇒ binOpSynthesizer(And)
    case "xor"  ⇒ binOpSynthesizer(Xor)
    case "plus" ⇒ binOpSynthesizer(Plus)
  }

  def synthesizersForOperators(ops: Seq[String]) =
    Seq(ZeroSynthesizer, OneSynthesizer, IdentSynthesizer) ++ ops.map(synthesizer)

  def synthesize(synthesizers: Seq[Synthesizer], targetSize: Int): Seq[Expr] = {
    val x = Ident("x")
    lazy val ctx: Context = new Context {
      val vars: Seq[Ident] = Seq(x)
      def synthesize(remainingSize: Int): Seq[SynthesisResult] =
        if (remainingSize == 0) Nil
        else synthesizers.filter(_.canGenerate(remainingSize)).flatMap(_.generate(ctx, remainingSize))
    }

    ctx.synthesize(targetSize).map(_.result)
  }
  def numSolutions(targetSize: Int, ops: String*): Int = synthesize(synthesizersForOperators(ops), targetSize).size
}
