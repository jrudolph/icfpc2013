package net.virtualvoid.program

import scala.concurrent.forkjoin.ThreadLocalRandom

case class Example(input: Long, output: Long) {
  def matchesProgram(program: Program): Boolean =
    Interpreter.eval(program)(input) == output
}

object Synthesis {
  case class SynthesisResult(result: Expr, remainingSize: Int)
  trait Context {
    def vars: Seq[Ident]
    def synthesize(remainingSize: Int): Stream[SynthesisResult]
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
  def unOpSynthesizer(unOp: UnaryOp): Synthesizer =
    new Synthesizer(2, Int.MaxValue) {
      def generate(ctx: Context, remainingSize: Int): Seq[SynthesisResult] =
        for {
          SynthesisResult(arg, rem) ← ctx.synthesize(remainingSize - 1)
        } yield SynthesisResult(UnaryOpApply(unOp, arg), rem)
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
    case "if0"   ⇒ If0Synthesizer
    case "and"   ⇒ binOpSynthesizer(And)
    case "xor"   ⇒ binOpSynthesizer(Xor)
    case "or"    ⇒ binOpSynthesizer(Or)
    case "plus"  ⇒ binOpSynthesizer(Plus)
    case "not"   ⇒ unOpSynthesizer(Not)
    case "shl1"  ⇒ unOpSynthesizer(Shl1)
    case "shr1"  ⇒ unOpSynthesizer(Shr1)
    case "shr4"  ⇒ unOpSynthesizer(Shr4)
    case "shr16" ⇒ unOpSynthesizer(Shr16)
  }

  def synthesizersForOperators(ops: Seq[String]) =
    Seq(ZeroSynthesizer, OneSynthesizer, IdentSynthesizer) ++ ops.map(synthesizer)

  def synthesize(synthesizers: Seq[Synthesizer], targetSize: Int): Stream[Program] = {
    val x = Ident("x")
    lazy val ctx: Context = new Context {
      val vars: Seq[Ident] = Seq(x)
      def synthesize(remainingSize: Int): Stream[SynthesisResult] =
        if (remainingSize == 0) Stream.empty
        else synthesizers.toStream.filter(_.canGenerate(remainingSize)).flatMap(_.generate(ctx, remainingSize))
    }

    ctx.synthesize(targetSize - 1).map(x ⇒ Program("x", x.result))
  }
  def synthesize(targetSize: Int, ops: String*): Stream[Program] = synthesize(synthesizersForOperators(ops), targetSize)

  def numSolutions(targetSize: Int, ops: String*): Int = synthesize(targetSize, ops: _*).size

  def findMatching(examples: Seq[Example], targetSize: Int, ops: String*): Option[Program] = {
    var tried = 0
    synthesize(targetSize, ops: _*).find { p ⇒
      tried += 1
      if (tried % 1000 == 0) println(s"Now at $tried")
      examples.forall(_.matchesProgram(p))
    }
  }

  def genExamples(p: Program, num: Int = 10): Seq[Example] = {
    val e = Interpreter(p)
    Seq.fill(num)(ThreadLocalRandom.current().nextLong()).map(x ⇒ Example(x, e(x)))
  }
}

/*
Success(
TrainingProblem(
(lambda (x_7052) (plus (plus (or 1 x_7052) x_7052) x_7052)),
TPdJVGzfEl7QynADKD83nRYt,
8,
List(or, plus)))
*/

/*
Success(TrainingProblem((lambda (x_10029) (if0 (or (shr4 0) (and 1 x_10029)) x_10029 1)),kOYPCUDqQcWQINfrhpRGdLuC,10,List(and, if0, or, shr4)))
 */
