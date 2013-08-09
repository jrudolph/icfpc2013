package net.virtualvoid.program

import scala.concurrent.forkjoin.ThreadLocalRandom
import scala.annotation.tailrec

case class Example(input: Long, output: Long) {
  def matchesProgram(program: Program): Boolean =
    Interpreter.eval(program)(input) == output
}

object Synthesis {
  val x = Ident("x")
  val y = Ident("y")
  val z = Ident("z")
  val xs = Seq(x)
  val xyz = Seq(x, y, z)

  case class NumSetup(remaining: Int, vars: Seq[Ident], inFold: Boolean, alreadyFold: Boolean) {
    def +(i: Int) = copy(remaining = remaining + i)
    def -(i: Int) = this + (-i)
  }
  case class SynthesisResult(result: Expr, setup: NumSetup)
  trait Context {
    def synthesize(setup: NumSetup): Seq[SynthesisResult]
  }
  abstract class Synthesizer(val minSize: Int, val maxSize: Int) {
    def generate(ctx: Context, setup: NumSetup): Seq[SynthesisResult]

    def canGenerate(setup: NumSetup): Boolean = setup.remaining >= minSize
  }

  object ZeroSynthesizer extends Synthesizer(1, 1) {
    def generate(ctx: Context, setup: NumSetup): Seq[SynthesisResult] = Seq(SynthesisResult(Zero, setup - 1))
  }
  object OneSynthesizer extends Synthesizer(1, 1) {
    def generate(ctx: Context, setup: NumSetup): Seq[SynthesisResult] = Seq(SynthesisResult(One, setup - 1))
  }
  object IdentSynthesizer extends Synthesizer(1, 1) {
    def generate(ctx: Context, setup: NumSetup): Seq[SynthesisResult] = setup.vars.map(x ⇒ SynthesisResult(x, setup - 1))
  }
  def unOpSynthesizer(unOp: UnaryOp): Synthesizer =
    new Synthesizer(2, Int.MaxValue) {
      def generate(ctx: Context, setup: NumSetup): Seq[SynthesisResult] =
        for {
          SynthesisResult(arg, s1) ← ctx.synthesize(setup - 1)
        } yield SynthesisResult(UnaryOpApply(unOp, arg), s1)
    }
  def binOpSynthesizer(binOp: BinaryOp): Synthesizer =
    new Synthesizer(3, Int.MaxValue) {
      def generate(ctx: Context, setup: NumSetup): Seq[SynthesisResult] =
        for {
          SynthesisResult(arg1, s1) ← ctx.synthesize(setup - 1)
          SynthesisResult(arg2, s2) ← ctx.synthesize(s1)
        } yield SynthesisResult(BinOpApply(binOp, arg1, arg2), s2)
    }
  object If0Synthesizer extends Synthesizer(4, Int.MaxValue) {
    def generate(ctx: Context, setup: NumSetup): Seq[SynthesisResult] =
      for {
        SynthesisResult(arg1, s1) ← ctx.synthesize(setup - 1)
        SynthesisResult(arg2, s2) ← ctx.synthesize(s1)
        SynthesisResult(arg3, s3) ← ctx.synthesize(s2)
      } yield SynthesisResult(If0(arg1, arg2, arg3), s3)
  }
  object TFoldSynthesizer extends Synthesizer(4, Int.MaxValue) {
    def generate(ctx: Context, setup: NumSetup): Seq[SynthesisResult] =
      for {
        SynthesisResult(arg1, s1) ← ctx.synthesize(NumSetup(setup.remaining - 2, xs, false, true))
        SynthesisResult(arg2, s2) ← ctx.synthesize(NumSetup(s1.remaining, xyz, true, true))
      } yield SynthesisResult(Fold(arg1, Zero, "y", "z", arg2), NumSetup(s2.remaining, xs, false, true))

    override def canGenerate(setup: NumSetup): Boolean =
      !setup.alreadyFold && setup.remaining >= 4
  }
  object FoldSynthesizer extends Synthesizer(5, Int.MaxValue) {
    def generate(ctx: Context, setup: NumSetup): Seq[SynthesisResult] =
      for {
        SynthesisResult(arg1, s1) ← ctx.synthesize(NumSetup(setup.remaining - 3, xs, false, true))
        SynthesisResult(arg2, s2) ← ctx.synthesize(NumSetup(s1.remaining, xs, false, true))
        SynthesisResult(arg3, s3) ← ctx.synthesize(NumSetup(s2.remaining, xyz, true, true))
      } yield SynthesisResult(Fold(arg1, arg2, "y", "z", arg3), NumSetup(s3.remaining, xs, false, true))

    override def canGenerate(setup: NumSetup): Boolean =
      !setup.alreadyFold && setup.remaining >= 5
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
    case "tfold" ⇒ TFoldSynthesizer
    case "fold"  ⇒ FoldSynthesizer
  }
  val binops = Set("and", "xor", "or", "plus")
  val unops = Set("not", "shl1", "shr1", "shr4", "shr16")

  def synthesizersForOperators(ops: Seq[String]) =
    Seq(ZeroSynthesizer, OneSynthesizer, IdentSynthesizer) ++ ops.map(synthesizer)

  def synthesize(synthesizers: Seq[Synthesizer], targetSize: Int): Seq[Program] = {
    val ctx = contextFor(synthesizers, targetSize)
    ctx.synthesize(NumSetup(targetSize - 1, xs, false, false)).map(x ⇒ Program("x", x.result))
  }
  def contextFor(synthesizers: Seq[Synthesizer], targetSize: Int): Context = {
    lazy val ctx: Context = new Context {
      def synthesize(setup: NumSetup): Seq[SynthesisResult] =
        if (setup.remaining == 0) Stream.empty
        else synthesizers.filter(_.canGenerate(setup)).toStream.flatMap(_.generate(ctx, setup))
    }
    ctx
  }

  def synthesize(targetSize: Int, ops: String*): Seq[Program] = synthesize(synthesizersForOperators(ops), targetSize)

  def numSolutionsNew(targetSize: Int, ops: String*): Long = {
    val numBins = ops.count(binops.contains)
    val numUns = ops.count(unops.contains)
    val numConst = 3

    sealed trait Choice {
      def isFold: Boolean = false
    }
    case object Const extends Choice
    case object UnOp extends Choice
    case object BinOp extends Choice
    case object If0 extends Choice
    case object TFold extends Choice {
      override def isFold: Boolean = true
    }
    case object Fold extends Choice {
      override def isFold: Boolean = true
    }

    def containOp(tag: String): Int = if (ops.contains(tag)) 1 else 0

    val allChoices = Seq(
      (Const, 3),
      (UnOp, numUns),
      (BinOp, numBins),
      (If0, containOp("if0")),
      (TFold, containOp("tfold")),
      (Fold, containOp("fold"))).filter(_._2 > 0).map(_._1)
    //println(s"The choices are $choices")

    case class Result(setup: NumSetup, total: Long)

    def choices(setup: NumSetup): Seq[Choice] = if (setup.alreadyFold) allChoices.filterNot(_.isFold) else allChoices

    var numsCache = collection.immutable.Map.empty[NumSetup, Seq[Result]]
    def nums(setup: NumSetup): Seq[Result] = {
      if (numsCache.contains(setup)) numsCache(setup)
      else {
        val res = numsCalc(setup)
        numsCache = numsCache + (setup -> res)
        res
      }
    }
    def numsCalc(setup: NumSetup): Seq[Result] =
      choices(setup).flatMap(numAt(setup)).groupBy(_.setup).map {
        case (setup, entries) ⇒ Result(setup, entries.map(_.total).sum)
      }.toSeq
    def numAt(setup: NumSetup)(choice: Choice): Seq[Result] = choice match {
      case Const ⇒ if (setup.remaining >= 1) Seq(Result(setup - 1, 3 + (if (setup.inFold) 2 else 0))) else Nil
      case UnOp ⇒
        if (setup.remaining >= 2)
          for {
            Result(s1, nums) ← nums(setup - 1)
          } yield Result(s1, nums * numUns)
        else Nil
      case BinOp ⇒
        if (setup.remaining >= 3)
          for {
            Result(s1, nums1) ← nums(setup - 2)
            Result(s2, nums2) ← nums(s1 + 1)
          } yield Result(s2, nums1 * nums2 * numBins)
        else Nil
      case If0 ⇒
        if (setup.remaining >= 4)
          for {
            Result(s1, nums1) ← nums(setup - 3)
            Result(s2, nums2) ← nums(s1 + 1)
            Result(s3, nums3) ← nums(s2 + 1)
          } yield Result(s3, nums1 * nums2 * nums3)
        else Nil
      case TFold ⇒
        if (setup.remaining >= 4)
          for {
            Result(NumSetup(r1, _, _, _), nums1) ← nums(NumSetup(setup.remaining - 3, Nil, false, true))
            Result(NumSetup(r2, _, _, _), nums2) ← nums(NumSetup(r1 + 1, Nil, true, true))
          } yield Result(NumSetup(r2, Nil, false, true), nums1 * nums2)
        else Nil
      case Fold ⇒
        if (setup.remaining >= 5)
          for {
            Result(NumSetup(r1, _, _, _), nums1) ← nums(NumSetup(setup.remaining - 4, Nil, false, true))
            Result(NumSetup(r2, _, _, _), nums2) ← nums(NumSetup(r1 + 1, Nil, false, true))
            Result(NumSetup(r3, _, _, _), nums3) ← nums(NumSetup(r2 + 1, Nil, true, true))
          } yield Result(NumSetup(r3, Nil, false, true), nums1 * nums2 * nums3)
        else Nil
    }
    val allRes = nums(NumSetup(targetSize - 1, Nil, false, false))
    //println(allRes)
    allRes.find(_.setup.remaining == 0).map(_.total).getOrElse(0L)
    //val x = Ident("x")
    //val ctx = contextFor(synthesizersForOperators(ops), targetSize, Seq(x))
    //ctx.numSolutions(1, targetSize - 1)
  }
  def numSolutions(targetSize: Int, ops: String*): Int = {
    var counter = 0
    synthesize(targetSize, ops: _*).foreach { _ ⇒
      counter += 1
      if (counter % 10000 == 0) println(s"Now at $counter")
      if (counter > 10000000) return 10000000
    }
    counter
  }

  def findMatching(examples: Seq[Example], targetSize: Int, ops: String*): Option[Program] = {
    def createFinder() = new Finder(examples, targetSize, ops, synthesize(targetSize, ops: _*).asInstanceOf[Stream[Program]])
    val finder = createFinder()
    val start = System.nanoTime()
    val result = finder.find()
    val end = System.nanoTime()
    val rate = finder.tried.toDouble * 1000000000d / (end - start)
    println(f"Speed: $rate%8f t / s")
    result
  }
  def tryTraining(t: TrainingProblem, num: Int = 10) = {
    val p = Parser(t.challenge)
    val exs = genExamples(p, num)
    findMatching(exs, t.size, t.operators: _*)
  }

  class Finder(examples: Seq[Example], targetSize: Int, ops: Seq[String], var s: Stream[Program]) {
    var tried = 0
    @tailrec final def find(): Option[Program] = {
      if (s.isEmpty) None
      else {
        tried += 1
        if (tried % 100000 == 0) println(s"Now at $tried")
        val candidate = s.head
        s = s.tail
        if (examples.forall(_.matchesProgram(candidate))) return Some(candidate)
        else find()
      }
    }
  }

  def genExamples(p: Program, num: Int = 10): Seq[Example] = {
    val e = Interpreter(p)
    testValues(num).map(x ⇒ Example(x, e(x)))
  }
  def testValues(num: Int = 10) = Seq.fill(num)(ThreadLocalRandom.current().nextLong())
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
