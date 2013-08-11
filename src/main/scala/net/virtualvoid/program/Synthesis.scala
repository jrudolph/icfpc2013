package net.virtualvoid.program

import scala.concurrent.forkjoin.ThreadLocalRandom
import scala.annotation.tailrec
import java.util.concurrent.atomic.{ AtomicReference, AtomicBoolean, AtomicLong }
import scala.collection.TraversableOnce

case class Example(input: Long, output: Long) {
  def matchesProgram(program: Program): Boolean =
    Interpreter.eval(program)(input) == output
}

object Synthesis {
  val x = Ident("x")
  val y = Ident("y")
  val z = Ident("z")
  val xs = singleColl(x)
  val xyz = many(x, y, z)

  /*type Coll[T] = TraversableOnce[T]
  def emptyColl[T]: Coll[T] = Traversable.empty
  def singleColl[T](t: T): Coll[T] = Traversable(t)*/
  type Coll[T] = Vector[T]
  def emptyColl[T]: Coll[T] = Vector.empty
  def singleColl[T](t: T): Coll[T] = Vector(t)
  def many[T](ts: T*): Coll[T] = ts.toVector

  //type Coll[T] = Stream[T]
  //def emptyColl[T]: Coll[T] = Stream.empty
  //def singleColl[T](t: T): Coll[T] = Stream(t)
  //def many[T](ts: T*): Coll[T] = ts.toStream

  case class NumSetup(remaining: Int, vars: Seq[Ident], inFold: Boolean, alreadyFold: Boolean, depth: Int = 0) {
    def +(i: Int) = copy(remaining = remaining + i, depth = depth + 1)
    def -(i: Int) = this + (-i)

    override def equals(obj: scala.Any): Boolean = obj match {
      case NumSetup(rem, vs, in, al, _) ⇒ remaining == rem && inFold == in && alreadyFold == al && vars.size == vs.size
      case _                            ⇒ false
    }
    override def hashCode(): Int = (remaining, vars.size, inFold, alreadyFold).hashCode()
  }
  case class SynthesisResult(result: Expr, setup: NumSetup)
  trait Context {
    def synthesize(setup: NumSetup)(f: SynthesisResult ⇒ Unit): Unit
  }
  abstract class Synthesizer(val minSize: Int, val maxSize: Int) {
    def generate(ctx: Context, setup: NumSetup, f: SynthesisResult ⇒ Unit): Unit

    def canGenerate(setup: NumSetup): Boolean = setup.remaining >= minSize
  }

  object ZeroSynthesizer extends Synthesizer(1, 1) {
    def generate(ctx: Context, setup: NumSetup, f: SynthesisResult ⇒ Unit): Unit = f(SynthesisResult(Zero, setup - 1))
  }
  object OneSynthesizer extends Synthesizer(1, 1) {
    def generate(ctx: Context, setup: NumSetup, f: SynthesisResult ⇒ Unit): Unit = f(SynthesisResult(One, setup - 1))
  }
  object IdentSynthesizer extends Synthesizer(1, 1) {
    def generate(ctx: Context, setup: NumSetup, f: SynthesisResult ⇒ Unit): Unit = setup.vars.foreach(x ⇒ f(SynthesisResult(x, setup - 1)))
  }
  def unOpSynthesizer(unOp: UnaryOp): Synthesizer =
    new Synthesizer(2, Int.MaxValue) {
      def generate(ctx: Context, setup: NumSetup, f: SynthesisResult ⇒ Unit): Unit =
        ctx.synthesize(setup - 1) {
          case SynthesisResult(arg, s1) ⇒ f(SynthesisResult(UnaryOpApply(unOp, arg), s1))
        }
    }
  def binOpSynthesizer(binOp: BinaryOp): Synthesizer =
    new Synthesizer(3, Int.MaxValue) {
      def generate(ctx: Context, setup: NumSetup, f: SynthesisResult ⇒ Unit): Unit =
        ctx.synthesize(setup - 1) {
          case SynthesisResult(arg1, s1) ⇒
            ctx.synthesize(s1) {
              case SynthesisResult(arg2, s2) ⇒ f(SynthesisResult(BinOpApply(binOp, arg1, arg2), s2))
            }
        }
    }
  object If0Synthesizer extends Synthesizer(4, Int.MaxValue) {
    def generate(ctx: Context, setup: NumSetup, f: SynthesisResult ⇒ Unit): Unit =
      ctx.synthesize(setup - 1) {
        case SynthesisResult(arg1, s1) ⇒
          ctx.synthesize(s1) {
            case SynthesisResult(arg2, s2) ⇒
              ctx.synthesize(s2) {
                case SynthesisResult(arg3, s3) ⇒ f(SynthesisResult(If0(arg1, arg2, arg3), s3))
              }
          }
      }
  }
  object TFoldSynthesizer extends Synthesizer(4, Int.MaxValue) {
    def generate(ctx: Context, setup: NumSetup, f: SynthesisResult ⇒ Unit): Unit =
      ctx.synthesize(NumSetup(setup.remaining - 2, xs, false, true)) {
        case SynthesisResult(arg1, s1) ⇒
          ctx.synthesize(NumSetup(s1.remaining, xyz, true, true)) {
            case SynthesisResult(arg2, s2) ⇒ f(SynthesisResult(Fold(arg1, Zero, "y", "z", arg2), NumSetup(s2.remaining, xs, false, true)))
          }
      }

    override def canGenerate(setup: NumSetup): Boolean =
      !setup.alreadyFold && setup.remaining >= 4
  }
  object FoldSynthesizer extends Synthesizer(5, Int.MaxValue) {
    def generate(ctx: Context, setup: NumSetup, f: SynthesisResult ⇒ Unit): Unit =
      ctx.synthesize(NumSetup(setup.remaining - 2, xs, false, true)) {
        case SynthesisResult(arg1, s1) ⇒
          ctx.synthesize(NumSetup(s1.remaining, xs, false, true)) {
            case SynthesisResult(arg2, s2) ⇒
              ctx.synthesize(NumSetup(s2.remaining, xyz, true, true)) {
                case SynthesisResult(arg3, s3) ⇒
                  f(SynthesisResult(Fold(arg1, arg2, "y", "z", arg3), NumSetup(s3.remaining, xs, false, true)))
              }
          }
      }

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

  def synthesize(synthesizers: Seq[Synthesizer], targetSize: Int, f: Program ⇒ Unit): Unit = {
    val ctx = contextFor(synthesizers, targetSize)
    ctx.synthesize(NumSetup(targetSize - 1, xs, false, false)) { x ⇒ f(Program("x", x.result)) }
  }
  def contextFor(synthesizers: Seq[Synthesizer], targetSize: Int): Context = {
    val synths = many(synthesizers: _*)
    lazy val ctx: Context = new Context {
      def synthesize(setup: NumSetup)(f: SynthesisResult ⇒ Unit): Unit =
        if (setup.remaining > 0) {
          if (setup.depth < 2) synths.filter(_.canGenerate(setup)).par.foreach(_.generate(ctx, setup, f))
          else synths.filter(_.canGenerate(setup)).foreach(_.generate(ctx, setup, f))
        }
    }
    ctx
  }

  def synthesize(targetSize: Int, ops: String*)(f: Program ⇒ Unit): Unit = synthesize(synthesizersForOperators(ops), targetSize, f)

  def findMatching(examples: Seq[Example], targetSize: Int, ops: String*): Option[Program] = {
    val tried = new AtomicLong()
    @volatile var res: Option[Program] = None
    @volatile var found = false

    def inner(): Option[Program] = {
      synthesize(targetSize, ops: _*) { candidate ⇒
        if (found) return res
        if (tried.incrementAndGet() % 100000 == 0) println(s"Now at $tried")
        if (examples.forall(_.matchesProgram(candidate))) {
          println(s"Found $candidate")
          res = Some(candidate)
          found = true
          return Some(candidate)
        }
      }
      res
    }

    val start = System.nanoTime()
    val result = inner()
    val end = System.nanoTime()
    val rate = tried.get.toDouble * 1000000000d / (end - start)
    println(f"Speed: $rate%8f t / s")
    result
  }

  class Finder(examples: Seq[Example], targetSize: Int, ops: Seq[String], var s: Coll[Program]) {
    var tried = new AtomicLong()
    final def find(): Option[Program] = {
      /*s.par.find { candidate ⇒
        if (tried.incrementAndGet() % 100000 == 0) println(s"Now at $tried")
        examples.forall(_.matchesProgram(candidate))
      }*/
      if (s.isEmpty) None
      else {
        if (tried.incrementAndGet() % 100000 == 0) println(s"Now at $tried")
        val candidate = s.head
        s = s.tail
        if (examples.forall(_.matchesProgram(candidate))) return Some(candidate)
        else find()
      }
    }
  }

  def numSolutionsNew(targetSize: Int, ops: String*): Long = {
    val numBins = ops.count(binops.contains)
    val numUns = ops.count(unops.contains)
    val numConst = 3

    object OneVar extends Seq[Ident] {
      def length: Int = 1
      def apply(idx: Int): Ident = ???
      def iterator: Iterator[Ident] = ???
    }
    object TwoVars extends Seq[Ident] {
      def length: Int = 2
      def apply(idx: Int): Ident = ???
      def iterator: Iterator[Ident] = ???
    }

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
      case Const ⇒ if (setup.remaining >= 1) Seq(Result(setup - 1, 3 + setup.vars.size)) else Nil
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
          } yield Result(s2, nums1 * nums2 * numBins / 2 /* because of commutativity ? */ )
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
            Result(NumSetup(r1, _, _, _, _), nums1) ← nums(NumSetup(setup.remaining - 3, emptyColl, false, true))
            Result(NumSetup(r2, _, _, _, _), nums2) ← nums(NumSetup(r1 + 1, OneVar, true, true))
          } yield Result(NumSetup(r2, emptyColl, false, true), nums1 * nums2)
        else Nil
      case Fold ⇒
        if (setup.remaining >= 5)
          for {
            Result(NumSetup(r1, _, _, _, _), nums1) ← nums(NumSetup(setup.remaining - 4, emptyColl, false, true))
            Result(NumSetup(r2, _, _, _, _), nums2) ← nums(NumSetup(r1 + 1, emptyColl, false, true))
            Result(NumSetup(r3, _, _, _, _), nums3) ← nums(NumSetup(r2 + 1, TwoVars, true, true))
          } yield Result(NumSetup(r3, emptyColl, false, true), nums1 * nums2 * nums3)
        else Nil
    }
    val allRes = nums(NumSetup(targetSize - 1, emptyColl, false, false))
    //println(allRes)
    allRes.find(_.setup.remaining == 0).map(_.total).getOrElse(0L)
  }

  def tryTraining(t: TrainingProblem, num: Int = 10) = {
    val p = Parser(t.challenge)
    val exs = genExamples(p, num)
    findMatching(exs, t.size, t.operators: _*)
  }

  def genExamples(p: Program, num: Int = 10): Seq[Example] = {
    val e = Interpreter(p)
    testValues(num).map(x ⇒ Example(x, e(x)))
  }
  def testValues(num: Int = 10) = Seq.fill(num)(ThreadLocalRandom.current().nextLong())

  TrainingProblem("(lambda (x_11) (if0 (and (plus (or (not x_11) (shr4 x_11)) x_11) 1) (not (xor (shl1 1) (shr4 (shl1 x_11)))) (xor (shr16 (not (plus x_11 x_11))) x_11)))", "uHCrZqnrKMBm6HIMBlLwpNUg", 25, Seq("bonus", "and", "if0", "not", "or", "plus", "shl1", "shr16", "shr4", "xor"))
  def analyseBonus(p: Program) = p.body match {
    case If0(BinOpApply(And, x, One), thenB, elseB) ⇒ (Metadata.size(p), Metadata.size(x), Metadata.size(thenB), Metadata.size(elseB))
  }

  def numSolutionsBonus(p: Problem): Long = {
    val meta = Set("bonus", "if0")
    val opsClean = p.operators.filterNot(meta.contains)
    val sizeClean = p.size - 4 // lambda, if0, and, 1

    def c(ifS: Int, thenS: Int) = Some((ifS, thenS, sizeClean - ifS - thenS)).filter(_._3 >= 5)

    val poss = Seq(
      c(5, 5), c(5, 7), c(7, 5), c(7, 7)).flatten

    def numSolutionsForCombi(c: (Int, Int, Int)): Long =
      numSolutionsNew(c._1, opsClean: _*) *
        numSolutionsNew(c._2, opsClean: _*) *
        numSolutionsNew(c._3, opsClean: _*)

    poss.map(numSolutionsForCombi).sum
  }
}
