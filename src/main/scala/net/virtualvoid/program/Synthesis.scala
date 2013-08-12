package net.virtualvoid.program

import scala.concurrent.forkjoin.ThreadLocalRandom
import scala.annotation.tailrec
import java.util.concurrent.atomic.{ AtomicReference, AtomicBoolean, AtomicLong }
import scala.collection.TraversableOnce
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Deadline
import scala.concurrent.duration._

sealed trait Example {
  def matchesProgram(program: Program): Boolean
  def input: Long

  def not: Example
}

case class PositiveExample(input: Long, output: Long) extends Example {
  def matchesProgram(program: Program): Boolean =
    Interpreter.eval(program)(input) == output

  def not: Example = NegativeExample(input, output)
}
case class NegativeExample(input: Long, output: Long) extends Example {
  def matchesProgram(program: Program): Boolean =
    Interpreter.eval(program)(input) != output

  def not: Example = PositiveExample(input, output)
}
case class CalculatedExample(input: Long, condition: Long ⇒ Boolean) extends Example {
  def matchesProgram(program: Program): Boolean =
    condition(Interpreter.eval(program)(input))

  def not: Example = CalculatedExample(input, v ⇒ !condition(v))
}

object Synthesis {
  val x = Ident("x")
  val y = Ident("y")
  val z = Ident("z")
  val xs = singleColl(x)
  val xyz = many(x, y, z)
  val xy = many(x, y)

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
          ctx.synthesize(NumSetup(s1.remaining, xy, true, true)) {
            case SynthesisResult(arg2, s2) ⇒ f(SynthesisResult(Fold(arg1, Zero, "x", "y", arg2), NumSetup(s2.remaining, xs, false, true)))
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

  def findMatching(problem: Problem, examples: Seq[Example]): Option[Program] = {
    if (problem.isBonus)
      //findMatching(examples, 5, problem.operators.filterNot(bonusMetaOps.contains): _*) orElse
      //findMatching(examples, 6, problem.operators.filterNot(bonusMetaOps.contains): _*) orElse
      findMatchingBonus(examples, problem.size, problem.operators, bigBonusSettings)
    else
      findMatching(examples, 5, problem.operators: _*) orElse
        findMatching(examples, 8, problem.operators: _*) orElse
        findMatching(examples, 10, problem.operators.filterNot(bonusMetaOps.contains): _*) orElse
        findMatching(examples, 12, problem.operators.filterNot(b ⇒ b == "fold" || bonusMetaOps.contains(b)): _*)
    //.orElse(
    //findMatchingBonus(examples, problem.size, problem.operators, settingsForTotal(problem.size)))
    //findMatching(examples, problem.size, problem.operators: _*)
  }

  def findMatching(examples: Seq[Example], targetSize: Int, ops: String*): Option[Program] = {
    val start = System.nanoTime()
    val deadline = Deadline.now + 8.seconds

    val tried = new AtomicLong()
    @volatile var res: Option[Program] = None
    @volatile var found = false

    def inner(): Option[Program] = {
      synthesize(targetSize, ops: _*) { candidate ⇒
        if (found) return res
        //if (tried.incrementAndGet() % 100000 == 0) println(s"Now at $tried")
        if (examples.forall(_.matchesProgram(candidate))) {
          //println(s"Found $candidate")
          res = Some(candidate)
          found = true
          return Some(candidate)
        } else if (deadline.isOverdue()) return None
      }
      res
    }

    val result = inner()
    val end = System.nanoTime()
    val rate = tried.get.toDouble * 1000000000d / (end - start)
    //if (result.isDefined) println(s"Found ${result.get}")
    //println(f"Speed: $rate%8f t / s, total searched ${tried.get}")
    result
  }
  /*def tryBonusAgain(examples: Seq[Example], newExample: Example, lastResult: Program, targetSize: Int, ops: Seq[String]): Option[Program] = {

  }*/
  val bigBonusSettings =
    Settings(
      configs = Seq((13, 13), (12, 13), (13, 12), (13, 11), (11, 13), (12, 12), (12, 11), (11, 12)),
      offset = 4,
      exampleFunc = input ⇒ CalculatedExample(input, v ⇒ (v & 1L) == 0),
      programCons = (cond, thenB, elseB) ⇒ Program("x", If0(And(One, cond), thenB, elseB)))

  val smallBonusConfigs =
    Seq(
      (7, 7),
      (6, 8),
      (7, 5),
      (5, 7),
      (9, 9),
      (9, 8),
      (8, 9),
      (7, 9),
      (7, 8),
      (8, 7),
      (8, 6),
      (7, 6),
      (8, 8),
      (6, 7),
      (6, 6),
      (5, 6),
      (6, 5),
      (5, 5),
      (9, 7))

  case class Settings(configs: Seq[(Int, Int)], offset: Int, exampleFunc: Long ⇒ Example, programCons: (Expr, Expr, Expr) ⇒ Program)
  def bonusSettings(size: Int) = Settings(
    configs = allConfigsForTotal(size - 4),
    offset = 4,
    exampleFunc = input ⇒ CalculatedExample(input, v ⇒ (v & 1L) == 0),
    programCons = (cond, thenB, elseB) ⇒ Program("x", If0(And(One, cond), thenB, elseB)))

  def allConfigsForTotal(rem: Int): Seq[(Int, Int)] = {
    def get(rem: Int): Seq[Int] = 1 to rem
    (for {
      a ← get(rem)
      b ← get(rem - a - 1) if a < 8 && b < 8
      sum = a + b
      if rem - sum < 8
    } yield (a, b)).sortBy(a ⇒ -(a._1 + a._2)).filter(a ⇒ a._1 + a._2 >= 4)
  }

  def settingsForTotal(size: Int) = Settings(
    configs = allConfigsForTotal(size - 2),
    offset = 2,
    exampleFunc = input ⇒ CalculatedExample(input, _ == 0),
    programCons = (cond, thenB, elseB) ⇒ Program("x", If0(cond, thenB, elseB)))

  def findMatchingBonus(examples: Seq[Example], targetSize: Int, ops: Seq[String], settings: Settings = bonusSettings(25)): Option[Program] = {
    val deadline = Deadline.now + 10.seconds
    import settings._
    if (examples.size > 10) None

    val cleanOps = bonusCleanOps(ops)
    val allExamplesSet = examples.toSet

    case class PartialResult(part: Expr, matching: Seq[Example], notMatching: Seq[Example])

    def find(size: Int, examples: Seq[Example]): Seq[PartialResult] = {
      val results = new ArrayBuffer[PartialResult]

      val tried = new AtomicLong()
      synthesize(size, cleanOps: _*) { candidate ⇒
        if (tried.incrementAndGet() % 100000 == 0) println(s"Now at $tried")
        val (matching, notMatching) = examples.partition(_.matchesProgram(candidate))
        if (matching.size > 0 && Metadata.size(candidate) == size) results += PartialResult(candidate.body, matching, notMatching)
        /*if (matching.size >= threshold)
          return Some(PartialResult(candidate.body, matching, notMatching))*/
      }
      results.toSeq
    }

    def tryWithConfig(conditionSize: Int, firstSize: Int): Option[Program] = {
      if (deadline.isOverdue()) None
      else if (offset + conditionSize + firstSize <= targetSize) {
        case class Partitioning(first: Expr, second: Expr, conditionedExamples: Seq[Example])

        def findPartitioning(firstExamples: Set[Example]): Option[Partitioning] = {
          findMatching(firstExamples.toSeq, firstSize + 1, cleanOps: _*).flatMap {
            case Program(_, firstPart) ⇒
              val secondSize = targetSize - 4 - conditionSize - firstSize
              val secondExamples = allExamplesSet -- firstExamples
              findMatching(secondExamples.toSeq, secondSize + 1, cleanOps: _*).map {
                case Program(_, secondPart) ⇒
                  val trueExamples = firstExamples.map(e ⇒ exampleFunc(e.input))
                  val falseExamples = secondExamples.map(e ⇒ exampleFunc(e.input).not)
                  val allExamples = trueExamples ++ falseExamples

                  Partitioning(firstPart, secondPart, allExamples.toSeq)
              }
          }
        }
        def findCondition(part: Partitioning): Option[Program] = {
          findMatching(part.conditionedExamples, conditionSize + 1, cleanOps: _*).map {
            case Program(_, condition) ⇒
              println(s"Found condition: $condition")

              val result = programCons(condition, part.first, part.second)
              println(s"Full program is: $result")
              require(examples.forall(_.matchesProgram(result)),
                "Not all programs matched. Failure at " + {
                  val broken = examples.filterNot(_.matchesProgram(result))
                  broken.mkString(", ")
                })

              result
          }
        }
        examples.toSet.subsets //.toStream
          .toSeq
          .par
          .flatMap(findPartitioning)
          .flatMap(findCondition)
          .headOption

        /*find(firstSize + 1, examples).sortBy(-_.matching.size).toStream.flatMap {
          case PartialResult(part, firstMatches, notMatches) ⇒
            val p = Program("x", part)
            require(firstMatches.forall(_.matchesProgram(p)))

            val secondSize = targetSize - 4 - conditionSize - firstSize
            println(s"Found first (second size: $secondSize, missing: ${notMatches.size}): $part")
            //find(secondSize, examples, secondSize).flatMap {
            //case PartialResult(secondPart, m, m2) ⇒
            findMatching(notMatches, secondSize + 1, cleanOps: _*).flatMap {
              case Program(_, secondPart) ⇒

                println(s"Found second: $secondPart")
                val trueExamples = firstMatches.map(e ⇒ CalculatedExample(e.input, v ⇒ (v & 1L) == 0))
                val falseExamples = notMatches.map(e ⇒ CalculatedExample(e.input, v ⇒ (v & 1L) != 0))
                val allExamples = trueExamples ++ falseExamples
                findMatching(allExamples, conditionSize + 1, cleanOps: _*).map {
                  case Program(_, condition) ⇒
                    println(s"Found condition: $condition")

                    val result = Program("x", If0(And(One, condition), part, secondPart))
                    println(s"Full program is: $result")
                    require(examples.forall(_.matchesProgram(result)),
                      "Not all programs matched. Failure at " + {
                        val broken = examples.filterNot(_.matchesProgram(result))
                        broken.mkString(", ")
                      })

                    result
                }
            }
        }.headOption*/
      } else None
    }

    //val configs =
    /*if (targetSize >= 25)
        Seq(
          (7, 7), (7, 5), (5, 7), (6, 7), (7, 6), (8, 6), (6, 8), (6, 6))
      else if (targetSize == 19) Seq(
        (5, 5),
        (5, 7),
        (7, 5),
        (7, 7))

        17 =
        7 7 3
        5 5 7
        4 6 7
        6 4 7
        5 6 6
        6 5 6
        4 7 6
        7 4 6
        7 5 5
        6 6 5
        5 7 5
        8 4 5


      else */

    configs.foreach { c ⇒
      println(s"Now trying config $c")
      tryWithConfig(c._1, c._2) match {
        case None        ⇒
        case s @ Some(p) ⇒ return s
      }
    }

    None
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
          } yield Result(s3, nums1 * Math.max(nums2, nums3))
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
    findMatching(t.problem, exs)
  }

  def genExamples(p: Program, num: Int = 10): Seq[Example] = {
    val e = Interpreter(p)
    testValues(num).map(x ⇒ PositiveExample(x, e(x)))
  }
  def testValues(num: Int = 10) = {
    require(num >= 2)
    Seq.fill(num - 2)(ThreadLocalRandom.current().nextLong()) ++ Seq(-1L, 0x1122334455667788L)
  }

  def analyseBonus(p: Program) = p.body match {
    case If0(BinOpApply(And, x, One), thenB, elseB) ⇒ (Metadata.size(p), Metadata.size(x), Metadata.size(thenB), Metadata.size(elseB))
  }

  def numSolutionsBonus(p: Problem): Long = {
    val opsClean = bonusCleanOps(p.operators)
    val sizeClean = p.size - 4 // lambda, if0, and, 1

    def c(ifS: Int, thenS: Int) = Some((ifS, thenS, sizeClean - ifS - thenS)).filter(_._3 >= 5)

    val poss = Seq(
      c(5, 5), c(5, 7), c(7, 5), c(7, 7)).flatten

    def numSolutionsForCombi(c: (Int, Int, Int)): Long =
      numSolutionsNew(c._1, opsClean: _*) +
        numSolutionsNew(c._2, opsClean: _*) +
        numSolutionsNew(c._3, opsClean: _*)

    poss.map(numSolutionsForCombi).sum
  }
  val bonusMetaOps = Set("bonus", "if0")
  def bonusCleanOps(ops: Seq[String]): Seq[String] =
    ops.filterNot(bonusMetaOps.contains)
}
