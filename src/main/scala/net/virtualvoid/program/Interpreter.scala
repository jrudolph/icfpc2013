package net.virtualvoid.program

import scala.annotation.tailrec

object Interpreter {

  def apply(p: Program): Long ⇒ Long = eval(p) _
  def eval(p: Program)(value: Long): Long =
    eval(p.body, p.param, value, Nil)

  val byteShifts = (0 until 64 by 8).map(_.toLong)
  val byteMasks = (0 until 64 by 8).map(0xffL << _.toLong)

  def eval(expr: Expr, mainParam: String, mainVal: Long, bindings: Seq[(String, Long)]): Long = {
    def evalInner(e: Expr): Long = e match {
      case Zero                             ⇒ 0
      case One                              ⇒ 1
      case Ident(name) if name == mainParam ⇒ mainVal
      case If0(target, thenBody, elseBody) ⇒
        if (evalInner(target) == 0) evalInner(thenBody) else evalInner(elseBody)
      case Fold(source, init, param1, param2, body) ⇒
        val sourceV = evalInner(source)
        val initV = evalInner(init)

        val bytes =
          (byteShifts, byteMasks).zipped.map { (shift, mask) ⇒
            (sourceV & mask) >>> shift
          }
        bytes.foldLeft(initV) { (run, byte) ⇒
          val main = if (param1 == mainParam) byte else mainVal // allow shadowing in TFold
          eval(body, mainParam, main, Seq((param1, byte), (param2, run)))
        }

      case UnaryOpApply(op, arg) ⇒
        val a = evalInner(arg)
        op match {
          case Not   ⇒ ~a
          case Shl1  ⇒ a << 1
          case Shr1  ⇒ a >>> 1
          case Shr4  ⇒ a >>> 4
          case Shr16 ⇒ a >>> 16
        }
      case BinOpApply(op, arg1, arg2) ⇒
        val a1 = evalInner(arg1)
        val a2 = evalInner(arg2)
        op match {
          case And  ⇒ a1 & a2
          case Or   ⇒ a1 | a2
          case Xor  ⇒ a1 ^ a2
          case Plus ⇒ a1 + a2
        }
      case Ident(other) ⇒ bindings.find(_._1 == other).getOrElse(throw new RuntimeException(s"Missing var '$other' in $expr"))._2
    }

    evalInner(expr)
  }

  def simplify(p: Program): Program = p.copy(body = simplify(p.body))
  def simplify(expr: Expr): Expr = {
    val res = transformChildren(expr)(simplify) match {
      case BinOpApply(And, Zero, _)                        ⇒ Zero
      case BinOpApply(And, _, Zero)                        ⇒ Zero
      case BinOpApply(And, UnaryOpApply(Not, Zero), other) ⇒ other
      case BinOpApply(And, other, UnaryOpApply(Not, Zero)) ⇒ other
      case BinOpApply(Or, Zero, other)                     ⇒ other
      case BinOpApply(Or, other, Zero)                     ⇒ other
      case BinOpApply(Or, UnaryOpApply(Not, Zero), other)  ⇒ UnaryOpApply(Not, Zero)
      case BinOpApply(Or, other, UnaryOpApply(Not, Zero))  ⇒ UnaryOpApply(Not, Zero)
      case BinOpApply(Xor, Zero, other)                    ⇒ other
      case BinOpApply(Xor, other, Zero)                    ⇒ other
      case BinOpApply(Xor, UnaryOpApply(Not, Zero), other) ⇒ UnaryOpApply(Not, other)
      case BinOpApply(Xor, other, UnaryOpApply(Not, Zero)) ⇒ UnaryOpApply(Not, other)
      case BinOpApply(Plus, Zero, other)                   ⇒ other
      case BinOpApply(Plus, other, Zero)                   ⇒ other
      case UnaryOpApply(Not, UnaryOpApply(Not, inner))     ⇒ inner
      case If0(Zero, thenB, _)                             ⇒ thenB
      case If0(One, _, elseB)                              ⇒ elseB
      case f @ Fold(Ident("x"), Zero, v1, v2, body) if v1 != "x" && v2 != "x" && !usesVar("x", body) ⇒
        Fold(Ident("x"), Zero, "x", v2, renameVar(v1, "x", body))
      case x ⇒ x
    }
    if (res != expr) simplify(res)
    else expr
  }
  def renameVar(From: String, to: String, e: Expr): Expr =
    transformChildren(e, alsoLeaves = true) {
      case Ident(From) ⇒ Ident(to)
      case x           ⇒ x
    }

  def usesVar(target: String, e: Expr): Boolean = e match {
    case Zero                                     ⇒ false
    case One                                      ⇒ false
    case Ident(name)                              ⇒ target == name
    case UnaryOpApply(op, arg)                    ⇒ usesVar(target, arg)
    case BinOpApply(op, arg1, arg2)               ⇒ usesVar(target, arg1) || usesVar(target, arg2)
    case If0(cond, thenBody, elseBody)            ⇒ usesVar(target, cond) || usesVar(target, thenBody) || usesVar(target, elseBody)
    case Fold(source, init, param1, param2, body) ⇒ usesVar(target, source) || usesVar(target, init) || usesVar(target, body)
  }
  def transformChildren(expr: Expr, alsoLeaves: Boolean = false)(f: Expr ⇒ Expr): Expr =
    expr match {
      case Zero                            ⇒ if (alsoLeaves) f(expr) else expr
      case One                             ⇒ if (alsoLeaves) f(expr) else expr
      case Ident(name)                     ⇒ if (alsoLeaves) f(expr) else expr
      case UnaryOpApply(op, arg)           ⇒ UnaryOpApply(op, transformChildren(arg, alsoLeaves)(f))
      case BinOpApply(op, arg1, arg2)      ⇒ BinOpApply(op, transformChildren(arg1, alsoLeaves)(f), transformChildren(arg2, alsoLeaves)(f))
      case If0(target, thenBody, elseBody) ⇒ If0(transformChildren(target, alsoLeaves)(f), transformChildren(thenBody, alsoLeaves)(f), transformChildren(elseBody, alsoLeaves)(f))
      case Fold(source, init, param1, param2, body) ⇒
        Fold(transformChildren(source, alsoLeaves)(f), transformChildren(init, alsoLeaves)(f), param1, param2, transformChildren(body, alsoLeaves)(f))
    }
}
