package net.virtualvoid.program

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
          eval(body, mainParam, mainVal, Seq((param1, byte), (param2, run)))
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
      case Ident(other) ⇒ bindings.find(_._1 == other).get._2
    }

    evalInner(expr)
  }
}
