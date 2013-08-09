package net.virtualvoid.program

object Metadata {
  /*
                             |0| = 1
                             |1| = 1
                             |x| = 1
                |(if0 e0 e1 e2)| = 1 + |e0| + |e1| + |e2|
|(fold e0 e1 (lambda (x y) e2))| = 2 + |e0| + |e1| + |e2|
                      |(op1 e0)| = 1 + |e0|
                   |(op2 e0 e1)| = 1 + |e0| + |e1|
                |(lambda (x) e)| = 1 + |e|
   */
  def size(p: Program): Int = 1 + size(p.body)
  def size(e: Expr): Int = e match {
    case Zero                                     ⇒ 1
    case One                                      ⇒ 1
    case Ident(name)                              ⇒ 1
    case If0(target, thenBody, elseBody)          ⇒ 1 + size(target) + size(thenBody) + size(elseBody)
    case Fold(source, init, param1, param2, body) ⇒ 2 + size(source) + size(init) + size(body)
    case UnaryOpApply(op, arg)                    ⇒ 1 + size(arg)
    case BinOpApply(op, arg1, arg2)               ⇒ 1 + size(arg1) + size(arg2)
  }
}
