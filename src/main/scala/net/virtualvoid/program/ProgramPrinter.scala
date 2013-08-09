package net.virtualvoid.program

object ProgramPrinter {
  def print(p: Program): String = s"(lambda (${p.param}) ${print(p.body)}"
  def print(e: Expr): String = e match {
    case Zero                                     ⇒ "0"
    case One                                      ⇒ "1"
    case Ident(name)                              ⇒ name
    case If0(target, thenBody, elseBody)          ⇒ s"(if0 ${print(target)} ${print(thenBody)} ${print(elseBody)})"
    case Fold(source, init, param1, param2, body) ⇒ s"(fold ${print(source)} ${print(init)} (lambda ($param1 $param2) ${print(body)}))"
    case UnaryOpApply(op, arg)                    ⇒ s"(${op.name} ${print(arg)})"
    case BinOpApply(op, arg1, arg2)               ⇒ s"(${op.name} ${print(arg1)} ${print(arg2)})"
  }
}
