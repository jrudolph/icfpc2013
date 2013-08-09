package net.virtualvoid.program

/*
 program    P ::= "(" "lambda" "(" id ")" e ")"
 expression e ::= "0" | "1" | id
               | "(" "if0" e e e ")"
               | "(" "fold" e e "(" "lambda" "(" id id ")" e ")" ")"
               | "(" op1 e ")"
               | "(" op2 e e ")"
          op1 ::= "not" | "shl1" | "shr1" | "shr4" | "shr16"
          op2 ::= "and" | "or" | "xor" | "plus"
          id  ::= [a-z][a-z_0-9]*
*/
case class Program(param: String, body: Expr)
sealed trait Expr
case object Zero extends Expr
case object One extends Expr
case class Ident(name: String) extends Expr
case class If0(target: Expr, thenBody: Expr, elseBody: Expr) extends Expr
case class Lambda2(param1: String, param2: String, body: Expr)
case class Fold(source: Expr, init: Expr, lambda: Lambda2) extends Expr

case class UnaryOpApply(op: UnaryOp, arg: Expr) extends Expr
case class BinOpApply(op: BinaryOp, arg1: Expr, arg2: Expr) extends Expr

sealed trait UnaryOp
case object Not extends UnaryOp
case object Shl1 extends UnaryOp
case object Shr1 extends UnaryOp
case object Shr4 extends UnaryOp
case object Shr16 extends UnaryOp

sealed trait BinaryOp
case object And extends BinaryOp
case object Or extends BinaryOp
case object Xor extends BinaryOp
case object Plus extends BinaryOp
