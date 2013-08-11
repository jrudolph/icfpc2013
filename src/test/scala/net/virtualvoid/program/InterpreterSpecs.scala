package net.virtualvoid.program

import org.specs2.mutable.Specification

class InterpreterSpecs extends Specification {
  "The interpreter" should {
    "unops" in {
      "not x" in {
        val p = Interpreter.eval("(lambda (x) (not x))") _

        p(0xab) === 0xFFFFFFFFFFFFFF54L
        p(0x1122334455667788L) === 0xEEDDCCBBAA998877L
        p(0x8877665544332211L) === 0x778899AABBCCDDEEL
      }
      "shr4 1" in {
        val p = Interpreter.eval("(lambda (x) (shr4 1))") _

        p(0xab) === 0L
      }
      "shl1 x" in {
        val p = Interpreter.eval("(lambda (x) (shl1 x))") _

        p(0xab) === 0x0000000000000156L
        p(0x1122334455667788L) === 0x22446688AACCEF10L
        p(0x8877665544332211L) === 0x10EECCAA88664422L
        p(0xffffffffffffffffL) === 0xFFFFFFFFFFFFFFFEL
      }
      "shr1 x" in {
        val p = Interpreter.eval("(lambda (x) (shr1 x))") _

        p(0xab) === 0x0000000000000055L
        p(0x1122334455667788L) === 0x089119A22AB33BC4L
        p(0x8877665544332211L) === 0x443BB32AA2199108L
        p(0xffffffffffffffffL) === 0x7FFFFFFFFFFFFFFFL
      }
      "shr16 x" in {
        val p = Interpreter.eval("(lambda (x) (shr16 x))") _

        //[0x0000000000000000, 0x0000112233445566, 0x0000887766554433]
        p(0xab) === 0x0000000000000000L
        p(0x1122334455667788L) === 0x0000112233445566L
        p(0x8877665544332211L) === 0x0000887766554433L
        p(0xffffffffffffffffL) === 0x0000FFFFFFFFFFFFL
      }
      "shr4 not 1" in {
        val p = Interpreter.eval("(lambda (x) (shr4 (not 0)))") _

        p(0) === 0x0fffffffffffffffL
      }
      "shr4 not x" in {
        val p = Interpreter.eval("(lambda (x) (shr4 (not x)))") _

        p(0xab) === 0x0FFFFFFFFFFFFFF5L
        p(0x1122334455667788L) === 0x0EEDDCCBBAA99887L
        p(0x8877665544332211L) === 0x0778899AABBCCDDEL
        p(0L) === 0x0FFFFFFFFFFFFFFFL
      }
    }
    "binops" in {
      "and" in {
        val p = Interpreter.eval("(lambda (x) (and x (shr4 (not x))))") _

        //[0x00000000000000A1, 0x0020104010201080, 0x0070001000300010]
        p(0xab) === 0x00000000000000A1L
        p(0x1122334455667788L) === 0x0020104010201080L
        p(0x8877665544332211L) === 0x0070001000300010L
      }
      "or" in {
        val p = Interpreter.eval("(lambda (x) (or x (shr4 (not x))))") _

        //[0x0FFFFFFFFFFFFFFF, 0x1FEFFFCFFFEFFF8F, 0x8F7FEFDFEFBFEFDF]
        p(0xab) === 0x0FFFFFFFFFFFFFFFL
        p(0x1122334455667788L) === 0x1FEFFFCFFFEFFF8FL
        p(0x8877665544332211L) === 0x8F7FEFDFEFBFEFDFL
      }
      "xor" in {
        val p = Interpreter.eval("(lambda (x) (xor x (shr4 (not x))))") _

        //[0x0FFFFFFFFFFFFF5E, 0x1FCFEF8FEFCFEF0F, 0x8F0FEFCFEF8FEFCF]
        p(0xab) === 0x0FFFFFFFFFFFFF5EL
        p(0x1122334455667788L) === 0x1FCFEF8FEFCFEF0FL
        p(0x8877665544332211L) === 0x8F0FEFCFEF8FEFCFL
      }
      "plus" in {
        val p = Interpreter.eval("(lambda (x) (plus x (shr4 (not x))))") _

        //[0x10000000000000A0, 0x201010101010100F, 0x8FEFEFEFEFEFEFEF]
        p(0xab) === 0x10000000000000A0L
        p(0x1122334455667788L) === 0x201010101010100FL
        p(0x8877665544332211L) === 0x8FEFEFEFEFEFEFEFL
      }
    }
    "if0" in {
      val p = Interpreter.eval("(lambda (x) (if0 x 1 (and x (shr4 (not x)))))") _

      //[0x00000000000000A1, 0x0020104010201080, 0x0070001000300010]
      p(0) === 1
      p(0xab) === 0x00000000000000A1L
      p(0x1122334455667788L) === 0x0020104010201080L
      p(0x8877665544332211L) === 0x0070001000300010L
    }
    "if0" in {
      val p = Interpreter.eval("(lambda (x) (fold x 0 (lambda (y z) (or y z))))") _

      //[0x0000000000000000, 0x00000000000000AB, 0x00000000000000FF, 0x00000000000000FF]
      p(0) === 0x0000000000000000L
      p(0xab) === 0x00000000000000ABL
      p(0x1122334455667788L) === 0x00000000000000FFL
      p(0x8877665544332211L) === 0x00000000000000FFL
    }
  }
}
