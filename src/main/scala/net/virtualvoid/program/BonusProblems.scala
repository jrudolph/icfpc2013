package net.virtualvoid.program

import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.Future

object BonusProblems {
  val small = Seq(
    TrainingProblem("(lambda (x_4) (if0 (and (plus (shr16 (shr16 (shl1 x_4))) (shr16 x_4)) 1) (not (plus x_4 (not 1))) (and (not 1) (not x_4))))", "hUUWUAUw5JAaqkh0RuNSCJfj", 21, Seq("bonus", "and", "if0", "not", "plus", "shl1", "shr16")), TrainingProblem("(lambda (x_15) (if0 (and (not (xor (shl1 1) (shr4 (shl1 x_15)))) 1) (or (shr16 (shl1 x_15)) x_15) (xor (shr16 (shr16 x_15)) 1)))", "YOZhkIXLBYFKkVQldfhAgCRr", 21, Seq("bonus", "and", "if0", "not", "or", "shl1", "shr16", "shr4", "xor")), TrainingProblem("(lambda (x_11) (if0 (and (or (shr4 (shr4 x_11)) x_11) 1) (xor (or x_11 (plus x_11 x_11)) x_11) (plus (xor x_11 (plus 1 x_11)) x_11)))", "RJNLHIfvZl3FNAwx3AQw8ynr", 23, Seq("bonus", "and", "if0", "or", "plus", "shr4", "xor")), TrainingProblem("(lambda (x_15) (if0 (and (not (xor (shl1 1) (shr4 (shl1 x_15)))) 1) (or (shr16 (shl1 x_15)) x_15) (xor (shr16 (shr16 x_15)) 1)))", "yvhzISnuMCcxHz9dxlZ8WcaA", 21, Seq("bonus", "and", "if0", "not", "or", "shl1", "shr16", "shr4", "xor")), TrainingProblem("(lambda (x_9) (if0 (and (plus (shl1 (shr4 x_9)) x_9) 1) (xor x_9 (shr1 (not (plus 1 x_9)))) (or 1 (shl1 (shr16 x_9)))))", "JIn30G6udsLieAwqtuqgB2vP", 21, Seq("bonus", "and", "if0", "not", "or", "plus", "shl1", "shr1", "shr16", "shr4", "xor")), TrainingProblem("(lambda (x_14) (if0 (and (plus (shr1 (shr1 x_14)) x_14) 1) (or 1 (shl1 (and (shr4 x_14) 1))) (xor (plus x_14 1) 1)))", "gvfOnhGV4E0n1OUDIAAmJzRd", 21, Seq("bonus", "and", "if0", "or", "plus", "shl1", "shr1", "shr4", "xor")), TrainingProblem("(lambda (x_14) (if0 (and (plus x_14 (shl1 (shl1 (and x_14 1)))) 1) (and (shl1 x_14) (shr16 x_14)) (xor 1 (plus x_14 x_14))))", "Iv5yv8S80HBsffNsjwCIu6n8", 21, Seq("bonus", "and", "if0", "plus", "shl1", "shr16", "xor")), TrainingProblem("(lambda (x_14) (if0 (and (plus x_14 (shl1 (shl1 (and x_14 1)))) 1) (and (shl1 x_14) (shr16 x_14)) (xor 1 (plus x_14 x_14))))", "Iv5yv8S80HBsffNsjwCIu6n8", 21, Seq("bonus", "and", "if0", "plus", "shl1", "shr16", "xor")), TrainingProblem("(lambda (x_5) (if0 (and (and 1 (shr16 (shr16 x_5))) 1) (plus (shr4 (shr16 x_5)) 1) (and (shr1 x_5) (and (shl1 x_5) x_5))))", "8QXp5577X9CeGCjACNehyNqd", 21, Seq("bonus", "and", "if0", "plus", "shl1", "shr1", "shr16", "shr4")), TrainingProblem("(lambda (x_17) (if0 (and (or x_17 (shr16 (shr1 (shr4 (shr1 x_17))))) 1) (or (shl1 (not 1)) x_17) (xor (shl1 (or 1 (shr4 x_17))) x_17)))", "g1y6BB8hAxA2iNbedAfga0gJ", 23, Seq("bonus", "and", "if0", "not", "or", "shl1", "shr1", "shr16", "shr4", "xor")), TrainingProblem("(lambda (x_4) (if0 (and (and (shr1 (not x_4)) x_4) 1) (and x_4 (plus x_4 1)) (and (shr4 x_4) (plus (not x_4) (not 0)))))", "GP6l8AKu97GXA0IIKHfUlrBB", 22, Seq("bonus", "and", "if0", "not", "plus", "shr1", "shr4")), TrainingProblem("(lambda (x_9) (if0 (and (plus (shl1 (shr4 x_9)) x_9) 1) (xor x_9 (shr1 (not (plus 1 x_9)))) (or 1 (shl1 (shr16 x_9)))))", "kxN6FFtoTD3fo8defWktY8FB", 21, Seq("bonus", "and", "if0", "not", "or", "plus", "shl1", "shr1", "shr16", "shr4", "xor")), TrainingProblem("(lambda (x_13) (if0 (and (or (shl1 (not 1)) x_13) 1) (or x_13 (shr4 (shr16 (plus x_13 x_13)))) (xor (shr16 (not (plus x_13 x_13))) x_13)))", "fWUgfg8D5iiSV93XsDYucV6p", 23, Seq("bonus", "and", "if0", "not", "or", "plus", "shl1", "shr16", "shr4", "xor")), TrainingProblem("(lambda (x_15) (if0 (and (not (xor (shl1 1) (shr4 (shl1 x_15)))) 1) (and (or x_15 (shr1 (not x_15))) 1) (xor (or 1 x_15) x_15)))", "4v6jkoyf6FiTATcT05JoYQ8b", 23, Seq("bonus", "and", "if0", "not", "or", "shl1", "shr1", "shr4", "xor")), TrainingProblem("(lambda (x_15) (if0 (and (or (not 1) (if0 x_15 1 x_15)) 1) (xor (plus x_15 1) 1) (and x_15 (plus x_15 x_15))))", "apfgzsKDCyAI9gksHc0wEE09", 21, Seq("bonus", "and", "if0", "not", "or", "plus", "xor")), TrainingProblem("(lambda (x_4) (if0 (and (and (not (shr1 x_4)) (shr16 (not 0))) 1) (xor (plus 1 x_4) 1) (or (plus x_4 (not (shr16 x_4))) x_4)))", "AUFtWkCm16HxDwLrf0Sx96VL", 23, Seq("bonus", "and", "if0", "not", "or", "plus", "shr1", "shr16", "xor")), TrainingProblem("(lambda (x_17) (if0 (and (or x_17 (shr16 (shr1 (shr4 (shr1 x_17))))) 1) (or (shl1 (not 1)) x_17) (xor (shl1 (or 1 (shr4 x_17))) x_17)))", "QLuNTxWhkkeG3QfJMfLZoAgf", 23, Seq("bonus", "and", "if0", "not", "or", "shl1", "shr1", "shr16", "shr4", "xor")), TrainingProblem("(lambda (x_9) (if0 (and (plus (shr16 (shr16 (shl1 x_9))) (shr16 x_9)) 1) (plus x_9 (shl1 (and (shr4 x_9) x_9))) (plus 1 (plus x_9 x_9))))", "yDvL8M6pclo3cc4nBUPZdkkH", 23, Seq("bonus", "and", "if0", "plus", "shl1", "shr16", "shr4")))
    .drop(11)
  //TrainingProblem("(lambda (x_11) (if0 (and (plus (or (not x_11) (shr4 x_11)) x_11) 1) (not (xor (shl1 1) (shr4 (shl1 x_11)))) (xor (shr16 (not (plus x_11 x_11))) x_11)))", "uHCrZqnrKMBm6HIMBlLwpNUg", 25, Seq("bonus", "and", "if0", "not", "or", "plus", "shl1", "shr16", "shr4", "xor"))
  def tryAll = {
    val finished = new AtomicLong
    val total = small.size
    small.par.map { x ⇒
      val res = Synthesis.tryTraining(x, 3)
      val i = finished.incrementAndGet()
      println(s"Finished $i/$total")
      (x, res)
    }.seq
  }

  def getTrainingProblems(number: Int, train: TrainRequest): Unit = {
    import Client.system.dispatcher
    val f =
      (0 until number).map { _ ⇒
        Client.train(train)
      }
    Future.sequence(f).foreach { s ⇒
      val str =
        s.map { x ⇒
          import x._
          s"""TrainingProblem(\"$challenge\", \"$id\", $size, Seq(${operators.map("\"" + _ + "\"").mkString(", ")}))"""

        }.mkString(",\n")

      println(s"Seq($str)")
    }
  }

  lazy val problems15if0 = Seq(TrainingProblem("(lambda (x_28579) (plus (xor (plus (shl1 x_28579) (not (xor (if0 (plus (shr1 0) (shr1 (shr16 x_28579))) x_28579 x_28579) x_28579))) x_28579) x_28579))", "0ThJTFcUEVaq6f0oVUugW3ge", 20, Seq("if0", "not", "plus", "shl1", "shr1", "shr16", "xor")),
    TrainingProblem("(lambda (x_27702) (not (and (shr1 (xor (shl1 (or (or (if0 (plus (shr4 (shr4 x_27702)) x_27702) 1 1) x_27702) 1)) 1)) x_27702)))", "xjKgo6so7gwQhtp3tjZQa3eC", 20, Seq("and", "if0", "not", "or", "plus", "shl1", "shr1", "shr4", "xor")),
    TrainingProblem("(lambda (x_26703) (shl1 (plus (and (shr4 (shr16 (shr16 (and (if0 (plus (shr1 (shr1 (shr4 x_26703))) 0) 1 x_26703) x_26703)))) x_26703) x_26703)))", "fvXds9ojUWvJZ3swwiRXAOh9", 20, Seq("and", "if0", "plus", "shl1", "shr1", "shr16", "shr4")),
    TrainingProblem("(lambda (x_27343) (xor (and (plus (not 1) (xor (or (if0 (shr16 (shr16 (or x_27343 x_27343))) 0 x_27343) x_27343) 1)) 1) x_27343))", "opCbEPUckBaXiZQvOReYx4Ze", 20, Seq("and", "if0", "not", "or", "plus", "shr16", "xor")),
    TrainingProblem("(lambda (x_28384) (or (shr1 (shr4 (shr4 (shl1 (or (not (or (if0 (xor (plus x_28384 1) x_28384) 0 x_28384) x_28384)) 1))))) 0))", "L1EMtHDWLrZZv8X24vQAsbR3", 20, Seq("if0", "not", "or", "plus", "shl1", "shr1", "shr4", "xor")),
    TrainingProblem("(lambda (x_28295) (or (shr16 (shr16 (plus (or (plus (if0 (or (xor 1 0) (shr16 x_28295)) 0 x_28295) 0) x_28295) x_28295))) 1))", "r3TbJV3VelWzGnPNCcditVG4", 20, Seq("if0", "or", "plus", "shr16", "xor")),
    TrainingProblem("(lambda (x_26897) (shr16 (and (plus (and (shr1 (shr4 (shr1 (if0 (xor (shr1 (shl1 (shr16 1))) x_26897) 0 x_26897)))) x_26897) 0) x_26897)))", "HOgwg9hLPRObmG81zIDhHXOY", 20, Seq("and", "if0", "plus", "shl1", "shr1", "shr16", "shr4", "xor")),
    TrainingProblem("(lambda (x_26919) (shr4 (xor (shr1 (or (shr1 (shr4 (and (if0 (or (or (shr16 0) 0) x_26919) x_26919 1) 0))) x_26919)) 0)))", "Dt6tRhbUpmv6SmEHUgBxgWrH", 20, Seq("and", "if0", "or", "shr1", "shr16", "shr4", "xor")),
    TrainingProblem("(lambda (x_27003) (and (shr16 (shl1 (or (shr4 (plus (shl1 (if0 (or (shr16 (or x_27003 x_27003)) 1) x_27003 0)) x_27003)) 1))) x_27003))", "IlL1mvtxYvG1vRUi3LN1LeVW", 20, Seq("and", "if0", "or", "plus", "shl1", "shr16", "shr4")),
    TrainingProblem("(lambda (x_27255) (shl1 (xor (xor (plus x_27255 (not 1)) (not (plus (if0 (xor (shr1 x_27255) (shr1 0)) 1 x_27255) x_27255))) x_27255)))", "Mi2DBDSkvBfJI4MXls8auaId", 20, Seq("if0", "not", "plus", "shl1", "shr1", "xor")))

}
