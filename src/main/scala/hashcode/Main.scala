package hashcode

import java.io.PrintStream
import scala.util.Failure
import scala.util.Success

object Main extends App {
  val problem = Parser.read()
  val solution = Solver.solve(problem)
  Validator.score(solution, problem) match {
    case Success(score) =>
      println(s"score : $score")
      Formatter.write(solution, score)
    case Failure(e) =>
      e.printStackTrace()
  }

}