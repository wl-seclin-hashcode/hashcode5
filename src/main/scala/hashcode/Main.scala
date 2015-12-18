package hashcode

import java.io.PrintStream
import scala.util.Failure
import scala.util.Success
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Main extends App {
  val problem = Parser.read()
  val solution = Solver.solve(problem)
  Future(Visualizer.display(problem, solution))
  Validator.score(solution, problem) match {
    case Success(score) =>
      println(s"score : $score")
      Formatter.write(solution, score)
    case Failure(e) =>
      e.printStackTrace()
  }

}