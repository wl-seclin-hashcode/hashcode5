package hashcode

import java.io.PrintStream
import scala.util.Failure
import scala.util.Success

object Main extends App {
  val problem = Parser.read()
  Visualizer.display(problem, List.fill(100)(List.fill(100)(Point(10,10,1))))
  val solution = Solver.solve(problem)
  Validator.score(solution, problem) match {
    case Success(score) =>
      println(s"score : $score")
      Formatter.write(solution, score)
    case Failure(e) =>
      e.printStackTrace()
  }

}