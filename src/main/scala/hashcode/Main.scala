package hashcode

import java.io.PrintStream
import scala.util.Failure
import scala.util.Success
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import java.awt.Graphics
import java.awt.Dimension

object Main extends App {
  val problem = Parser.read()
  val solution = Solver.solve(problem)
  val steps=Validator.states(solution, problem).map(_.ballons.values.toVector)
  Future(Visualizer(paint, steps, problem).display())
  Validator.score(solution, problem) match {
    case Success(score) =>
      println(s"score : $score")
      Formatter.write(solution, score)
    case Failure(e) =>
      e.printStackTrace()
  }

  def paint(g: Graphics, d: Dimension, p: Problem, step: Vector[Point]): Unit = {
    for {
      Point(row, col, _) <- p.targetCells
      (x, y) = coords(d, p, col, row)
    } g.drawString("o", x, y)

    for {
      Point(row, col, _) <- step
      (x, y) = coords(d, p, col, row)
    } g.drawString("x", x, y)

    def coords(d: Dimension, p: Problem, x: Int, y: Int): (Int, Int) =
      ((x * d.getWidth / p.nbCols).toInt,
        (y * d.getHeight / p.nbRows).toInt)
  }

}