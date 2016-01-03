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
  showCalmSpots()
  val solution = Solver.solve(problem)
  val steps = Validator.states(solution, problem).map(_.ballons.toVector)
  Visualizer(paint, steps, problem, problem.nbTurns)
  Validator.score(solution, problem) match {
    case Success(score) =>
      println(s"score : $score")
      Formatter.write(solution, score)
    case Failure(e) =>
      e.printStackTrace()
  }

  def showCalmSpots() {
    val cycles = Solver.cycles(problem)

    def paintSpots(g: Graphics, d: Dimension, p: Problem, step: Int): Unit = {
      drawTargetCells(g, d, p)

      for {
        w <- cycles
        Point(row, col, _) <- w
        (x, y) = coords(d, p, col, row)
      } g.drawString("+", x, y)
    }

    Visualizer(paintSpots, Vector.fill(100)(0), problem, problem.nbTurns)

  }

  def paint(g: Graphics, d: Dimension, p: Problem, step: Vector[(Int, Point)]): Unit = {
    drawTargetCells(g, d, p)

    for {
      (b, Point(row, col, h)) <- step
      if h > 0
      (x, y) = coords(d, p, col, row)
    } g.drawString((b % 10).toString, x, y)

  }

  def coords(d: Dimension, p: Problem, x: Int, y: Int): (Int, Int) =
    ((x * d.getWidth / p.nbCols).toInt,
      (y * d.getHeight / p.nbRows).toInt)

  def drawTargetCells(g: Graphics, d: Dimension, p: Problem) =
    for {
      Point(row, col, _) <- p.targetCells
      (x, y) = coords(d, p, col, row)
    } g.drawString(".", x, y)

}