package hashcode

import java.awt.{Dimension, Graphics}

import scala.util.{Failure, Success}
import scala.util.Random

object Main extends App {
  val problem = Parser.read()
  showWindMap()
  //  showCalmSpots()
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

  def showWindMap() {
    val windByHeight = problem.winds.groupBy { case (p, wv) => p.height }

    def paintWind(g: Graphics, d: Dimension, p: Problem, step: Int): Unit = {
      drawTargetCells(g, d, p)

      for {
        (point, wv) <- Random.shuffle(windByHeight(step).toList).take(100000)
        (x, y) = coords(d, p, point.col, point.row)
        point2 = point.addVector(wv)
        (x2, y2) = coords(d, p, point2.col, point2.row)
        if x2 - x > 0 && y2 - y > 0 && x2 - x < 30 && y2 - y < 100
      } {
        g.drawLine(x, y, x2, y2)
        g.drawOval(x2, y2, 2, 2)
      }
    }

    Visualizer(paintWind, 1 to 8, problem, problem.nbTurns)

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
    } g.drawString("o", x, y)

}