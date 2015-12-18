package hashcode

import javax.swing.JFrame
import java.awt.Frame
import javax.swing.JPanel
import java.awt.Graphics
import java.util.Timer
import java.util.TimerTask
import scala.util.Random._
import java.awt.event.KeyAdapter
import java.awt.event.KeyEvent
import java.awt.event.KeyEvent._
import java.awt.Dimension

object Visualizer {
  var delayMs = 1000
  var turn = 0
  val frame = new JFrame("visu")
  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  frame.setVisible(true)
  frame.setExtendedState(Frame.MAXIMIZED_BOTH)
  frame.addKeyListener(keyHandler)
  val content = frame.getContentPane
  content.add(drawPanel)
  var timer = new Timer
  var paintTask: Option[TimerTask] = None

  def display(problem: Problem, ballonsOverTime: List[List[Point]]) = {
    drawPanel.problem = Some(problem)
    drawPanel.ballonsOverTime = ballonsOverTime
    updateSpeed(0)
  }

  def updateSpeed(delta: Int) = {
    paintTask.map(_.cancel())
    paintTask = Some(newPaintTask)
    delayMs = Math.max(50, delayMs + delta)
    timer.scheduleAtFixedRate(paintTask.get, 0, delayMs)
    println(s"delay : $delayMs")
  }

  def speedUp() {
    updateSpeed(-50)
  }
  def speedDown() {
    updateSpeed(50)
  }

  lazy val keyHandler = new KeyAdapter {
    override def keyPressed(e: KeyEvent) {
      e.getKeyCode match {
        case VK_UP   => speedUp()
        case VK_DOWN => speedDown()
        //        case VK_LEFT  => speedUp()
        //        case VK_RIGHT => speedUp()
        case _       =>
      }
      println(e.getKeyCode)
    }
  }

  def newPaintTask = new TimerTask {
    override def run(): Unit = {
      drawPanel.repaint()
      turn += 1
      frame.setTitle(s"turn $turn delay : $delayMs ms")
      if (turn >= 400) cancel()
    }
  }

  lazy val drawPanel = new JPanel {
    var problem: Option[Problem] = None
    var ballonsOverTime: List[List[Point]] = Nil

    def coords(d: Dimension, p: Problem, x: Int, y: Int): (Int, Int) =
      ((x * d.getWidth / p.nbCols).toInt,
        (y * d.getHeight / p.nbRows).toInt)

    def paintProblem(g: Graphics, d: Dimension)(p: Problem) {
      for {
        Point(row, col, _) <- p.targetCells
        (x, y) = coords(d, p, col, row)
      } g.drawString("o", x, y)

      for {
        Point(row, col, _) <- ballonsOverTime(turn)
        (x, y) = coords(d, p, col, row)
      } g.drawString("x", x, y)

    }

    override def paintComponent(g: Graphics) {
      super.paintComponent(g)
      problem.map(paintProblem(g, getSize))
    }
  }
}