package hashcode

import java.awt.{ Dimension, Frame, Graphics }
import java.awt.event.{ KeyAdapter, KeyEvent }
import java.awt.event.KeyEvent._
import java.util.{ Timer, TimerTask }
import javax.swing.{ JFrame, JPanel }
import scala.swing.MainFrame
import scala.swing.Panel
import scala.swing.event.KeyPressed
import scala.swing.event.Key
import java.awt.Graphics2D

case class Visualizer[T, P](
    painter: (Graphics, Dimension, P, T) => Unit,
    steps: Seq[T],
    problem: P,
    maxTurns: Int) {

  val delayDelta = 50
  var delayMs = 1000
  var turn = 0
  val frame = new MainFrame {
    contents = new Panel {
      listenTo(keys)
      reactions += {
        case KeyPressed(_, key, _, _) => handleKey(key)
      }
      override def paintComponent(g: Graphics2D) {
        painter(g, size, problem, steps(turn))
      }
      focusable = true
      requestFocus() // to receive key events
    }
    maximize()
    visible = true
  }
  var timer = new Timer
  var paintTask: Option[TimerTask] = None
  pause()

  def handleKey(key: Key.Value) = {
    key match {
      case Key.Up =>
        updateSpeed(-delayDelta)
      case Key.Down =>
        updateSpeed(delayDelta)
      case Key.Left =>
        prevTurn()
      case Key.Right =>
        nextTurn()
      case Key.Space =>
        pause()
      case _ =>
    }
    doPaint()
  }

  def nextTurn() = if (turn < maxTurns) turn += 1
  def prevTurn() = if (turn > 0) turn -= 1

  def pause() = paintTask match {
    case Some(task) =>
      task.cancel()
      paintTask = None
    case None =>
      paintTask = Some(newPaintTask)
      timer.scheduleAtFixedRate(paintTask.get, delayMs, delayMs)
  }

  def updateSpeed(delta: Int): Unit = {
    paintTask.map(_.cancel())
    paintTask = Some(newPaintTask)
    delayMs = Math.max(50, delayMs + delta)
    timer.scheduleAtFixedRate(paintTask.get, delayMs, delayMs)
  }

  def doPaint(): Unit = {
    frame.repaint()
    frame.title = s"turn $turn/$maxTurns delay : $delayMs ms"
  }

  def newPaintTask = new TimerTask {
    override def run() = {
      nextTurn()
      doPaint()
    }
  }
}