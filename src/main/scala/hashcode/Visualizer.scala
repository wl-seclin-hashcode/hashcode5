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

object Visualizer extends App {
  var delayMs = 1000
  val frame = new JFrame("visu")
  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  frame.setVisible(true)
  frame.setExtendedState(Frame.MAXIMIZED_BOTH)
  frame.addKeyListener(keyHandler)
  val content = frame.getContentPane
  content.add(drawPanel)
  var timer = new Timer
  var paintTask: Option[TimerTask] = None
  updateSpeed(0)

  def updateSpeed(delta: Int) = {
    paintTask.map(_.cancel())
    paintTask = Some(newPaintTask)
    delayMs += delta
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
      drawPanel.points = List.fill(200)((nextInt(800), nextInt(600)))
      drawPanel.repaint()
    }
  }

  lazy val drawPanel = new JPanel {
    var points = List.empty[(Int, Int)]

    override def paintComponent(g: Graphics) {
      super.paintComponent(g)
      g.drawString("This is my custom Panel!", 10, 20)
      g.drawRect(10, 10, 300, 300)
      for ((x, y) <- points) g.drawString("o", x, y)
    }
  }
}