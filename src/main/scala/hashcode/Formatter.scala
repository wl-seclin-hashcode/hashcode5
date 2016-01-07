package hashcode

import java.io.PrintStream

object Formatter {
  def write(solution: Solution, score: Int): Unit = {
    val name = s"target/out.${score}.txt"
    val f = new PrintStream(name)
    for { list <- solution.sol.groupBy(_.round).toList.sortBy(_._1).map(_._2) } {
      f.println(list.sortBy(_.balloonId).map(_.move).mkString(" "))
    }
    f.close
    println(s"wrote to $name")
  }

  def read(score: Int): Solution = {
    val name = s"out.${score}.txt"
    val lines = io.Source.fromFile(name).getLines
    val cmds = for {
      (l, turn) <- lines.zipWithIndex
      (move, balloon) <- l.split(" ").zipWithIndex
    } yield Command(balloon, move.toInt, turn)
    Solution(cmds.toVector)
  }

}