package hashcode

import java.io.PrintStream

object Formatter {
  def write(solution: Solution, score: Int): Unit = {
    val name = s"out.${score}.txt"
    val f = new PrintStream(name)
    for { list <- solution.sol.groupBy(_.round).toList.sortBy(_._1).map(_._2) } {
      f.println(list.sortBy(_.balloonId).map(_.move).mkString(" "))
    }
    f.close
    println(s"wrote to $name")
  }
}