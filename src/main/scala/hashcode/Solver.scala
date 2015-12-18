package hashcode

import scala.util.Random

object Solver {
  def brownianBalloon(balloon: Int) = {
    var height = 0
    for {
      round <- 0 until 400
    } yield {
      val move =
        if (height <= 1) Random.nextInt(2)
        else if (height == 8) -Random.nextInt(2)
        else Random.nextInt(3) - 1
      height += move
      Command(balloon, move, round)
    }
  }
  def solve(problem: Problem): Solution = {
    import problem._
    val commands = for {
      balloon <- 0 until nbBallons
      cmd <- brownianBalloon(balloon)
    } yield cmd
    Solution(commands.toList)
  }
}