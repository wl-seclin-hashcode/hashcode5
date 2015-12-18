package hashcode

object Solver {
  def solve(problem: Problem): Solution = {
    import problem._
    val commands = for {
      round <- 0 until nbTurns
      balloon <- 0 until nbBallons
    } yield {
      val move = if (round % 8 == 0 && balloon == round / 8) 1 else 0
      Command(balloon, move, round)
    }
    Solution(commands.toList)
  }
}