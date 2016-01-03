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
    implicit class SolutionOps(s: Solution) {
      lazy val score = {
        Validator.score(s, problem).get
      }

      def +(o: Solution) = Solution(s.sol ++ o.sol)
    }

    import problem._
    val initialSolution = Solution(Vector.empty)
    (0 until nbBallons).foldLeft(initialSolution) {
      case (sol, bal) =>
        val best = List.fill(1)(brownianBalloon(bal)).map(cmds =>
          Solution(cmds.toVector) + sol)
          .maxBy(_.score)
        println(s"score after balloon $bal : ${best.score}")
        best
    }
  }
}