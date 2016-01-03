package hashcode

import scala.util.Random

object Solver {
  def brownianBalloon(balloon: Int, from: Int = 0, height: Int = 0) = {
    var h = height
    for {
      round <- from until 400
    } yield {
      val move =
        if (height <= 1) Random.nextInt(2)
        else if (height == 8) -Random.nextInt(2)
        else Random.nextInt(3) - 1
      h += move
      Command(balloon, move, round)
    }
  }

  def solve(problem: Problem): Solution = {
    val noWind = for {
      (p, wv) <- problem.winds
      p2 <- p.reachable
      t = p2.addVector(wv)
      t2 <- t.reachable
      wvt <- problem.winds.get(t2)
      pb = t2.addVector(wvt)
      if pb == p
    } yield (p, t)

    noWind.foreach(println)

    implicit class SolutionOps(s: Solution) {
      lazy val score = {
        Validator.score(s, problem).get
      }
    }

    import problem._
    val initialSolution = Solution(Vector.empty)
    (0 until nbBallons).foldLeft(initialSolution) {
      case (sol0, bal) =>
        val (s, h) = (0 until nbTurns).foldLeft((sol0, 0)) {
          case ((sol, height), turn) =>
            val tries = (10 - 2 * math.sqrt(turn)).toInt max 0
            if (tries > 0) {
              val candidates = sol :: List.fill(tries)(brownianBalloon(bal, turn, height)).map(cmds =>
                Solution(sol.sol.take(turn + nbTurns * bal) ++ cmds.toVector))
              val best = candidates.maxBy(_.score)
              println(s"score after balloon $bal @turn $turn: ${best.score} ($tries tries)")
              (best, height + best.sol(turn).move)
            } else (sol, height)
        }
        s
    }
  }
}