package hashcode

import scala.util.Random

object Solver {

  def cycles(problem: Problem): Iterable[List[Point]] = {

    def reachableFrom(point: Point, depth: Int): Set[List[Point]] =
      if (depth == 0) point.reachable.toSet.map((e: Point) => List(e))
      else for {
        p2 <- point.reachable.toSet[Point]
        wv <- problem.winds.get(p2).toSet[WindVector]
        t = p2.addVector(wv)
        rf <- reachableFrom(t, depth - 1)
      } yield t :: rf

    def ncycle(n: Int) = for {
      p <- problem.winds.keys
      rf <- reachableFrom(p, n)
      if rf contains p
    } yield rf

    (for (i <- 2 to 3) yield {
      println(s"looking for $i-cycles")
      val found = ncycle(i)
      println(s"${found.size} $i-cycles found")
      found.foreach(println)
      found
    }).flatten
  }

  def brownianBalloon(balloon: Int, from: Int = 0, height: Int = 0) = {
    var h = height
    for {
      round <- from until 400
    } yield {
      val move =
        if (h <= 1) Random.nextInt(2)
        else if (h == 8) -Random.nextInt(2)
        else Random.nextInt(3) - 1
      h += move
      Command(balloon, move, round)
    }
  }

  def solve(problem: Problem): Solution = {
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
              (best, height + best.sol(turn + nbTurns * bal).move)
            } else (sol, height)
        }
        s
    }
  }
}