package hashcode

import scala.util.Random

case class Solver(problem: Problem) {

  def cycles: Iterable[List[Point]] = {

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

  case class Infos(dh: Int, targetPoint: Point, covers: Boolean, speed: Double)

  def lt(i: Infos, j: Infos) = {
    (!i.covers && j.covers) ||
      (i.covers && j.covers && i.speed > j.speed) ||
      (!i.covers && !j.covers && i.speed < j.speed)
  }

  def movesFrom(p: Point) = for {
    dh <- -1 to 1
    pm = p.addHeight(dh)
    wv <- problem.winds.get(pm)
    p2 = pm.addVector(wv)
    if problem.winds.isDefinedAt(p2)
    covers = problem.connectedCitiesMap.get(Point2D(p2.row, p2.col)).map(_.size).sum > 0
  } yield Infos(dh, p2, covers, wv.speed)

  def smartBalloon(balloon: Int, sol: Solution): Vector[Command] = {
    val (cmds, _) = (1 to problem.nbTurns).foldLeft((Vector[Command](), problem.startPoint)) {
      case ((commands, p), turn) =>
        val moves = movesFrom(p)
        if (turn > 20 || balloon == 0) {
          moves.sortWith(lt).lastOption match {
            case Some(Infos(dh, p, _, _)) => (commands :+ Command(balloon, dh, turn), p)
            case _                        => (commands, p)
          }
        } else {
          val movesScores = moves.sortWith(lt).zipWithIndex.map {
            case (info, i) =>
              val tries = 10 * (i + 2)
              println(s"running $tries brownian for bal $balloon at turn $turn starting from ${info.targetPoint}")
              val brCmds = List.fill(tries)(brownianBalloon(balloon, turn + 1, info.targetPoint.height))
              val Infos(dh, p2, _, _) = info
              val c = Command(balloon, dh, turn)
              val score = brCmds.map { cs => Solution(sol.sol ++ commands ++ (c +: cs)) }.map(_.score).max
              (c, score, p2)
          }
          val (best, score, p2) = movesScores.maxBy(_._2)
          println(s"best score : $score for bal $balloon at turn $turn ")
          (commands :+ best, p2)
        }
    }
    cmds
  }

  def brownianBalloon(balloon: Int, from: Int, height: Int) = {
    var h = height
    for {
      round <- from to problem.nbTurns
    } yield {
      val move =
        if (h <= 1) Random.nextInt(2)
        else if (h >= 8) -Random.nextInt(2)
        else Random.nextInt(3) - 1
      h += move
      Command(balloon, move, round)
    }
  }

  val ratio = 3
  val selectFromMoves: PartialFunction[IndexedSeq[Infos], Infos] = possible => possible.size match {
    case 1 => possible.head
    case 2 => if (Random.nextInt(ratio) == 0) possible.head else possible(1)
    case 3 => if (Random.nextInt(ratio * ratio) == 0) possible.head else if (Random.nextInt(ratio) == 0) possible(1) else possible(2)
  }

  def brownianHeuristicBalloon(balloon: Int, fromTurn: Int, fromPoint: Point) = {
    var pos = fromPoint
    for {
      round <- fromTurn until problem.nbTurns
    } yield {
      val possible = movesFrom(pos).sortWith(lt)
      selectFromMoves.lift(possible) match {
        case Some(Infos(dh, p2, _, _)) =>
          pos = p2
          Command(balloon, dh, round)
        case None => Command(balloon, 0, round)
      }
    }
  }

  def solve: Solution = {
    import problem._
    val initialSolution = Solution(Vector.empty)
    (0 until nbBallons).foldLeft(initialSolution) {
      case (sol0, bal) =>
        val s = (0 until nbTurns).foldLeft(sol0) {
          case (sol, turn) =>
            val tries = (50 - 10 * math.sqrt(turn)).toInt max 0
            val startPos = sol.balloonAt(bal, turn)
            if (tries > 0 && startPos.isDefined) {
              val candidates = sol :: List.fill(tries)(brownianHeuristicBalloon(bal, turn, startPos.get)).map(cmds =>
                Solution(sol.sol.take(turn + nbTurns * bal) ++ cmds.toVector))
              val best = candidates.maxBy(_.score)
              println(s"score after balloon $bal @turn $turn: ${best.score} ($tries tries)")
              best
            } else sol
        }
        s
    }
  }

  implicit class SolutionOps(s: Solution) {
    lazy val score = {
      Validator.score(s, problem).get
    }

    def balloonAt(bal: Int, turn: Int): Option[Point] = {
      val windMap = problem.winds + (problem.startPoint → WindVector(0, 0))
      var pos = Some(problem.startPoint)
      (0 until turn).foldLeft[Option[Point]](Some(problem.startPoint)) {
        case (Some(p), r) =>
          val cmd = s.sol.find { case Command(`bal`, dh, `r`) => true; case _ => false }.get
          val p2 = p.copy(height = p.height + cmd.move)
          problem.winds.get(p2) match {
            case Some(w) => Some(p2.addVector(w))
            case None    => None
          }
        case _ => None
      }
    }

  }
}