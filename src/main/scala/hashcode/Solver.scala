package hashcode

import scala.util.Random
import scala.collection.parallel.immutable.ParVector
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import grizzled.slf4j.Logging

case class Solver(problem: Problem, initialSolution: Option[Solution]) extends Logging {
  import problem._

  def solve: Solution = {
    val init = PartialSolution(Vector.fill(nbTurns)(Set.empty), Vector.empty)
    val sol = (0 until nbBallons).foldLeft(init) {
      case (sol, bal) =>
        val lastColumn = bestSolForBal(bal, sol)
        val bestSlot = lastColumn.maxBy(_.score)
        val s = sol.addBalloon(bestSlot, bal)
        println(s"bal $bal : score=${bestSlot.score} sol : $s")
        s
    }
    Solution(sol.moves)
  }

  case class Slot(score: Int, pos: Point, parent: Option[Slot]) {
    def commands(bal: Int, round: Int = nbTurns): Vector[Command] = parent match {
      case None    => Vector.empty
      case Some(p) => Command(bal, pos.height - p.pos.height, round) +: p.commands(bal, round - 1)
    }

    def posHistory: Vector[Point] = parent match {
      case None    => Vector.empty
      case Some(p) => p.posHistory :+ pos
    }
  }
  case class PartialSolution(positions: Vector[Set[Point]], moves: Vector[Command]) {
    def addBalloon(bestSlot: Slot, bal: Int): PartialSolution = {
      PartialSolution(
        (positions zip bestSlot.posHistory.padTo(nbTurns, startPoint)).map { case (set, p) => set + p },
        moves = moves ++ bestSlot.commands(bal))
    }
  }

  def bestSolForBal(bal: Int, sol: PartialSolution): Set[Slot] = {
    val initialSlots = Set(Slot(0, startPoint, None))
    (0 until nbTurns).foldLeft(initialSlots) {
      case (slots, turn) =>
        val nextSlots = for {
          slot <- slots
          if slot.score >= 0
          dh <- -1 to 1
          nh = slot.pos.height + dh
          if nh <= nbHeights && nh > 0
        } yield {
          move(dh, Some(slot.pos)) match {
            case Some(nextPoint) =>
              val score = Validator.score(sol.positions(turn).toList, nextPoint, problem)
              Slot(score + slot.score, nextPoint, Some(slot))
            case None =>
              slot
          }
        }
        nextSlots.groupBy(_.pos.height).mapValues(_.maxBy(_.score)).values.toSet
    }
  }

  def solveMichel: Solution = {
    def generate(count: Int) = (Vector.fill(count)(hybridSolution)).maxBy(_.score)
    def combine(generation: Int): Future[Solution] = {
      debug(s"starting generations $generation")
      if (generation == 0) {
        debug(s"starting actual generation")
        Future(generate(40))
      } else for {
        List(s1, s2) <- Future.sequence(List(combine(generation - 1), combine(generation - 1)))
      } yield {
        val best = Seq(s1, s2).maxBy(_.score)
        info(s"gen $generation found score ${best.score}")
        Formatter.write(best, best.score)
        s1.combineWith(s2) //, 1 - 1 / (1 + generation.toDouble))
      }
    }

    Await.result(combine(5), Duration.Inf)
  }

  def solveOld: Solution = {
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
        Formatter.write(s, s.score)
        s
    }
  }

  def randomSolution = Solution(Vector.tabulate(nbBallons)(brownianBalloon(_)).flatten)
  def hybridSolution = Solution(Vector.tabulate(nbBallons)(brownianThenQuickBalloon(_, Random.nextInt(problem.nbTurns - 10))).flatten)
  def emptySolution = Solution(Vector.tabulate(nbBallons)(emptyBalloon).flatten)

  def emptyBalloon(b: Int) = Vector.tabulate(nbTurns)(Command(b, 0, _))

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

  def move(dh: Int, optP: Option[Point]) =
    for {
      p <- optP
      pm = p.addHeight(dh)
      wv <- problem.winds.get(pm)
      p2 = pm.addVector(wv)
      if problem.winds.isDefinedAt(p2)
    } yield p2

  def applyMoves(cmds: Iterable[Command], p: Point = problem.startPoint) =
    cmds.foldLeft(Some(p): Option[Point]) { case (optPt, cmd) => move(cmd.move, optPt) }

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

  def quickBalloon(balloon: Int, start: Point = problem.startPoint, from: Int = 1): Vector[Command] = {
    val (cmds, endPoint) = (from to problem.nbTurns).foldLeft((Vector[Command](), start)) {
      case ((commands, previous), turn) =>
        movesFrom(previous).sortWith(lt).lastOption match {
          case Some(Infos(dh, point, _, _)) => (commands :+ Command(balloon, dh, turn), point)
          case _                            => (commands :+ Command(balloon, 0, turn), previous)
        }
    }
    cmds
  }

  def brownianThenQuickBalloon(bal: Int, brownianTurns: Int) = {
    val initialCmds = brownianBalloon(bal).take(brownianTurns)
    val quickTurns = problem.nbTurns - brownianTurns
    val endCmds = applyMoves(initialCmds) match {
      case Some(point) => quickBalloon(bal, point, brownianTurns + 1)
      case None        => emptyBalloon(bal).take(quickTurns)
    }
    initialCmds ++ endCmds
  }

  def brownianBalloon(balloon: Int, from: Int = 0, height: Int = 0) = {
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

  implicit class SolutionOps(s: Solution) {
    lazy val score = {
      Validator.score(s, problem).get
    }

    def replace(bal: Int, commands: Int => Vector[Command]) =
      Solution(s.sol.filterNot(_.balloonId == bal) ++ commands(bal))

    lazy val scoresWithoutBalloon: Map[Int, Int] =
      Map((for (bal <- 0 to nbBallons) yield bal -> replace(bal, emptyBalloon).score): _*)

    lazy val balloonsByScore = scoresWithoutBalloon.toList.sortBy(_._2).map(_._1)

    def combineWith(s2: Solution, ratio: Double = 0.5) = {
      val count1 = (nbBallons * ratio).toInt
      val count2 = nbBallons - count1
      val res = Solution(bestCommandsByBallons(count1) ++ s2.bestCommandsByBallons(count2, count1))
      debug(s"$score (keep $count1) + ${s2.score} (keep $count2) => combined: ${res.score}")
      res
    }

    def bestCommandsByBallons(nbBal: Int, shiftId: Int = 0): Vector[Command] =
      for {
        (balloon, bId) <- balloonsByScore.take(nbBal).zipWithIndex.toVector
        (b, cmds) <- s.byBalloon
        if b == balloon
        c <- cmds
      } yield c.copy(balloonId = bId + shiftId)

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