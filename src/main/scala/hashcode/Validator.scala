package hashcode

import scala.util.{ Success, Try }

object Validator {
  case class State(ballons: Map[Int, Point]) extends AnyVal {
    def applyCommand(command: Command) = if (!ballons.contains(command.balloonId)) this
    else {
      val pos = ballons(command.balloonId)
      State(ballons.updated(command.balloonId, pos.addHeight(command.move)))
    }
    def applyWind(windMap: Map[Point, WindVector]) = {
      State(
        ballons.mapValues(p => p.addVector(windMap(p)))
          .filter { case (i, p) => !p.isLost })
    }
    def validate(): Unit = ballons.values.foreach(_.validate())
  }

  def score(solution: Solution, problem: Problem): Try[Int] = {

    val allStates = states(solution, problem)
    val scoreByTurn = allStates.map(state => problem.connectedCount(state.ballons.values))
    Success(scoreByTurn.sum)
  }

  def states(solution: Solution, problem: Problem) = {
    val initState = Vector.tabulate(problem.nbBallons)({ i => i -> problem.startPoint }).toMap
    val windMap = problem.winds.withDefaultValue(WindVector(0, 0))
    solution.byRound.toVector.sortBy(_._1).scanLeft(State(initState)) {
      case (state, (_, commands)) =>
        val s = commands.foldLeft(state) { (s, c) => s.applyCommand(c) }
        s.applyWind(windMap)
    }
  }
}