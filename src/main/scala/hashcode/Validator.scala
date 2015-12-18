package hashcode

import scala.util.{Success, Try}

object Validator {
  case class State(ballons : Map[Int, Point]) extends AnyVal {
    def applyCommand(command: Command) = {
      val pos = ballons(command.balloonId)
      State(ballons.updated(command.balloonId, pos.addHeight(command.move)))
    }
    def applyWind(windMap: Map[Point, Vector]) = {
      State(ballons.mapValues(p => p.addVector(windMap(p))))
    }
  }

  def score(solution: Solution, problem: Problem): Try[Int] = {

    val allStates = states(solution,problem)
    println(allStates)
    Success(42)
  }

  def states(solution: Solution, problem: Problem) = {
    val initState = List.tabulate(problem.nbBallons)({ i => i->problem.startPoint }).toMap
    val windMap = problem.winds.withDefaultValue(Vector(0,0))
    solution.byRound.toList.sortBy(_._1).scanLeft(State(initState)) { case (state,(_,commands)) =>
      val s = commands.foldLeft(state) { (s,c) => s.applyCommand(c) }
      s.applyWind(windMap)
    }
  }
}