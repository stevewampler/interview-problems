package com.sgw.problems

import scala.io.Source

/**
 * We are organizing a Open JuggleFest. There will be thousands of participants
 * split into teams. Each team will attempt to complete a juggling circuit consisting of several tricks.
 * Each circuit emphasizes different aspects of juggling, requiring hand to eye coordination (H),
 * endurance (E) and pizzazz (P) in various amounts for successful completion.
 * Each juggler has these abilities in various amounts as well. How good a match they are for a
 * circuit is determined by the dot product of the juggler’s and the circuit’s H, E, and P values.
 * The higher the result, the better the match.
 *
 * Each participant will be on exactly one team and there will be a distinct circuit for each
 * team to attempt. Each participant will rank in order of preference their top X circuits.
 * Since we would like the audiences to enjoy the performances as much as possible, when assigning
 * jugglers to circuits we also want to consider how well their skills match up to the circuit.
 * In fact we want to match jugglers to circuits such that no juggler could switch to a circuit that
 * they prefer more than the one they are assigned to and be a better fit for that circuit than one
 * of the other jugglers assigned to it.
 *
 * To help us create the juggler/circuit assignments write a program in a language of your choice
 * that takes as input a file of circuits and jugglers and outputs a file of circuits and juggler assignments.
 * The number of jugglers assigned to a circuit should be the number of jugglers divided by the number of
 * circuits. Assume that the number of circuits and jugglers will be such that each circuit will have
 * the same number of jugglers with no remainder.
 *
 * Input file
 *
 * One line per circuit or juggler. All circuits will come before any jugglers. Circuit lines start
 * with a C and juggler lines start with a J. Names of circuits and jugglers will never have spaces.
 * A skill and the rating for that skill are separated by a colon. Circuit lines have the circuit names
 * followed by skills. Juggler lines have the juggler names followed by skills, followed by circuits
 * in order of preference, separated by commas. Example:
 *
 * C C0 H:7 E:7 P:10
 * C C1 H:2 E:1 P:1
 * C C2 H:7 E:6 P:4
 *
 * J J0 H:3 E:9 P:2 C2,C0,C1
 * J J1 H:4 E:3 P:7 C0,C2,C1
 * J J2 H:4 E:0 P:10 C0,C2,C1
 * J J3 H:10 E:3 P:8 C2,C0,C1
 * J J4 H:6 E:10 P:1 C0,C2,C1
 * J J5 H:6 E:7 P:7 C0,C2,C1
 * J J6 H:8 E:6 P:9 C2,C1,C0
 * J J7 H:7 E:1 P:5 C2,C1,C0
 * J J8 H:8 E:2 P:3 C1,C0,C2
 * J J9 H:10 E:2 P:1 C1,C2,C0
 * J J10 H:6 E:4 P:5 C0,C2,C1
 * J J11 H:8 E:4 P:7 C0,C1,C2
 *
 * Output file
 *
 * One line per circuit assignment. Each line should contain the circuit name followed by the
 * juggler name, followed by that juggler’s circuits in order of preference and the match score for
 * that circuit. The line should include all jugglers matched to the circuit. The example below is a
 * valid assignment for the input file above.
 *
 * C2 J6 C2:128 C1:31 C0:188, J3 C2:120 C0:171 C1:31, J10 C0:120 C2:86 C1:21, J0 C2:83 C0:104 C1:17
 * C1 J9 C1:23 C2:86 C0:94, J8 C1:21 C0:100 C2:80, J7 C2:75 C1:20 C0:106, J1 C0:119 C2:74 C1:18
 * C0 J5 C0:161 C2:112 C1:26, J11 C0:154 C1:27 C2:108, J2 C0:128 C2:68 C1:18, J4 C0:122 C2:106 C1:23
 */
object JuggleFest {

  /**
   * A juggling Circuit.
   *
   * @param name name of the circuit
   * @param attrs the circuit's attributes in order of hand to eye coordination (H),
   * endurance (E) and pizzazz (P)
   */
  case class Circuit(name: String, attrs: Vector[Int]) {
    def score(jugglerAttrs: Vector[Int]): Int = dot(attrs, jugglerAttrs)
  }

  /**
   * A class holding a score (dot product) for how well a juggler's attributes match a circuit's attributes.
   *
   * @param circuit the circuit
   * @param score a match score
   */
  case class MatchScore(circuit: Circuit, score: Int) {
    def lt(ms2: MatchScore): Boolean = {
      assert(ms2.circuit.name == circuit.name)

      score > ms2.score
    }

    override def toString = s"${circuit.name}:$score"
  }

  /**
   * A Juggler.
   *
   * @param name the juggler's name
   * @param attrs the juggler's attributes in order of hand to eye coordination (H),
   * endurance (E) and pizzazz (P)
   * @param matchScores a sequence of circuit match scores in order of the juggler's circuit preference
   */
  case class Juggler(name: String, attrs: Vector[Int], matchScores: Seq[MatchScore]) {
    val matchScoresMap = matchScores.map(matchScore => (matchScore.circuit, matchScore)).toMap

    def maybeMatchScore(circuit: Circuit): Option[MatchScore] = matchScoresMap.get(circuit)

    def score(circuit: Circuit): Int = maybeMatchScore(circuit).map(_.score).getOrElse(0)

    def prefers(circuit: Circuit, prefLevel: Int): Boolean = matchScores.take(prefLevel).exists(ms => ms.circuit == circuit)

    override def toString = s"$name ${matchScores.mkString(" ")}"
  }

  /**
   * A juggler-assignment strategy that Adds Jugglers to juggling Circuits in the order of how well
   * each Juggler's skills match the Circuit while also taking into account the Juggler's Circuit
   * preferences such that:
   *
   * 1) no Juggler could switch to a Circuit that they prefer more than the one they are assigned to and
   * 2) be a better fit for that Circuit than the other Jugglers assigned to it.
   *
   * @return a two tuple containing the new list of unassigned Jugglers and the a map containing
   *         the new mapping of circuits to assigned Jugglers.
   */
  object AssignmentStrategy {
    /**
     * Adds Jugglers to the specified Circuit up to the specified number of jugglers-per-circuit.
     * Juggler's are assigned to a Circuit in the order of how well each Juggler's skills match the Circuit (i.e. by
     * MatchScore) while also taking into account the Juggler's Circuit preferences.
     *
     * @param circuit the juggling Circuits
     * @param prefLevel how deep to go into a Juggler's Circuit-preferences to go to find a Juggler suitable for the Circuit (one based)
     * @param jugglersPerCircuit the max number of Juggler's per Circuit
     * @param unassignedJugglers the list of Jugglers that have not yet been assigned to a Circuit
     * @param assignments a map of Circuits to list of Jugglers assigned to a Circuit
     *
     * @return a two tuple containing the new list of unassigned Jugglers and the a map containing
     *         the new mapping of circuits to assigned Jugglers.
     */
    private def assignJugglers(
        circuit: Circuit,
        prefLevel: Int,
        jugglersPerCircuit: Int,
        unassignedJugglers: List[Juggler],
        assignments: Map[Circuit, List[Juggler]]
    ): (List[Juggler], Map[Circuit, List[Juggler]]) = {
      // get the circuits current list of assigned jugglers
      val assignedJugglers = assignments.getOrElse(circuit, List[Juggler]()) // get the circuit's currently assigned jugglers

      // get the list of newly assigned Jugglers based on how well the Juggler's skills match the Circuit
      // and the Juggler's Circuit preferences
      val newlyAssignedJugglers = unassignedJugglers
        .sortWith { case (j1, j2) => j1.score(circuit) > (j2.score(circuit)) } // sort by match score for the current circuit
        .takeWhile(juggler => juggler.prefers(circuit, prefLevel)) // take the top jugglers that prefer to be in the circuit
        .take(jugglersPerCircuit - assignedJugglers.size)

      // return the updated list of unassigned jugglers and the circuit's updated list of assigned jugglers
      (unassignedJugglers diff newlyAssignedJugglers, assignments.updated(circuit, newlyAssignedJugglers ++ assignedJugglers))
    }

    /**
     * Adds Jugglers to each Circuit, in the specified list of Circuits, up to the specified number of jugglers-per-circuit.
     * Juggler's are assigned to a Circuit in the order of how well each Juggler's skills match the Circuit skill level
     * while also taking into account the Juggler's Circuit preferences.
     *
     * @param circuits the list of juggling Circuits
     * @param prefLevel how deep to go into a Juggler's Circuit-preference list to find a Juggler suitable for a Circuit (one based)
     * @param jugglersPerCircuit the max number of Juggler's per Circuit
     * @param unassignedJugglers the list of Jugglers that have not yet been assigned to a Circuit
     * @param assignments a map of Circuits to list of Jugglers assigned to a Circuit
     *
     * @return a two tuple containing the new list of unassigned Jugglers and the a map containing
     *         the new mapping of circuits to assigned Jugglers.
     */
    private def assignJugglers(
        circuits: List[Circuit],
        prefLevel: Int,
        jugglersPerCircuit: Int,
        unassignedJugglers: List[Juggler],
        assignments: Map[Circuit, List[Juggler]]
    ): (List[Juggler], Map[Circuit, List[Juggler]]) =
      circuits.foldLeft((unassignedJugglers, assignments)) { //
        case ((unassignedJugglers2, assignments2), circuit) => assignJugglers(circuit, prefLevel, jugglersPerCircuit, unassignedJugglers2, assignments2)
      }

    def apply(circuits: List[Circuit], jugglers: List[Juggler]): List[(Circuit, List[Juggler])] = {
      val jugglersPerCircuit = jugglers.size / circuits.size

      val maxPreferenceLevel = jugglers.map(juggler => juggler.matchScores.length).max

      val (_, assignments) = (1 to maxPreferenceLevel).foldLeft((jugglers, Map[Circuit, List[Juggler]]())) { // loop over the jugglers' preferred circuits
        case ((unassignedJugglers, assignments2), preferenceLevel) => assignJugglers(circuits, preferenceLevel, jugglersPerCircuit, unassignedJugglers, assignments2)
      }

      assignments.map {
        case (circuit, assignedJugglers) => (circuit, assignedJugglers.sortWith {
          case (j1, j2) => j1.score(circuit) > (j2.score(circuit))
        })
      }
        .toList
        .sortBy {
        case (circuit, _) => circuit.name
      }
    }
  }

  def parse(source: Source): (Seq[Circuit], Seq[Juggler]) = parse(source.getLines().toSeq)

  def parse(recs: Seq[String]): (Seq[Circuit], Seq[Juggler]) = {
    val (circuitRecs, jugglerRecs) = recs
      .map(rec => rec.trim)
      .filter(rec => !rec.isEmpty)
      .map(rec => rec.split(" "))
      .partition(values => values(0) == "C")

    val circuitsMap = circuitRecs.map(values =>
      (
        values(1),
        Circuit(
          name = values(1),
          attrs = parseAttrs(values.drop(2))
        )
      )
    ).toMap

    val jugglers = jugglerRecs.map(values => {
      val attrs = parseAttrs(values.drop(2))
      Juggler(
        name = values(1),
        attrs,
        values(5).split(',').map(circuitName => {
          val circuit = circuitsMap.getOrElse(
            circuitName,
            throw new RuntimeException(s"Invalid circuit name '$circuitName'!")
          )
          MatchScore(circuit, circuit.score(attrs))
        })
      )
    })

    (circuitsMap.values.toList, jugglers.toList)
  }

  private def parseAttrs(values: Seq[String]): Vector[Int] = values.take(3).map(parseAttr).toVector

  private def parseAttr(value: String): Int = value.substring(value.indexOf(":") + 1).toInt

  private def dot(a: Vector[Int], b: Vector[Int]): Int = a.zip(b).map { case (ax, bx) => ax * bx }.sum

  def formatAssignments(assignments: Seq[(Circuit, Seq[Juggler])]): Seq[String] = assignments.map {
    case (circuit, assignedJugglers) => s"${circuit.name} ${assignedJugglers.mkString(", ")}"
  }

  def main(args: Array[String]) {
    val in = if (args.isEmpty) {
      scala.io.Source.stdin
    } else {
      scala.io.Source.fromFile(args(0))
    }

    val input = in.getLines().toList

    val (circuits, jugglers) = parse(input)

    val assignments = AssignmentStrategy(circuits.toList, jugglers.toList)

    val output = formatAssignments(assignments)

    output.foreach(println)
  }
}
