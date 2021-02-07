package com.sgw.problems

import java.io.File

import scala.annotation.tailrec
import scala.collection.SortedSet
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
    * A Juggler whose primary responsibility is to find the circuit that's the best fit for his or her
    * skills and circuit preferences.
    *
    * @param name the juggler's name
    * @param attrs the juggler's coordination (H), endurance (E), and pizzazz (P) attributes
    * @param preferredCircuits the juggler's preferred circuits
    * @param circuits all of the circuits
    */
  case class Juggler(
    name: String,
    attrs: Vector[Int],
    preferredCircuits: Seq[Circuit],
    circuits: Seq[Circuit]
  ) {
    private val preferredCircuitIterator = preferredCircuits.toIterator
    private val circuitIterator = circuits.toIterator

    /**
      * Returns the next preferred circuit or, if none of those are left, one of the other circuits.
      */
    def nextPreferredCircuit: Circuit = synchronized {
      if (preferredCircuitIterator.hasNext) {
        preferredCircuitIterator.next()
      } else if (circuitIterator.hasNext) {
        circuitIterator.next()
      } else {
        throw new RuntimeException(s"Juggler $name ran out of circuits!")
      }
    }

    override def toString = s"""$name ${preferredCircuits.map { circuit =>
      s"""${circuit.name}:${circuit.score(this)}"""
    }.mkString(" ")}"""
  }

  /**
    * A juggling circuit whose responsibilities are to:
    * <ul>
    *   <li>score a juggler's skills against this circuit's attributes,</li>
    *   <li>maintain a sorted and bounded list of jugglers by score, and</li>
    *   <li>tell jugglers when they need to shift to another circuit.</li>
    * </ul>
    *
    * @param name this circuit's name
    * @param attrs the circuit's coordination (H), endurance (E), and pizzazz (P) attributes
    * @param numJugglersPerCircuit the number of jugglers allowed in this circuit
    */
  case class Circuit(name: String, attrs: Vector[Int], numJugglersPerCircuit: Int) {

    private implicit object JugglerOrdering extends Ordering[(Int, Juggler)] {
      /**
        * Order's jugglers ascending by score. If two juggler's have the same score, they're ordered by name.
        */
      def compare(scoreAndJuggler1: (Int, Juggler), scoreAndJuggler2: (Int, Juggler)): Int = {

        if (scoreAndJuggler1._1 == scoreAndJuggler2._1) {
          scoreAndJuggler1._2.name.compare(scoreAndJuggler2._2.name)
        } else if (scoreAndJuggler1._1 > scoreAndJuggler2._1) {
          1
        } else {
          -1
        }
      }
    }

    private var _jugglers = SortedSet[(Int, Juggler)]()

    /**
      * Returns a copy of this Circuit's juggler's list in priority (highest score first) order.
      */
    def jugglers: List[Juggler] = _jugglers.toList.reverse.map { case (_, juggler) =>
      juggler
    }

    private def dot(a: Vector[Int], b: Vector[Int]): Int = a.zip(b).map { case (ax, bx) => ax * bx }.sum

    /**
      * Scores a juggler based on how good a fit the juggler's attributes (skills) are compared to this circuit's
      * attributes.
      *
      * @param juggler the juggler
      *
      * @return the juggler's score relative to this circuit
      */
    def score(juggler: Juggler): Int = dot(attrs, juggler.attrs)

    /**
      * Assigns the specified juggler to this circuit. If the addition of the new juggler causes the circuit to have
      * too many jugglers, the juggler with the lowest score is added to the returned list of unassigned jugglers.
      *
      * @param juggler the juggler to be assigned to this circuit
      * @param unassignedJugglers the list of unassigned jugglers
      *
      * @return the list of unassigned jugglers
      */
    def assign(juggler: Juggler, unassignedJugglers: Seq[Juggler]): Seq[Juggler] = {

      _jugglers = _jugglers + ((score(juggler), juggler)) // add the juggler's score and the juggler to this circuit (sorted by score)

      // if this circuit has too many jugglers ...
      if (_jugglers.size > numJugglersPerCircuit) {

        val rejectedJuggler: Juggler = _jugglers.head._2 // get the juggler with the lowest score

        _jugglers = _jugglers.tail // and drop the juggler from this circuit

        rejectedJuggler +: unassignedJugglers // add the juggler to the list of unassigned jugglers
      } else {
        unassignedJugglers
      }
    }

    override def toString = s"""$name ${jugglers.map(_.toString).mkString(", ")}"""
  }

  /**
    * Parses a list of input records (strings) into a list of circuits and jugglers.
    *
    * @param records input records
    */
  def parse(records: Seq[String]): (Seq[Circuit], Seq[Juggler]) = {

    def parseAttr(value: String): Int = value.substring(value.indexOf(":") + 1).toInt

    def parseAttrs(values: Seq[String]): Vector[Int] = values.take(3).map(parseAttr).toVector

    val (circuitRecs, jugglerRecs) = records
      .map(record => record.trim)
      .filter(record => !record.isEmpty)
      .map(record => record.split(" "))
      .partition(values => values(0) == "C")

    val numJugglersPerCircuit: Int = jugglerRecs.size / circuitRecs.size

    assert(jugglerRecs.size % numJugglersPerCircuit == 0)

    val circuits = circuitRecs.map(values => Circuit(
      name = values(1),
      attrs = parseAttrs(values.drop(2)),
      numJugglersPerCircuit
    ))

    val circuitNameToCircuitMap = circuits.map(circuit => (circuit.name, circuit)).toMap

    val jugglers = jugglerRecs.map(values => {
      val preferredCircuits = values(5).split(',').map(circuitName => circuitNameToCircuitMap(circuitName)).toSeq

      Juggler(
        name = values(1),
        attrs = parseAttrs(values.drop(2)),
        preferredCircuits = preferredCircuits,
        circuits = circuits
      )
    })

    (circuits, jugglers)
  }

  /**
    * Check the juggling circuits to make sure they all contain the correct number of jugglers and that all of the
    * jugglers are in one and only one circuit.
    *
    * @param circuits the list of circuits with jugglers assigned to them
    * @param jugglers the list of jugglers
    */
  def checkCircuits(circuits: Seq[Circuit], jugglers: Seq[Juggler]): Unit = {
    val numJugglersPerCircuit = jugglers.size / circuits.size

    val jugglerSet = circuits.foldLeft(Set[Juggler]()) {
      case (jugglerSet, circuit) => {
        assert(circuit.jugglers.size == numJugglersPerCircuit)

        jugglerSet ++ circuit.jugglers
      }
    }

    assert(jugglerSet.size == jugglers.size)
  }


  /**
    * Assigns all of the unassigned jugglers to circuits.
    *
    * @param unassignedJugglers the list of unassigned jugglers
    */
  @tailrec
  def assign(unassignedJugglers: Seq[Juggler]): Unit = {

    if (unassignedJugglers.isEmpty) return

    val nextJuggler = unassignedJugglers.head

    val nextCircuit = nextJuggler.nextPreferredCircuit

    assign(nextCircuit.assign(nextJuggler, unassignedJugglers.tail))
  }

  def parseAndAssignFromInput(input: Seq[String]): Seq[Circuit] = {

    val (circuits, jugglers) = parse(input)

    val startTime: Long = System.nanoTime()

    assign(jugglers)

    val endTime: Long = System.nanoTime()

    val elapseTime: Long = endTime - startTime

    println(s"Num circuits: ${circuits.size}")
    println(s"Num jugglers: ${jugglers.size}")
    println(s"Assignment elapse time (ns): $elapseTime (ns), ${elapseTime/1e6} (ms), ${elapseTime/circuits.size} (ns/circuit), ${elapseTime/jugglers.size} (ns/juggler)")

    checkCircuits(circuits, jugglers)

    circuits
  }

  def parseAndAssignFromSource(source: Source): Seq[Circuit] = try {
    parseAndAssignFromInput(source.getLines().toList)
  } finally {
    source.close()
  }

  def parseAndAssignFromFile(file: File): Seq[Circuit] = parseAndAssignFromSource(Source.fromFile(file))

  def parseAndAssignFromFile(file: String): Seq[Circuit] = parseAndAssignFromSource(Source.fromFile(file))

  def parseAndAssignFromResource(resource: String): Seq[Circuit] = parseAndAssignFromSource(Source.fromResource(resource))

  /**
    * Transforms the circuits, and their assigned jugglers, into a list of output strings.
    *
    * @param circuits the list of circuits with jugglers assigned to them
    */
  def formatAssignments(circuits: Seq[Circuit]): Seq[String] = circuits.map(circuit => circuit.toString)
}
