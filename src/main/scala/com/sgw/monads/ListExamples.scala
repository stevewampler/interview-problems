package com.sgw.monads

/**
  * @author swampler
  */
object ListExamples extends App {

  // building lists
  val list1: List[Int] = List(1, 2, 3, 4)
  val list2: List[String] = List("a", "b", "c", "d")

  // list of "Any" type
  val list3: List[Any] = List(1, "a", 2, "b", 3, "c", 4, "d")

  // building a list with the cons operator and Nil
  val list4 = "a" :: "b" :: "c" :: "d" :: Nil

  // creating an empty list using Nil
  val emptyList1: List[Nothing] = Nil
  val emptyList2: List[Int] = List.empty[Int]

  println(s"list1 = $list1")
  println(s"list2 = $list2")
  println(s"list3 = $list3")
  println(s"list4 = $list4")
  println(s"emptyList1 = $emptyList1")
  println(s"emptyList2 = $emptyList2")

  // head of a list
  println(s"list1.head = ${list1.head}")



  // mapping over one list inside of mapping over another
  val listOfLists: List[List[String]] = list1.map(num => list2.map(char => s"$char$num"))
  println(s"listOfLists = $listOfLists")

  // flattening a list of lists
  val flattenedList: List[String] = listOfLists.flatten
  println(s"flattenedList = $flattenedList")

  // use flatMap instead
  val flattenedList2: List[String] = list1.flatMap(num => list2.map(char => s"$char$num"))
  println(s"flattenedList2 = $flattenedList2")

  // zipping two lists together to create a list of pairs
  val zippedLists: List[(Int, String)] = list1.zip(list2)
  println(s"zippedLists = $zippedLists")

  println {
    // combining the zipped list's pairs
    zippedLists.map { case (num, char) =>
      s"$char$num"
    }
  }

  // combinations of two lists using a for comprehension (for loop)
  val combinations: List[(Int, String)] = for {
    a <- list1 // list1.map
    b <- list2 // list1.map
  } yield (a, b)

  println(s"combinations = $combinations")

  // folding over a list to sum the list's values
  val sum1: Int = list1.foldLeft(0) { case (accumulator, ithValue) =>
    accumulator + ithValue
  }
  println(s"sum1 = $sum1")

  // or just call "sum"
  val sum2: Int = list1.sum
  println(s"sum1 = $sum2")

  // head of a list
  val head1 = list1.head
  println(s"head1 = $head1")

  // tail of a list
  val tail1 = list1.tail
  println(s"tail1 = tail1")

  // head of an empty list -> error
  try {
    val head1 = emptyList1.head
    println(s"head1 = $head1")
  } catch {
    case ex: Exception => println(ex)
  }

  // instead, use headOption (no null check or try/catch!)
  val maybeHead2: Option[Int] = emptyList2.headOption
  println(s"maybeHead2 = $maybeHead2")

  // process the option in a functional way using map and getOrElse ...
  val value2 = maybeHead2.map(value => value + 1).getOrElse(-1)
  println(s"value2 = $value2")

  // if the head wasn't empty, then the map function would be called ...
  val maybeHead1: Option[Int] = list1.headOption
  println(s"maybeHead1 = $maybeHead1")
  val value1 = maybeHead1.map(value => value + 1).getOrElse(-1)
  println(s"value1 = $value1")
}
