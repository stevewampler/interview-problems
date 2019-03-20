package com.sgw.problems

/*
You are in charge of a display advertising program. Your ads are displayed on websites all over the internet.
You have some CSV input data that counts how many times that users have clicked on an ad on each individual domain.
Every line consists of a count and a domain name. It looks like this:

counts = [ "900,google.com",
     "60,mail.yahoo.com",
     "10,mobile.sports.yahoo.com",
     "40,sports.yahoo.com",
     "300,yahoo.com",
     "10,stackoverflow.com",
     "2,en.wikipedia.org",
     "1,es.wikipedia.org",
     "1,mobile.sports" ]

Write a function that takes this input as a parameter and returns a data structure containing the number of hits
that were recorded on each domain AND each domain under it.
For example, an impression on "mail.yahoo.com" counts for "mail.yahoo.com", "yahoo.com", and "com".
(Subdomains are added to the left of their parent domain. So "mail" and "mail.yahoo" are not valid domains.
Note that "mobile.sports" appears as a separate domain as the last item of the input.)

Sample output (in any order/format):

getTotalsByDomain(counts)
1320    com
 900    google.com
 410    yahoo.com
  60    mail.yahoo.com
  10    mobile.sports.yahoo.com
  50    sports.yahoo.com
  10    stackoverflow.com
   3    org
   3    wikipedia.org
   2    en.wikipedia.org
   1    es.wikipedia.org
   1    mobile.sports
   1    sports

Things I learned:
- it's hard to code without an IDE
- don't assume the interviewer will give you a lot of help (nor should they), especially on the big O stuff. You've got
  to go back to the code and inspect it.
- you can use the web to look stuff up (at least in this case)
- split takes either a regex or a character
- groupBy returns a Map(key, Seq[(key, value)])
- you know how to do this. The hard parts were the lack of helpful hints from the IDE, so you have to think about,
  and perhaps be explicit about types, but be loose about them.
- I got hung up on:
  - split taking a regex
  - the groupBy returning a Map, so all of the .map calls returned a map, and when I flipped the result to be
    "count -> string", I also got a map, but since there were duplicate values for a count of 1, one or more of them
    got thrown out. The thing was to convert the map back to a list of pairs before reversing the pairs.
- It's okay to use shortcuts like "_._1", but be careful with the syntax. It's easy to write "_.1" and then get
  confused.
- Read the compile errors and especially the line numbers.
- Use your intuition.
- I worried too much about the order of the domain name and count in each output line. It would have been sufficient
to generate lines where the domain came first.
*/

object GetTotalsByDomain extends App {
  val counts = Vector(
    "900,google.com",
    "60,mail.yahoo.com",
    "10,mobile.sports.yahoo.com",
    "40,sports.yahoo.com",
    "300,yahoo.com",
    "10,stackoverflow.com",
    "2,en.wikipedia.org",
    "1,es.wikipedia.org",
    "1,mobile.sports"
  )

  // O(n * (s + m)) where:
  //
  // - n is the number of items in counts (9 in the example),
  // - s is the max number of characters in a domain name (23 in the example), and
  // - m is the max number of names in a domain name (4 in the example)
  def getTotalsByDomain(counts: Vector[String]): Seq[(Int, String)] = {
    // map is O(n), split is O(s)
    counts.map { str => str.split(',') }.map { case Array(count, domain) =>
      count.toInt -> domain
    }.flatMap { case (count, domain) =>
      // split is O(s) and reverse is O(m)
      val arrR = domain.split('.').reverse
      val c = arrR.size

      // O(m)
      (1 to c).foldLeft(List[String]()) { case (acc, i) =>
        // O(m)
        arrR.take(i).mkString(".") :: acc
      }.map { str =>
        str -> count
      }
    }.groupBy { case (str, _) =>
      str
    }.map { case (key, values) =>
      key -> values.map(_._2).sum
    }.toList.map { case (key, count) =>
      count -> key.split('.').reverse.mkString(".")
    }
  }

  // Here's a somewhat better implementation that simply returns a map of domain -> count and therefore
  // avoids the need for the toList. It also avoids doing a couple of calls to "reverse" and one call to split.
  // O(n * (m + s)) where:
  // - n is the number of items in the count vector,
  // - m is the max number of parts in a domain name, and
  // - s is the max length of a domain name in the original list
  def getTotalsByDomain2(counts: Vector[String]): Map[String, Int] = {
    counts.map { str =>
      str.split(',')
    }.flatMap { case Array(count, domain) =>
      val arr = domain.split('.') // domain parts
      val c = arr.length // the number of parts

      // fold to gather the domains and sub-domains into a list
      (0 until c).foldLeft(List[String]()) { case (acc, i) =>
        arr.drop(i).mkString(".") :: acc
      }.map { domain => // pair each domain in the list with the integer count
        domain -> count.toInt
      }
    }.groupBy { case (domain, _) => // group the domains to get a Map[domain, Seq[(domain, count)]]
      domain // group by the domain
    }.mapValues { values => // Seq[(key, value)] where the value is still a String count
      values.map(_._2).sum // convert the values into integers and sum them
    }
  }

  // what it might look like to code it fast
  def getTotalsByDomain3(counts: Vector[String]): Map[String, Int] =
    counts.map(_.split(',')).map { case Array(count, domain) =>
      count.toInt -> domain.split('.')
    }.flatMap { case (count, domainParts) =>
      domainParts.indices.foldLeft(List[String]()) { case (acc, i) =>
        domainParts.drop(i).mkString(".") :: acc
      }.map(_ -> count)
    }.groupBy(_._1).mapValues(_.map(_._2).sum)

  def getTotalsByDomain4(counts: Vector[String]) = counts.map(_.split(',')).flatMap { case Array(c, d) =>
    val arr = d.split('.')
    arr.indices.foldLeft(List[(String, String)]()) { case (acc, i) =>
      (arr.drop(i).mkString("."), c) :: acc
    }
  }.groupBy(_._1).mapValues(_.map(_._2.toInt).sum)

  val result = getTotalsByDomain(counts)
  val expected = List(
    1320 -> "com",
    900 -> "google.com",
    410 -> "yahoo.com",
    60 -> "mail.yahoo.com",
    10 -> "mobile.sports.yahoo.com",
    50 -> "sports.yahoo.com",
    10 -> "stackoverflow.com",
    3 -> "org",
    3 -> "wikipedia.org",
    2 -> "en.wikipedia.org",
    1 -> "es.wikipedia.org",
    1 -> "mobile.sports",
    1 -> "sports"
  )

//  println("result")
//  result.foreach(println)
//  println("expected")
//  expected.foreach(println)

  assert(
    result.toSet == expected.toSet
  )

  val result2 = getTotalsByDomain2(counts)
  val expected2 = expected.map(_.swap)

  assert(
    result2.toSet == expected2.toSet
  )

  val result3 = getTotalsByDomain3(counts)

  assert(
    result3.toSet == expected2.toSet
  )

  val result4 = getTotalsByDomain4(counts)
  assert(
    result4.toSet == expected2.toSet
  )
}
