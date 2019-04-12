package com.sgw.problems

/**
  * Write a function to return the top 10 search results given a collection of document strings and a set of
  * string queries. The function's output should contain a list of job description ids, where a job description
  * id is just the index of the job description in the sequence of job descriptions.
  *
  * Solution: Use a reverse index that maps each word in each document to a set of document ids.
  * Each word in a query can then be mapped to a document id, and the document ids can be counted and the count
  * used to sort the document ids.
  */
object FuzzySearch {

  private def fuzzySearch(reverseIndex: Map[String, Set[Int]], str: String): List[Int] = {
    str.split(' ').toSet.foldLeft(Map[Int, Int]()) { case (docIdToCountMap, word) =>
      reverseIndex.getOrElse(word, Set()).foldLeft(docIdToCountMap) { case (docIdToCountMap, docId) =>
        docIdToCountMap.updated(docId, docIdToCountMap.getOrElse(docId, 0) + 1)
      }
    }.toList.sortBy { case (docId, count) =>
      (-count, docId)
    }.take(10).filter { case (_, count) =>
      count > 0
    }.map { case (docId, _) =>
      docId
    }
  }

  // O(docs.size * doc.size)
  private def createReverseIndex(docs: List[String]): Map[String, Set[Int]] =
    docs.map(_.toLowerCase).zipWithIndex.flatMap { case (doc, index) =>
      doc.split(' ').distinct.map { word =>
        word -> index
      }.toList
    }.foldLeft(Map[String, Set[Int]]()) { case (map, (word, index)) =>
      map.updated(word, map.getOrElse(word, Set()) + index)
    }

  def fuzzySearch(docs: List[String], queries: List[String]): List[(String, List[Int])] = {
    // word to docIds map
    val reverseIndex: Map[String, Set[Int]] = createReverseIndex(docs)

    queries.map { query =>
      query.toLowerCase
    }.map { query =>
      query -> fuzzySearch(reverseIndex, query)
    }
  }

  def main(args: Array[String]): Unit = {
    val docs = Array(
      "a quick brown fox jumped over the fence",
      "why did the chicken cross the road under the fence",
      "hello brown fox and chicken",
      "hello world",
      "no match",
      "fox 1",
      "fox 2",
      "fox 3",
      "fox 4",
      "fox 5",
      "fox 6",
      "fox 7",
      "fox 8",
      "fox 9",
      "fox 10",
      "fox 11",
      "fox 12"
    )

    val queries = List(
      "quick brown",
      "hello",
      "fox",
      "foo bar",
      "hello world"
    )

    fuzzySearch(docs.toList, queries).foreach { case (query, docIds) =>
      println("-----")
      println(query)
      docIds.map { docId =>
        docId -> docs(docId)
      }.foreach(println)
    }
  }
}
