package com.sgw.problems

import com.twitter.algebird.{BF, BloomFilterMonoid}

import scala.annotation.tailrec
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

object URLShortener {
  val ID_LENGTH = 7
  val HOST = "tny.us"
  val SHORT_URL_LENGTH = HOST.length + 1 + ID_LENGTH
}

/**
 * A mock up of a URL shortener.
 *
 * TODO: replace the Maps with LruMaps
 */
case class URLShortener(
  idBloomFilter: BF = new BloomFilterMonoid(6, 32, 1).zero,
  urlBloomFilter: BF = new BloomFilterMonoid(6, 32, 1).zero,
  idToURLMap: Map[String, String] = Map[String, String](),
  urlToIdMap: Map[String, String] = Map[String, String]()
) {
  /**
   * Returns a shorted URL for the specified long URL.
   *
   * @param longURL the URL to be shortened
   *
   * @return the short URL
   */
  def shorten(longURL: String): Future[(String, URLShortener)] = future {
    synchronized {
      // if we might have already seen the url ...
      val (id, newURLShortener) = if (urlBloomFilter.contains(longURL).isTrue) {
        // if the url is in the LruMap ...
        urlToIdMap.get(longURL).map(id => (id, this)).getOrElse {
          // see if the DB contains the URL, and if so, return the URL's id
          // TODO for now, assume the DB doesn't contain the URL and just get the shortened URL's id
          shortenImpl(longURL)
        }
      } else {
        // we haven't seen the URL, so shorten it
        shortenImpl(longURL)
      }

      (URLShortener.HOST + "/" + id, newURLShortener)
    }
  }

  private def shortenImpl(longURL: String): (String, URLShortener) = {
    @tailrec
    def uniqueId: String = {
      // calculate a random string id
      val id = Random.alphanumeric.take(URLShortener.ID_LENGTH).mkString("")

      // if the id is already in use, calculate a new one, otherwise return the id
      if (idBloomFilter.contains(id).isTrue) uniqueId else id
    }

    // get a unique id for the URL
    val id = uniqueId

    // TODO: put the id and URL into the DB

    // return the id and a new URLShortener
    (id, URLShortener(
      idBloomFilter + id,
      urlBloomFilter + longURL,
      idToURLMap.updated(id, longURL),
      urlToIdMap.updated(longURL, id)
    ))
  }

  def ++(other: URLShortener): URLShortener = URLShortener(
    idBloomFilter ++ other.idBloomFilter,
    urlBloomFilter ++ other.urlBloomFilter,
    idToURLMap ++ other.idToURLMap,
    urlToIdMap ++ other.urlToIdMap
  )

  def lookup(id: String): Future[Option[String]] = future {
    synchronized {
      // if we might already know the id ...
      if (idBloomFilter.contains(id).isTrue) {
        // see if the local LruMap contains the id
        idToURLMap.get(id) orElse {
          // see if the DB contains the id
          // TODO for now, just return None
          None
        }
      } else { // we don't know the id
        None
      }
    }
  }
}
