package com.sgw.problems

import java.util.concurrent.TimeUnit

import org.scalatest.{Matchers, FlatSpec}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

class URLShortenerSpec extends FlatSpec with Matchers {
  private val DURATION = Duration(5, TimeUnit.SECONDS)

  "A URLShorter" should "return the same shortened URL for the same long URL" in {
    val longURL = "www.google.com/foo/bar/baz"

    val shortener = URLShortener()

    val future = shortener.shorten(longURL) flatMap {
      case (shortURL1, shortener1) =>
        shortener1.shorten(longURL) map {
          case (shortURL2, _) => (shortURL1, shortURL2)
        }
      }

    val (url1, url2) = Await.result(future, DURATION)

    url1.length should be (URLShortener.SHORT_URL_LENGTH)
    url1.startsWith(URLShortener.HOST) should be (right = true)
    url1 should be (url2)
  }

  it should "return None for an unknown id" in {
    val shortener = URLShortener()

    Await.result(shortener.lookup("foo"), DURATION) should be (None)
  }

  it should "return the long url from an id" in {
    val longURL = "www.google.com/foo/bar?baz=true"
    val shortener = URLShortener()
    val (shortURL, shortener1) = Await.result(shortener.shorten(longURL), DURATION)
    val id = shortURL.substring(URLShortener.HOST.length + 1)

    Await.result(shortener1.lookup(id), DURATION) should be (Some(longURL))
  }

  "Two shorteners" should "be able to be combined" in {
    val longURL1 = "www.google.com/foo/bar/baz"
    val longURL2 = "www.facebook.com/foo/bar"

    val shortener = URLShortener()

    // shorten the same URL twice but not using the returned shortener on the 2nd one
    val (shortURL1, shortener1) = Await.result(shortener.shorten(longURL1), DURATION)
    val (shortURL2, shortener2) = Await.result(shortener.shorten(longURL1), DURATION)

    shortURL1 should not be (right = shortURL2)

    // shortening the same URL again with the shorteners returned above, should return the same URLS
    val (shortURL3, shortener3) = Await.result(shortener1.shorten(longURL1), DURATION)

    shortURL3 should be (right = shortURL1)

    val (shortURL4, shortener4) = Await.result(shortener2.shorten(longURL1), DURATION)

    shortURL4 should be (right = shortURL2)

    // now shorten a 2nd URL with the one of the shorteners
    val (shortURL5, shortener5) = Await.result(shortener3.shorten(longURL2), DURATION)

    Await.result(shortener5.lookup(shortURL5.substring(URLShortener.HOST.length + 1)), DURATION) should be (right = Some(longURL2))
    Await.result(shortener4.lookup(shortURL5.substring(URLShortener.HOST.length + 1)), DURATION) should be (right = None)

    // combine the shorteners
    val shortener6 = shortener4 ++ shortener5

    // the combined shortener should be able to find the id of the last shortened URL
    Await.result(shortener6.lookup(shortURL5.substring(URLShortener.HOST.length + 1)), DURATION) should be (right = Some(longURL2))
  }
}
