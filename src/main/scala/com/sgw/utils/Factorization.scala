package com.sgw.utils

object Factorization {
  private lazy val primes = PrimeSieve.primesList(150000).map(_.toLong).toArray

  def primeFactors(x: Long, primeIndex: Int = 0, factors: List[Long] = List()): List[Long] = try {
    val f1 = primes(primeIndex)

    if (f1 >= x) return factors

    if (x % f1 == 0) {
      val f2 = x / f1

      f1 :: primeFactors(f2, primeIndex, f2 :: factors)
    } else {
      primeFactors(x, primeIndex + 1, factors)
    }
  } catch {
    case t: Throwable => throw new RuntimeException(s"Field to find the prime factors for $x.", t)
  }

  def factors(x: Long): List[Long] = {
    val pfs = primeFactors(x)

    val pfs2 = pfs.takeWhile(_ <= Math.sqrt(x)) // only need factors <= sqrt(x)

    val ofs = try {
      (2 to pfs2.size).
        map(i => pfs2.combinations(i).map(_.product).filter(_ < x).filter(x % _ == 0).toList).
        takeWhile(_.nonEmpty).
        flatten
    } catch {
      case t: Throwable => throw new RuntimeException(s"Failed to get the factors for $x. pfs=${pfs.mkString(",")}", t)
    }

    //    println(ofs.mkString(","))
    (1L :: x :: pfs ++ ofs).distinct
  }
}
