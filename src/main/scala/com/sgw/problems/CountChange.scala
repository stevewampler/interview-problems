package com.sgw.problems

/**
  * Write a recursive function that counts how many different ways you can make change for an amount,
  * given a list of coin denominations.
  * For example, there are 3 ways to give change for 4 if you have coins with
  * denomination 1 and 2: 1+1+1+1, 1+1+2, 2+2.
  */
object CountChange {
  def countChange(money: Int, coins: List[Int]): Int = {
    // coins assumed to be sorted from smallest to largest denom.
    def go(money: Int, coins: List[Int], changeCombo: List[Int] = List(), changeCombos: List[List[Int]] = List()): List[List[Int]] =
      if (coins.isEmpty)
        changeCombos
      else {
        val newMoney = money - coins.head

        if (newMoney < 0)
          changeCombos // no need to check the tail (larger coins)
        else {
          val newChangeCombo = coins.head :: changeCombo

          // if we found a solution ...
          if (newMoney == 0)
            newChangeCombo :: changeCombos
          else {
            // keep trying the current head
            go(newMoney, coins, newChangeCombo, changeCombos)
          } ++ go(money, coins.tail, changeCombo, changeCombos)
        }
      }

    val sortedCoins = coins.sortWith(_.compareTo(_) < 0)

    //    println(s"sortedCoins = $sortedCoins")

    val results = go(money, sortedCoins)

    //    println(s"results = $results")

    results.size
  }
}
