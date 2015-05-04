package com.sgw.problems

import scala.io.Source
import scala.util.parsing.json.JSON

object Bundles {
  private def except(msg: String) = throw new RuntimeException(msg)

  private case class Product(id: Int, price: Double, bundleId: Option[Int] = None)

  private object Product {
    def apply(productMap: Map[String, Double], bundleId: Option[Int]): Product = Product(
      productMap.getOrElse("id", except("Product doesn't have an id.")).toInt,
      productMap.getOrElse("price", except("Product doesn't have a price.")),
      bundleId
    )
  }

  private case class Bundle(id: Int, products: Set[Product], total: Double)

  private object Bundle {
    def apply(bundleMap: Map[String, Any]): Bundle = {
      val bundleId = bundleMap.getOrElse("bundle_id", except("Bundle doesn't have a bundle_id.")).asInstanceOf[Double].toInt

      val products = bundleMap.getOrElse("products", except("Bundle doesn't have any products.")).asInstanceOf[List[Map[String, Double]]]
        .map(productMap => Product(productMap, Some(bundleId))).toSet

      val total = bundleMap.getOrElse("total", except("Bundle doesn't not have a total.")).asInstanceOf[Double]

      Bundle(bundleId, products, total)
    }
  }

  /**
   * Finds all of the valid combinations of products constrained by the product's bundling.
   *
   * @param productIdsBundleIdsAndPricesList a list of tuples where each tuple contains a product id, bundle id, and product price
   * @param bundleMap a map used to
   * @param combo an accumulator for the current bundle combination
   * @param result an accumulator for the valid bundle combinations
   *
   * To slow, need to try pruning based on:
   * * max/min bundle prices
   * * ?
   */
  private def combinations(
    productIdsBundleIdsAndPricesList: List[List[(Int, Int, Double)]],
    bundleMap: Map[Int, Bundle],
    combo: List[(Int, Int, Double)] = List(),
    result: List[List[(Int, Int, Double)]] = List()
  ): List[List[(Int, Int, Double)]] = {
    // done yet?
    if (productIdsBundleIdsAndPricesList.isEmpty) {
      // only consider valid combinations
      if (isValid(combo, bundleMap))
        return combo :: result
      else
        return result
    }

    // get the next row of tuples
    val bundleIdsAndPrices = productIdsBundleIdsAndPricesList.head

    bundleIdsAndPrices.foldLeft(result) {
      case (result2, bundleIdAndPrice) =>
        combinations(productIdsBundleIdsAndPricesList.tail, bundleMap, bundleIdAndPrice :: combo, result2)
    }
  }

  /**
   * Returns true if the specified combination of bundles is valid.
   */
  private def isValid(combo: List[(Int, Int, Double)], bundleMap: Map[Int, Bundle]): Boolean = {
    val productSet = combo.foldLeft(Set[Product]()) {
      case (productSet2, (_, bundleId, _)) => bundleMap(bundleId).products ++ productSet2
    }

    productSet.size == combo.size
  }

  def main(args: Array[String]) {
    if (args.length != 1) {
      println("You must specify the path to a resource directory containing a products.json and bundles.json file.")
      System.exit(1)
    }

    // here where the input files live
    val dir = if (args(0).endsWith("/")) args(0) else args(0) + "/"

    // skip reading the products under the assumption that the bundles.json file contains all of the products

    // read the bundles file
    val bundlesJSON = Source.fromInputStream(getClass.getResourceAsStream(dir + "bundles.json")).getLines().mkString("\n")

    // read the bundles under the assumption that they only reference products in the products.json file
    val bundles = JSON.parseFull(bundlesJSON).get.asInstanceOf[List[Map[String,Any]]].map(Bundle.apply)
    val bundleMap = bundles.map(bundle => (bundle.id, bundle)).toMap

    // pivot the list of bundles into a list of product id, bundle id, and product price
    val productIdBundleIDAndPrice = bundles.flatMap(bundle => bundle.products.map(product => (product.id, bundle.id, product.price)))

    // group all of the bundle ids and product prices by product id
    val productIdToBundleIdAndPriceMap = productIdBundleIDAndPrice.groupBy {
      case (productId, _, _) => productId
    }

    // don't need the redundant product id, so strip it off
    val productIdBundleIdsAndPricesList = productIdToBundleIdAndPriceMap.map {
      case (_, list) => list
    }.toList

    // find all of the valid bundle combinations
    val combos = combinations(productIdBundleIdsAndPricesList, bundleMap).reverse

    // create a tuple for each bundle combination and its total cost
    val comboAndPriceTotals = combos.map(combo => (combo, combo.foldLeft(0.0) {
      case (total, (_, _, price)) => total + price
    }))

    // find the best bundle combination based on total cost
    val bestComboAndPrice = comboAndPriceTotals.minBy {
      case (combo1, cost1) => cost1
    }

    // get the best bundle ids and order-total
    val (bundleIds, orderTotal) = bestComboAndPrice match {
      case (combo, price) => (
        combo.foldLeft(Set[Int]()) {
          case (bundleIds2, (_, bundleId, _)) => bundleIds2 + bundleId
        },
        price
        )
    }

    println(s"""|{
                |  "bundle_ids": [${bundleIds.toList.sorted.mkString(", ")}],
                |  "order_total": $orderTotal
                |}""".stripMargin)
  }
}

