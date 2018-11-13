package com.inkenkun.x1.ml.exercise.chapter_nine

/**
 * 最急降下法で線形モデルを求めます
 *
 * @param a 学習率
 * @param iterationNum イテレーションの最大数
 * @param stopRate これより小さくなったら停止する
 */
case class LinearLikeliRegression(
  a: Double = 0.01,
  iterationNum: Int = 10000,
  stopRate: Double = 0.0001
) {

  final def train(
    X: Array[Array[Double]],
    Y: Array[Double],
    theta: Array[Double] = Array.empty[Double]
  ): Array[Double] = {
    val newX     = X.map(xs => 1d +: xs)
    val newTheta = if (theta.nonEmpty) theta else Array.fill(newX.head.length)(0.5d)

    run(newX, Y, newTheta)
  }

  @scala.annotation.tailrec
  private final def run(
    X: Array[Array[Double]],
    Y: Array[Double],
    theta: Array[Double] = Array.empty[Double],
    iter: Int = 0
  ): Array[Double] = {
    val hypothtsis: Array[Double] => Double = { xs =>
      (theta zip xs).foldLeft(0d) { case (acc, (t, x)) =>
        acc + (t * x)
      }
    }
    val newTheta = theta.zipWithIndex map { case (v, i) =>
      v - cost(X, Y, a, i)(hypothtsis)
    }
    if (newTheta.exists(x => x.isNaN || x.isInfinity)) {
      println(s"NaN exists in newTheta: ${newTheta.mkString(",")}")
      theta
    } else {
      val maxDiff  = newTheta.zip(theta).map(ts => Math.abs(ts._1 - ts._2)).max

      if (iter >= iterationNum || maxDiff < stopRate) {
        println(s"Because `iter($iter) >= iterationNum($iterationNum) || maxDiff($maxDiff) < stopRate($stopRate)`, stop iteration.")
        newTheta
      }
      else {
        run(X, Y, newTheta, iter + 1)
      }
    }
  }

  private def cost(x: Array[Array[Double]], y: Array[Double], a: Double, i: Int)(
    hypothesis: Array[Double] => Double
  ): Double = {
    val xy = x zip y
    xy.foldLeft(0d){ case (acc, (x1, y1)) =>
      acc + (hypothesis(x1) - y1) * x1(i)
    } * (a / xy.length)
  }
}
