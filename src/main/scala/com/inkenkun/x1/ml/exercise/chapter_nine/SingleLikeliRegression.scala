package com.inkenkun.x1.ml.exercise.chapter_nine

/**
 * 最急降下法で線形モデルを求めます
 *
 * @param a 学習率
 * @param iterationNum イテレーションの最大数
 * @param stopRate これより小さくなったら停止する
 */
case class SingleLikeliRegression(
  a: Double = 0.01,
  iterationNum: Int = 10000,
  stopRate: Double = 0.00001
) {
  @scala.annotation.tailrec
  final def train(
    X: Array[Double],
    Y: Array[Double],
    theta: Array[Double] = Array(0.5, 0.5),
    iter: Int = 0
  ): Array[Double] = {
    val hypothtsis: Double => Double = x => theta(0) + theta(1) * x
    val newTheta = theta.zipWithIndex map { case (v, i) => v - cost(X, Y, a, i)(hypothtsis) }
    val maxDiff  = newTheta.zip(theta).map(ts => Math.abs(ts._1 - ts._2)).max

    if (iter >= iterationNum || maxDiff < stopRate) {
      println(s"Because `iter($iter) >= iterationNum($iterationNum) || maxDiff($maxDiff) < stopRate($stopRate)`, stop iteration.")
      newTheta
    }
    else
      train(X, Y, newTheta, iter + 1)
  }

  private def cost(x: Array[Double], y: Array[Double], a: Double, p: Double)(
    hypothesis: Double => Double
  ): Double = {
    val xy = x zip y
    xy.foldLeft(0d){ case (acc, (x1, y1)) =>
      acc + (hypothesis(x1) - y1) * Math.pow(x1, p)
    } * (a / xy.length)
  }
}
