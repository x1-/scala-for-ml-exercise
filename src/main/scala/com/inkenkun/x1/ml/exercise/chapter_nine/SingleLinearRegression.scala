package com.inkenkun.x1.ml.exercise.chapter_nine

import org.apache.commons.math3.stat.regression.SimpleRegression

case class SingleLinearRegression (
  xt: Vector[Double],
  expected: Vector[Double]
) {

  val model: Option[(Double, Double)] = train

  def zipToSeries(x: Vector[Double], y: Vector[Double]): Vector[Array[Double]] = {
    x.zip(y.view).map {
      case (_x, _y) => Array[Double](_x, _y)
    }
  }

  def train: Option[(Double, Double)] = {
    val regr = new SimpleRegression(true)
    regr.addData(zipToSeries(xt, expected).toArray)
    Some((regr.getSlope, regr.getIntercept))
  }
}
