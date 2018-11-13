package com.inkenkun.x1.ml.exercise.chapter_nine

import models.RegressionModel

class MultiLinearRegression (
  xt: Vector[Array[Double]],
  expected: Vector[Double]
) {
  def train: Option[RegressionModel] = {
    val mLr = new MultiLinearRAdapter
    mLr.createModel(
      expected,
      xt
    )
    Some(RegressionModel(mLr.weights, mLr.rss))
  }
}
