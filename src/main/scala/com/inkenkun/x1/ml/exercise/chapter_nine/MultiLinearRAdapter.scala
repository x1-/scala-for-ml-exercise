package com.inkenkun.x1.ml.exercise.chapter_nine

import org.apache.commons.math3.stat.regression.OLSMultipleLinearRegression


class MultiLinearRAdapter extends OLSMultipleLinearRegression {
  def createModel(y: Vector[Double], x: Vector[Array[Double]]): Unit = {
    super.newSampleData(y.toArray, x.toArray)
  }
  def weights: Array[Double] = estimateRegressionParameters
  def rss: Double = calculateResidualSumOfSquares
}
