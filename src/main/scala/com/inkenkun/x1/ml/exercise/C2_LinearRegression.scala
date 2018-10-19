package com.inkenkun.x1.ml.exercise

import java.io.File

import breeze.generic._
import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.plot._
import com.github.tototoshi.csv._
//import vegas._
//import vegas.render.WindowRenderer._

import scala.util.Random

object C2_LinearRegression {

  def main(args: Array[String]): Unit = {

    // CSVを読み込みます
    val df = csvread(new File("BostonHousing.csv"), skipLines = 1)

    val y = df(::, 13)
    val X = df(::, 0 to 12)

    // 犯罪率をプロットしてみます
    val f = Figure()
    val p = f.subplot(0)
    p += plot(X(::, 0), y, '.')
    p.xlabel = "per capita crime rate by town"
    p.ylabel = "Median value of owner-occupied homes in $1000's"
    f.saveas("BostonHousing.png")

    // 正規化します
    val stdX = normalize(X(::, *))


    val omega = ω(stdX, y.toDenseMatrix.t)
    println(omega)

//    val y2 = (omega * stdX) + DenseVector.fill(y.length){0.5}.toDenseMatrix
  }
  // パラメータの探索を実装します
  def ω(X: DenseMatrix[Double], y: DenseMatrix[Double]): DenseMatrix[Double] = {
    val a = X.t * X
//    println(a)
    val b = inv(a)
    val c = b * X.t
    c * y
  }
}
