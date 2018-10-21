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

/**
  * Breeze
  * Breeze is a library for numerical processing. It aims to be generic, clean, and powerful without sacrificing (much) efficiency.
  * 
  * https://github.com/scalanlp/breeze
  * https://github.com/scalanlp/breeze/wiki/Quickstart
  * https://github.com/scalanlp/breeze/wiki/Linear-Algebra-Cheat-Sheet
  */
object C2_LinearRegression {

  def main(args: Array[String]): Unit = {

    // CSVを読み込みます
    val df = csvread(new File("BostonHousing.csv"), skipLines = 1)

    val y = df(::, 13)
    val X = df(::, 0 to 12)

    // 犯罪率をプロットしてみます
//    val f = Figure()
//    val p = f.subplot(0)
//    p += plot(X(::, 0), y, '.')
//    p.xlabel = "per capita crime rate by town"
//    p.ylabel = "Median value of owner-occupied homes in $1000's"
//    f.saveas("BostonHousing.png")

    // 正規化します
    val stdX = normalize(X(::, *))


    val omega = ω(stdX, y.toDenseMatrix.t)
    println(omega)

    println(stdX)

    val y2 = stdX(*, ::) * omega.toDenseVector
    val yy = sum(y2(*, ::)) + DenseVector.fill(stdX.rows){0.5}
    println(yy)

//    val dm = DenseMatrix(
//      (1.0,2.0,3.0),
//      (4.0,5.0,6.0))
//    val res = dm(*, ::) + DenseVector(3.0, 4.0, 0.0)
//    println(res)


    val f = Figure()
    val p = f.subplot(0)
    p += plot(X(::, 0), y, '.')
    p += plot(X(::, 0), yy, '.')
    p.xlabel = "per capita crime rate by town"
    p.ylabel = "Median value of owner-occupied homes in $1000's"
    //    f.saveas("BostonHousing.png")

    f.saveas("pred_BostonHousing.png")
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
