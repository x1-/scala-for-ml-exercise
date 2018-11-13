package com.inkenkun.x1.ml.exercise

import java.io.File

import breeze.linalg._
import breeze.math._
import breeze.plot._

import scala.util.{Failure, Success, Try}

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
    val df: DenseMatrix[Double] = csvread(new File("BostonHousing.csv"), skipLines = 1)

    val y: DenseVector[Double] = df(::, 13)
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


    val f = Figure()
    val p = f.subplot(0)
    p += plot(stdX(::, 0), y, '.')
    p += plot(stdX(::, 0), yy, '.')
    p.xlabel = "per capita crime rate by town"
    p.ylabel = "Median value of owner-occupied homes in $1000's"
    f.saveas("BostonHousing.png")
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


object C3_LinearRegression {

  trait CsvReading[T, A] { val reader: ITransform[T, A] }

  trait SplitingXY[T, A] { val splitter: ITransform[T, A] }

  trait Normalization[T, A] { val normalizer: ITransform[T, A] }

  trait Calculate[T, A] { val calculator: ITransform[T, A] }

  class LeastSquareMethod[S, T, U, V, W] {
    self: CsvReading[S, T] with SplitingXY[T, U] with Normalization[U, V] with Calculate[V, W] =>

    def |> (s: S): Try[W] = for {
      t <- reader |> s
      u <- splitter |> t
      v <- normalizer |> u
      w <- calculator |> v
    } yield w
  }

  object LinearRegression extends App {

    import cats.Monoid

    type Matrix = DenseMatrix[Double]
    type X      = DenseMatrix[Double]
    type Y      = DenseVector[Double]
    type XY     = (X, Y)
    type Omega  = DenseMatrix[Double]

    val leastSquareMethod = new LeastSquareMethod[String, Matrix, XY, XY, (X, Y, Omega)]
      with CsvReading[String, Matrix]
      with SplitingXY[Matrix, XY]
      with Normalization[XY, XY]
      with Calculate[XY, (X, Y, Omega)] {

      val reader: ITransform[String, Matrix] = ???
      val splitter: ITransform[Matrix, XY] = ???
      val normalizer: ITransform[XY, XY] = ???
      val calculator: ITransform[XY, (X, Y, Omega)] = ???
    }

    val leastSquareMethod2 = new LeastSquareMethod[String, Matrix, XY, XY, (X, Y, Omega)]
      with CsvReading[String, Matrix]
      with SplitingXY[Matrix, XY]
      with Normalization[XY, XY]
      with Calculate[XY, (X, Y, Omega)] {

      val reader: ITransform[String, Matrix] = new ITransform[String, Matrix] {
        override def |> : PartialFunction[String, Try[Matrix]] = {
          case path =>
            Try(csvread(new File(path), skipLines = 1))
        }
      }
      val splitter: ITransform[Matrix, XY] = new ITransform[Matrix, XY] {
        override def |> : PartialFunction[Matrix, Try[XY]] = {
          case matrix =>
            Try(matrix(::, 0 to 12), matrix(::, 13))
        }
      }
      val normalizer: ITransform[XY, XY] = new ITransform[XY, XY] {
        override def |> : PartialFunction[XY, Try[XY]] = {
          case (x, y) =>
            Try(normalize(x(::, *)), y)
        }
      }
      // パラメータの探索を実装します
      def ω(X: DenseMatrix[Double], y: DenseMatrix[Double]): DenseMatrix[Double] = {
        inv(X.t * X) * X.t * y
      }
      val calculator: ITransform[XY, (X, Y, Omega)] = new ITransform[XY, (X, Y, Omega)] {
        override def |> : PartialFunction[XY, Try[(X, Y, Omega)]] = {
          case (x, y) =>
            val omega = ω(x, y.toDenseMatrix.t)
            Try(x, y, omega)
        }
      }
    }

    leastSquareMethod2 |> "BostonHousing.csv" match {
      case Success((x, y, omega)) => {
        // プロットします
        val xxy = x(*, ::) * omega.toDenseVector
        val predY = sum(xxy(*, ::)) + DenseVector.fill(x.rows){0.5}

        val f = Figure()
        val p = f.subplot(0)
        p += plot(x(::, 0), y, '.')
        p += plot(x(::, 0), predY, '.')
        p.xlabel = "per capita crime rate by town"
        p.ylabel = "Median value of owner-occupied homes in $1000's"
        f.saveas("BostonHousing.png")

      }
      case Failure(e) => println(s"error = $e")
    }
  }
}
