package com.inkenkun.x1.ml.exercise

import java.io.File
import com.github.tototoshi.csv._
import vegas._
import vegas.render.WindowRenderer._

import scala.util.Random

object A_GettingStarted {
  def main(args: Array[String]): Unit = {
    val reader = CSVReader.open(new File("IRIS.csv"))
    val data: Vector[Vector[Double]] = reader.all().tail.map(v => v.map(_.toDouble).toVector).toVector
    println(data.head)

    val labels = kMeans(3, data, 10)
    println(labels)

    val reader2 = CSVReader.open(new File("IRIS.csv"))
    val data2 = reader2.allWithHeaders().zipWithIndex.map { case (m, n) =>
      m.mapValues(_.toDouble) + ("cluster" -> labels(n))
    }
    println(data2)
    val plot = Vegas("Iris").
      withData(data2).
      encodeX("SepalLengthCm", Quant).
      encodeY("SepalWidthCm", Quant).
      encodeColor(field="cluster", dataType=Nominal).
      mark(Point)
    plot.show

    val reader3 = CSVReader.open(new File("IRIS_label.csv"))
    val view = reader3.allWithHeaders()
    val plot2 = Vegas("Iris").
      withData(view).
      encodeX("SepalLengthCm", Quant).
      encodeY("SepalWidthCm", Quant).
      encodeColor(field="Species", dataType=Nominal).
    mark(Point)
    plot2.show

//    val commonOptions = ScatterOptions()
//      .mode(ScatterMode.Marker)
//      .marker(MarkerOptions().size(12).lineWidth(1))
//    val commonAxisOptions = AxisOptions()
//      .tickLength(5)
//      .gridWidth(2)
//    val setosa     = view.filter(m => m.get("Species").contains("Iris-setosa"))
//    val versicolor = view.filter(m => m.get("Species").contains("Iris-versicolor"))
//    val virginica  = view.filter(m => m.get("Species").contains("Iris-virginica"))
//    val p = Plot()
//      .withScatter(setosa.map(_("SepalLengthCm").toDouble), setosa.map(_("SepalWidthCm").toDouble), commonOptions.name("setosa").text("setosa"))
//      .withScatter(versicolor.map(_("SepalLengthCm").toDouble), versicolor.map(_("SepalWidthCm").toDouble), commonOptions.name("versicolor").text("versicolor"))
//      .withScatter(virginica.map(_("SepalLengthCm").toDouble), virginica.map(_("SepalWidthCm").toDouble), commonOptions.name("virginica").text("virginica"))
//      .xAxisOptions(commonAxisOptions.title("SepalLengthCm"))
//      .yAxisOptions(commonAxisOptions.title("SepalLengthCm"))
//
//    val figure = Figure().plot(p)
//    draw(p, "iris", writer.FileOptions(overwrite=true))
  }

  type Centroids = Vector[Vector[Double]]
  type Data      = Vector[Vector[Double]]
  type Labels    = Vector[Int]

  /**
   * k平均法
   * https://ja.wikipedia.org/wiki/K%E5%B9%B3%E5%9D%87%E6%B3%95
   * 1. 分割対象となるクラスタ数kを決める
   * 2. データが含まれる空間にランダムにk個の点(セントロイド)を置き、それぞれのクラスタの中心とする
   * 3. 各データがセントロイドのうちどれに最も近いかを計算して、そのデータが所属するクラスタとする
   * 4. セントロイドの位置をそのクラスタに含まれるデータの重心になるように移動する
   * 5. 各セントロイドの重心が変わらなくなるまで3, 4を繰り返す
   *
   * @param clusters
   * @param data
   * @param iteration
   */
  def kMeans(clusters: Int, data: Vector[Vector[Double]], iteration: Int): Labels = {
    val rand = new Random
    val features = data.head.size

//    lazy val centroids = randomMatrix(features, rand).take(clusters).toVector
//    lazy val centroids   : Vector[Vector[Double]] = Vector(data(0), data(80), data(110))
    lazy val centroids = Vector(
      Vector(5.0, 3.5, 1.0, 0.1),
      Vector(5.5, 2.0, 3.8, 1.1),
      Vector(8.2, 4.2, 7.1, 3.0)
    )
//    lazy val newCentroids: Vector[Vector[Double]] = Vector.fill(clusters)(Vector.fill(features)(0d))

    val (clustered, _) = run(data, centroids, iteration)

    data.map(nearest(_, clustered))
  }

  def run(data: Data, centroids: Centroids, num: Int): (Centroids, Int) = {
    num match {
      case 0 => (centroids, num)
      case _ =>
        val labels: Vector[Int] = data.map(nearest(_, centroids))
        println(labels)

        val newCentroids = for {
          i <- centroids.indices
        } yield {
          centroid(data.zipWithIndex.filter(p => labels(p._2) == i).map(_._1))
        }
        println(newCentroids)
        println("----------------------------------------------------------------------")
        run(data, newCentroids.toVector, num - 1)
    }
  }


  def randomMatrix(c: Int, r: Random): Stream[Vector[Double]] =
    randomVector(r.nextDouble(), r).take(c).toVector #:: randomMatrix(c, r)

  def randomVector(v: Double, r: Random): Stream[Double] = v #:: randomVector(r.nextDouble(), r)

  /**
   * ユークリッド距離
   * https://ja.wikipedia.org/wiki/%E3%83%A6%E3%83%BC%E3%82%AF%E3%83%AA%E3%83%83%E3%83%89%E8%B7%9D%E9%9B%A2
   * @param vec
   * @param centroids
   * @return
   */
  def nearest(vec: Vector[Double], centroids: Vector[Vector[Double]]): Int = {
    val m = centroids.zipWithIndex.minBy {
      case (v, _) =>
        Math.sqrt(vec.zip(v).foldLeft(0d){ case (acc, (p, q)) => acc + Math.pow(q - p, 2) })
    }
    m._2
  }

  def centroid(data: Vector[Vector[Double]]): Vector[Double] = {
    data.foldLeft(Vector.fill(data.head.size)(0d)){ (acc, v) =>
      v.zip(acc).map{ case (d1, d2) => d1 + d2 }
    }.map { v =>
      v / data.size
    }
  }
}
