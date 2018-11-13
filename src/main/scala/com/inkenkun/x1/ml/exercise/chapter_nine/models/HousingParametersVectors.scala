package com.inkenkun.x1.ml.exercise.chapter_nine.models

import scala.io.Source

case class HousingParametersVectors(
  contents: Vector[HousingParameters]
) {

  def toExpVector: Vector[Array[Double]] = {
    contents.map(_.X)
  }

  def toTargetVector: Vector[Double] = {
    contents.map(_.y)
  }

}

object HousingParametersVectors {

  def from(csvPath: String): HousingParametersVectors = {
    val contents = Source.fromFile(csvPath).getLines.drop(1).map { line =>
      val set = line.split(",").map(_.toDouble)
      val exp = set.dropRight(1)
      val target = set.last
      HousingParameters(exp, target)
    }.toVector
    HousingParametersVectors(contents)
  }
}
