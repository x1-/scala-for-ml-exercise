package com.inkenkun.x1.ml.exercise.chapter_nine.models

import scala.io.Source

case class AgeSalaryList(
  contents: Vector[AgeSalary]
) {

  def toAgeVector: Vector[Double] = contents.map(_.age)
  def toSalaryVector: Vector[Double] = contents.map(_.salary)
}

object AgeSalaryList {

  def from(csvPath: String): AgeSalaryList ={
    val contents = Source.fromFile(csvPath).getLines.drop(1).map { line =>
      val set = line.split(",").map(_.toDouble)
      AgeSalary(set(0), set(1))
    }.toVector
    AgeSalaryList(contents)
  }
}
