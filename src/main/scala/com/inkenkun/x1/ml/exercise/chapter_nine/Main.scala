package com.inkenkun.x1.ml.exercise.chapter_nine

import java.io.File

import breeze.linalg._
import breeze.plot._
import models.{AgeSalaryList, HousingParametersVectors}



object Main {


  def main(args: Array[String]): Unit = {
    singleRegression
    multiRegression
  }

  private def multiRegression = {
    val housingParametersVectors = HousingParametersVectors.from("data/housing.csv")
    println(housingParametersVectors.toTargetVector)
    val regression = new MultiLinearRegression(
      housingParametersVectors.toExpVector,
      housingParametersVectors.toTargetVector)
//    regression.train.foreach(_.weights.foreach(println(_)))

//    val vv = DenseVector(1.0, 2.0, 0.5)
//    val dm = DenseMatrix((1.0,2.0,3.0),
//      (4.0,5.0,6.0))
//    dm(*, ::) *= vv
//    println(dm)
//
//    val ndm = DenseMatrix.horzcat(dm, DenseVector(9.0, 8.0).toDenseMatrix.t)
//    println(ndm)

    val df: DenseMatrix[Double] = csvread(new File("data/housing.csv"), skipLines = 1)
    val X = df(::, 0 to 12)
    val targetY = df(::, 13)
    val stdX: DenseMatrix[Double] = normalize(X(::, *))

    val multi = LinearLikeliRegression(iterationNum = 1000)
    val params3 = multi.train(
      stdX(*, ::).toIndexedSeq.map(_.inner.toArray).toArray,
      targetY.toArray
    )
    println("weights:")
    params3.foreach(println)


    val ps: DenseVector[Double] = new DenseVector(params3)
    val ones = DenseVector.ones[Double](stdX.rows)
    val xss = DenseMatrix.horzcat(ones.toDenseMatrix.t, stdX(::, 0 to 12))
    xss(*, ::) *= ps

    val y = sum(xss, Axis._1)
    println(y)

    // プロットしてみます
    val f2 = Figure()
    val p = f2.subplot(0)
    p += plot(stdX(::, 6), targetY, '.')
    p += plot(stdX(::, 6), y, '.')
    p.xlabel = "x1"
    p.ylabel = "y"
    f2.saveas("housing.png")




  }


  private def singleRegression = {
    val ageSalaryList = AgeSalaryList.from("data/Salary_Data.csv")

//    val df: DenseMatrix[Double] = csvread(new File("data/Salary_Data.csv"), skipLines = 1)
//    // 年次と給料の関係をプロットしてみます
//    val f = Figure()
//    val p = f.subplot(0)
//    p += plot(df(::, 0), df(::, 1), '.')
//    p.xlabel = "YearsExperience"
//    p.ylabel = "Salary"
//    f.saveas("Salary_Data.png")

    val linReg = SingleLinearRegression(
      ageSalaryList.toAgeVector,
      ageSalaryList.toSalaryVector
    )

    linReg.model match {
      case Some(m) =>
        println(s"slope: ${m._1}, intercept: ${m._2}")
      case None =>
        println("Error")
    }


    val reg = SingleLikeliRegression()
    val params = reg.train(
      ageSalaryList.toAgeVector.toArray,
      ageSalaryList.toSalaryVector.toArray
    )
    println(s"slope: ${params(1)}, intercept: ${params(0)}")

    val reg2 = LinearLikeliRegression()
    val params2 = reg2.train(
      ageSalaryList.toAgeVector.map(Array(_)).toArray,
      ageSalaryList.toSalaryVector.toArray
    )
    println(s"slope: ${params2(1)}, intercept: ${params2(0)}")

    val df: DenseMatrix[Double] = csvread(new File("data/Salary_Data.csv"), skipLines = 1)
    val y = df(::, 0).map(x => params(0) + params(1) * x)
    val y2 = df(::, 0).map(x => params2(0) + params2(1) * x)

    // 年次と給料の関係をプロットしてみます
    val f = Figure()
    val p = f.subplot(0)
    p += plot(df(::, 0), df(::, 1), '.')
    p += plot(df(::, 0), y)
    p += plot(df(::, 0), y2)
    p.xlabel = "YearsExperience"
    p.ylabel = "Salary"
    f.saveas("Salary_Data.png")

  }
}
