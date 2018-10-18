package com.inkenkun.x1.ml.exercise

import scala.util.Try
import org.scalaml.Predef.Context._
import org.scalaml.filtering
import org.scalaml.util.MapUtils.Counter
import org.scalaml.Predef._

object B_DataPipelines {

  trait ITransform[T, A] { self =>
    def |> : PartialFunction[T, Try[A]] //2
    def map[B](f: A => B): ITransform[T, B] = new ITransform[T,B] {
      override def |> : PartialFunction[T, Try[B]] = new PartialFunction[T, Try[B]] {
        override def isDefinedAt(t: T): Boolean = self.|>.isDefinedAt(t)
        override def apply(t: T): Try[B] = self.|>(t).map(f) //6
      }
    }
    def flatMap[B](
      f: A => ITransform[T, B]
    ): ITransform[T, B] = new ITransform[T, B] {
      override def |> : PartialFunction[T, Try[B]] = new PartialFunction[T, Try[B]] {
        override def isDefinedAt(t: T): Boolean = self.|>.isDefinedAt(t)
        override def apply(t: T): Try[B] = self.|>(t).flatMap(f(_).|>(t))
      }
    }

    def andThen[B](tr: ITransform[A, B]): ITransform[T, B] = new ITransform[T, B] {
      override def |> : PartialFunction[T, Try[B]] = new PartialFunction[T, Try[B]] {
        override def isDefinedAt(t: T): Boolean =
            self.|>.isDefinedAt(t) && tr.|>.isDefinedAt(self.|>(t).get)
        override def apply(t: T):Try[B] = tr.|>(self.|>(t).get)
      }
    }
  }

  trait Config
  case class ConfigInt(iParam: Int) extends Config
  case class ConfigDouble(fParam: Double) extends Config
  case class ConfigArrayDouble(fParams: Array[Double]) extends Config

  abstract class ETransform[T,A](
    config: Config
  ) extends ITransform[T,A] { self =>
    override def map[B](f: A => B): ETransform[T,B] = new ETransform[T, B](config) {
//      override def |> : PartialFunction[T, Try[B]] = super.|>
      override def |> : PartialFunction[T, Try[B]] = ???
    }
    def flatMap[B](f: A => ETransform[T,B]): ETransform[T,B] = new ETransform[T, B](config) {
//      override def |> : PartialFunction[T, Try[B]] = super.|>
      override def |> : PartialFunction[T, Try[B]] = ???
    }
    def andThen[B](tr: ETransform[A,B]): ETransform[T,B] = new ETransform[T, B](config) {
//      override def|> :PartialFunction[T, Try[B]] = super.|>
      override def|> :PartialFunction[T, Try[B]] = ???
    }
  }

  trait Sampling[T,A] { val sampler: ETransform[T, A] }
  trait Normalization[T,A] { val normalizer: ETransform[T, A] }
  trait Aggregation[T,A] { val aggregator: ETransform[T, A] }

  class Workflow[T,U,V,W] { self: Sampling[T,U]
    with Normalization[U,V] with Aggregation[V,W] =>
    def |> (t: T): Try[W] = for {
      u <- sampler |> t
      v <- normalizer |> u
      w <- aggregator |> v
    } yield w
  }

  type DblF = Double => Double
  type DblVec = Vector[Double]

  val samples = 100; val normRatio = 10; val splits = 4
  val workflow = new Workflow[DblF, DblVec, DblVec, Int]
    with Sampling[DblF, DblVec]
    with Normalization[DblVec, DblVec]
    with Aggregation[DblVec, Int] {
    val sampler = new ETransform[DblF, DblVec](config = ConfigInt(samples)) {
      override def |> : PartialFunction[DblF, Try[DblVec]] = {
        case f =>
          Try(Vector.tabulate(samples)(n => f(n/samples)))
      }
    }
    val normalizer = new ETransform[DblVec, DblVec](config = ConfigInt(normRatio)) {
      override def |> : PartialFunction[DblVec, Try[DblVec]] = ???
    }
    val aggregator = new ETransform[DblVec, Int](config = ConfigInt(splits)) {
      override def |> : PartialFunction[DblVec, Try[Int]] = {
        case x: DblVec if x.nonEmpty =>
          Try(x.indices.find(x(_) == 1.0).getOrElse(-1))
      }
    }
  }

//  trait ToDouble[T] { def apply(t: T): Double }

  trait PreprocessingModule[T] {
    trait Preprocessor[T] { //7
      def execute(x: Vector[T]): Try[DblVec]
    }
    val preprocessor: Preprocessor[T]  //8
    class ExpMovingAverage[T : ToDouble](p: Int)(implicit num: Numeric[T]) extends Preprocessor[T] {
      val expMovingAvg = filtering.movaverage.ExpMovingAverage.apply[T](p)
      val pfn = expMovingAvg |>
      override def execute(x: Vector[T]): Try[DblVec] = pfn(x)
    }
    class DFTFilter[T : ToDouble](
      fc: Double,
      g: (Double,Double) => Double
    ) extends Preprocessor[T] { //12
      val filter = filtering.dft.DFTFilter.apply[T](fc, 1e-5)(g)
      val pfn = filter |>
      override def execute(x: Vector[T]):Try[DblVec] = pfn(x)
    }

  }

  sealed trait VLabel
  case class TP() extends VLabel
  case class TN() extends VLabel
  case class FP() extends VLabel
  case class FN() extends VLabel

  trait Validator{ def score: Double }
  abstract class AValidator[T] protected (
    labeled: Vector[(Array[T], Int)]
  ) extends Validator {
    def this(expected: Vector[Int], xt: Vector[Array[T]]) = this(xt.zip(expected))
    override def score: Double
  }

  import BinaryValidation._
  class BinaryValidation[T <: AnyVal](
    expected: Vector[Int],
    xt: Vector[Array[T]]
  )(implicit predict: Array[T] => Int) extends AValidator[T](expected, xt) {

    val counters = expected.zip(xt.map( predict(_)))
      .aggregate(new Counter[VLabel])((cnt, ap) =>
        cnt + classify(ap._1, ap._2), _ ++ _ )

    override def score: Double = f1
    lazy val f1= 2.0*precision*recall/(precision + recall)
    lazy val precision = compute(FP())
    lazy val recall = compute(FN())

    def compute(n: VLabel): Double =
      1.0/(1.0 + counters(n)/counters(TP()))

    def classify(predicted: Int, expected: Int): VLabel = if(expected == predicted)
      if(expected == POSITIVE) TP() else TN()
      else if(expected == POSITIVE) FP() else FN()
  }
  object BinaryValidation {
    final val POSITIVE = 1
    /**
     * Default constructor for the ClassValidation
     *
     * @param expected Array of pair (actual value, labeled/expected value)
     * @param xt Input time series
     * @param predict Prediction function
     */
    def apply[T <: AnyVal](
      expected: Vector[Int],
      xt: Vector[Array[T]]
    )(implicit predict: Array[T] => Int): Try[BinaryValidation[T]] = Try {
      new BinaryValidation(expected, xt)(predict)
    }

    def auPRC[T <: AnyVal](
      binValidations: List[BinaryValidation[T]]
    ): Double = binValidations./:(0.5)(
      (s, valid) => s + valid.precision- valid.recall
    )
  }

  type LabeledData[T] = Vector[(Array[T], Int)]
  abstract class OneFoldValidation[T: ToDouble]( xt: Vector[Array[T]],
    expected: Vector[Int],
    ratio: Double) {
      val trainSet: LabeledData[T]
      val validSet: LabeledData[T]
  }

  trait Imputing[T, A] { val imputer: ITransform[T, A] }

  trait Normalization[T, A] { val normalizer: ITransform[T, A] }

  trait OneHotEncoding[T, A] { val encoder: ITransform[T, A] }

  class BasicPreprocessing[T, U, V, W] {
    self: Imputing[T, U] with Normalization[U, V] with OneHotEncoding[V, W] =>

    def |> (t: T): Try[W] = for {
      u <- imputer |> t
      v <- normalizer |> u
      w <- encoder |> v
    } yield w
  }
  sealed trait BloodType
  case object A extends BloodType
  case object B extends BloodType
  case object O extends BloodType
  case object AB extends BloodType

  case class Human(height: Option[Double], bloodType: BloodType)
  case class ImputedHuman(height: Double, bloodType: BloodType)

  object HumanExample extends App {

    val basicPreprocessing = new BasicPreprocessing[Seq[Human], Seq[ImputedHuman], Seq[ImputedHuman], Seq[Double]]
      with Imputing[Seq[Human], Seq[ImputedHuman]]
      with Normalization[Seq[ImputedHuman], Seq[ImputedHuman]]
      with OneHotEncoding[Seq[ImputedHuman], Seq[Double]] {

      val imputer = new ITransform[Seq[Human], Seq[ImputedHuman]] {
        override def |> : PartialFunction[Seq[Human], Try[Seq[ImputedHuman]]] = new PartialFunction[Seq[Human], Try[Seq[ImputedHuman]]] {
          override def isDefinedAt(t: Seq[Human]): Boolean = self.|>.isDefinedAt(t)
          override def apply(t: T): Try[B] = self.|>(t).map(f) //6
        }
      }

      val normalizer = ???

      val bloodVector = Vector(A, B, O, AB)
      val encoder = ???

      val sampleInput = Seq(
        Human(Some(170.0), A),
        Human(None, A),
        Human(Some(150.0), B),
        Human(Some(155.0), AB)
      )

      basicPreprocessing |> sampleInput match {
        case Success(res) => println(s"result = ${res.toString}")
        case Failure(e) => println(s"error = $e")
      }

    }
}
