package com.inkenkun.x1.ml

import scala.util.Try

package object exercise {
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
}
