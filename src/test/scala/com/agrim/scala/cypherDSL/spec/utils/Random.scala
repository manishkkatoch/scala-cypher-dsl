package com.agrim.scala.cypherDSL.spec.utils

import shapeless.{::, Generic, HList, HNil, Lazy}

import scala.util.{Random => uRandom}

trait Random[A] {
  def get: A
}

object Random {

  def randomize[A](implicit random: Random[A]): A = random.get

  def createRandom[A](func: () => A): Random[A] = new Random[A] {
    override def get: A = func()
  }

  implicit val intRandom: Random[Int]       = createRandom(() => uRandom.nextInt(10))
  implicit val stringRandom: Random[String] = createRandom(() => uRandom.alphanumeric.take(10).mkString)
  implicit val hNilRandom: Random[HNil]     = createRandom(() => HNil)

  implicit def genericRandom[A, R](implicit
                                   gen: Generic.Aux[A, R],
                                   random: Lazy[Random[R]]): Random[A] =
    createRandom(() => gen.from(random.value.get))

  implicit def hListRandom[H, T <: HList](
      implicit
      hRandom: Lazy[Random[H]],
      tRandom: Random[T]
  ): Random[H :: T] = createRandom(() => hRandom.value.get :: tRandom.get)

}
