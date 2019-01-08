package com.fr.upem.partiel

import java.time.Instant
import java.time.temporal.ChronoUnit.YEARS


// Part 1 (10pts)
object Part1 {

  // 1.1 Apply 'mul2' using pattern matching to the given integer (.5pts)
  def mul2(i: Int): Int = i * 2
  def applyMul2WithPatternMatching(i: Option[Int]): Option[Int] = i match {
    case Some(i) => Some(2 * i)
    case None => None
  }

  // 1.2 Apply 'mul2' WITHOUT using pattern matching to the given integer (.5pts)
  def applyMul2WithoutPatternMatching(i: Option[Int]): Option[Int] = if(i==Some) i*2 else None

  // 1.3 Refactor the following code using pattern matching (1pts)
  sealed trait Animal
  case object Cat extends Animal
  case object Bird extends Animal
  case class Dog(age: Int) extends Animal

  /*def formatAnimal(animal: Animal): String =
    if (animal == Cat)
      "It's a cat"
    else if (animal == Bird)
      "It's a bird"
    else if (animal.isInstanceOf[Dog])
      s"It's a ${animal.asInstanceOf[Dog].age} year old dog"
    else
      throw new RuntimeException("This should not happen but I'm a Java developer !")*/

  def formatAnimal(animal: Animal): String = animal match{
    case Cat => "It's a cat"
    case Bird => "It's a bird"
    case _ if (animal.isInstanceOf[Dog]) => s"It's a ${animal.asInstanceOf[Dog].age} year old dog"
    case _ => throw new RuntimeException("This should not happen but I'm a Java developer !")
  }


  // 1.4 Find the index of the given element if any, use recursivity (1pts)
  def indexOf[A](l: List[A], a: A): Option[Int] = l match {
    case x::xs => Some(l.foldRight(0)((x,acc) => if(x == a)  acc else acc + 1))
    case Nil => None
  }

  // 1.5 Throw away all errors (.5pts)
  case class Error(message: String)
  def keepValid[A](l: List[Either[Error, A]]): List[A] = l match {
    case x::xs if x == Left(Error("err")) =>  keepValid(xs)
    case x::xs => x.right.get::keepValid(xs)
    case Nil => Nil
  }

  // 1.6 Aggregate values (.5pts)
  def aggregate[A](l: List[A], combine: (A, A) => A, empty: A): A = l match {
    case x::xs => xs.foldLeft(x)(combine)
    case Nil => empty
  }

  // 1.7 Aggregate valid values (.5pts)
  def aggregateValid[A](l: List[Either[Error, A]], combine: (A, A) => A, empty: A): A = l match {
    case x::xs => aggregate(keepValid(xs), combine, empty)
    case Nil => empty
  }

  // 1.8 Create the Monoid typeclass and rewrite the above "aggregateValid" (.5pts)

  trait Monoid[A] {
    def empty: A
    def combine(x: A, y: A): A
  }

  implicit val aggregateValidMInt : Monoid[Int] = new Monoid[Int] {
    override def empty: Int = empty

    override def combine(a1: Int, a2: Int): Int = a1+a2
  }

  def aggregateValidM[A](l: List[Either[Error, A]])(implicit ev : Monoid[A]): A = l match {
    case Nil => ev.empty
    case x::xs => aggregate(keepValid(xs),ev.combine,ev.empty)
  }

  // 1.9 Implement the Monoid typeclass for Strings and give an example usage with aggregateValidM (.5pts)

  implicit val aggregateValidMSting : Monoid[String] = new Monoid[String] {
    override def empty: String = empty

    override def combine(a1: String, a2: String): String = a1+a2
  }

  def aggregateValidMSting[String](l: List[Either[Error, String]])(implicit ev : Monoid[String]): String = l match {
    case Nil => ev.empty
    case x::xs => aggregate(keepValid(xs),ev.combine,ev.empty)
  }

  // 1.10 Refactor the following object oriented hierarchy with an ADT (1.5pts)
  sealed trait FinancialAsset {
    def computeEarnings: Double
  }

  sealed trait FlatRateAsset extends FinancialAsset {
    protected val rate: Double
    protected val amount: Double

    override def computeEarnings: Double = amount + (amount * rate)
  }

  object LivretA {
    val Rate: Double = 0.75
  }

  class LivretA(override val amount: Double) extends FlatRateAsset {
    override protected val rate: Double = LivretA.Rate
  }

  object Pel {
    val Rate: Double = 1.5
    val GovernmentGrant: Int = 1525
  }

  class Pel(override val amount: Double, creation: Instant) extends FlatRateAsset {
    override protected val rate: Double = Pel.Rate
    override def computeEarnings: Double =
      if (Instant.now().minus(4, YEARS).isAfter(creation))
        super.computeEarnings + Pel.GovernmentGrant
      else
        super.computeEarnings
  }

  object CarSale {
    val StateHorsePowerTaxation: Int = 500
  }
  class CarSale(amount: Int, horsePower: Int) extends FinancialAsset {
    override def computeEarnings: Double = amount - (CarSale.StateHorsePowerTaxation * horsePower)
  }

  // 1.11 Extract the "computeEarnings" logic of the above hierarchy
  // into an "Earnings" typeclass and create the adequate instances (1.5pts)

  // 1.12 Rewrite the following function with your typeclass (.5pts)
  def computeTotalEarnings(assets: List[FinancialAsset]): Double =
    assets.map(_.computeEarnings).sum

  // 1.13 Enrich the "String" type with an "atoi" extension method that parses the
  // given String to an Int IF possible (1pts)

}
