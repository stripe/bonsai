package com.stripe.bonsai

import java.io._

import scala.reflect.ClassTag

import scala.collection.immutable.BitSet

import org.scalatest.{ WordSpec, Matchers }
import org.scalatest.prop.Checkers

import org.scalacheck.{ Arbitrary, Gen, Prop }
import org.scalacheck.Arbitrary._

import com.stripe.bonsai.layout._

class LayoutSpec extends WordSpec with Matchers with Checkers {
  private[this] def roundTrip[A: Layout](vec: Vec[A]): Vec[A] = {
    val baos = new ByteArrayOutputStream()
    Layout[A].write(vec, new DataOutputStream(baos))
    Layout[A].read(new DataInputStream(new ByteArrayInputStream(baos.toByteArray)))
  }

  "DenseArrayLayout" should {
    "not use byte dict encoding if there are 256 unique values" in {
      val xs: Seq[Double] = Seq.tabulate(256)(_.toDouble)
      val vec: Vec[Double] = Vec[Double](xs: _*)
      roundTrip(vec) == vec
    }
  }

  def layoutCheck[A: Arbitrary: Layout: ClassTag](name: String): Unit = {
    s"Vec[$name].equals" should {
      "use structural equality" in {
        Prop.forAll { (xs: Vector[A]) =>
          val xs1 = (Layout[A].newBuilder ++= xs).result()
          val xs2 = (Layout[A].newBuilder ++= xs).result()
          ("elements match" |: Prop(xs1 == xs2)) &&
          ("hashCodes match" |: Prop(xs1.hashCode == xs2.hashCode))
        }
      }
    }

    s"Vec[$name].hashCode" should {
      "should agree with equals" in {
        Prop.forAll { (xs1: Vector[A], xs2: Vector[A]) =>
          val vec1 = (Layout[A].newBuilder ++= xs1).result()
          val vec2 = (Layout[A].newBuilder ++= xs2).result()
          "hashCode mismatch implies inequality" |:
            Prop(vec1.hashCode == vec2.hashCode || vec1 != vec2)
        }
      }
    }

    s"Layout[$name].newBuilder" should {
      "start empty" in {
        val vec = Layout[A].newBuilder.result()
        "size is 0" |: Prop(vec.size == 0)
      }

      "reflect elements added" in {
        Prop.forAll { (xs: Vector[A]) =>
          val xs1 = (Layout[A].newBuilder ++= xs).result
          val xs2 = xs.foldLeft(Layout[A].newBuilder)(_ += _).result
          ("sizes match" |: Prop(xs.size == xs1.size && xs.size == xs2.size)) &&
          ("elements match" |: Prop(
            (0 until xs.size).forall { i => xs(i) == xs1(i) } &&
            (0 until xs.size).forall { i => xs(i) == xs2(i) }
          ))
        }
      }
    }

    s"Layout[$name].write" should {
      "round-trip through read" in {
        Prop.forAll { (xs: Vector[A]) =>
          val vec1 = Layout[A].newBuilder.++=(xs).result
          val vec2 = roundTrip(vec1)
          vec1 == vec2
        }
      }
    }

    s"Vec[$name].deflate" should {
      "be equivalent to inflated Vec" in {
        Prop.forAll { (xs: Vector[A]) =>
          val vec: Vec[A] = new Vec[A] {
            def size: Int = xs.size
            def apply(i: Int): A = xs(i)
          }
          vec.deflate == vec
        }
      }
    }
  }

  layoutCheck[Unit]("Unit")
  layoutCheck[Boolean]("Boolean")
  layoutCheck[Byte]("Byte")
  layoutCheck[Short]("Short")
  layoutCheck[Int]("Int")
  layoutCheck[Long]("Long")
  layoutCheck[Float]("Float")
  layoutCheck[Double]("Double")
  layoutCheck[Char]("Char")
  layoutCheck[String]("String")
  layoutCheck[Option[Long]]("Option[Long]")
  layoutCheck[Either[String, Long]]("Either[String, Long]")
  layoutCheck[(Short, String)]("(Short, String)")
  layoutCheck[(Short, Int, Double)]("(Short, Int, Double)")

  case class Point(x: Double, y: Double)
  object Point extends {
    implicit val PointLayout: Layout[Point] =
      new ProductLayout[Double, Double, Point](
        Layout[Double], Layout[Double],
        { case Point(x, y) => (x, y) },
        Point(_, _)
      )

    implicit val arbPoint: Arbitrary[Point] =
      Arbitrary(for {
        x <- arbitrary[Double]
        y <- arbitrary[Double]
      } yield Point(x, y))
  }

  layoutCheck[Point]("Point")
}
