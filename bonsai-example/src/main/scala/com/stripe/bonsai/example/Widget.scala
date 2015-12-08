package com.stripe.bonsai
package example

import scala.util.Random

import org.github.jamm.MemoryMeter

sealed trait Widget
case class Sprocket(radius: Int, weight: Option[Double]) extends Widget
case class Doodad(length: Int, width: Int, weight: Option[Double]) extends Widget

object Widget {
  implicit val WidgetLayout: Layout[Widget] = {
    Layout[Either[(Int, Option[Double]), ((Int, Int), Option[Double])]].transform(
      {
        case Left((r, wt)) => Sprocket(r, wt)
        case Right(((l, w), wt)) => Doodad(l, w, wt)
      },
      {
        case Sprocket(r, wt) => Left((r, wt))
        case Doodad(l, w, wt) => Right(((l, w), wt))
      }
    )
  }
}

object WidgetExample extends App {

  def genDouble: Option[Double] =
    if (Random.nextDouble < 0.3) {
      None
    } else {
      Some(Random.nextDouble)
    }

  def genWidget: Option[Widget] =
    if (Random.nextDouble < 0.25) {
      None
    } else if (Random.nextBoolean) {
      Some(Sprocket(Random.nextInt, genDouble))
    } else {
      Some(Doodad(Random.nextInt, Random.nextInt, genDouble))
    }

  val uncompressed = Vector.fill(10000)(genWidget)
  val compressed = layout.Vec(uncompressed: _*)

  val meter = new MemoryMeter()
  val uncompressedSize = meter.measureDeep(uncompressed)
  val compressedSize = meter.measureDeep(compressed)

  println(s"uncompressed:   ${uncompressedSize} bytes")
  println(s"compressed:     ${compressedSize} bytes")
  println(f"${uncompressedSize / compressedSize.toDouble}%.1fx reduction")
}
