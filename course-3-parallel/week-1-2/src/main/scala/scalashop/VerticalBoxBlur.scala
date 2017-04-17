package scalashop

import org.scalameter._
import common._

import scala.collection.mutable.ListBuffer

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for (x <- from until end; y <- 0 until src.height) {
      dst.update(x, y, scalashop.boxBlurKernel(src, x, y, radius))
    }
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val points = List.range(0, src.width, src.width / numTasks)
    val tuples = tupleBlur(points, src.width, src.width / numTasks)
    for (x <- tuples) {
      task(blur(src, dst, x._1, x._2, radius)).join()
    }

  }

  def tupleBlur(list: List[Int], width: Int, gap: Int): List[(Int, Int)] = {
    val listFinal : ListBuffer[(Int, Int)] = new ListBuffer()

    for (x <- list.indices) {
      listFinal += ((list(x), clamp(list(x) + gap, 0, width)))
    }

    listFinal.toList
  }
}
