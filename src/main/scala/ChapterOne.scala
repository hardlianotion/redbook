
import math.{pow, sqrt}
import scala.annotation.tailrec
import scala.util.Try
import Try.*

object ChapterOne:

  // Exercise 2.1
  def fibSlow (n: Long): Long =
    if n <= 1 then n
    else fibSlow (n - 1) + fibSlow (n - 2)

  def fibVerify (n: Long): Long =
    ((pow ((1 + sqrt (5)) / 2.0, n) - pow ((1 - sqrt (5)) / 2.0, n)) / sqrt (5.0)).toLong

  def fibForReal (n: Long): Long =
    @tailrec
    def impl (i: Long, fib_1: Long, fib_2: Long): Long =
      if i == 0 then
        0
      else if i == 1 then
        fib_1
      else
        impl (i - 1, fib_1 + fib_2, fib_1)

    impl (n, 1, 0)

  def demoFib (name: String, attempt: Long => Long, range: Range): Unit =
    println (name)
    for
      i <- range
    do
      val fibAttempt = Try (attempt (i))

      fibAttempt.fold(
        e => println (s"failure: ${e.getMessage}"),
        fib => println (s"i = $i, $name = $fib, verification = ${fibVerify (i)}, diff = ${fib - fibVerify (i)}")
      )

  def runFib =
    Array (
      ("boo fib", ChapterOne.fibSlow), ("yay fib", ChapterOne.fibForReal)).map:
      case (name, fn) => ChapterOne.demoFib (name, fn, 5 until 10)

  // Exercise 2.2
  def isSorted [A] (as: Array [A], gt: (A, A) => Boolean): Boolean =
    @tailrec
    def impl (idx: Int, lastIncrease: Boolean): Boolean =
      if idx >= as.length then
        true
      else if gt (as (idx), as (idx - 1)) == lastIncrease then
        impl (idx + 1, lastIncrease)
      else
        false

    if as.length <= 2 then
      true
    else
      impl (2, gt (as (1), as (0)))

  // Exercise 2.3
  def curry [A, B, C] (f: (A, B) => C): A => B => C =
    a => b => f (a, b)

  // Exercise 2.4
  def uncurry [A, B, C] (f: A => B => C): (A, B) => C =
    (a, b) => f (a) (b)

  // Exercise 2.5
  def compose [A, B, C] (f: B => C, g: A => B): A => C =
    a => f (g (a))
