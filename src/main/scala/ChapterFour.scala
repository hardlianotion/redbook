object ChapterFour:

  enum Option [+A]:
    case None
    case Some (thing: A)

    def map [B] (f: A => B): Option [B] =
      this match
        case None => None
        case Some (a) => Some (f (a))

    def flatMap [B] (f: A => Option [B]): Option [B] = {
      this.map (f).getOrElse (None)
    }

    def getOrElse [B >: A] (default : => B): B =
      this match
        case None => default
        case Some (a) => a

    def orElse [B >: A] (default: => Option [B]): Option [B] =
      this.map (Some (_)).getOrElse (default)

    def filter (pred: A => Boolean): Option [A] =
      flatMap: a =>
        if pred (a) then
          Some (a)
        else
          None

  object Option:
    def lift [A, B] (f: A => B): Option [A] => Option [B] =
      _.map (f)

    def map2Alt [A, B, C] (a: Option [A], b: Option [B]) (f: (A, B) => C): Option [C] =
      (a, b) match
        case (None, _) => None
        case (_, None) => None
        case (Some (a), Some (b)) => Some (f (a, b))

    def map2 [A, B, C] (a: Option [A], b: Option [B]) (f: (A, B) => C): Option [C] =
      a.flatMap (x => b.map (y => f (x, y)))

    def traverse [A, B] (list: List [A]) (f: A => Option [B]): Option [List[B]] =
      list.foldRight (Some (List.empty [B])): (lhs, acc) =>
        acc.flatMap {
          xs => f (lhs).map (_ :: xs)
        }

    def sequence [A] (list: List [Option [A]]): Option [List[A]] =
      list.foldRight (Some (List.empty [A])): (lhs, acc) =>
        acc.flatMap (
          xs => lhs.map (x => x :: xs)
        )

    def sequenceAlt [A] (list: List [Option[A]]): Option [List[A]] =
      traverse (list) (_.orElse (None))


  enum Either [+E, +A]:
    case Left (error: E)
    case Right (value: A)

    def map [B] (f: A => B): Either [E, B] =
      this match
        case Left (e) => Left (e)
        case Right (a) => Right (f (a))

    def flatMap [EE >: E, B] (f: A => Either [EE, B]): Either [EE, B] =
      this match
        case Left (e) => Left (e)
        case Right (a) => f (a)

    def orElse [EE >: E, B >: A] (default: => Either [EE, B]): Either [EE, B] =
      this match
        case Left (e) => default
        case Right (a) => Right (a)

    def map2 [EE >: E, B, C] (that: Either [EE, B]) (f: (A, B) => C): Either [EE, C] =
      for
        a <- this
        b <- that
      yield
        f (a, b)

  import scala.util.control.NonFatal

  object Either:
    def catchNonFatal [A] (a: => A): Either [Throwable, A] =
      try Right (a)
      catch case NonFatal (e) => Left (e)


    def sequence [E, A] (list: List [Either [E, A]]): Either [E, List [A]] =
      list.foldRight (Right (List.empty [A])): (lhs, agg) =>
        for
          xs <- agg
          l <- lhs
        yield
          l :: xs

    def traverse [E, A, B] (list: List [A]) (f: A => Either [E, B]): Either [E, List [B]] =
      list.foldRight (Right (List.empty [B])): (lhs, agg) =>
        for
          xs <- agg
          l <- f (lhs)
        yield
          l :: xs

    def sequenceAlt [E, A] (list: List [Either [E, A]]): Either [E, List[A]] =
      traverse (list) (identity)

import ChapterFour.Option.*
import ChapterFour.Option
import ChapterFour.Either.*
import ChapterFour.Either

  @main
  def run =
    val someOne: Option [Int] = Some (1)
    val noOne: Option [Int] = None

    println (s"running checks ...")

    assert (someOne.map (_ + 1) == Some (2))
    assert (noOne.map (x => x + 1) == None)
    assert (someOne.flatMap (x => Some (x + 1)) == Some (2))
    assert (noOne.flatMap (x => Some (x + 1)) == None)
    assert (someOne.getOrElse (0) == 1)
    assert (noOne.getOrElse (0) == 0)
    assert (someOne.orElse (Some (5)) == Some (1))
    assert (noOne.orElse (Some (0)) == Some (0))
    assert (someOne.filter (_ == 1) == Some (1))
    assert (someOne.filter (_ == 2) == None)
    assert (noOne.filter (_ == 1) == None)
    assert (noOne.filter (_ == 2) == None)

    def defined [A] (mibbe: Option [A]): Boolean =
      mibbe match
        case None => false
        case _ => true

    def undefined [A] (mibbe: Option [A]): Boolean =
      !defined (mibbe)

    val numbers = Seq.from (1 to 10).map (_.toDouble)
    val noNumbers = Seq.empty [Double]
    val number = Seq (1.0)

    def mean (numbers: Seq [Double]): Option [Double] =
      if numbers.isEmpty then
        None
      else
        Some (numbers.sum / numbers.length)

    def variance (numbers: Seq [Double]): Option [Double] =
      if numbers.length < 2 then
        None
      else
        val sMean = mean (numbers)
        sMean.map (
          sm => numbers.map (n => (n - sm) * (n - sm)).sum / (numbers.length - 1)
        )

    assert (defined (mean (numbers)))
    assert (defined (mean (number)))
    assert (undefined (mean (noNumbers)))
    assert (defined (variance (numbers)))
    assert (undefined (variance (number)))
    assert (undefined (variance (noNumbers)))

    assert (map2 (Some (1), Some (2)) (_ + _) == Some (3))
    assert (map2Alt (Some (1), Some (2)) (_ + _) == Some (3))
    assert (map2Alt [Int, Int, Int] (None, Some (2)) ((x, y) => x + y) == None)
    assert (map2Alt [Int, Int, Int] (Some (1), None) ((x, y) => x + y) == None)
    assert (map2 [Int, Int, Int] (None, Some (2)) ( (x, y) => x + y) == None)
    assert (map2 [Int, Int, Int] (Some (1), None) ( (x, y) => x + y) == None)

    val test = (1 to 5).map (Some (_)).toList
    val failTest = test.dropRight (2) ++ List (None) ++ test.takeRight (2)

    assert (Option.sequence (test) == Some (List (1, 2, 3, 4, 5)))
    assert (Option.sequence (failTest) == None)
    assert (Option.sequenceAlt (test) == Some (List (1, 2, 3, 4, 5)))
    assert (Option.sequenceAlt (failTest) == None)

    val error = RuntimeException ("This is a mistake")
    val anotherError = RuntimeException ("This is also a mistake")
    val left = Left [RuntimeException, Int] (error)
    val anotherLeft = Left [RuntimeException, Int] (anotherError)
    val right = Right (5)
    val anotherRight = Right (10)

    assert (right.map (_ + 5) == Right (10))
    assert (left.map (_ + 5) == Left (error))
    assert (right.flatMap (x => Right (x + 5)) == Right (10))
    assert (left.flatMap (x => Right (x + 5)) == Left (error))
    assert (right.flatMap (x => Left (anotherError)) == Left (anotherError))
    assert (left.flatMap (x => Left (anotherError)) == Left (error))
    assert (right.orElse (anotherRight) == right)
    assert (left.orElse (anotherRight) == anotherRight)
    assert (right.orElse (anotherLeft) == right)
    assert (left.orElse (anotherLeft) == anotherLeft)
    assert (right.map2 (anotherRight) ((x, y) => x + y) == Right (15))
    assert (right.map2 (anotherLeft) ((x, y) => x + y) == Left (anotherError))
    assert (left.map2 (anotherRight) ((x, y) => x + y) == Left (error))
    assert (left.map2 (anotherLeft) ((x, y) => x + y) == Left (error))

    val eitherTest = (1 to 5).map (Right (_)).toList
    val eitherTestFail = eitherTest.dropRight (2) ++ List (anotherLeft) ++ eitherTest.takeRight (2)

    println (s"finished tests ${Either.sequence (eitherTestFail)}")
    assert (Either.sequence (eitherTest) == Right (List (1, 2, 3, 4, 5)))
    assert (Either.sequence (eitherTestFail) == anotherLeft)
    assert (Either.sequenceAlt (eitherTest) == Right (List (1, 2, 3, 4, 5)))
    assert (Either.sequenceAlt (eitherTestFail) == anotherLeft)

    println (s"finished tests")