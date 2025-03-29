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


import ChapterFour.Option.*
import ChapterFour.Option

  @main
  def run =
    val someOne: Option [Int] = Some (1)
    val noOne: Option [Int] = None

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

    val numbers = Seq.from (1 to 10).map (_.toDouble)

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