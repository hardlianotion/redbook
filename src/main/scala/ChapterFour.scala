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

import ChapterFour.Option.{None, Some}
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
