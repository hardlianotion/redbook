import ChapterThree.Calamity.{HasArrived, NotYet}
import ChapterThree.Tree.{Branch, Leaf}
import jdk.internal.joptsimple.internal.Messages.message

import scala.annotation.tailrec
import scala.util.{Failure, Try}

object ChapterThree:
  enum List [+A]:
    case Nil
    case Cons (head: A, tail: List [A])

  object List:

    def fromItems [A] (as: A*): List [A] =
      @tailrec
      def impl (acc: List [A], ls: Seq [A]): List [A] =
        if ls.isEmpty then
          acc
        else
          impl (Cons (ls.head, acc), ls.tail)
      impl (Nil, as.reverse)

    def sum (ints: List [Int]): Int =
      ints match
        case Nil => 0
        case Cons (head, tail) => head + sum (tail)

    def product (ints: List [Int]): Int =
      ints match
        case Nil => 1
        case Cons (0, _) => 0
        case Cons (head, tail) => head + product (tail)

    // exercise 3.2
    def unsafeTail [A] (list: List [A]): List [A] =
      list match
        case Nil => sys.error (s"The list is empty")
        case Cons (h, t) => t

    def unsafeHead [A] (list: List [A]): A =
      list match
        case Nil => sys.error (s"The list is empty")
        case Cons (h, _) => h

    // exercise 3.2.bis
    def tail [A] (list: Cons [A]): List [A] =
      list.tail

    def head [A] (list: Cons [A]): A =
      list.head

    // exercise 3.3
    def setHead [A] (list: Cons [A], head: A): List [A] =
      Cons (head, list.tail)

    // exercise 3.4
    def drop [A] (list: List [A], n: Int): List [A] =
      assert (n >= 0)
      (n, list) match
        case (_, Nil) => Nil
        case (0, ls) => ls
        case (n, ls) => drop (unsafeTail (ls), n - 1)

    // exercise 3.5
    def dropWhile [A] (list: List [A], pred: A => Boolean): List [A] =
      list match
        case Nil => Nil
        case Cons (head, tail) =>
          if pred (head) then
            dropWhile (tail, pred)
          else
            list

    // exercise 3.6 // unexpectedly expensive
    def init [A] (list: Cons [A]): List [A] =
      list.tail match
        case Nil => Nil
        case t: Cons [A] => Cons (list.head, init (t)) // let's improve this


    def foldRight [A, B] (list: List [A], acc: B, f: (A, B) => B): B =
      list match
        case Nil => acc
        case Cons (h, t) => f (h, foldRight (t, acc, f))

    def foldRight [A, B] (list: List [A], acc: B, broken: B, breaker: A => Boolean, f: (A, B) => B): B =
      list match
        case Nil => acc
        case Cons (h, _) if breaker (h) => broken
        case Cons (h, t) => f (h, foldRight (t, acc, broken, breaker, f))

    // exercise 3.7  What are the issues with short-circuiting in presence of zeros?
    def productRight (list: List [Int]): Int =
      foldRight (list, 1, _ * _)

    def productRightBreaker (list: List [Int]): Int =
      foldRight (list, 1, 0, _ == 0, _ * _)

    // exercise 3.9
    def lengthRight [A] (list: List [A]): Int =
      foldRight (list, 0, (_, l) => l + 1)

    // exercise 3.10
    @tailrec
    def foldLeft [A, B] (list: List [A], acc: B, f: (B, A) => B): B =
      list match
        case Nil => acc
        case Cons (head, tail) => foldLeft (tail, f (acc, head), f)

    // Exercise 3.11
    def sumFold (list: List [Int]): Int =
      foldLeft (list, 0, _ + _)

    def productLeft (list: List [Int]): Int =
      foldLeft (list, 1, _ * _)

    def length (list: List [Int]): Int =
      foldLeft (list, 0, (l, _) => l + 1)

    // exercise 3.12
    def reverse [A] (list: List [A]): List [A] =
      foldLeft (list, Nil, (b, a) => Cons (a, b))

    // exercise 3.13
    def foldRightAsLeft [A, B] (list: List [A], acc: B, f: (A, B) => B): B =
      foldLeft (reverse (list), acc, (a, b) => f (b, a))

    def foldRightAsLeftLux [A, B] (list: List [A], acc: B, f: (A, B) => B): B =
      val step = (acc: B => B, a: A) => (b: B) => f (a, acc (b))
      foldLeft [A, B => B] (list, identity, step) (acc)

    def foldLeftAsRight [A, B] (list: List [A], acc: B, f: (B, A) => B): B =
      foldRight (reverse (list), acc, (a, b) => f (b, a))

    def foldLeftAsRightLux [A, B] (list: List [A], acc: B, f: (B, A) => B): B =
      val step = (a: A, acc: B => B) => (b: B) => acc (f (b, a))
      foldRight [A, B => B] (list, identity, step) (acc)

    // exercise 3.14
    def append [A] (left: List [A], right: List [A]): List [A] =
      foldLeft (reverse (left), right, (acc, r) => Cons (r, acc))

    // exercise 3.15
    def flatten [A] (lol: List [List [A]]): List [A] =
      foldLeft (lol, Nil, (b, a) => append (b, a))

    // utility
    def mapAsLeft [A, B] (list: List [A], f: A => B): List [B] =
      foldLeft (reverse (list), Nil, (b, a) => Cons (f (a), b))

    // exercise 3.18
    def map [A, B] (list: List [A], f: A => B): List [B] =
      list match
        case Nil => Nil
        case Cons (h, t) => Cons (f (h), map (t, f))

    // exercise 3.19
    def filter [A] (list: List [A], pred: A => Boolean): List [A] =
      list match
        case Nil => Nil
        case Cons (h, t) =>
          if (pred (h)) then
            Cons (h, filter (t, pred))
          else
            filter (t, pred)

    // exercise 3.20
    def flatMap [A, B] (as: List [A], f: A => List [B]): List [B] =
      flatten (map (as, f))

    // exercise 3.21
    def filterAsFlatMap [A] (list: List [A], pred: A => Boolean): List [A] =
      flatMap (list, a => if pred (a) then Cons (a, Nil) else Nil)

  // exercise 3.24
  def hasSubsequence [A] (list: scala.List [A], cand: scala.List [A]): Boolean =
    val pruned = cand.foldLeft (list): (acc, h) =>
      acc.dropWhile (_ != h)

    pruned.nonEmpty

  import List.*

  def matchTest = List.fromItems (1, 2, 3, 4, 5) match
    case Cons (x, Cons (2, Cons (4, _))) => x
    case Nil => 42
    case Cons (x, Cons (y, Cons (3, Cons (4, _)))) => x + y
    case Cons (h, t) => h + sum (t)
  // this is an interesting warning
  // case _ => 101

  // exercise 3.8
  // it generalises!
  def foldResult = foldRight (List.fromItems (1, 2, 3), Nil: List [Int], Cons (_, _))

  enum Calamity [T]:
    case HasArrived (error: Throwable)
    case NotYet (t: T)

    def hasArrived: Boolean = this match
      case HasArrived(_) => true
      case NotYet(_) => false

    def isDeferred: Boolean = this match
      case HasArrived(_) => false
      case NotYet(_) => true

  object Calamity:
    def apply [T] (thunk: => T): Calamity [T] =
      try {
        NotYet (thunk)
      } catch {
        case error: Throwable => HasArrived (error)
      }

  // exercise 3.10
  val largeList = List.fromItems ( (1 to 60000) *)
  def testRight = productRight (largeList)
  def testLeft = productLeft (largeList)

  // exercise 3.16
  def addOner (list: List [Int]): List [Int] =
    mapAsLeft (list, _ + 1)

  // exercise 3.17
  def stringer (list: List [Double]): List [String] =
    mapAsLeft (list, _.toString)

  // exercise 3.22
  def addInt (left: List [Int], right: List [Int]): List [Int] =
    (left, right) match
      case (Nil, Nil) => Nil
      case (l, Nil) => l
      case (Nil, r) => r
      case (Cons (lh, lt), Cons (rh, rt)) => Cons (lh + rh, addInt (lt, rt))

  // exercise 3.23
  def combine [A] (left: List [A], right: List [A], f: (A, A) => A): List [A] =
    (left, right) match
      case (Nil, Nil) => Nil
      case (l, Nil) => l
      case (Nil, r) => r
      case (Cons (lh, lt), Cons (rh, rt)) => Cons (f (lh, rh), combine (lt, rt, f))

  def combinePrune [A, B, C] (left: List [A], right: List [B], f: (A, B) => C): List [C] =
    (left, right) match
      case (Nil, Nil) => Nil
      case (l, Nil) => Nil
      case (Nil, r) => Nil
      case (Cons (lh, lt), Cons (rh, rt)) => Cons (f (lh, rh), combinePrune (lt, rt, f))

  enum Tree [+A]:
    case Leaf (value: A)
    case Branch (left: Tree [A], right: Tree [A])

  object Tree:

    def size [A] (tree: Tree [A]): Int =
      tree match
        case Leaf (_) => 1
        case Branch (l, r) => size (l) + size (r)

    // exercise 3.25

    def maximum (tree: Tree [Int]): Int =
      tree match
        case Leaf (i) => i
        case Branch (l, r) => maximum (l) max (maximum (r))

    // exercise 3.26

    def depth [A] (tree: Tree [A]): Int =
      tree match
        case Leaf (i) => 1
        case Branch (l, r) => 1 + (depth (l) max (depth (r)))

    // exercise 3.27

    def map [A, B] (tree: Tree [A], f: A => B): Tree [B] =
      tree match
        case Leaf (a) => Leaf (f (a))
        case Branch (l, r) => Branch (map (l, f), map (r, f))

    // size, maximum, depth, map
    // exercise 3.28

    def fold [A, B] (tree: Tree [A], f:A => B, g: (B, B) => B): B =
      tree match
        case Leaf (a) => f (a)
        case Branch (l, r) => g (fold (l, f, g), fold (r, f, g))

  def size [A] (tree: Tree [A]): Int =
    Tree.fold (tree, _ => 1, _ + _)

  def maximum (tree: Tree [Int]): Int =
    Tree.fold (tree, identity, math.max)

  def depth [A] (tree: Tree [A]): Int =
    Tree.fold (tree, _ => 1, (l, r) => 1 + l max r)

  def map [A, B] (tree: Tree [A], f: A => B): Tree [B] =
    Tree.fold (tree, a => Leaf (f (a)), (l, r) => Branch (l, r))

  def run =
    val ls = List.fromItems (1, 2, 3, 4, 5)
    val consLs: Cons [Int] = Cons (1, Cons (2, Cons (3, Cons (4, Cons (5, Nil)))))
    assert (ls == Cons (1, Cons (2, Cons (3, Cons (4, Cons (5, Nil))))))
    println (s"running list checks ...")
    assert (matchTest == 3)
    assert (unsafeTail (ls) == List.fromItems (2, 3, 4, 5))
    assert (setHead (consLs, 7) == List.fromItems (7, 2, 3, 4, 5))
    assert (addInt (List.fromItems (1, 2, 3), List.fromItems (4, 5, 6)) == List.fromItems (5, 7, 9))
    assert (addInt (List.fromItems (1, 2, 3), List.fromItems (4, 5, 6, 7)) == List.fromItems (5, 7, 9, 7))
    assert (drop (ls, 3) == List.fromItems (4, 5))
    assert (dropWhile(ls, _ < 3) == List.fromItems(3, 4, 5))
    assert (init (consLs) == List.fromItems (1, 2, 3, 4))
    assert (productRightBreaker (ls) == 120)
    assert (lengthRight (ls) == 5)
    assert (foldLeft (ls, 0, _ - _) == -15)
    assert (foldRight (ls, 0, _ - _) == 3)
    assert (sumFold (ls) == 15)
    assert (Calamity {productRight (largeList)}.hasArrived)
    assert (Calamity {productLeft (largeList)}.isDeferred)
    assert (length (largeList) == 60000)
    assert (reverse (ls) == List.fromItems (5, 4, 3, 2, 1))
    assert (foldLeftAsRight (ls, 0, _ - _) == -15)
    assert (foldLeftAsRightLux (ls, 0, _ - _) == -15)
    assert (foldRightAsLeft (ls, 0, _ - _) == 3)
    assert (foldRightAsLeftLux (ls, 0, _ - _) == 3)
    assert (append(ls, ls) == List.fromItems (1, 2, 3, 4, 5, 1, 2, 3, 4, 5))
    assert (flatten (List.fromItems (ls, reverse (ls))) == List.fromItems (1, 2, 3, 4, 5, 5, 4, 3, 2, 1))
    assert (mapAsLeft (ls, _ + 1) == List.fromItems (2, 3, 4, 5, 6))
    assert (List.map (ls, _ + 1) == List.fromItems (2, 3, 4, 5, 6))
    assert (List.filter (ls, _ < 3) == List.fromItems (1, 2))
    assert (List.flatMap (ls, x => List.fromItems (1, x)) == List.fromItems (1, 1, 1, 2, 1, 3, 1, 4, 1, 5))
    assert (List.filterAsFlatMap (ls, _ < 3) == List.fromItems (1, 2))
    assert (hasSubsequence(scala.List (1, 2, 3, 4, 5), scala.List (3, 5)))
    assert (!hasSubsequence(scala.List (1, 2, 3, 4, 5), scala.List (5, 3)))
    assert (addOner (List.fromItems (1, 2, 3)) == List.fromItems (2, 3, 4))
    assert (stringer (List.map (ls, _.toDouble)) == List.fromItems ("1.0", "2.0", "3.0", "4.0", "5.0"))
    assert (addInt (ls, reverse (ls)) == List.fromItems (6, 6, 6, 6, 6))
    assert (combine (ls, reverse (ls), _ + _) == List.fromItems (6, 6, 6, 6, 6))

    val tree = Branch (
                Branch (Leaf (1), Leaf (2)),
                Branch (Leaf (3), Leaf (4))
              )
    val shifted = Branch (
                    Branch (Leaf (2), Leaf (3)),
                    Branch (Leaf (4), Leaf (5))
                  )

    println (s"running tree checks ...")
    assert (Tree.size (tree) == 4)
    assert (Tree.maximum (tree) == 4)
    assert (Tree.depth (tree) == 3)
    assert (Tree.map (tree, _ + 1) == shifted)

    // fold version
    assert (size (tree) == 4)
    assert (maximum (tree) == 4)
    assert (depth (tree) == 3)
    assert (map (tree, _ + 1) == shifted)

    println (s"done")