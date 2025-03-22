object ChapterThree:
  enum List [+A]:
    case Nil
    case Cons (head: A, tail: List [A])

  object List:
    def apply [A] (as: A*): List [A] =
      if as.isEmpty then
        Nil
      else
        Cons (as.head, apply (as.tail *))

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
        case (n, ls) => drop (ls, n - 1)

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

    // exercise 3.14
    def append [A] (left: List [A], right: List [A]): List [A] =
      foldLeft (reverse (left), right, (acc, r) => Cons (r, acc))

    // exercise 3.15
    def flatten [A] (lol: List [List [A]]): List [A] =
      foldLeft (reverse (lol), Nil, (b, a) => append (b, a))

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

    !pruned.isEmpty

  import List.*

  val result = List (1, 2, 3, 4, 5) match
    case Cons (x, Cons (2, Cons (4, _))) => x
    case Nil => 42
    case Cons (x, Cons (y, Cons (3, Cons (4, _)))) => x + y
    case Cons (h, t) => h + sum (t)
  // this is an interesting warning
  // case _ => 101

  // exercise 3.8
  // it generalises!
  val foldResult = foldRight (List (1, 2, 3), Nil: List [Int], Cons (_, _))

  // exercise 3.10
  val testList = List ( (1 until 60000) *)

  val testRight = productRight (testList)
  val testLeft = productLeft (testList)

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
        case Branch (l, r) => depth (l) max (depth (r))

    // exercise 3.27

    def map [A, B] (tree: Tree [A], f: A => B): Tree [B] =
      tree match
        case Leaf (a) => Leaf (f (a))
        case Branch (l, r) => Branch (map (l, f), map (r, f))

    // size, maximum, depth, map
    // exercise 3.28

    def fold [A, B] (tree: Tree [A], agg: B, f: (B, A) => B, g: (B, B) => B): B =
      tree match
        case Leaf (a) => f (agg, a)
        case Branch (l, r) => g (fold (l, agg, f, g), fold (r, agg, f, g))

  def size [A] (tree: Tree [A]): Int =
    Tree.fold (tree, 0, (x, _) => x + 1, _ + _)

  def maximum (tree: Tree [Int]): Int =
    Tree.fold (tree, Int.MinValue, math.max, math.max)

  def depth [A] (tree: Tree [A]): Int =
    Tree.fold (tree, 0, (a, _) => a + 1, (l, r) => l max r)

  def map [A, B] (tree: Tree [A], f: A => B): Tree [B] =
    ???
