package problems

/**
 * Created by cedricbastin on 10/04/15.
 */
object P01to10 extends App {
  //problem 1
  def last[T](list:List[T]) = list.reverse.head //check for empty list
  assert(last(List(1, 1, 2, 3, 5, 8)) == 8)

  //problem 2
  def penultimate[T](list:List[T]) = list(list.size-2)
  assert(penultimate(List(1, 1, 2, 3, 5, 8)) == 5)

  //problem 3
  def nth[T](n:Int, list:List[T]) = list(n)
  assert(nth(2, List(1, 1, 2, 3, 5, 8)) == 2)


  //problem 6
  def isPalindrome[T](list:List[T]) = {
    val list2 = list.reverse
    list.zip(list2).forall(e => e._1 == e._2)
  }
  assert(isPalindrome(List(1, 2, 3, 2, 1)))

  //problem 7
  def flatten(list:List[Any]):List[Any] = list match {
    case Nil => Nil
    case (x:List[Any]) :: xs => flatten(x) ::: flatten(xs)
    case x :: xs => List(x) ::: flatten(xs)
  }
  assert(flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8))

  //problem 8
  def compress[T](list:List[T]):List[T] = list match {
    case Nil => Nil
    case x :: xs => x :: compress(xs.dropWhile(_ == x))
  }
  assert(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e))

  //problem 9
  def pack[T](list:List[T]):List[List[T]] = list match {
    case Nil => Nil
    case x :: xs =>
      val rest = xs.takeWhile(_ == x)
      (x :: rest) :: pack(xs.dropWhile(_ == x))
  }
  assert(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))

  //problem 10
  def encode[T](list:List[T]):List[(Int, T)] = pack(list).map(x => (x.size, x.head))
  assert(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))

  println("done")
}
