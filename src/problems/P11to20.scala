package problems

/**
 * Created by cedricbastin on 10/04/15.
 */

object P11to20 extends App{
  //problem 11
  def encodeModified[T](list:List[T]):List[Any] = P01to10.encode(list).map(res => if (res._1 == 1) res._2 else res)
  assert(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))

  //problem 12
  def decode[T](list:List[(Int, T)]):List[T] = list.map(e => for (i <- 1 to e._1) yield e._2).flatten
  assert(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))

  //problem 13

  //problem 14
  def duplicate[T](list:List[T]):List[T] = list match {
    case Nil => Nil
    case x :: xs => x :: x :: duplicate(xs)
  }
  assert(duplicate(List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))

  //problem 15
  def duplicateN[T](n:Int, list:List[T]):List[T] = list flatMap {e =>
    for (i <- 1 to n) yield e
  }
  assert(duplicateN(3, List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))

  //problem 16
  def drop[T](n:Int, list:List[T]):List[T] = if (list.size < n)list else list.take(n-1) ::: drop(n, list.drop(n))
  assert(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))

  //problem 17
  def split[T](n:Int, list:List[T]):(List[T],List[T]) = (list.take(n), list.drop(n))
  assert(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))

  //problem 18
  def slice[T](start:Int, end:Int, list:List[T]):List[T] = list.take(end).drop(start)
  assert(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g))

  //problem 19
  def rotate[T](n:Int, list:List[T]):List[T] = {
    if (n == 0) list
    else if (n > 0) list.drop(n) ::: list.take(n)
    else list.reverse.take(-n).reverse ::: list.reverse.drop(-n).reverse
  }
  assert(rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
  assert(rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))

  //problem 20
  def removeAt[T](n:Int, list:List[T]):(List[T], T) = (list.take(n) ::: list.drop(n+1), list(n))
  assert(removeAt(1, List('a, 'b, 'c, 'd)) == (List('a, 'c, 'd),'b))

  println("done 11-20")
}
