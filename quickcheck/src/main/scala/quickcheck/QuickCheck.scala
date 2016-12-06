package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    e <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(e,h)
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minElement") = forAll { (a: A , b: A) =>
    var h = insert(a, empty)
    h = insert(b, h)
    val min = if (a < b) a else b
    findMin(h) == min
  }
  
  property("deleteMin") = forAll { a: A =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))  
  }
  
  property("sortedDelete") = forAll { h: H  =>
    def getDeletedList(h: H): List[A] = {
      if (isEmpty(h)) List()
      else 
        findMin(h)::getDeletedList(deleteMin(h))      
    }
    
    val list = getDeletedList(h)
    list == list.sorted
  }

  property("mergedMin") = forAll { (h1: H , h2: H)  =>
    val h = meld(h1, h2)
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val min = if (min1 < min2) min1 else min2
    findMin(h) == min
  }  

  property("moveMinToSecondHeapMeld") = forAll { (h1: H , h2: H)  =>
    def compare(h1: H, h2: H): Boolean = {
      if (h1 == empty) h2 == empty
      else if (h2 == empty) h1 == empty
      else {
        val min1 = findMin(h1)
        val min2 = findMin(h2)
        min1 == min2 && compare(deleteMin(h1), deleteMin(h2))
      }
    }
    val min1 = findMin(h1)
    val hOrig = meld(h1,h2)
    val hChanged = meld(deleteMin(h1),insert(min1,h2))
    compare(hOrig,hChanged)
  }  

}
