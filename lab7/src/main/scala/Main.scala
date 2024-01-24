@main
def mainProg: Unit = {
  println("----------")
  val testSequence:Seq[String] = Seq("apple", "banana", "cherry", "date", "elderberry")
  println(subSeq(testSequence,1,3))

  val exe2Seq: Seq[(Char,Double)] = Seq(('A',2.0),('B',3.0),('C',1.9),('D',2.0))
  println(getMinMax(exe2Seq))
  println(remElems(testSequence,2))

  val exe4Seq1 = Seq(1, 2, 3)
  val exe4Seq2 = Seq(2, 2, 1, 3)
  println(diff(exe4Seq1,exe4Seq2))

  val exe5Seq = Seq(Some(5.4), Some(-2.0), Some(1.0), None, Some(2.6))
  println(sumOption(exe5Seq))

  val exe6Seq = Seq(1, 1, 2, 4, 4, 4, 1, 3)
  println(deStutter(exe6Seq))

  val exe7Seq = Seq(1, 2, 2, 4)
  println(isOrdered(exe7Seq)(_ < _))

  val exe8Seq = Seq('a','b','a','c','c','a');
  println(freq(exe8Seq))
  println(threeNumbers(10))

  println("----------")
}


//exercise 1
def subSeq[A](seq: Seq[A], begIdx: Int, endIdx: Int): Seq[A] ={
  seq.drop(begIdx).take((endIdx - begIdx)+1)
}
//exercise 2
def getMinMax[A](seq: Seq[(A, Double)]): Option[(A, A)] = {
  Some((seq.maxBy(x => x._2)._1, seq.minBy(x => x._2)._1))
}
//exercise 3
def remElems[A](seq: Seq[A], k: Int): Seq[A] = {
  seq.zipWithIndex.filter( x => x._2 != k).map( x => x._1)
}
//exercise 4
def diff[A](seq1: Seq[A], seq2: Seq[A]): Seq[A] = {
  seq1.zip(seq2).filter( x => x._1 != x._2).map( x => x._1)
}
// exerrcise 5
def sumOption(seq: Seq[Option[Double]]): Double = {
  seq.foldLeft(0.0)((acc, value) => value match {
    case Some(value) => acc + value
    case None => acc + 0
  })
} //Double

//Exercise 6 
def deStutter[A](seq: Seq[A]): Seq[A] ={
  seq.foldLeft(List.empty[A])((accList,value) =>
    if(accList.contains(value)) accList else accList :+ value 
)
} //Seq[A]

//Exercise 7
def isOrdered[A](seq: Seq[A])(leq:(A, A) => Boolean): Any = {
  seq.sliding(2).forall {
    case Seq(a,b) => leq(a,b)
  }
}

def freq[A](seq: Seq[A]): Set[(A, Int)] = {
  seq.groupBy(x => x).map(x => (x._1,x._2.length)).toSet
}

def threeNumbers(n: Int): Any = {
  for{
    a <- (1 to n).toList
    b <- (1 to n).toList
    c <- (1 to n).toList
    if (a*a + b*b == c*c)
    if (a < b)
  } yield (a,b,c)
}///Seq[(Int, Int, Int)]