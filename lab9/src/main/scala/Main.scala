@main
def mainProg: Unit = {
  val linie = io.Source
    .fromResource("ogniem-i-mieczem.txt")
    .getLines.toList
  //  def histogram(maks: Int): String = {
  // ???
  // }
  println(countLetters(linie))
}
def countLetters(str:List[String]): Any = {
  str
  .map(line =>
    countChar(line.toLowerCase)
  ).foldLeft(List.empty[(Char,Int)])((acc,countedLetters)=>
    println(acc)
    println(countedLetters)
    countedLetters.foldLeft()
    acc
  )
}
def countChar(str:String):Any ={
  str.groupBy(x => x).filter(x => x._1.isLetter).map {
    case letter => (letter._1,letter._2.length)
  }.toList
}

  
