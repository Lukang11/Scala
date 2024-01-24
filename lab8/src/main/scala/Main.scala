@main
def mainProg: Unit = {
  println("Hello, World!")
  println(countChars("hello"))

  // val strefy: Seq[String] = java.util.TimeZone.getAvailableIDs.toSeq
  // println(sortTimeZone(strefy))

  // val code = Seq(1, 3, 2, 2, 4, 5)
  // val move = Seq(2, 1, 2, 4, 7, 2)
  // println(score(code)(move))
  // println(code.contains(move(1)))

  val playerList = List(("Łukasz", "Angielski", 5, 12),
                        ("Tomek", "Hula", 2, 17),
                        ("Paweł", "Drań",16, 7),
                        ("Asia", "Sporrtowa", 10, 13),
                        ("Łukasz", "Angielski", 2, 10),
                        ("Tomek", "Hula", 12, 7),
                        ("Paweł", "Drań",6, 15),
                        ("Asia", "Sporrtowa", 20, 3),
                        ("Asia", "NieSporrtowa", 20, 3))
  println(calScore(playerList))
}
//exercise 1
def countChars(str: String): Int = {
  str.distinct.length
}
//exercise 2
def sortTimeZone( seq:Seq[String]): Any = {
  seq.filter(x => x.contains("Europe")).map( x => x.stripPrefix("Europe/")).sortBy(x => (x.length,x))
}

//exerrcise 3
def score(code: Seq[Int])(move: Seq[Int]): Any = {
  (
    code.zip(move).filter( x => x._1 == x._2).length
    ,
    move.foldLeft(List.empty)((acc,value) => if (acc.contains(value)) acc else acc :+ value).foldLeft(0)((acc,value) => if(code.contains(value)) acc + 1 else acc)
  )
}
//exercise 4
def calScore( seq:List[(String,String,Int,Int)]): Any = {
  seq.groupBy(x => (x._1,x._2)).map( x => x._2).map(x=> x.foldLeft(("","",0.0,0.0,0))((acc,value) => 
      (value._1,value._2,acc._3 + value._3,acc._4 + value._4,acc._5+1)
    )).map( x => (x._1,x._2,x._3/x._5,x._4/x._5,(x._3+x._4)/x._5)).toSeq.sortBy( x => (x._3 + x._4,x._2)).reverse
}