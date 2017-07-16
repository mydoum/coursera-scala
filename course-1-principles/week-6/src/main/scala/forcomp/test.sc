import forcomp.Anagrams._

val str = "Hello"
val str2 = "World"

val lstr = List(str, str2)

val littleDic = List("ate", "eat")

str.groupBy(ch => ch)
val occs = str.groupBy(ch => ch).map(elem => (elem._1, elem._2.length)).toList

lstr.reduce(_ + _)

val occList = littleDic.map(word => (wordOccurrences(word), List(word))).groupBy(pair => pair._1) map { case (k, v) => k -> v.flatMap(_._2) }

val ex = List(('a', 2), ('b', 2))

def findAll(elem: (Char, Int)): Occurrences = elem match {
  case elem if (elem._2 == 1) => List(elem)
  case elem => findAll((elem._1, elem._2 - 1)) ::: List(elem)
}

findAll(ex(0))

def disassemble(occ: Occurrences): Occurrences = occ match {
  case Nil => Nil
  case x :: xs => findAll(x) ::: disassemble(xs)
}

val disEx = disassemble(ex)
disassemble(ex).groupBy(elem => elem._1)
disassemble(occs)

val disResult = disassemble(ex).toSet.subsets().map(_.toList).toList
disassemble(occs).toSet.subsets().map(_.toList).toList

def isClean(occ: Occurrences): Boolean = {
  !occ.groupBy(elem => elem._1).exists(_._2.length != 1)
}

isClean(ex)
isClean(disEx)

def clean(occ: List[Occurrences]): List[Occurrences] = occ match {
  case Nil => Nil
  case x :: xs => if (isClean(x)) x :: clean(xs) else clean(xs)
}

clean(disResult)

val occ1 = List(('a', 2), ('b', 1))
val occ2 = List(('a', 1), ('b', 1))

def subOne(x: Occurrences, toSubs: (Char, Int)): Occurrences = {
  val mapOcc = x.toMap
  val elem = mapOcc get toSubs._1

  elem match {
    case None => x
    case Some(value: Int) if (value - toSubs._2) > 0 => (mapOcc + (toSubs._1 -> (value - toSubs._2))).toList
    case Some(value: Int) if (value - toSubs._2) == 0 => (mapOcc - toSubs._1).toList
    case Some(value: Int) if (value - toSubs._2) < 0 => x
  }
}

subOne(List(('a', 2), ('b', 1)), ('a', 1))

def subtract(x: Occurrences, y: Occurrences): Occurrences = y match {
  case Nil => x
  case head :: tail => subtract(subOne(x, head), tail)
}

subtract(occ1, occ2)

val sentence = List("Linux", "rulez")

val occ = sentenceOccurrences(sentence)

val combis = combinations(occ)

def create(elem: (Char, Int)): Word = elem._2 match {
  case 0 => ""
  case _ => elem._1 + create(elem._1, elem._2 - 1)
}

def occurrencesToWord(occ: Occurrences): Word = {
  occ.map(create(_)).foldLeft("")(_ + _)
}
/*
val you = List(('y', 1), ('o', 3), (' ', 1), ('u', 1))
create(('a', 3))
OccurrencesToWord(you)
*/
def occurrencesAnagrams(occ: Occurrences): List[Word] = dictionaryByOccurrences get occ match {
  case None => List()
  case Some(result) => result
}

dictionaryByOccurrences get List(('y', 1), ('o', 1), ('u', 1)) match {
  case None => List()
  case Some(result) => result
}

/*def fullList(comb: List[Occurrences]): Map[Occurrences, List[Word]] = comb match {
  case Nil => Map()
  case x::xs if occurrencesAnagrams(x) != List() => Map(x -> occurrencesAnagrams(x)) ++ fullList(xs)
  case _::xs => fullList(xs)
}

fullList(combis)
*/

val comb2 = (for (elem <- combinations(occ)) yield (elem, wordAnagrams(occurrencesToWord(elem)))).filter(_._2.nonEmpty).toMap

/*for (elem <- combinations(occ)) yield dictionaryByOccurrences get elem match {
  case None => List()
  case Some(result) => result
}*/

def findAll(base: Occurrences, dict: Map[Occurrences, List[Word]]): List[Sentence] = {
  if (base == List()) List()
  else {
    val comb = combinations(base)
    for {
      el <- comb
      w <- dict.getOrElse(el, Nil)
    } yield (el, w)
  }
}

findAll(occ, comb2)

