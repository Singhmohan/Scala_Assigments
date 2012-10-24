package forcomp

import common._

object Anagrams {

  type Word = String

  type Sentence = List[Word]

  type Occurrences = List[(Char, Int)]

  val dictionary: List[Word] = loadDictionary

  def wordOccurrences(w: Word): Occurrences =
    w.toList.groupBy(_.toLower).map(_ match {case (a,b) => (a, b.length)}).toList.sortWith(_._1 < _._1)

  def sentenceOccurrences(s: Sentence): Occurrences = s match {
      case Nil => Nil
      case x :: xs => s.map(wordOccurrences(_)).reduce(_++_).groupBy(_._1).map(_ match {
        case (c,xs) => (c, xs.map(_._2).sum)
      }).toList.sortWith(_._1 < _._1)
    }

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary.groupBy(wordOccurrences(_))

  def wordAnagrams(word: Word): Iterable[Word] =
    dictionaryByOccurrences.get(wordOccurrences(word)).flatten

  def combinations(xs: Occurrences): List[Occurrences] = xs match {
    case Nil => List(Nil)
    case (_,0) :: ys => combinations(ys)
    case (c,n) :: ys => combinations((c,n-1) :: ys) ++ combinations((c,n-1) :: ys).map(zs =>
      zs.find(_._1 == c) match {
        case Some(_) => zs.map(_ match {
            case (d, o) => if (d == c) (d,o+1) else (d, o)
          })
        case None => (c,1) :: zs
      })
  }

  def subtract(xs: Occurrences, ys: Occurrences): Occurrences =
    xs.map(x => ys.find(_._1 == x._1) match {
      case Some(y) => (x._1, x._2 - y._2)
      case None => x
    }).filter(_._2 != 0)

  def sentenceAnagrams(s: Sentence): List[Sentence] = {
    def h(xs: Occurrences): Iterable[Word] = dictionaryByOccurrences.get(xs).flatten
    def f(xs: Occurrences): List[Sentence] = xs match {
      case Nil => List(Nil)
      case _ => combinations(xs) flatMap (c => h(c) flatMap (w => f(subtract(xs,c)).map(w :: _)))
    }
    f(sentenceOccurrences(s))
  }
}