package com.indix.codekata

case class Dictionary(words:List[String]) {

  def makeWordChain(startWord: String, endWord: String): List[String] = {
    if(startWord.equals(endWord)) return List(startWord)

    nextWords(startWord).map(next => {
      without(startWord).makeWordChain(next, endWord)
    }).filterNot(_.isEmpty).map(startWord :: _).headOption.getOrElse(Nil)
  }

  def without(word:String) = Dictionary(words.filterNot(_ == word))
  
  def nextWords(word: String) = {
    words.filter(dictWord => isVaryingByOne(word, dictWord))
  }
  
  def isVaryingByOne(currentWord: String, dictWord: String): Boolean = {
    numberOfChanges(currentWord, dictWord) == 1
  }

  def numberOfChanges(currentWord: String, dictWord: String): Int = {
    currentWord.zip(dictWord).count(tuple => tuple._1 != tuple._2)
  }
}
