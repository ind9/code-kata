package com.indix.codekata

import org.scalatest.{Matchers, FlatSpec}

class DictionaryTest extends FlatSpec with Matchers {

  it should "translate from cat-to-dog" in {
    val dictionary = Dictionary(List("cat","mat", "cot", "dot", "dog", "cog"))
    dictionary.makeWordChain("cat", "dog") should be (List("cat", "cot", "dot", "dog"))
  }

  it should "give me back an empty chain incase there is no chain possible" in {
    val dictionary = Dictionary(List("mat", "cot", "dot", "dog", "cog"))
    dictionary.makeWordChain("mat", "dog") should be (List())
  }

  it should "makeChain of single element for same source and destination" in {
    val dictionary = Dictionary(List("cat", "cot", "dot", "dog", "cog"))
    dictionary.makeWordChain("cat","cat") should be(List("cat"))
  }

  it should "return the next word with just one character changed" in {
    val dictionary = Dictionary(List("cat", "cot", "dot", "dog", "cog"))
    dictionary.nextWords("cat") should be (List("cot"))
    dictionary.nextWords("cot") should be (List("cat","dot","cog"))
  }


  it should "return true if cat and cot differ by 1" in {
    val dictionary = Dictionary(List("cat", "cot", "dot", "dog", "cog"))
    dictionary.isVaryingByOne("cat", "cot") should be(true)
    dictionary.isVaryingByOne("dot", "cot") should be(true)

    dictionary.isVaryingByOne("dot", "cog") should be(false)
    dictionary.isVaryingByOne("dot", "dot") should be(false)
  }
}
