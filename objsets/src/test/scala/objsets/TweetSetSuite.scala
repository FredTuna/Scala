package objsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
    val set6 = set5.incl(new Tweet("e", "e body", 42))
    val set7 = set5.incl(new Tweet("f", "f body", 1))
    val set8 = set5 union set6 union set7
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  def printList(l: TweetList) = l.foreach((x: Tweet) => println(x.toString))

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("mostRetweeted: set5") {
    new TestSets {
      val mostRetweeted = set5.mostRetweeted
      assert(mostRetweeted.user == "a" || mostRetweeted.user == "b")
    }
  }

  test("mostRetweeted: set8") {
    new TestSets {
      val mostRetweeted = set8.mostRetweeted
      assert(mostRetweeted.user == "e")
    }
  }

  test("mostRetweeted: empty throws exception") {
    new TestSets {
      intercept[java.util.NoSuchElementException] {
        set1.mostRetweeted
      }
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

  test("descending: set8") {
    new TestSets {
      val trends = set8.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "e")
    }
  }

  test("descending: empty set") {
    new TestSets {
      val trends = set1.descendingByRetweet
      assert(trends.isEmpty)
    }
  }

  }
