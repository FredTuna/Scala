package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  trait UnionTestSets extends TestSets{
    val sUnion1 = union(s1, s2)
    val sUnion2 = union(sUnion1, s3)
  }

  test("union contains all elements of each set") {
    new UnionTestSets {

      assert(contains(sUnion1, 1), "Union 1 1")
      assert(contains(sUnion1, 2), "Union 1 2")
      assert(!contains(sUnion1, 3), "Union 1 3")
      assert(contains(sUnion2, 3), "Union 2 1")
      assert(contains(sUnion2, 3), "Union 2 3")
    }
  }

  trait IntersectTestSets extends UnionTestSets{
    val sIntersect1 = intersect(s1, s2)
    val sIntersect2 = intersect( sUnion1, union(s2, s3))
  }

  test("intersection contains  elements in both set") {
    new IntersectTestSets {

      assert(!contains(sIntersect1, 1), "intersect 1 1")
      assert(!contains(sIntersect1, 2), "intersect 1 2")
      assert(!contains(sIntersect1, 3), "intersect  13")

      assert(!contains(sIntersect2, 1), "intersect 2 1")
      assert(contains(sIntersect2, 2), "intersect 2 2")
      assert(!contains(sIntersect2, 3), "intersect 2 3")
    }
  }

  trait DiffTestSets extends IntersectTestSets{
    val sDiff1 = diff(s1, s2)
    val sDiff2 = diff(s2, sUnion2)
    val sDiff3= diff(sUnion2, sIntersect2)
  }
  test("diff contains  elements only in first set") {
    new DiffTestSets {

      assert(contains(sDiff1, 1), "diff 1 1")
      assert(!contains(sDiff1, 2), "diff 1 2")
      assert(!contains(sDiff2, 1), "diff 2 1")
      assert(!contains(sDiff2, 2), "diff 2 2")
      assert(!contains(sDiff3, 2), "diff  3 2")
      assert(contains(sDiff3, 3), "diff  3 3")
    }
  }

  trait FilterTestSets extends DiffTestSets{
    val sFilter1 = filter(s1, (x: Int) => x == 1)
    val sFilter2 = filter(sUnion2, (x: Int) => (x % 2) == 0)
  }

  test("filter contains elements from first set that fit criteria p") {
    new FilterTestSets {

      assert(contains(sFilter1, 1), "filter 1 1")
      assert(!contains(sFilter1, 2), "filter 1 2")
      assert(contains(sFilter2, 2), "filter 2 2")
      assert(!contains(sFilter2, 3), "filter 2 3")
      assert(!contains(sFilter2, 4), "filter 2 4")
    }
  }

  trait QueryTestSets extends FilterTestSets{
    val sMod4Set = (x: Int) => (x % 4) == 0
  }

  test("forall checks if all elements of the set satisfy the condition p") {
    new QueryTestSets {

      assert(forall(sUnion2, (x: Int) => x < 4), "forall union2 < 3")
      assert(!forall(sUnion2, (x: Int) => x < 2), "forall union2 < 2")

      printSet(sMod4Set)

      assert(forall(sMod4Set, (x: Int) => (x % 2) == 0), "forall mod4 mod2")
      assert(!forall(sMod4Set, (x: Int) => x == 201), "forall mod4 mod3")
    }
  }

  test("exists checks if one element of the set satisfy the condition p") {
    new QueryTestSets {

      assert(exists(sUnion2, (x: Int) => x < 3), "exists union2 < 3")
      assert(!exists(sUnion2, (x: Int) => x < 1), "exists union2 < 1")

      assert(exists(sMod4Set, (x: Int) => x == 400), "exists mod4 400")
      assert(!exists(sMod4Set, (x: Int) => x == 301), "exists mod4 300")
    }
  }


  test("map returns set where very element is modified by p") {
    new QueryTestSets {

      val sMapx10 = map(sMod4Set, (x: Int) => x * 3)
      val sMap1 = map(sMod4Set, (x: Int) => 1)

      printSet(sMapx10)
      assert(contains(sMapx10, 40), "mapx10 has 40")
      assert(contains(sMapx10, 10), "mapx10 * 10 has 10")

      printSet(sMap1)
      assert(contains(sMap1, 1), "map1 has 1")
      assert(!contains(sMap1, 40), "map1 has 40")
    }
  }


}
