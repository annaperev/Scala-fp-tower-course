package exercises.valfunction

import exercises.valfunction.ValueFunctionExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ValueFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  /////////////////////////////////////////////////////
  // Exercise 1: String API with higher-order functions
  /////////////////////////////////////////////////////

  // replace `ignore` by `test` to enable the test
  test("selectDigits examples") {
    assert(selectDigits("hello4world-80") == "480")
    assert(selectDigits("welcome") == "")
  }

  // replace `ignore` by `test` to enable the test
  test("selectDigits length is smaller") {
    forAll { (text: String) =>
      assert(selectDigits(text).length <= text.length)
    }
  }

  test("selectDigits only returns number") {
    forAll { (text: String) =>
      selectDigits(text).foreach(c => assert(c.isDigit))
    }
  }

  test("secret examples") {
    assert(secret("welcome123") == "**********")
  }

  test("secret length is the same") {
    forAll { (text: String) =>
      assert(secret(text).length == text.length)
    }
  }

  test("secret only returns '*' ") {
    forAll { (text: String) =>
       secret(text).foreach(c => assert(c.equals('*')))
    }
  }

  test("secret idempotence") {
    forAll { (text: String) =>
      val once = secret(text)
      val twice = secret(secret(text))
      assert(once == twice)
    }
  }

  test ("isValidUsername examples"){
    assert(isValidUsername("john-doe"))
    assert(!isValidUsername("*john*"))
  }

  test ("isValidUsername"){
    forAll{(username: String) =>
      assert(isValidUsername(username.reverse) == isValidUsername(username))
    }
  }
  ///////////////////////
  // Exercise 2: Point
  ///////////////////////
  test ("Point isPositive"){
    forAll{ (x: Int, y: Int, z: Int) =>
      assert(Point(x.max(0), y.max(0), z.max(0)).isPositive)
    }
  }

  test ("Point forAll"){
    forAll{ (x: Int, y: Int, z: Int, predicate: Int => Boolean) =>
      assert(Point(x, y, z).forAll(predicate) == List(x, y, z).forall(predicate))
    }
  }


}
