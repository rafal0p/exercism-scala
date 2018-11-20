import org.scalatest.{FunSuite, Matchers}

/** @version 3.2.0 */
class FanTest extends FunSuite with Matchers {

  test("small fan") {
    Fan.generate(2) should be(
      """**.*
        |.***
        |***.
        |*.**
      """.stripMargin)
  }

  test("medium fan") {
    Fan.generate(3) should be(
      """***..*
        |.**.**
        |..****
        |****..
        |**.**.
        |*..***
      """.stripMargin)
  }

  test("big fan") {
    Fan.generate(4) should be(
      """****...*
        |.***..**
        |..**.***
        |...*****
        |*****...
        |***.**..
        |**..***.
        |*...****
      """.stripMargin)
  }

  test("huge fan") {
    Fan.generate(5) should be(
      """*****....*
        |.****...**
        |..***..***
        |...**.****
        |....******
        |******....
        |****.**...
        |***..***..
        |**...****.
        |*....*****
      """.stripMargin)
  }

}
