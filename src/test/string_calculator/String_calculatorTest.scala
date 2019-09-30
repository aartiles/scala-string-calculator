package string_calculator

import org.scalatest._
import org.scalatest.Matchers._

final class String_calculatorTest extends WordSpec with GivenWhenThen {
  "String_calculator" should {
    "add" in {
      Given("a String_calculator")

      val string_calculator = new String_calculator

      When("numbers is an empty string")

      val numbers = ""
      val result = string_calculator.add(numbers)

      Then("it should return 0")

      result shouldBe "0"
    }

    "add a single number" in {
      Given("a String_calculator")

      val string_calculator = new String_calculator

      When("numbers is a single number")

      val numbers = "3"
      val result = string_calculator.add(numbers)

      Then("it should return the same number")

      result shouldBe "3.0"
    }

    "add a two numbers" in {
      Given("a String_calculator")

      val string_calculator = new String_calculator

      When("numbers has two numbers")

      val numbers = "3,2"
      val result = string_calculator.add(numbers)

      Then("it should return the sum of both numbers")

      result shouldBe "5.0"
    }

    "add a number and empty" in {
      Given("a String_calculator")

      val string_calculator = new String_calculator

      When("numbers has number and empty")

      val numbers = "3,"
      val result = string_calculator.add(numbers)

      Then("it should return the sum of the number and zero")

      result shouldBe "3.0"
    }

    "add unlimited numbers" in {
      Given("a String_calculator")

      val string_calculator = new String_calculator

      When("numbers has many numbers")

      val numbers = "3,5.2,,4,2.1"
      val result = string_calculator.add(numbers)

      Then("it should return the sum of all the numbers")

      result shouldBe "14.3"
    }

    "add accept \n as numbers separator" in {
      Given("a String_calculator")

      val string_calculator = new String_calculator

      When("numbers separated by \n")

      val numbers = "3,5.2,,4\n2.1"
      val result = string_calculator.add(numbers)

      Then("it should return the sum of all the number")

      result shouldBe "14.3"
    }

    "add identifies separator at invalid position" in {
      Given("a String_calculator")

      val string_calculator = new String_calculator

      When("numbers separated by \n")

      val numbers = "3,5.2,,4\n2.1"
      val result = string_calculator.add(numbers)

      Then("it should return the sum of all the number")

      result shouldBe "14.3"
    }

  }
}