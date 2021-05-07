package scala2021.skarasik.task06

import scala.collection.parallel.CollectionConverters._

object Sex extends Enumeration {
  val Male, Female = Value
}

case class Person(name: String, age: Int, email: String, sex: Sex.Value, height: Double)

object PersonValidator {
  def validateGreedy(person: Person): Option[String] = {
    validators.view.flatMap(validator => validator(person)).headOption
  }

  def validateAll(person: Person): List[String] = {
    validators
      .flatMap(validator => validator(person))
  }

  def validateAllInParallel(person: Person): List[String] = {
    validators
      .par
      .flatMap(validator => validator(person))
      .toList
  }

  private val validators: List[Person => Option[String]] = List(
    validateName,
    validateAge,
    validateEmail,
    validateHeight
  )

  private def validateName(person: Person): Option[String] = {
//    println("validateName is called")
    person.name match {
      case name if name.isEmpty => Option("Name must not be empty.")
      case name if !name.matches("^[a-zA-Z]+$") => Option("Name must contain latin letters only.")
      case _ => None
    }
  }

  private def validateAge(person: Person): Option[String] = {
//    println("validateAge is called")
    person.age match {
      case age if 1 to 99 contains age => None
      case _ => Option("Age must be in range [1, 99].")
    }
  }

  private def validateEmail(person: Person): Option[String] = {
//    println("validateEmail is called")

    val regex = """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r

    person.email match {
      case email if email.isEmpty => None
      case email if regex.matches(email) => None
      case _ => Option("Invalid email.")
    }
  }

  private def validateHeight(person: Person): Option[String] = {
//    println("validateHeight is called")

    person match {
      case Person(_, _, _, sex, height) if sex == Sex.Male && height < 100 =>
        Option("Male must be at least 100 cm tall.")
      case _ => None
    }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val testCases = Seq(
      (Person("Siamion", 22, "skarasik@exadel.com", Sex.Male, 162), "valid"),
      (Person("Siamion98", 22, "skarasik@exadel.com", Sex.Male, 162), "error in name"),
      (Person("Siamion98", 22, "skarasik", Sex.Male, 162), "errors in name and email")
    )

    for (testCase <- testCases) {
      val (person, caseName) = testCase
      println(s"test case: ${caseName}")

      println("validateGreedy")
      println(PersonValidator.validateGreedy(person))

      println("validateAll")
      println(PersonValidator.validateAll(person))

      println("validateAllInParallel")
      println(PersonValidator.validateAllInParallel(person))

      println()
    }
  }
}
