package scala2021.skarasik.task06

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object PersonValidatorV2 {
  def validateGreedy(name: String, age: Int, email: String, sex: Sex.Value, height: Int): Either[String, Person] = {
    for {
      name <- validateName(name)
      age <- validateAge(age)
      email <- validateEmail(email)
      height <- validateHeight(height, sex)
    } yield Person(name, age, email, sex, height)
  }

  def validateAll(name: String, age: Int, email: String, sex: Sex.Value, height: Int)
  : Either[List[String], Person] = {
    val validatedFields = Seq(
      validateName(name),
      validateAge(age),
      validateEmail(email),
      validateHeight(height, sex)
    )

    validatedFields.partitionMap(identity) match {
      case (Nil, _) => Right(Person(name, age, email, sex, height))
      case (errors, _) => Left(errors.toList)
    }
  }

  def validateAllInParallel(name: String, age: Int, email: String, sex: Sex.Value, height: Int)
  : Either[List[String], Person] = {
    val validateFutures = Seq(
      Future { validateName(name) },
      Future { validateAge(age) },
      Future { validateEmail(email) },
      Future { validateHeight(height, sex) }
    )

    val validatedFields = Await.result(Future.sequence(validateFutures), Duration.Inf)

    validatedFields.partitionMap(identity) match {
      case (Nil, _) => Right(Person(name, age, email, sex, height))
      case (errors, _) => Left(errors.toList)
    }
  }

  private def validateName(name: String): Either[String, String] = {
    if (name.isEmpty) {
      Left("Name must not be empty.")
    } else if (!name.matches("^[a-zA-Z]+$")) {
      Left("Name must contain latin letters only.")
    } else {
      Right(name)
    }
  }

  private def validateAge(age: Int): Either[String, Int] = {
    if (0 < age && age < 100) {
      Right(age)
    } else {
      Left("Age must be in range (0, 100).")
    }
  }

  private def validateEmail(email: String): Either[String, String] = {
    val regex = """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"""

    if (email.isEmpty || email.matches(regex)) {
      Right(email)
    } else {
      Left("Invalid email.")
    }
  }

  private def validateHeight(height: Int, sex: Sex.Value): Either[String, Int] = {
    if (sex == Sex.Male && height < 100) {
      Left("Male must be at least 100 cm tall.")
    } else {
      Right(height)
    }
  }
}

object MainV2 {
  def main(args: Array[String]): Unit = {
    val testCases = Seq(
      (("Siamion", 22, "skarasik@exadel.com", Sex.Male, 162), "valid"),
      (("Siamion98", 22, "skarasik@exadel.com", Sex.Male, 162), "error in name"),
      (("Siamion98", 22, "skarasik", Sex.Male, 162), "errors in name and email")
    )

    for (testCase <- testCases) {
      val ((name, age, email, sex, height), caseName) = testCase
      println(s"test case: ${caseName}")

      println("validateGreedy")
      println(PersonValidatorV2.validateGreedy(name, age, email, sex, height))

      println("validateAll")
      println(PersonValidatorV2.validateAll(name, age, email, sex, height))

      println("validateAllInParallel")
      println(PersonValidatorV2.validateAllInParallel(name, age, email, sex, height))

      println()
    }
  }
}
