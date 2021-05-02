package scala2021.skarasik.task05

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}

import scala.concurrent.ExecutionContext.Implicits.global

class CompanyDBSuite extends AnyFunSuite with TableDrivenPropertyChecks {
  val employees = List(
    Employee(1, "Steve", 1),
    Employee(3, "Mark", 1),
    Employee(4, "Jane", 1),
    Employee(7, "Samuel", 2),
    Employee(10, "Igor", 2),
    Employee(11, "Naveen", 4),
    Employee(12, "Christy", 5),
    Employee(15, "Megan", 3)
  )

  val departments = List(
    Department(1, "Marketing"),
    Department(2, "Sales"),
    Department(3, "Research"),
    Department(4, "IT"),
  )

  val managers = List(
    Manager("Marketing", 1),
    Manager("Sales", 10),
    Manager("IT", 14),
  )

  val findManagerOrErrorCases: TableFor2[String, Either[String, String]] = Table(
    ("employeeName", "ManagerNameOrError"),
    ("John", Left("There is no employee with name = John.")),
    ("Steve", Left("Employee with name = Steve is already a manager of its department.")),
    ("Mark", Right("Steve")),
    ("Igor", Left("Employee with name = Igor is already a manager of its department.")),
    ("Christy", Left("There is no department with id = 5.")),
    ("Naveen", Left("There is no manager employee with id = 14.")),
    ("Megan", Left("There is no manager with department name = Research.")),
    ("Samuel", Right("Igor")),
  )

  val db: CompanyDB = CompanyDB(employees, departments, managers)

  test("findManager") {
    forAll(findManagerOrErrorCases) {
      (employeeName, expectedManagerName) => {
        assert(db.findManagerName(employeeName) == expectedManagerName.toOption)
      }
    }
  }

  test("findManagerOrError") {
    forAll(findManagerOrErrorCases) {
      (employeeName, expectedManagerNameOrError) => {
        assert(db.findManagerNameOrError(employeeName) == expectedManagerNameOrError)
      }
    }
  }

  test("findManagerOrErrorAsync") {
    forAll(findManagerOrErrorCases) {
      (employeeName, expectedManagerNameOrError) => {
        db.findManagerNameOrErrorAsync(employeeName).map(result => result == expectedManagerNameOrError)
      }
    }
  }

  test("findEmployeeManagers") {
    val placeholder = "Not Found"

    val expectedInfos = Seq(
      Info("Steve", "Marketing", placeholder),
      Info("Mark", "Marketing", "Steve"),
      Info("Jane", "Marketing", "Steve"),
      Info("Samuel", "Sales", "Igor"),
      Info("Igor", "Sales", placeholder),
      Info("Naveen", "IT", placeholder),
      Info("Christy", placeholder, placeholder),
      Info("Megan", "Research", placeholder)
    )

    assert(db.findEmployeeManagers == expectedInfos)
  }
}
