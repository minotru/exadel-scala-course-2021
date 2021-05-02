package scala2021.skarasik.task05

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class Employee(id: Int, name: String, departmentId: Int)

case class Department(id: Int, name: String)

case class Manager(departmentName: String, employeeId: Int)

case class Info(employeeName: String, departmentName: String, managerName: String)

private case class DBEntry(
  employeeOrError: Either[String, Employee],
  departmentOrError: Either[String, Department],
  managerOrError: Either[String, Manager],
  managerEmployeeOrError: Either[String, Employee]
)

case class CompanyDB(
   var employees: Seq[Employee],
   var departments: Seq[Department],
   var managers: Seq[Manager]
) {
  def findManagerName(employeeName: String): Option[String] = {
    findManagerNameOrError(employeeName).toOption
  }

  def findManagerNameOrError(employeeName: String): Either[String, String] = {
    val entry = joinEmployeeInfo(employeeName)

    for {
      employee <- entry.employeeOrError
      department <- entry.departmentOrError
      manager <- entry.managerOrError
      managerEmployee <- entry.managerEmployeeOrError
    } yield managerEmployee.name
  }

  def findManagerNameOrErrorAsync(employeeName: String): Future[Either[String, String]] = {
    Future { findManagerNameOrError(employeeName) }
  }

  def findEmployeeManagers: List[Info] = {
    val placeholder = "Not Found"

    employees.map(employee => {
      val entry = joinEmployeeInfo(employee.name)
      Info(
        employeeName = employee.name,
        departmentName = entry.departmentOrError.fold(x => placeholder, _.name),
        managerName = entry.managerEmployeeOrError.fold(x => placeholder, _.name)
      )
    }).toList
  }

  private [this] def joinEmployeeInfo(employeeName: String): DBEntry = {
    val employeeOrError = employees.find(_.name == employeeName) match {
      case Some(employee) => Right(employee)
      case None => Left(s"There is no employee with name = ${employeeName}.")
    }

    val departmentOrError = employeeOrError.flatMap(employee => {
      departments.find(_.id == employee.departmentId) match {
        case Some(department) => Right(department)
        case None => Left(s"There is no department with id = ${employee.departmentId}.")
      }
    })

    val managerOrError = departmentOrError.flatMap(department => {
      managers.find(_.departmentName == department.name) match {
        case Some(manager) => Right(manager)
        case None => Left(s"There is no manager with department name = ${department.name}.")
      }
    })

    val managerEmployeeOrError = managerOrError.flatMap(manager => {
      employees.find(_.id == manager.employeeId) match {
        case Some(managerEmployee) =>
          if (managerEmployee.name == employeeName) {
            Left(s"Employee with name = ${employeeName} is already a manager of its department.")
          } else {
            Right(managerEmployee)
          }
        case None => Left(s"There is no manager employee with id = ${manager.employeeId}.")
      }
    })

    DBEntry(employeeOrError, departmentOrError, managerOrError, managerEmployeeOrError)
  }
}
