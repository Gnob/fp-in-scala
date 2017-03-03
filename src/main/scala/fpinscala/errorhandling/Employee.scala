package fpinscala.errorhandling

/**
  * Employee
  *
  * @author Gnob
  * @since 2017. 03. 02.
  */
case class Employee(name: String, department: String)

object Employees {
  val employeePool: Seq[Employee] = Seq(Employee("Joe", "Dev"), Employee("Chris", "Ops"))

  def lookupByName(name: String): Option[Employee] =
    if (name == employeePool.head.name) Some(employeePool.head)
    else if (name == employeePool.last.name) Some(employeePool.last)
    else None
}
