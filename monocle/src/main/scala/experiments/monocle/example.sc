import monocle.Lens
import monocle.function.Cons.headOption

case class Street(number: Int, name: String)
case class Address(city: String, street: Street)
case class Company(name: String, address: Address)
case class Employee(name: String, company: Company)

val street = Street(23, "high street")
val address = Address("london", street)
val company = Company("awesome inc", address)
val employee = Employee("john", company)

val streetNameLens = Lens[Street, String](_.name)(name => _.copy(name = name))
val streetLens = Lens[Address, Street](_.street)(street => _.copy(street = street))
val addressLens = Lens[Company, Address](_.address)(address => _.copy(address = address))
val companyLens = Lens[Employee, Company](_.company)(company => _.copy(company = company))

val lens = companyLens composeLens addressLens composeLens streetLens composeLens streetNameLens
lens.modify(_.capitalize)(employee)

val lens2 = companyLens composeLens addressLens composeLens streetLens composeLens streetNameLens composeOptional headOption
lens2.modify(_.toUpper)(employee)
