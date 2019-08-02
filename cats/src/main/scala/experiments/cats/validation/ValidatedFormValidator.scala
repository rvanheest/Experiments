package experiments.cats.validation

import cats.data.Validated
import cats.syntax.either._

trait ValidatedFormValidator {

  type Validation[A] = Validated[DomainValidation, A]

  def validateUserName(userName: String): Validation[String] = {
    EitherFormValidator.validateUsername(userName).toValidated
  }

  def validatePassword(password: String): Validation[String] = {
    EitherFormValidator.validatePassword(password).toValidated
  }

  def validateFirstName(firstName: String): Validation[String] = {
    EitherFormValidator.validateFirstname(firstName).toValidated
  }

  def validateLastName(lastName: String): Validation[String] = {
    EitherFormValidator.validateLastname(lastName).toValidated
  }

  def validateAge(age: Int): Validation[Int] = {
    EitherFormValidator.validateAge(age).toValidated
  }

  // doesn't work since Validated is NOT a monad; it's an applicative functor
//  def validateForm(username: String,
//                   password: String,
//                   firstName: String,
//                   lastName: String,
//                   age: Int): Validation[RegistrationData] = {
//    for {
//      validatedUserName <- validateUserName(username)
//      validatedPassword <- validatePassword(password)
//      validatedFirstName <- validateFirstName(firstName)
//      validatedLastName <- validateLastName(lastName)
//      validatedAge <- validateAge(age)
//    } yield RegistrationData(validatedUserName, validatedPassword, validatedFirstName, validatedLastName, validatedAge)
//  }
}

object ValidatedFormValidator extends ValidatedFormValidator
