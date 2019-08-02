package experiments.cats.validation

import cats.data.ValidatedNec
import cats.syntax.contravariantSemigroupal._
import cats.syntax.validated._

trait NecFormValidator {

  type ValidationResult[A] = ValidatedNec[DomainValidation, A]

  def validateUserName(userName: String): ValidationResult[String] = {
    if (userName matches "^[a-zA-Z0-9]+$") userName.validNec
    else UsernameHasSpecialCharacters.invalidNec
  }

  def validatePassword(password: String): ValidationResult[String] = {
    if (password matches "(?=^.{10,}$)((?=.*\\d)|(?=.*\\W+))(?![.\\n])(?=.*[A-Z])(?=.*[a-z]).*$") password.validNec
    else PasswordDoesNotMeetCriteria.invalidNec
  }

  def validateFirstName(firstName: String): ValidationResult[String] = {
    if (firstName matches "^[a-zA-Z]+$") firstName.validNec
    else FirstNameHasSpecialCharacters.invalidNec
  }

  def validateLastName(lastName: String): ValidationResult[String] = {
    if (lastName matches "^[a-zA-Z]+$") lastName.validNec
    else LastNameHasSpecialCharacters.invalidNec
  }

  def validateAge(age: Int): ValidationResult[Int] = {
    if (age >= 18 && age <= 75) age.validNec
    else AgeIsInvalid.invalidNec
  }

  def validateForm(username: String, password: String, firstName: String, lastName: String,
                   age: Int): ValidationResult[RegistrationData] = {
    (validateUserName(username),
      validatePassword(password),
      validateFirstName(firstName),
      validateLastName(lastName),
      validateAge(age)).mapN(RegistrationData)
  }
}

object NecFormValidator extends NecFormValidator

object NexFormValidatorTest extends App {
  println {
    NecFormValidator.validateForm(
      username = "Joe",
      password = "Passw0r$1234",
      firstName = "John",
      lastName = "Doe",
      age = 21
    ).leftMap(_.toNonEmptyList.toList).toEither
  }

  println {
    NecFormValidator.validateForm(
      username = "Joe%%%",
      password = "password",
      firstName = "John",
      lastName = "Doe",
      age = 21
    ).leftMap(_.toNonEmptyList.toList).toEither
  }
}
