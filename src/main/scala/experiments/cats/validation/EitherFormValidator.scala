package experiments.cats.validation.form

import cats.syntax.either._

trait EitherFormValidator {

  type Validation[A] = Either[DomainValidation, A]

  def validateUsername(username: String): Validation[String] = {
    Either.cond(
      username matches "^[a-zA-Z0-9]+$",
      username,
      UsernameHasSpecialCharacters
    )
  }

  def validatePassword(password: String): Validation[String] = {
    Either.cond(
      password matches "(?=^.{10,}$)((?=.*\\d)|(?=.*\\W+))(?![.\\n])(?=.*[A-Z])(?=.*[a-z]).*$",
      password,
      PasswordDoesNotMeetCriteria
    )
  }

  def validateFirstname(firstName: String): Validation[String] = {
    Either.cond(
      firstName matches "^[a-zA-Z]+$",
      firstName,
      FirstNameHasSpecialCharacters
    )
  }

  def validateLastname(lastName: String): Validation[String] = {
    Either.cond(
      lastName matches "^[a-zA-Z]+$",
      lastName,
      LastNameHasSpecialCharacters
    )
  }

  def validateAge(age: Int): Validation[Int] = {
    Either.cond(
      age >= 18 && age <= 75,
      age,
      AgeIsInvalid
    )
  }

  def validateForm(username: String,
                   password: String,
                   firstName: String,
                   lastName: String,
                   age: Int): Validation[RegistrationData] = {
    // this only shows the first error, as a for-comprehension is fail-fast
    for {
      validatedUserName <- validateUsername(username)
      validatedPassword <- validatePassword(password)
      validatedFirstName <- validateFirstname(firstName)
      validatedLastName <- validateLastname(lastName)
      validatedAge <- validateAge(age)
    } yield RegistrationData(validatedUserName, validatedPassword, validatedFirstName, validatedLastName, validatedAge)
  }
}

object EitherFormValidator extends EitherFormValidator

object EitherFormValidatorTest extends App {
  println {
    EitherFormValidator.validateForm(
      username = "fakeUs3rname",
      password = "password",
      firstName = "John",
      lastName = "Doe",
      age = 15
    )
  }
}
