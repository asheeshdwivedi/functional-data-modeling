package fdm

import _root_.fdm.delete_empty.Currency

/** Sometimes we don't want to take the time to model data precisely. For example, we might want to model an email
  * address with a string, even though most strings are not valid email addresses.
  *
  * In such cases, we can save time by using a smart constructor, which lets us ensure we model only valid data, but
  * without complicated data types.
  */
object cradit_card {

  sealed trait CreditCard {
    def number: String;
  }
  object CraditCard       {

    // 12--18 EXTRA digits, always start with 4
    final case class Visa(visaDigit: VisaDigits) extends CreditCard {
      def number: String = "4" + ""
    }

    type SecurityCode4
    final case class Amex(securityCode: SecurityCode4) extends CreditCard {
      def number: String = ???
    }
  }
  sealed trait VisaDigitsExtra
  object VisaDigitsExtra {
    case object None                                                                       extends VisaDigitsExtra
    final case class One(d1: Digit)                                                        extends VisaDigitsExtra
    final case class Two(d1: Digit, d2: Digit)                                             extends VisaDigitsExtra
    final case class Three(d1: Digit, d2: Digit, d3: Digit)                                extends VisaDigitsExtra
    final case class Four(d1: Digit, d2: Digit, d3: Digit, d4: Digit)                      extends VisaDigitsExtra
    final case class Five(d1: Digit, d2: Digit, d3: Digit, d4: Digit, d5: Digit)           extends VisaDigitsExtra
    final case class Six(d1: Digit, d2: Digit, d3: Digit, d4: Digit, d5: Digit, d6: Digit) extends VisaDigitsExtra
  }
  final case class VisaDigits(
    d1: Digit,
    d2: Digit,
    d3: Digit,
    d4: Digit,
    d5: Digit,
    d6: Digit,
    d7: Digit,
    d8: Digit,
    d9: Digit,
    d10: Digit,
    d11: Digit,
    d12: Digit,
    visaDigitExtra: VisaDigitsExtra
  )
  sealed trait Digit     {
    def toInt: Int
    override def toString(): String = toInt.toString()
  }
  object Digit           {
    case object _1 extends Digit { def toInt: Int = 1 }
    case object _2 extends Digit { def toInt: Int = 2 }
    case object _3 extends Digit { def toInt: Int = 3 }
    case object _4 extends Digit { def toInt: Int = 4 }
    case object _5 extends Digit { def toInt: Int = 5 }
    case object _6 extends Digit { def toInt: Int = 6 }
    case object _7 extends Digit { def toInt: Int = 7 }
    case object _8 extends Digit { def toInt: Int = 8 }
    case object _9 extends Digit { def toInt: Int = 9 }
  }

}
object smart_constructors {
  sealed abstract case class Email private (value: String)
  object Email {
    def fromString(email: String): Option[Email] =
      if (email.matches("""/\w+@\w+.com""")) Some(new Email(email) {}) else None
  }

  /** EXERCISE 1
    *
    * Create a smart constructor for `NonNegative` which ensures the integer is always non-negative.
    */
  sealed abstract case class NonNegative private (value: Int)
  object NonNegative {
    def fromInt(nonNegative: Int): Option[NonNegative] =
      Option.when(nonNegative >= 0)(new NonNegative(nonNegative) {})
  }

  /** EXERCISE 2
    *
    * Create a smart constructor for `Age` that ensures the integer is between 0 and 120.
    */
  sealed abstract case class Age private (value: Int)
  object Age {
    def fromInt(age: Int): Option[Age] = Option.when(age >= 0 && age <= 120)(new Age(age) {})
  }

  /** EXERCISE 3
    *
    * Create a smart constructor for password that ensures some security considerations are met.
    */
  sealed abstract case class Password private (value: String)

  object Password {
    def fromString(password: String): Option[Password] =
      Option.when(password.nonEmpty && password.length() > 0)(new Password(password) {})
  }
}

object applied_smart_constructors {

  /** EXERCISE 1
    *
    * Identify the weaknesses in this data type, and use smart constructors (and possibly other techniques) to correct
    * them.
    */
  trait Balance
  object Balanace {
    sealed abstract class USD private (doller: BigInt, cents: BigInt) extends Balance
  }

  sealed abstract case class AccountId private (id: String)
  sealed abstract case class Name private (name: String)
  final case class BankAccount(id: AccountId, name: Name, balance: Balance, opened: java.time.Instant)

  /** EXERCISE 2
    *
    * Identify the weaknesses in this data type, and use smart constructors (and possibly other techniques) to correct
    * them.
    */
  final case class Age private (vale: Int)         extends AnyVal
  final case class Salary private (salary: Double) extends AnyVal
  final case class Person private (age: Age, name: Name, salary: Salary)

  /** EXERCISE 3
    *
    * Identify the weaknesses in this data type, and use smart constructors (and possibly other techniques) to correct
    * them.
    */
  type Int4Address
  final case class SecurityEvent(machine: Int4Address, timestamp: java.time.Instant, eventType: EventType)
  sealed trait EventType {
    def value: Int
  }
  object EventType       {
    case object PortScanning    extends EventType { override val value: Int = 0 }
    case object DenialOfService extends EventType { override val value: Int = 1 }
    case object InvalidLogin    extends EventType { override val value: Int = 2 }
  }

}
