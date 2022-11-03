package fdm

object phantom_types {

  /** EXERCISE 1
    *
    * Add a phantom type parameter to `Socket`, which can keep track of the state of the socket: either `Created` or
    * `Connected`. Use this type parameter in the methods below to improve their type safety.
    */
  type Created
  type Connected
  trait Socket[State]

  def createSocket(): Socket[Created]                                            = ???
  def connectSocket(address: String, socket: Socket[Created]): Socket[Connected] = ???
  def readSocket(socket: Socket[Connected]): Array[Byte]                         = ???

  /** EXERCISE 2
    *
    * Introduce a type parameter to this data type to model whether a `Path` is a file, a directory, or either a file or
    * a directory. Use this to improve the type safety of the `readFile` and `listDirectory` methods.
    *
    * Note: In order to ensure safety, you will have to make the constructors of `Path` private, so that outside code
    * cannot call those constructors with just any type parameter. This is a requirement of using phantom types
    * properly.
    */
  type File
  type Directory
  type Unknown
  sealed trait Path[A] { self =>

    def /(name: String)(implicit ev: A <:< Directory): Path[Unknown] = Path.ChildOf(self.widen[Directory], name)

    def widen[Parent](implicit ev: A <:< Parent): Path[Parent] = self.asInstanceOf[Path[Parent]]

  }
  object Path {
    case object Root                                                 extends Path[Directory]
    final case class ChildOf[T](path: Path[Directory], name: String) extends Path[T]
  }

  def readFile(path: Path[File]): String                        = ???
  def listDirectory(path: Path[Directory]): List[Path[Unknown]] = ???

  /** EXERCISE 3
    *
    * Phantom types work well with intersection types (`with` in Scala 2.x). They have many wide-ranging applications,
    * including making builder patterns safer.
    *
    * Introduce a phantom type parameter for `PersonBuilder`, and arrange such that the setters add a new type into a
    * type intersection, and that the build method requires both age and name to be set in order to build the person.
    *
    * Note: As before, you must make the constructors of the data type with a phantom type parameter private, so they
    * cannot be called from outside code.
    */
  trait Openable
  trait Closeable
  type Resource = Openable with Closeable
  trait Sockets extends Openable with Closeable
  val soc: Resource = new Sockets {}

  type Age
  type Name
  type Address

  final case class Person(name: String, age: Int, address: List[String])

  final case class PersonBuilder[+Set] private (age: Option[Int], name: Option[String], address: Option[List[String]]) {

    def address(list: List[String]): PersonBuilder[Set with Address] = copy(address = Some(list))

    def age(v: Int): PersonBuilder[Set with Age] = copy(age = Some(v))

    def name(s: String): PersonBuilder[Set with Name] = copy(name = Some(s))

    def build(implicit ev: Set <:< Name with Age): Person =
      Person(name.get, age.get, address.getOrElse(Nil))

  }
  object PersonBuilder {
    val empty: PersonBuilder[Any] = PersonBuilder(None, None, None)
  }

  val x = PersonBuilder.empty
    .name("Asheesh")
    .age(40)
    .build
}
