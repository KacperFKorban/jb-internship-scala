//> using scala 3.5.0
//> using dep "org.scalameta::munit:1.0.2"

/**
TREE = NODE | ID
NODE = ‘(‘ TREE* ‘)’
ID = [a-zA-Z0-9]+
*/
enum Tree:
  case Id(value: String)
  case Node(trees: Seq[Tree])

  /** Equality test between two trees */
  infix def eq(other: Tree): Boolean = (this, other) match
    case (Id(value), Id(otherValue)) => value == otherValue
    case (Node(trees), Node(otherTrees)) => trees.zip(otherTrees).forall((t, ot) => t eq ot)
    case _ => false

  /** Helper constructor for Node */
  def Node(trees: Tree*): Tree.Node = Tree.Node(trees)

  /** Convert the tree to a string in TL format */
  def toTL: String = this match
    case Id(value) => value
    case Node(trees) => s"(${trees.map(_.toTL).mkString(" ")})"

/** Utils for debug tracing */
object trace:
  import Parser.*
  var indent = 0
  val indentSize = 2
  inline val shouldTrace = false
  inline def withTrace[T](msg: String)(op: => T): T =
    log(s"start $msg")
    indent += indentSize
    val result = op
    indent -= indentSize
    log(s"end $msg")
    result
  inline def log(msg: String): Unit =
    if shouldTrace then println(s"${"  " * indent}$msg")

/** Custom Parser monad implementation */
object Parser:
  opaque type Parser[+A] = LazyList[Char] => Option[(A, LazyList[Char])]

  extension [A](parser: Parser[A])
    def run(input: LazyList[Char]): Option[A] = parser(input) match
      case Some((a, rest)) if rest.isEmpty => Some(a)
      case _ => None
    inline def withTrace(msg: String => String): Parser[A] = input => 
      trace.withTrace(msg(input.mkString)) {
        parser(input)
      }
    def map[B](f: A => B): Parser[B] = input => parser(input).map((a, rest) => (f(a), rest))
    def flatMap[B](f: A => Parser[B]): Parser[B] = input => parser(input).flatMap((a, rest) => f(a)(rest))
    def orElse[B >: A](that: Parser[B]): Parser[B] = input => parser(input).orElse(that(input))
    def filter(f: A => Boolean): Parser[A] = input => parser(input).filter((a, _) => f(a))
    def <* [B](that: Parser[B]): Parser[A] = parser.flatMap(a => that.map(_ => a))
    def *> [B](that: Parser[B]): Parser[B] = parser.flatMap(_ => that)
    @scala.annotation.nowarn("msg=non-Unit value")
    def unit: Parser[Unit] = input => parser(input).map((_, input))
    def maybe: Parser[Option[A]] = input => parser(input).map((a, rest) => (Some(a), rest)).orElse(Some(None, input))
    def rep: Parser[Seq[A]] =
      parser.flatMap(a => parser.rep.map(as => a +: as)).orElse(pure(Seq.empty))
    def repAtLeastOnce: Parser[Seq[A]] =
      for
        a <- parser
        as <- parser.rep
      yield a +: as
    def repSep[B](separator: Parser[Unit]): Parser[Seq[A]] =
      parser.flatMap { a =>
        separator.flatMap(_ => parser).rep.map(as => a +: as)
      }.orElse(pure(Seq.empty))

  def pure[A](a: A): Parser[A] = input => Some((a, input))

  def predicate(f: Char => Boolean): Parser[Char] = input => input match
    case h #:: t if f(h) => Some((h, t))
    case _ => None

  def char(c: Char): Parser[Char] = input => input match
    case h #:: t if h == c => Some((c, t))
    case _ => None

  def acceptWhitespace: Parser[Char] =
    predicate(_.isWhitespace)

  def stripWhiteSpaces: Parser[Seq[Char]] =
    acceptWhitespace.rep

  def atLeastOneWhitespace: Parser[Unit] =
    acceptWhitespace.repAtLeastOnce.unit

  def acceptId: Parser[String] =
    predicate(_.isLetter).repAtLeastOnce.map(_.mkString)

object Tree:
  import Parser.*

  private val idParser: Parser[Tree] =
    for
      _ <- Parser.stripWhiteSpaces.withTrace(_ => "stipWhiteSpaces")
      id <- Parser.acceptId.map(Id.apply).withTrace(in => s"idParser($in)")
    yield id

  private val nodeParser: Parser[Tree] =
    (for
      _ <- Parser.stripWhiteSpaces.withTrace(_ => "stipWhiteSpaces")
      _ <- Parser.char('(').withTrace(_ => "char('(')")
      trees <- treeParser.repSep(Parser.stripWhiteSpaces.unit).withTrace(in => s"treeParser($in).repSep")
      _ <- Parser.stripWhiteSpaces.withTrace(_ => "stipWhiteSpaces")
      _ <- Parser.char(')').withTrace(_ => "char(')')")
      _ <- Parser.stripWhiteSpaces.withTrace(_ => "stipWhiteSpaces")
    yield Node(trees)).withTrace(in => s"nodeParser($in)")

  private val treeParser: Parser[Tree] =
    idParser.orElse(nodeParser).withTrace(in => s"treeParser($in)")

  /** Parse a tree from a string */
  def fromTL(in: String): Tree =
    val inStream = in.to(LazyList)
    treeParser.run(inStream)
      .getOrElse(throw new IllegalArgumentException(s"Failed to parse $in"))

  /** Replace a searchTree in the tree with replacement (it does not recursively substitute searchTree inside of the inserted replacement) */
  def replace(tree: Tree, searchTree: Tree, replacement: Tree): Tree = tree match
    case _ if tree eq searchTree => replacement
    case Node(trees) => Node(trees.map(replace(_, searchTree, replacement)))
    case _ => tree

class tests extends munit.FunSuite:
  test("parse id") {
    assertEquals(Tree.fromTL("a"), Tree.Id("a"))
  }

  test("parse node") {
    assertEquals(Tree.fromTL("(a b)"), Tree.Node(Seq(Tree.Id("a"), Tree.Id("b"))))
  }

  test("parse nested node") {
    assertEquals(Tree.fromTL("(a (b c))"), Tree.Node(Seq(Tree.Id("a"), Tree.Node(Seq(Tree.Id("b"), Tree.Id("c"))))))
  }

  test("parse complex tree") {
    assertEquals(Tree.fromTL("(a (b c) d)"), Tree.Node(Seq(Tree.Id("a"), Tree.Node(Seq(Tree.Id("b"), Tree.Id("c"))), Tree.Id("d"))))
  }

  test("parse complex tree with spaces") {
    assertEquals(Tree.fromTL("  (  a  (  b  c  )  d  )  "), Tree.Node(Seq(Tree.Id("a"), Tree.Node(Seq(Tree.Id("b"), Tree.Id("c"))), Tree.Id("d"))))
  }

  test("parse complex tree with spaces and newlines") {
    assertEquals(Tree.fromTL("  (  a  (  b  c  )  d  )  \n"), Tree.Node(Seq(Tree.Id("a"), Tree.Node(Seq(Tree.Id("b"), Tree.Id("c"))), Tree.Id("d"))))
  }

  test("parse complex tree with spaces and newlines and tabs") {
    assertEquals(Tree.fromTL("  (  a  (  b  c  )  d  )  \n\t"), Tree.Node(Seq(Tree.Id("a"), Tree.Node(Seq(Tree.Id("b"), Tree.Id("c"))), Tree.Id("d"))))
  }

  test("parse complex tree with spaces and newlines and tabs and carriage returns") {
    assertEquals(Tree.fromTL("  (  a  (  b  c  )  d  )  \n\t\r"), Tree.Node(Seq(Tree.Id("a"), Tree.Node(Seq(Tree.Id("b"), Tree.Id("c"))), Tree.Id("d"))))
  }

  test("parse a very whitespace minimal tree") {
    assertEquals(Tree.fromTL("(a(b c)d)"), Tree.Node(Seq(Tree.Id("a"), Tree.Node(Seq(Tree.Id("b"), Tree.Id("c"))), Tree.Id("d")))
    )
  }

  test("fromTL . toTL == id for id") {
    val id = Tree.Id("a")
    assertEquals(Tree.fromTL(id.toTL), id)
  }

  test("fromTL . toTL == id for node") {
    val node = Tree.Node(Seq(Tree.Id("a"), Tree.Id("b")))
    assertEquals(Tree.fromTL(node.toTL), node)
  }

  test("fromTL . toTL == id for nested node") {
    val node = Tree.Node(Seq(Tree.Id("a"), Tree.Node(Seq(Tree.Id("b"), Tree.Id("c"))))
    )
    assertEquals(Tree.fromTL(node.toTL), node)
  }

  test("fromTL . toTL == id for complex tree") {
    val node = Tree.Node(Seq(Tree.Id("a"), Tree.Node(Seq(Tree.Id("b"), Tree.Id("c"))), Tree.Id("d")))
    assertEquals(Tree.fromTL(node.toTL), node)
  }

  test("fromTL . toTL == id for complex tree with spaces") {
    val node = Tree.Node(Seq(Tree.Id("a"), Tree.Node(Seq(Tree.Id("b"), Tree.Id("c"))), Tree.Id("d")))
    assertEquals(Tree.fromTL(node.toTL), node)
  }

  test("replace id in id") {
    val tree = Tree.Id("a")
    val searchTree = Tree.Id("a")
    val replacement = Tree.Id("b")
    assertEquals(Tree.replace(tree, searchTree, replacement), replacement)
  }

  test("replace id in node") {
    val tree = Tree.Node(Seq(Tree.Id("a"), Tree.Id("b")))
    val searchTree = Tree.Id("a")
    val replacement = Tree.Id("c")
    assertEquals(Tree.replace(tree, searchTree, replacement), Tree.Node(Seq(Tree.Id("c"), Tree.Id("b")))
    )
  }

  test("replace node in node") {
    val tree = Tree.Node(Seq(Tree.Id("a"), Tree.Node(Seq(Tree.Id("b"), Tree.Id("c")))))
    val searchTree = Tree.Node(Seq(Tree.Id("b"), Tree.Id("c")))
    val replacement = Tree.Id("d")
    assertEquals(Tree.replace(tree, searchTree, replacement), Tree.Node(Seq(Tree.Id("a"), Tree.Id("d")))
    )
  }

  test("replace node in node with multiple occurrences") {
    val tree = Tree.Node(Seq(Tree.Id("a"), Tree.Node(Seq(Tree.Id("b"), Tree.Id("c"))), Tree.Node(Seq(Tree.Id("b"), Tree.Id("c")))))
    val searchTree = Tree.Node(Seq(Tree.Id("b"), Tree.Id("c"))
    )
    val replacement = Tree.Id("d")
    assertEquals(Tree.replace(tree, searchTree, replacement), Tree.Node(Seq(Tree.Id("a"), Tree.Id("d"), Tree.Id("d"))))
  }

  test("replace node in node with multiple occurrences and nested nodes") {
    val tree = Tree.Node(Seq(Tree.Id("a"), Tree.Node(Seq(Tree.Id("b"), Tree.Id("c"))), Tree.Node(Seq(Tree.Id("b"), Tree.Id("c")))))
    val searchTree = Tree.Node(Seq(Tree.Id("b"), Tree.Id("c")))
    val replacement = Tree.Node(Seq(Tree.Id("d"), Tree.Id("e")))
    assertEquals(Tree.replace(tree, searchTree, replacement), Tree.Node(Seq(Tree.Id("a"), Tree.Node(Seq(Tree.Id("d"), Tree.Id("e"))), Tree.Node(Seq(Tree.Id("d"), Tree.Id("e"))))))
  }
