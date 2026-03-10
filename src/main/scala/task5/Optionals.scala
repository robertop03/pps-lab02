package task5

// overall module
object Optionals:

  // type "public" definition, exposing structure
  enum OptionalInt:
    case Just(value: Int)
    case Empty()

  // operations (/algorithms)
  object OptionalInt:

    def isEmpty(opt: OptionalInt): Boolean = opt match
      case Empty() => true
      case _       => false

    def orElse(opt: OptionalInt, orElse: Int): Int = opt match
      case Just(a) => a
      case _       => orElse

    // PART 5 (MORE FUNCTIONAL COMBINATORS)
    def mapInt(input: OptionalInt)(predicate: OptionalInt => OptionalInt):  OptionalInt = input match
      case Empty() => input
      case _  => predicate(input)

    def filter(input: OptionalInt)(predicate: Int => Boolean):  OptionalInt = input match
      case Empty() => input
      case Just(x) => if(predicate(x)) then Just(x) else Empty()

    

@main def tryOptionals(): Unit =
  import Optionals.* // to work with Optionals (to see OptionalInt type)
  import OptionalInt.* // to directly access algorithms

  val s1: OptionalInt = Just(1)
  val s2: OptionalInt = Empty()

  println(s1) // Some(1)
  println(isEmpty(s1)) // false
  println(orElse(s1, 0)) // 1
  println(orElse(s2, 0)) // 0
