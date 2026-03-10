package task5

import org.junit.*
import org.junit.Assert.*
import Optionals.*

class OptionalIntTest:
  @Test def emptyOptionalShouldBeEmpty(): Unit =
    val empty = OptionalInt.Empty()
    assertTrue(OptionalInt.isEmpty(empty))

  @Test def nonEmptyOptionalShouldNotBeEmpty(): Unit =
    val nonEmpty = OptionalInt.Just(0)
    assertFalse(OptionalInt.isEmpty(nonEmpty))

  @Test def orElseShouldReturnDefaultWhenEmpty(): Unit =
    val nonEmpty = OptionalInt.Just(0)
    assertEquals(0, OptionalInt.orElse(nonEmpty, 1))

  @Test def orElseShouldReturnValueWhenNonEmpty(): Unit =
    val empty = OptionalInt.Empty()
    assertEquals(1, OptionalInt.orElse(empty, 1))

  // PART 5 (MORE FUNCTIONAL COMBINATORS)
  @Test def mapIntShouldReturnEmptyWhenEmpty(): Unit = 
    val empty = OptionalInt.Empty()
    val result = OptionalInt.mapInt(empty)(_ => OptionalInt.Just(10))
    assertTrue(OptionalInt.isEmpty(result))

  @Test def mapIntShouldComputePredicate(): Unit = 
     val nonEmpty = OptionalInt.Just(5)
     val predicate: OptionalInt => OptionalInt =
        case OptionalInt.Just(x) => OptionalInt.Just(x + 2)
        case OptionalInt.Empty() => OptionalInt.Empty()

     val result = OptionalInt.mapInt(nonEmpty)(predicate)
     assertEquals(OptionalInt.Just(7), result)

  @Test def filterShouldReturnEmptyWhenEmpty(): Unit = 
    val empty = OptionalInt.Empty()
    val result = OptionalInt.filter(empty)(_ => true)
    assertTrue(OptionalInt.isEmpty(result))
    
  @Test def filterShouldReturnEmptyWhenNotPredicate(): Unit = 
    val nonEmpty = OptionalInt.Just(5)
    val result = OptionalInt.filter(nonEmpty)(_ > 8)
    assertTrue(OptionalInt.isEmpty(result))

  @Test def filterShouldReturnValueWhenPredicateOk(): Unit = 
    val nonEmpty = OptionalInt.Just(5)
    val result = OptionalInt.filter(nonEmpty)(_ > 3)
    assertEquals(OptionalInt.Just(5), result)

