package task5

import org.junit.*
import org.junit.Assert.*
import Optionals.*

class OptionalTest:
  @Test def emptyOptionalShouldBeEmpty(): Unit =
    val empty = Optional.Empty()
    assertTrue(Optional.isEmpty(empty))

  @Test def nonEmptyOptionalShouldNotBeEmpty(): Unit =
    val nonEmpty = Optional.Maybe(0)
    assertFalse(Optional.isEmpty(nonEmpty))

  @Test def orElseShouldReturnDefaultWhenEmpty(): Unit =
    val nonEmpty = Optional.Maybe(0)
    assertEquals(0, Optional.orElse(nonEmpty, 1))

  @Test def orElseShouldReturnValueWhenNonEmpty(): Unit =
    val empty = Optional.Empty()
    assertEquals(1, Optional.orElse(empty, 1))

  /** Task 5 -- Look the behaviour of map operator */
  @Test def mapShouldReturnEmptyWhenEmpty(): Unit =
    val empty: Optional[Int] = Optional.Empty()
    val result = Optional.map(empty, _ + 1)
    assertTrue(Optional.isEmpty(result))

  @Test def mapShouldReturnTransformedValueWhenNonEmpty(): Unit =
    val nonEmpty = Optional.Maybe(0)
    val result = Optional.map(nonEmpty, _ + 1)
    assertEquals(1, Optional.orElse(result, 1))

  @Test def curriedMapShouldReturnEmptyWhenEmpty(): Unit =
    val empty: Optional[Int] = Optional.Empty()
    val intPredicate: Int => Boolean = _ > 2
    val result = Optional.curriedMap(empty)(intPredicate)
    assertTrue(Optional.isEmpty(result))

  @Test def curriedMapShouldReturnTransformedValueWhenNonEmpty(): Unit =
    val nonEmpty = Optional.Maybe(5)
    val maybeTrue = Optional.Maybe(true)
    val intPredicate: Int => Boolean = _ > 2
    assertEquals(maybeTrue, Optional.curriedMap(nonEmpty)(intPredicate))

  @Test def filterShouldReturnEmptyWhenEmpty(): Unit =
    val empty: Optional[Int] = Optional.Empty()
    val intPredicate: Int => Boolean = _ > 2
    val result = Optional.filter(empty, intPredicate)
    assertTrue(Optional.isEmpty(result))

  @Test def filterShouldReturnEmptyWhenPredicateNotSatisfied(): Unit =
    val nonEmpty = Optional.Maybe(0)
    val intPredicate: Int => Boolean = _ > 2
    val result = Optional.filter(nonEmpty, intPredicate)
    assertTrue(Optional.isEmpty(result))

  @Test def filterShouldReturnSameValueWhenPredicateSatisfied(): Unit =
    val nonEmpty = Optional.Maybe(5)
    val intPredicate: Int => Boolean = _ > 2
    val result = Optional.filter(nonEmpty, intPredicate)
    assertEquals(nonEmpty, result)

  @Test def curriedFilterShouldReturnEmptyWhenEmpty(): Unit =
    val empty: Optional[Int] = Optional.Empty()
    val intPredicate: Int => Boolean = _ > 2
    val result = Optional.curriedFilter(empty)(intPredicate)
    assertTrue(Optional.isEmpty(result))

  @Test def curriedFilterShouldReturnEmptyWhenPredicateNotSatisfied(): Unit =
    val nonEmpty = Optional.Maybe(0)
    val intPredicate: Int => Boolean = _ > 2
    val result = Optional.curriedFilter(nonEmpty)(intPredicate)
    assertTrue(Optional.isEmpty(result))

  @Test def curriedFilterShouldReturnSameValueWhenPredicateSatisfied(): Unit =
    val nonEmpty = Optional.Maybe(5)
    val intPredicate: Int => Boolean = _ > 2
    val result = Optional.curriedFilter(nonEmpty)(intPredicate)
    assertEquals(nonEmpty, result)
