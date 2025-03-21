package task5


object Optionals:
  /**
   * Optional is a type that represents a value that may or may not be present.
   * Similar to Optional in Java but using the ADT concept.
   * Therefore, an Optional is a sum type with two cases: Maybe and Empty.
   * Maybe contains the value, and Empty represents the absence of a value.
   *
   * @tparam A
   */
  enum Optional[A]:
    case Maybe(value: A)
    case Empty()

  object Optional:
    /**
     * isEmpty returns true if the optional is Empty, false otherwise.
     * Example:
     *
     * isEmpty(Empty()) == true
     * isEmpty(Maybe(1)) == false
     *
     * @param optional the optional to check
     * @tparam A the type of the optional
     * @return true if the optional is Empty, false otherwise
     */
    def isEmpty[A](optional: Optional[A]): Boolean = optional match
      case Empty() => true
      case _ => false

    /**
     *
     * getOrElse returns the value of the optional if it is Maybe, otherwise it returns the default value.
     * Example:
     * orElse(Maybe(1), 0) == 1
     * orElse(Empty(), 0) == 0
     *
     * @param optional the optional to get the value from
     * @param default the default value to return if the optional is Empty
     * @tparam A the type of the optional
     * @tparam B the type of the default value
     * @return the value of the optional if it is Maybe, otherwise the default value
     */
    def orElse[A, B >: A](optional: Optional[A], default: B): B = optional match
      case Maybe(value) => value
      case Empty() => default

    /**
     * map applies the function f to the value of the optional if it is Maybe, otherwise it returns Empty.
     * Example:
     *
     * map(Maybe(1), (x: Int) => x + 1) == Maybe(2)
     * map(Empty(), (x: Int) => x + 1) == Empty()
     *
     *
     * @param optional the optional to apply the function to
     * @param f the function to apply to the value of the optional
     * @tparam A the type of the optional
     * @tparam B the type of the result of the function
     * @return the result of applying the function to the value of the optional if it is Maybe, otherwise Empty
     */
    def map[A, B](optional: Optional[A], f: A => B): Optional[B] = optional match
      case Maybe(v) => Maybe(f(v))
      case _ => Empty()

    def curriedMap[A, B](optional: Optional[A])( f: A => B): Optional[B] = map(optional, f)

    /**
     * filter applies the predicate pred to the value of the optional, if it does not satisfy it, or it is Empty,
     * returns Empty, otherwise return the same optional.
     * Example:
     *
     * filter(Maybe(1), (x: Int) => x > 2) == Empty()
     * filter(Maybe(3), (x: Int) => x > 2) == Maybe(3)
     * filter(Empty(), (x: Int) => x > 2) == Empty()
     *
     * @param optional the optional to apply the predicate to
     * @param pred     the predicate to apply to the value of the optional
     * @tparam A the type of the optional
     * @return the same optional received in input if it satisfies the predicate, otherwise Empty
     */
    def filter[A](optional: Optional[A], pred: A => Boolean): Optional[A] = optional match
      case Maybe(v) if pred(v) => Maybe(v)
      case _ => Empty()

    def curriedFilter[A](optional: Optional[A])( pred: A => Boolean): Optional[A] = filter(optional, pred)
