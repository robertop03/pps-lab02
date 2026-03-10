package task5

object HelloScala extends App:
    // PART 2 (FUNCTIONS)
    // 3a i
    val f : Int => String = _ match
        case n if n >= 0 => "positive"
        case _ => "negative"


    println(f(10))
    println(f(0))
    println(f(-3))

    // 3a - ii
    def g(n: Int): String = n match
        case n if n >= 0 => "positive"
        case _ => "negative"


    println(g(10))
    println(g(0))
    println(g(-3))

    // 3b
    def neg(predicate: String => Boolean): String => Boolean = s => !predicate(s)
    val empty: String => Boolean = _ == ""
    val notEmpty = neg(empty)

    println(notEmpty("foo")) 
    println(notEmpty(""))
    println(notEmpty("foo") && !notEmpty(""))

    // 4
    val p1: Int => Int => Boolean => String = x => y => z =>
         if (x <= y) == z then "OK"
         else "NOT OK"

    val p2 : (Int, Int, Boolean) => String =
         (x, y, z) => if (x <= y) == z then "OK" else "NOT OK"

    println(p1(2)(3)(true))
    println(p1(5)(3)(true))
    println(p2(3, 4, true))
    println(p2(5, 4, false))
    println(p2(5, 4, true))
    println(p2(3, 4, false))


    def p4(x: Int, y: Int, z: Boolean): String = 
        if (x <= y) == z then "OK"
        else "NOT OK"

    def p3(x: Int)(y: Int)(z: Boolean): String =
        if (x <= y) == z then "OK"
        else "NOT OK"
    
    println(p3(2)(3)(true))
    val f1 = p3(2)
    println(f1(3)(true))
    println(p4(2, 3, true))
    println(p4(5, 3, true))

    // 5
    def compose(f: Int => Int, g: Int => Int): Int => Int =
        x => f(g(x))

    println(compose(_ - 1, _ * 2)(5))

    def composeGen[A, B, C](f: B => C, g: A => B): A => C =
        x => f(g(x))

    // Non serve nessun vincolo particolare: basta che
        // - g vada da A a B e f da B a C
    val q: Int => String = _.toString
    val r: Double => Int = _.toInt
    println(composeGen(q, r)(3.14))


    // PART 3 (RECURSION)
    // 7
    def power(base: Double, exponent: Int): Double = exponent match
        case n if n < 0 => throw new IllegalArgumentException("Esponente negativo non gestito")
        case 0 => 1
        case 1 => base
        case _ => base * power(base, exponent - 1)
    println(power(2, 3))
    println(power(2, 0))
    println(power(3, 1))
    // println(power(4, -1))

    def power2(base: Double, exponent: Int): Double =
        @annotation.tailrec
        def _power(exp: Int, acc: Double): Double = exp match
            case 0 => acc
            case _ => _power(exp - 1, acc * base)
        if(exponent == 0) 1.0
        else _power(exponent, 1.0)

    // 8
    def reverseNumber(n: Int): Int =
        @annotation.tailrec
        def reverse(rimanente: Int, acc: Int): Int = rimanente match
            case 0 => acc
            case _ => 
                val ultimaCifra = rimanente % 10
                val nuovoAcc = (acc * 10) + ultimaCifra
                reverse(rimanente / 10, nuovoAcc)
        reverse(n, 0)
        
    println(reverseNumber(12345))

    // PART 3 (SUM TYPES, PRODUCT TYPES, MODULES)
    enum Expr:
        case Literal(constant: Double)
        case Add(left: Expr, right: Expr)
        case Multiply(left: Expr, right: Expr)
    
    object ExpressionManager:
        def eval(expr: Expr): Double = expr match
            case Expr.Literal(v) => v
            case Expr.Add(l, r) => eval(l) + eval(r)
            case Expr.Multiply(l, r) => eval(l) * eval(r)

        def show(expr: Expr): String = expr match
            case Expr.Literal(v) => v.toString()
            case Expr.Add(l, r) => s"(${show(l)} + ${show(r)})"
            case Expr.Multiply(l, r) => s"(${show(l)} * ${show(r)})"
        