package codeletgen.skeleton

// This is my Complex class, it takes a T that needs an Operatiosn[T] implementation:
class Complex[T] (var r: T, val i: T)(implicit o: Operations[T]) {      
  import OperationsSyntax._

  def +(b: Complex[T]): Complex[T] = new Complex(o.add(r, b.r), o.add(i, b.i))
  def *(b: Complex[T]): Complex[T] = new Complex(o.mult(r, b.r), o.mult(i, b.i))

  override def toString(): String = "(" + r + ", " + i +"i)"
}


// The type class with the operations in our language.
// Add here operations that will need to exist in the IR.
sealed trait Operations[T] {
  def const(c: Int): T
  def load(i: Int): T
  def add(a: T, b: T): T
  def mult(a: T, b: T): T
}

object OperationsInstances {
  // The semantics for direct execution of our DSL programs.

  // Note the environment, which is used to provide the values for loads.
  def IntOps(env: Int => Int): Operations[Int] = new Operations[Int] {
    def add(a: Int, b: Int): Int = a + b
    def mult(a: Int, b: Int): Int = a * b
    def const(c: Int): Int = c
    def load(i: Int): Int = env(i)
  }

  // We are renaming String to Print just to denote this type
  // denotes the Printing semantics
  type Print = String

  def PrintOps(env: Int => Print) = new Operations[Print] {
    def add(a: Print, b: Print): Print = s"($a + $b)"
    def const(c: Int): Print = c.toString
    def mult(a: Print, b: Print): Print = s"($a * $b)"
    def load(i: Int): Print = env(i)
  }

  // The Ops instance for Complex type. This is also parameterized
  // to support different type of Complex, it in turns takes an Operations[T]
  // to delegate the actual operations on its components to it.
  def ComplexOps[T](implicit o: Operations[T]) = new Operations[Complex[T]] {
    def add(a: Complex[T], b: Complex[T]): Complex[T] = a + b
    def const(c: Int): Complex[T] = new Complex(o.const(c), o.const(0))
    def mult(a: Complex[T], b: Complex[T]): Complex[T] = a * b
    def load(i: Int): Complex[T] = new Complex(o.load(i), o.const(0))
  }

  def ComplexInt(env: Int => Int) = ComplexOps[Int](IntOps(env))

  def ComplexPrint(env: Int => Print) = ComplexOps[Print](PrintOps(env))
}

// The type class interface (Interface syntax, see Scala with Cats, Chapter 1)
object OperationsSyntax {

  implicit class SyntaxOperations[T](a: T) {
    def +(b: T)(implicit o: Operations[T]): T = o.add(a, b)
    def *(b: T)(implicit o: Operations[T]): T = o.mult(a, b)
  }

  implicit class SyntaxConstantOperations(c: Int) {
    def const[T](implicit o: Operations[T]): T = o.const(c)
    def load[T](implicit o: Operations[T]): T = o.load(c)
  }
}


class SimpleExamples[T](implicit o: Operations[T]) {
  import OperationsSyntax._

  // here are little programs in our DSL.  The semantics
  // is specified by the type parameter `T`.  Here, it can
  // be anything since `T` is not bound here.

  def expr1: T = 1.const + 2.const * 3.const

  def expr2: T = expr2(1.const, 2.const)

  private def expr2(x: T, y: T): T = x + y * x

  def pow2n : T = pow(2.const, 5)

  // The `pow` functions is staged (`n` is a compile-time value,
  // and `x` is a run-time value.
  private def pow(x : T, n : Int) : T = {
    if (n == 1)
      x
    else
      x * pow(x, n-1)
  }
  // an example to show how we are printing loads.
  def load : T = {
    val tmp1 = 1.load + 2.load
    val tmp2 = 3.load * 1.const
    tmp1 + tmp2
  }
}


object Test extends App {
  import OperationsInstances._


  // instantiate the direct semantics and the printing semantics
  val exI = new SimpleExamples()(IntOps( Map(1->1, 2->2, 3->3, 4->4) ))
  val exP = new SimpleExamples()(PrintOps( i=>s"input[$i]" ) )
  val exC = new SimpleExamples()(ComplexPrint(i=>s"input[$i]" ))
  //val exC = new SimpleExamples()(ComplexInt( Map(1->1, 2->2, 3->3, 4->4) ))

  // evaluate all examples under both semantics

  // println( exI.expr1 )
  // println( exP.expr1 )
  println( exC.expr1 )

  // println( exI.expr2 )
  // println( exP.expr2 )
  println( exC.expr2 )

  // println( exI.pow2n )
  // println( exP.pow2n )
  println( exC.pow2n )

  // println( exI.load )
  // println( exP.load )
  println( exC.load )
}