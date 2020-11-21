package codeletgen.skeleton.fft

// An implementation of two algorithms for computing DFT and IDFT (Inverse DFT).
//
// 1) Direct DFT, per definition of DFT. (https://en.wikipedia.org/wiki/Discrete_Fourier_transform)
// 2) Fast Fourier Transform, namely Cooley-Tukey (https://en.wikipedia.org/wiki/Fast_Fourier_transform)
//
// As in [FFTW], we select between DFT and IDFT with teh sign parameter.
// Note that IDFT is not scaled by `n`.  See the note at Eq 2 in [FFTW]
//
// The implementation below is fixed to concrete values of type Complex[Double].  That is, the
// implementation is not yet lifted to alternative semantics needed to generate the expression dag.

import cats.implicits.{catsStdShowForVector, toShow}
import cats.Show

//import spire.implicits._
import spire.algebra._
import spire.math.prime.{Factors, isPrime}
import spire.math.{Complex, abs}

// Define a custom print function for Complex.
// See Chapter 1 in "Scala with Cats" to learn about the SHow type class.
object showForComplex {
  implicit val complexShow: Show[Complex[Double]] = Show.show(c => {
    // the following is based on from https://rosettacode.org/wiki/Fast_Fourier_transform#Scala, (which was buggy)
    val a = "%1.3f" format c.real
    val b = "%1.3f" format abs(c.imag)
    (a, b) match {
      case (_, "0.000") => a
      case ("0.000", _) if c.imag > 0 => b + "i"
      case ("0.000", _) => "-" + b + "i"
      case ("-0.000", _) if c.imag > 0 => b + "i"
      case ("-0.000", _) => "-" + b + "i"
      case (_, _) if c.imag > 0 => a + " + " + b + "i"
      case (_, _) => a + " - " + b + "i"
    }
  })


  // This to have a cleaner presentation of all the operations that represent a Complex as string
  implicit val complexStrShow: Show[Complex[String]] = Show.show(c => s"\n\t===> r <==: ${c.real}\n\t==> i <==: ${c.imag}\n")

  // This to have a cleaner presentation of all the operations that represent a Complex as string
  implicit val complexNodeShow: Show[Complex[Node]] = Show.show(c => s"\n\t===> r <==: ${c.real}\n\t==> i <==: ${c.imag}\n")

  // This to generate the code needed for a Vector after applying the FFT:
  implicit val vectorShow : Show[Vector[Complex[Instruction]]] = Show.show(c => {
    val outVars = (c.zipWithIndex.map { case (v, i) => s"    out[$i].re = ${v.real};\n    out[$i].im = ${v.imag}"  }).mkString("\n")
    val code = Instruction.toCode()
    s"""
struct complex { double re, im; };

void generatedCode(struct complex in[4], struct complex out[4]) {
$code
$outVars
}
  """

  })
}

import showForComplex._

// -----------------------------------
// Node Field/Trig
// -----------------------------------

// -----------------------------------------------------------------
// This type class with all the operations we will need to do to 
// implement the Field/Trig for Complex:
// -----------------------------------------------------------------
sealed trait MyOperations[T] {
  def const(c: Int): T
  def const(c: Double): T
  def const(c: String): T
  def add(a: T, b: T): T
  def mult(a: T, b: T): T
  def div(a: T, b: T): T
  def negate(a: T): T
  def math(exp: String, v: T): T
}

// The type class interface, so the oeprations can be used implicitly:
object OperationsSyntax {
  implicit class SyntaxOperations[T](a: T) {
    def +(b: T)(implicit o: MyOperations[T]): T = o.add(a, b)
    def *(b: T)(implicit o: MyOperations[T]): T = o.mult(a, b)
    def /(b: T)(implicit o: MyOperations[T]): T = o.div(a, b)
    def -(b: T)(implicit o: MyOperations[T]): T = o.negate(o.add(a, b))
    def negate()(implicit o: MyOperations[T]): T = o.negate(a)
    def on(b: String)(implicit o: MyOperations[T]): T = o.math(b, a)
  }

  def const[T](c: Double)(implicit o: MyOperations[T]) : T = o.const(c)
  def const[T](c: Int)(implicit o: MyOperations[T]) : T = o.const(c)
  def const[T](c: String)(implicit o: MyOperations[T]) : T = o.const(c)
}

// -----------------------------------------------------------------
// Implement Field using our NodeOperations interface:
// -----------------------------------------------------------------
class NodeField[T](implicit o: MyOperations[T]) extends Field.WithDefaultGCD[T] {
  import OperationsSyntax._

  override def minus(a:T, b:T): T = a - b
  def negate(a:T): T = a.negate
  def plus(a:T, b:T): T = a + b
  def times(a:T, b:T): T = a * b
  def div(a:T, b:T): T = a / b

  def one: T = const(1.0)
  def zero: T = const(0.0)

  override def fromInt(n: Int): T = const(n)
  override def fromDouble(n: Double): T = const(n)
}

// -----------------------------------------------------------------
// Similarly, implement Trig using our NodeOperations interface:
// -----------------------------------------------------------------
class NodeTrig[T](implicit o: MyOperations[T]) extends Trig[T] {
  import OperationsSyntax._
  
  // and double to T:
  implicit def doubleToNum(d: Double) : T = const(d)

  def e: T  = math.E
  def pi: T = math.Pi

  def exp(a: T): T   = a on "Math.exp"
  def expm1(a: T): T = a on "Math.expm1"
  def log(a: T): T   = a on "Math.log"
  def log1p(a: T): T = a on "Math.log1p"

  def sin(a: T): T = a on "Math.sin"
  def cos(a: T): T = a on "Math.cos"
  def tan(a: T): T = a on "Math.tan"

  def asin(a: T): T = a on "Math.asin"
  def acos(a: T): T = a on "Math.acos"
  def atan(a: T): T = a on "Math.atan"
  def atan2(y: T, x: T): T = ???

  def sinh(x: T): T = x on "Math.sinh"
  def cosh(x: T): T = x on "Math.cosh"
  def tanh(x: T): T = x on "Math.tanh"

  def toRadians(a: T): T = ???
  def toDegrees(a: T): T = ???
}

// -----------------------------------------------------------------
// The implmentation of MyOperations on String
// -----------------------------------------------------------------
object StringOperations {
  def ops = new MyOperations[String] {
    def const(c: Int) = new String(c.toString())
    def const(c: Double) = new String(c.toString())
    def const(c: String) = new String(c)

    def add(a: String, b: String) = s"$a + $b"
    def mult(a: String, b: String) = s"$a * $b"
    def div(a: String, b: String)  = s"$a / $b"
    def negate(a: String) = s"-$a"
    def math(exp: String, v: String) = s"${exp}($v)"
  }
}

// -----------------------------------------------------------------
// The implmentation of MyOperations for Node (IR)
// -----------------------------------------------------------------
trait Node
case class Const(d: Double) extends Node
case class Add(a: Node, b: Node) extends Node
case class Mult(a: Node, b: Node) extends Node
case class Div(a: Node, b:Node) extends Node
case class Negate(a: Node) extends Node
case class Math(exp: String, v: Node) extends Node

object Node {
  def ops = new MyOperations[Node] {
    def const(c: Int) = new Const(c)
    def const(c: Double) = new Const(c)
    def const(c: String) = new Const(c.toDouble)

    def add(a: Node, b: Node) = new Add(a, b)
    def mult(a: Node, b: Node) = new Mult(a, b)
    def div(a: Node, b: Node)  = new Div(a, b)
    def negate(a: Node) = new Negate(a)
    def math(exp: String, v: Node) = new Math(exp, v)
  }
}

// -----------------------------------------------------------------
// The implementation of MyOperations for codegen (instructions)
// -----------------------------------------------------------------
class Instruction(label : String) {
  override def toString() = label
}

object Instruction {
  import scala.collection.mutable.ListBuffer
  
  private val all : ListBuffer[(String, String)] = ListBuffer()

  private implicit def append(expr: String) : Instruction = {
    val label = all.find(p => p._2 == expr)  match {
      case Some(value) => value._1
      case None => 
        val label = s"temp${all.size}"
        all. += ((label, expr))
        label
    }
    new Instruction(label)
  }
  
  def toCode() : String  = {
    all.map(i => s"    double ${i._1} = ${i._2};").mkString("\n")
  }

  def ops = new MyOperations[Instruction] {
    def const(c: Int) = new Instruction(c.toString())
    def const(c: Double) = new Instruction(c.toString())
    def const(c: String) = new Instruction(c)

    def add(a: Instruction, b: Instruction) = append(s"$a + $b")
    def mult(a: Instruction, b: Instruction) = append(s"$a * $b")
    def div(a: Instruction, b: Instruction)  = append(s"$a / $b")
    def negate(a: Instruction) = append(s"-$a")
    def math(exp: String, v: Instruction) = append(s"${exp}($v)")
  } 
}

// -----------------------------------
// Changes required for each scenario (the field/trig/adapter)
// -----------------------------------
trait Scenario[T] {
  val field : Field[T]
  val trig: Trig[T]
  val input: Vector[Complex[T]]
}
case class DefaultScenario(v: Vector[Complex[Double]]) extends Scenario[Double] {
  import spire.std.{DoubleIsField, DoubleIsTrig}

  val field = new DoubleIsField {}
  val trig  = new DoubleIsTrig {}
  val input = v
}
case class StringScenario(v: Vector[Complex[Double]]) extends Scenario[String] {
  private def toComplexString(v: Complex[Double]) : Complex[String] = new Complex[String](v.real.toString(), v.imag.toString())

  val field = new NodeField()(StringOperations.ops) {}
  val trig  = new NodeTrig()(StringOperations.ops) {}
  val input = v.map(toComplexString)
}
case class IRScenario(v: Vector[Complex[Double]]) extends Scenario[Node] {
  private def toComplexNode(v: Complex[Double]) : Complex[Node] =
    new Complex[Node](new Const(v.real), new Const(v.imag))
  
  val field = new NodeField()(Node.ops) {}
  val trig  = new NodeTrig()(Node.ops) {}
  val input = v.map(toComplexNode)
}
case class CodegenScenario(v: Vector[Complex[Double]]) extends Scenario[Instruction] {
  private def toComplexInstruction(v: (Complex[Double], Int)) : Complex[Instruction] =
      new Complex[Instruction](new Instruction(s"in[${v._2}].re"), new Instruction(s"in[${v._2}].im"))
  
  val field = new NodeField()(Instruction.ops) {}
  val trig  = new NodeTrig()(Instruction.ops) {}
  val input = v.zipWithIndex.map(toComplexInstruction)
}


object FFT {
  type T[x] = Complex[x]
  // arrays are modeled as functions from indices to values
  type Arr[x] = Int => T[x]

  // This function is a simplified version of the fftgen function from page 5 of [FFTW].
  // This version selects only between Cooley-Tukey and the direct algorithm.
  def fft[x](n: Int)(X: Arr[x])(sign: Int)(implicit f:Field[x], t:Trig[x]): Arr[x] = {
    if (isPrime(n))
      direct(n)(X)(sign)
    else {
      // Factor n into n1, n2, for a call to Cooley-Tukey.
      // No attention is paid to obtaining performance-optimal n1, n2.
      val factors = Factors(n)
      val (n1, n2) = factors.toList match {
        case (b,e) :: List() => (b pow (e-1), b)   // n factors into b^e
        case (b,e) :: _      => (b pow e, n / (b pow e))  // n factors to b^e * <more terms>
      }
      println(s"$n=$n1*$n2")
      assert(n1 * n2 == n)
      cooleyTukey(n1.toInt)(n2.toInt)(X)(sign)
    }
  }

  private def exp[x](n: Int, k: Int)(implicit f:Field[x], t:Trig[x]): T[x] = Complex.rootOfUnity(n,k)

  // transcribed from Fig 4 in [FFTW]
  def cooleyTukey[x](n1: Int)(n2: Int)(X: Arr[x])(sign: Int)(implicit o:Field[x], t:Trig[x]): Arr[x] = {
    def tmp1(j2: Int)          = fft(n1)(j1 => X(j1 * n2 + j2))(sign)
    def tmp2(i1: Int)(j2: Int) = exp(n1*n2, sign * i1 * j2) * tmp1(j2)(i1)
    def tmp3(i1: Int)          = fft(n2)(tmp2(i1))(sign)
    i => tmp3( i%n1 )( i/n1 )
  }

  def direct[x](n: Int)(X: Arr[x])(sign: Int)(implicit f:Field[x], t:Trig[x]): Arr[x] = {
    i =>
      (
        for (j <- 0 until n) yield {
          val cij = exp(n, sign * i * j)
          // FYI: The following definition is from wikipedia (DFT). Not semantically identical to above but a legal DFT.
          // val cij = Complex[Double] (scala.math.cos(2*Pi*i*j/n), -scala.math.sin(2*Pi*i*j/n))
          X(j) * cij
        })
        .foldLeft(Complex[x](f.zero))(_ + _) // sum the list of terms
  }
}

object FFT_Test extends App {
  import FFT._

  // X4 is the example from wikipedia on DFT
  val X4 = Vector(
    Complex(1.0, 0.0), Complex(2.0, -1.0),
    Complex(0.0, -1.0), Complex(-1.0, 2.0)
  )
  val X6 = Vector(
    Complex(1.0, 0.0), Complex(2.0, -1.0), Complex(0.0, -1.0),
    Complex(1.0,0), Complex(1.0,2.0), Complex(0.0,-3.0)
  )
  val X10 = Vector(
    Complex(1.0,0), Complex(1.0,0),
    Complex(1.0,0), Complex(1.0,0),
    Complex(0.0,0), Complex(0.0,2),
    Complex(0.0,0), Complex(0.0,0),
    Complex(0.0,0), Complex(0.0,2)
  )

  val scenario = DefaultScenario(X10)
  //val scenario = StringScenario(X4)
  //val scenario = IRScenario(X4)
  //val scenario = CodegenScenario(X4)

  implicit val f = scenario.field
  implicit val t = scenario.trig 

  val X = scenario.input

  val Y       = fft   (X.length)(X(_))(1)  // compute with CooleyTukey, when applicable
  val Ydirect = direct(X.length)(X(_))(1)

  val Z       = fft   (X.length)(Y(_))(-1)
  val Zdirect = direct(X.length)(Ydirect(_))(-1)

  println(X.show)
  println
  println(Vector.tabulate(X.length)(Y).show)
  println
  println(Vector.tabulate(X.length)(Ydirect).show)
  println
  println(Vector.tabulate(X.length)(Z).show)
  println(Vector.tabulate(X.length)(Zdirect).show)
}