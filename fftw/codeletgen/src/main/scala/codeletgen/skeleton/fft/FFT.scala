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

  implicit val complexStrShow: Show[Complex[String]] = Show.show(c => s"(${c.real} + ${c.imag}i)")
}

import showForComplex._

import spire.std.{DoubleIsField, DoubleIsTrig}

trait StringIsField extends Field.WithDefaultGCD[String] {
  override def minus(a:String, b:String): String = s"$a - $b"
  def negate(a:String): String = s"-$a"
  def one: String = "1.0"
  def plus(a:String, b:String): String = s"$a + $b"
  override def pow(a:String, b:Int): String = s"$a ^ $b"
  override def times(a:String, b:String): String = s"$a * $b"
  def zero: String = "0.0"

  override def fromInt(n: Int): String = n.toString()

  override def fromDouble(n: Double): String = n.toString()
  def div(a:String, b:String): String = s"$a / $b"
}

trait StringIsTrig extends Trig[String] {
  def e: String = "e"
  def pi: String = "Math.PI"

  def exp(a: String): String = s"Math.exp($a)"
  def expm1(a: String): String = s"Math.expm1($a)"
  def log(a: String): String = s"Math.log($a)"
  def log1p(a: String): String = s"Math.log1p($a)"

  def sin(a: String): String = s"Math.sin($a)"
  def cos(a: String): String = s"Math.cos($a)"
  def tan(a: String): String = s"Math.tan($a)"

  def asin(a: String): String = s"Math.asin($a)"
  def acos(a: String): String = s"Math.acos($a)"
  def atan(a: String): String = s"Math.atan($a)"
  def atan2(y: String, x: String): String = s"Math.atan2($y, $x)"

  def sinh(x: String): String = s"Math.sinh($x)"
  def cosh(x: String): String = s"Math.cosh($x)"
  def tanh(x: String): String = s"Math.tanh($x)"

  def toRadians(a: String): String = s"($a * 2 * pi) / 360"
  def toDegrees(a: String): String = s"($a * 360) / (2 * pi)"
}


object MySpireInstances {

  val stringField = new StringIsField {}

  val stringTrig = new StringIsTrig {}

  val doubleField = new DoubleIsField {}

  val doubleTrig = new DoubleIsTrig {}
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
  import MySpireInstances._

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

  // This is the original double-based (i.e. calculate) version:
  // val X = X10  // choose your input vector here
  // implicit val field = doubleField
  // implicit val trig  = doubleTrig 

  // This replaces field & trig with string-based implementation
  val X = X10.map(v => new Complex[String](v.real.toString(), v.imag.toString()) )  // choose your input vector here
  implicit val field = stringField
  implicit val trig = stringTrig 

  val Y       = fft   (X.length)(X(_))(1)  // compute with CooleyTukey, when applicable
  val Ydirect = direct(X.length)(X(_))(1)

  val Z       = fft   (X.length)(Y(_))(-1)
  val Zdirect = direct(X.length)(Ydirect(_))(-1)

  println(X.show)
  println
  println(Vector.tabulate(X.length)(Y).show)
  println(Vector.tabulate(X.length)(Ydirect).show)
  println
  println(Vector.tabulate(X.length)(Z).show)
  println(Vector.tabulate(X.length)(Zdirect).show)
}