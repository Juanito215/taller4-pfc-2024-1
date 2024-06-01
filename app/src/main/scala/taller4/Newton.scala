/**
 * Taller 3 - Programación Funcional
 * Autores: Samuel Escobar Rivera - 2266363
 *          Joseph David Herrera Libreros - 2266309
 *          Juan David Cuellar Lopez - 2266087
 * Profesor: Carlos A Delgado
 */

package taller4

class Newton {

  trait Expr
  case class Numero(d: Double) extends Expr
  case class Atomo(x: Char) extends Expr
  case class Suma(e1: Expr, e2: Expr) extends Expr
  case class Prod(e1: Expr, e2: Expr) extends Expr
  case class Resta(e1: Expr, e2: Expr) extends Expr
  case class Div(e1: Expr, e2: Expr) extends Expr
  case class Expo(e1: Expr, e2: Expr) extends Expr
  case class Logaritmo(e1: Expr) extends Expr

  // Función mostrar
  def mostrar(e: Expr): String = e match {
    case Numero(d) => d.toString
    case Atomo(x) => x.toString
    case Suma(e1, e2) => s"(${mostrar(e1)} + ${mostrar(e2)})"
    case Prod(e1, e2) => s"(${mostrar(e1)} * ${mostrar(e2)})"
    case Resta(e1, e2) => s"(${mostrar(e1)} - ${mostrar(e2)})"
    case Div(e1, e2) => s"(${mostrar(e1)} / ${mostrar(e2)})"
    case Expo(e1, e2) => s"(${mostrar(e1)} ^ ${mostrar(e2)})"
    case Logaritmo(e1) => s"(ln(${mostrar(e1)}))"
  }

  // Función derivar
  def derivar(f: Expr, a: Atomo): Expr = f match {
    case Numero(_) => Numero(0)
    case Atomo(x) if x == a.x => Numero(1)
    case Atomo(_) => Numero(0)
    case Suma(e1, e2) => Suma(derivar(e1, a), derivar(e2, a))
    case Resta(e1, e2) => Resta(derivar(e1, a), derivar(e2, a))
    case Prod(e1, e2) => Suma(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a)))
    case Div(e1, e2) => Div(Resta(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a))), Prod(e2, e2))
    case Expo(e1, Numero(n)) => Prod(Numero(n), Prod(Expo(e1, Numero(n - 1)), derivar(e1, a)))
    case Logaritmo(e1) => Div(derivar(e1, a), e1)
  }

  // Función evaluar
  def evaluar(f: Expr, a: Atomo, v: Double): Double = f match {
    case Numero(d) => d
    case Atomo(x) if x == a.x => v
    case Atomo(_) => 0.0
    case Suma(e1, e2) => evaluar(e1, a, v) + evaluar(e2, a, v)
    case Resta(e1, e2) => evaluar(e1, a, v) - evaluar(e2, a, v)
    case Prod(e1, e2) => evaluar(e1, a, v) * evaluar(e2, a, v)
    case Div(e1, e2) => evaluar(e1, a, v) / evaluar(e2, a, v)
    case Expo(e1, e2) => Math.pow(evaluar(e1, a, v), evaluar(e2, a, v))
    case Logaritmo(e1) => Math.log(evaluar(e1, a, v))
  }

  // Función limpiar
  def limpiar(f: Expr): Expr = f match {
    case Suma(Numero(0), e) => limpiar(e)
    case Suma(e, Numero(0)) => limpiar(e)
    case Resta(e, Numero(0)) => limpiar(e)
    case Prod(Numero(1), e) => limpiar(e)
    case Prod(e, Numero(1)) => limpiar(e)
    case Prod(Numero(0), _) => Numero(0)
    case Prod(_, Numero(0)) => Numero(0)
    case Div(e, Numero(1)) => limpiar(e)
    case Suma(e1, e2) => Suma(limpiar(e1), limpiar(e2))
    case Resta(e1, e2) => Resta(limpiar(e1), limpiar(e2))
    case Prod(e1, e2) => Prod(limpiar(e1), limpiar(e2))
    case Div(e1, e2) => Div(limpiar(e1), limpiar(e2))
    case Expo(e1, e2) => Expo(limpiar(e1), limpiar(e2))
    case Logaritmo(e1) => Logaritmo(limpiar(e1))
    case e => e
  }

  // Función raizNewton
  def raizNewton(f: Expr, a: Atomo, x0: Double, ba: (Expr, Atomo, Double) => Boolean): Double = {
    def iter(xi: Double): Double = {
      if (ba(f, a, xi)) xi
      else {
        val fxi = evaluar(f, a, xi)
        val fpxi = evaluar(derivar(f, a), a, xi)
        iter(xi - fxi / fpxi)
      }
    }
    iter(x0)
  }

  // Función buenaAprox (ejemplo de uso para raizNewton)
  def buenaAprox(f: Expr, a: Atomo, d: Double): Boolean = {
    evaluar(f, a, d).abs < 0.001
  }

  // Ejemplos de uso
  def main(args: Array[String]): Unit = {
    val expr1 = Suma(Atomo('x'), Numero(2))
    val expr2 = Prod(Atomo('x'), Atomo('x'))
    val expr3 = Suma(expr1, Expo(expr2, Numero(5)))
    val expr4 = Logaritmo(Atomo('x'))
    val expr5 = Prod(Div(expr1, expr2), Resta(expr3, expr4))
    val expr6 = Expo(Atomo('x'), Numero(3))


    println(mostrar(expr1)) // (x + 2.0)
    println(mostrar(expr2)) // (x * x)
    println(mostrar(expr3)) // ((x + 2.0) + ((x * x) ^ 5.0))
    println(mostrar(expr4)) // (ln(x))
    println(mostrar(expr5)) // (((x + 2.0) / (x * x)) * (((x + 2.0) + ((x * x) ^ 5.0)) - (ln(x))))
    println(mostrar(expr6)) // (x ^ 3.0)

    println(mostrar(derivar(expr6, Atomo('x')))) // ((x ^ 3.0) * (((1.0 * 3.0) / x) + (0.0 * (ln(x)))))
    println(mostrar(derivar(expr2, Atomo('x')))) // ((1.0 * x) + (x * 1.0))
    println(mostrar(derivar(expr2, Atomo('y')))) // ((0.0 * x) + (x * 0.0))
    println(mostrar(derivar(Suma(Atomo('k'), Prod(Numero(3.0), Atomo('x'))), Atomo('x')))) // (0.0 + ((0.0 * x) + (3.0 * 1.0)))

    println(evaluar(Numero(5.0), Atomo('x'), 1.0)) // 5.0
    println(evaluar(Atomo('x'), Atomo('x'), 5.0)) // 5.0
    println(evaluar(Suma(expr1, expr2), Atomo('x'), 5.0)) // 32.0
    println(evaluar(Prod(expr1, expr2), Atomo('x'), 5.0)) // 175.0
    println(evaluar(Resta(expr1, expr2), Atomo('x'), 5.0)) // -18.0
    println(evaluar(Div(expr1, expr2), Atomo('x'), 5.0)) // 0.28
    println(evaluar(Expo(expr1, expr2), Atomo('x'), 5.0)) // 1.341068619663965E21
    println(evaluar(Logaritmo(expr1), Atomo('x'), 5.0)) // 1.9459101490553132

    println(mostrar(limpiar(derivar(Suma(Atomo('k'), Prod(Numero(3.0), Atomo('x'))), Atomo('x'))))) // 3.0


    val e1 = Resta(Prod(Atomo('x'), Atomo('x')), Numero(2))
    println(raizNewton(e1, Atomo('x'), 1.0, buenaAprox)) // Ejemplo de uso de raizNewton
  }
}
