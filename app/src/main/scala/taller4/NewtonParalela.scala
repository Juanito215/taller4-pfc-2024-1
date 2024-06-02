/**
 * @author *Samuel Escobar Rivera - 2266363
 *         *Joseph David Herrera Libreros - 2266309
 *         *Juan David Cuellar Lopez - 2266087
 * @version 1.0
 */

package taller4

import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.math._


class NewtonParalela {

  trait Expr
  case class Numero(d: Double) extends Expr
  case class Atomo(x: Char) extends Expr
  case class Suma(e1: Expr, e2: Expr) extends Expr
  case class Prod(e1: Expr, e2: Expr) extends Expr
  case class Resta(e1: Expr, e2: Expr) extends Expr
  case class Div(e1: Expr, e2: Expr) extends Expr
  case class Expo(e1: Expr, e2: Expr) extends Expr
  case class Logaritmo(e1: Expr) extends Expr

  def mostrar(e: Expr): String = {
    val futureResult = e match {
      case Numero(d) => Future(d.toString)
      case Atomo(x) => Future(x.toString)
      case Suma(e1, e2) =>
        for {
          s1 <- mostrarAsync(e1)
          s2 <- mostrarAsync(e2)
        } yield s"($s1 + $s2)"
      case Prod(e1, e2) =>
        for {
          p1 <- mostrarAsync(e1)
          p2 <- mostrarAsync(e2)
        } yield s"($p1 * $p2)"
      case Resta(e1, e2) =>
        for {
          r1 <- mostrarAsync(e1)
          r2 <- mostrarAsync(e2)
        } yield s"($r1 - $r2)"
      case Div(e1, e2) =>
        for {
          d1 <- mostrarAsync(e1)
          d2 <- mostrarAsync(e2)
        } yield s"($d1 / $d2)"
      case Expo(e1, e2) =>
        for {
          exp1 <- mostrarAsync(e1)
          exp2 <- mostrarAsync(e2)
        } yield s"($exp1 ^ $exp2)"
      case Logaritmo(e1) =>
        for {
          l1 <- mostrarAsync(e1)
        } yield s"(ln($l1))"
    }
    Await.result(futureResult, Duration.Inf)
  }

  def mostrarAsync(e: Expr): Future[String] = e match {
    case Numero(d) => Future(d.toString)
    case Atomo(x) => Future(x.toString)
    case Suma(e1, e2) =>
      for {
        s1 <- mostrarAsync(e1)
        s2 <- mostrarAsync(e2)
      } yield s"($s1 + $s2)"
    case Prod(e1, e2) =>
      for {
        p1 <- mostrarAsync(e1)
        p2 <- mostrarAsync(e2)
      } yield s"($p1 * $p2)"
    case Resta(e1, e2) =>
      for {
        r1 <- mostrarAsync(e1)
        r2 <- mostrarAsync(e2)
      } yield s"($r1 - $r2)"
    case Div(e1, e2) =>
      for {
        d1 <- mostrarAsync(e1)
        d2 <- mostrarAsync(e2)
      } yield s"($d1 / $d2)"
    case Expo(e1, e2) =>
      for {
        exp1 <- mostrarAsync(e1)
        exp2 <- mostrarAsync(e2)
      } yield s"($exp1 ^ $exp2)"
    case Logaritmo(e1) =>
      for {
        l1 <- mostrarAsync(e1)
      } yield s"(ln($l1))"
  }

  def derivar(f: Expr, a: Atomo): Expr = {
    val futureResult = f match {
      case Numero(_) => Future.successful(Numero(0))
      case Atomo(x) if x == a.x => Future.successful(Numero(1))
      case Atomo(_) => Future.successful(Numero(0))
      case Suma(e1, e2) =>
        for {
          d1 <- derivarAsync(e1, a)
          d2 <- derivarAsync(e2, a)
        } yield Suma(d1, d2)
      case Resta(e1, e2) =>
        for {
          d1 <- derivarAsync(e1, a)
          d2 <- derivarAsync(e2, a)
        } yield Resta(d1, d2)
      case Prod(e1, e2) =>
        for {
          d1 <- derivarAsync(e1, a)
          d2 <- derivarAsync(e2, a)
        } yield Suma(Prod(d1, e2), Prod(e1, d2))
      case Div(e1, e2) =>
        for {
          d1 <- derivarAsync(e1, a)
          d2 <- derivarAsync(e2, a)
        } yield Div(Resta(Prod(d1, e2), Prod(e1, d2)), Prod(e2, e2))
      case Expo(e1, Numero(n)) =>
        derivarAsync(e1, a).map { d1 =>
          Prod(Numero(n), Prod(Expo(e1, Numero(n - 1)), d1))
        }
      case Logaritmo(e1) =>
        derivarAsync(e1, a).map { d1 =>
          Div(d1, e1)
        }
    }
    Await.result(futureResult, Duration.Inf)
  }

  def derivarAsync(f: Expr, a: Atomo): Future[Expr] = f match {
    case Numero(_) => Future.successful(Numero(0))
    case Atomo(x) if x == a.x => Future.successful(Numero(1))
    case Atomo(_) => Future.successful(Numero(0))
    case Suma(e1, e2) =>
      for {
        d1 <- derivarAsync(e1, a)
        d2 <- derivarAsync(e2, a)
      } yield Suma(d1, d2)
    case Resta(e1, e2) =>
      for {
        d1 <- derivarAsync(e1, a)
        d2 <- derivarAsync(e2, a)
      } yield Resta(d1, d2)
    case Prod(e1, e2) =>
      for {
        d1 <- derivarAsync(e1, a)
        d2 <- derivarAsync(e2, a)
      } yield Suma(Prod(d1, e2), Prod(e1, d2))
    case Div(e1, e2) =>
      for {
        d1 <- derivarAsync(e1, a)
        d2 <- derivarAsync(e2, a)
      } yield Div(Resta(Prod(d1, e2), Prod(e1, d2)), Prod(e2, e2))
    case Expo(e1, Numero(n)) =>
      derivarAsync(e1, a).map { d1 =>
        Prod(Numero(n), Prod(Expo(e1, Numero(n - 1)), d1))
      }
    case Logaritmo(e1) =>
      derivarAsync(e1, a).map { d1 =>
        Div(d1, e1)
      }
  }

  def evaluar(f: Expr, a: Atomo, v: Double): Double = {
    val futureResult = f match {
      case Numero(d) => Future.successful(d)
      case Atomo(x) => Future.successful(if (a.x == x) v else 0)
      case Suma(e1, e2) =>
        for {
          val1 <- evaluarAsync(e1, a, v)
          val2 <- evaluarAsync(e2, a, v)
        } yield val1 + val2
      case Resta(e1, e2) =>
        for {
          val1 <- evaluarAsync(e1, a, v)
          val2 <- evaluarAsync(e2, a, v)
        } yield val1 - val2
      case Prod(e1, e2) =>
        for {
          val1 <- evaluarAsync(e1, a, v)
          val2 <- evaluarAsync(e2, a, v)
        } yield val1 * val2
      case Div(e1, e2) =>
        for {
          val1 <- evaluarAsync(e1, a, v)
          val2 <- evaluarAsync(e2, a, v)
        } yield val1 / val2
      case Expo(e1, e2) =>
        for {
          val1 <- evaluarAsync(e1, a, v)
          val2 <- evaluarAsync(e2, a, v)
        } yield pow(val1, val2)
      case Logaritmo(e1) =>
        evaluarAsync(e1, a, v).map(log)
    }
    Await.result(futureResult, Duration.Inf)
  }

  def evaluarAsync(f: Expr, a: Atomo, v: Double): Future[Double] = f match {
    case Numero(d) => Future.successful(d)
    case Atomo(x) => Future.successful(if (a.x == x) v else 0)
    case Suma(e1, e2) =>
      for {
        val1 <- evaluarAsync(e1, a, v)
        val2 <- evaluarAsync(e2, a, v)
      } yield val1 + val2
    case Resta(e1, e2) =>
      for {
        val1 <- evaluarAsync(e1, a, v)
        val2 <- evaluarAsync(e2, a, v)
      } yield val1 - val2
    case Prod(e1, e2) =>
      for {
        val1 <- evaluarAsync(e1, a, v)
        val2 <- evaluarAsync(e2, a, v)
      } yield val1 * val2
    case Div(e1, e2) =>
      for {
        val1 <- evaluarAsync(e1, a, v)
        val2 <- evaluarAsync(e2, a, v)
      } yield val1 / val2
    case Expo(e1, e2) =>
      for {
        val1 <- evaluarAsync(e1, a, v)
        val2 <- evaluarAsync(e2, a, v)
      } yield pow(val1, val2)
    case Logaritmo(e1) =>
      evaluarAsync(e1, a, v).map(log)
  }

  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global

  def limpiar(f: Expr): Expr = {
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.Future

    def limpiarAsync(f: Expr): Future[Expr] = {
      f match {
        case Numero(_) => Future.successful(f)
        case Atomo(_) => Future.successful(f)
        case Suma(e1, e2) =>
          for {
            limpioE1 <- limpiarAsync(e1)
            limpioE2 <- limpiarAsync(e2)
          } yield (limpioE1, limpioE2) match {
            case (Numero(0), e) => e
            case (e, Numero(0)) => e
            case _ => Suma(limpioE1, limpioE2)
          }
        case Resta(e1, e2) =>
          for {
            limpioE1 <- limpiarAsync(e1)
            limpioE2 <- limpiarAsync(e2)
          } yield Resta(limpioE1, limpioE2)
        case Prod(e1, e2) =>
          for {
            limpioE1 <- limpiarAsync(e1)
            limpioE2 <- limpiarAsync(e2)
          } yield (limpioE1, limpioE2) match {
            case (Numero(0), _) => Numero(0)
            case (_, Numero(0)) => Numero(0)
            case (Numero(1), e) => e
            case (e, Numero(1)) => e
            case _ => Prod(limpioE1, limpioE2)
          }
        case Div(e1, e2) =>
          for {
            limpioE1 <- limpiarAsync(e1)
            limpioE2 <- limpiarAsync(e2)
          } yield (limpioE1, limpioE2) match {
            case (_, Numero(0)) => throw new ArithmeticException("Division by zero")
            case (e, Numero(1)) => e
            case (Numero(0), _) => Numero(0)
            case _ => Div(limpioE1, limpioE2)
          }
        case Expo(e1, e2) =>
          for {
            limpioE1 <- limpiarAsync(e1)
            limpioE2 <- limpiarAsync(e2)
          } yield (limpioE1, limpioE2) match {
            case (limpioE1, Numero(1)) => limpioE1
            case _ => Expo(limpioE1, limpioE2)
          }
        case Logaritmo(e1) =>
          for {
            limpioE1 <- limpiarAsync(e1)
          } yield Logaritmo(limpioE1)
      }
    }

    import scala.concurrent.Await
    import scala.concurrent.duration._

    Await.result(limpiarAsync(f), Duration.Inf)
  }

  def raizNewton(f: Expr, a: Atomo, x0: Double, ba: (Expr, Atomo, Double) => Boolean): Future[Double] = {
    def iter(xi: Double): Future[Double] = { // Esta es una función realiza la iteracion del metodo de Newton
      if (ba(f, a, xi)) Future.successful(xi) // Esta es una condición que evalua si la aproximación es suficientemente buena devuelve true
      else {
        val fxiFuture = evaluarAsync(f, a, xi) // Esta variable evalua f en el punto xi de manera asíncrona lo que devuelve un future double
        val fpxiFuture = evaluarAsync(derivar(f, a), a, xi) // Aca evalua la derivada de f en el punto xi lo que devuelve un future double
        for {
          fxi <- fxiFuture // Aca espera el resultado de la evaluación de f en xi
          fpxi <- fpxiFuture // Aca espera el resultado de la evaluación de la derivada f en xi
          nextXi = xi - fxi / fpxi // Calcula la siguiente aproximación usando el metodo de newton
          result <- iter(nextXi) // Aca hace una llamado de recursion con la nueva aproximación
        } yield result // devuelve el resultado de la iteración recursiva
      }
    }
    iter(x0) // comienza con el valor inicial
  }


  // Función buenaAprox (ejemplo de uso para raizNewton)
  def buenaAprox(f: Expr, a: Atomo, d: Double): Boolean = {
    Await.result(evaluarAsync(f, a, d).map(_.abs < 0.001), Duration.Inf) // aca evalua la funcion en el punto d y devuelve un double luego mapea
                                                                         // luego verifica si el valor absoluto de la evaluacion es menor de 0.001 y espera el resultado
                                                                         //  y el await espera el resultado del future sin limite de tiempo y devuelve true si cumple la condición
  }
}
