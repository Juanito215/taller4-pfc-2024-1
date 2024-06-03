/**
 * @author *Samuel Escobar Rivera - 2266363
 *         *Joseph David Herrera Libreros - 2266309
 *         *Juan David Cuellar Lopez - 2266087
 * @version 1.0
 */

package taller4.NewtonParalela_

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner
import taller4.NewtonParalela

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

@RunWith(classOf[JUnitRunner])
class NewtonParalelaRaiz extends AnyFunSuite {

  val e = new NewtonParalela

  def buenaAprox(f: e.Expr, a: e.Atomo, d: Double): Boolean = {
    Await.result(e.evaluarAsync(f, a, d).map(_.abs < 0.001), Duration.Inf)
  }

  test("raizNewtonTest1") {
    val expr = e.Resta(e.Prod(e.Atomo('x'), e.Atomo('x')), e.Numero(4.0))
    val raizFuture = e.raizNewton(expr, e.Atomo('x'), 2255.125, buenaAprox)
    val raiz = Await.result(raizFuture, Duration.Inf)
    assert(math.abs(raiz - 2.0) < 0.001)
  }

  test("raizNewtonTest2") {
    val expr = e.Expo(e.Suma(e.Prod(e.Numero(3.0), e.Atomo('x')), e.Expo(e.Atomo('x'), e.Numero(2.0))), e.Numero(2.0))
    val raizFuture = e.raizNewton(expr, e.Atomo('x'), 450.0548, buenaAprox)
    val raiz = Await.result(raizFuture, Duration.Inf)
    assert(buenaAprox(expr, e.Atomo('x'), raiz))
  }

  test("raizNewtonTest3") {
    val expr = e.Resta(e.Expo(e.Atomo('x'), e.Numero(3.0)), e.Prod(e.Numero(7.0), e.Atomo('x')))
    val raizFuture = e.raizNewton(expr, e.Atomo('x'), 7850.0575, buenaAprox)
    val raiz = Await.result(raizFuture, Duration.Inf)
    assert(buenaAprox(expr, e.Atomo('x'), raiz))
  }

  test("raizNewtonTest4") {
    val expr = e.Suma(
      e.Suma(
        e.Suma(
          e.Suma(
            e.Prod(e.Numero(3), e.Prod(e.Prod(e.Atomo('x'), e.Atomo('x')), e.Prod(e.Atomo('x'), e.Atomo('x')))), // 3x^4
            e.Resta(e.Numero(0), e.Prod(e.Numero(5), e.Prod(e.Prod(e.Atomo('x'), e.Atomo('x')), e.Atomo('x'))))  // -5x^3
          ),
          e.Prod(e.Numero(2), e.Prod(e.Atomo('x'), e.Atomo('x'))) // 2x^2
        ),
        e.Resta(e.Numero(0), e.Prod(e.Numero(7), e.Atomo('x'))) // -7x
      ),
      e.Numero(4) // +4
    )
    val raizFuture = e.raizNewton(expr, e.Atomo('x'), 250.355, buenaAprox)
    val raiz = Await.result(raizFuture, Duration.Inf)
    assert(buenaAprox(expr, e.Atomo('x'), raiz))
  }

  test("raizNewtonTest5") {
    val expr = e.Suma(
      e.Suma(
        e.Suma(
          e.Suma(
            e.Suma(
              e.Prod(e.Numero(4), e.Prod(e.Prod(e.Prod(e.Atomo('x'), e.Atomo('x')), e.Atomo('x')), e.Prod(e.Atomo('x'), e.Atomo('x')))), // 4x^5
              e.Resta(e.Numero(0), e.Prod(e.Numero(3), e.Prod(e.Prod(e.Prod(e.Atomo('x'), e.Atomo('x')), e.Atomo('x')), e.Atomo('x'))))  // -3x^4
            ),
            e.Prod(e.Numero(1), e.Prod(e.Prod(e.Atomo('x'), e.Atomo('x')), e.Atomo('x'))) // x^3
          ),
          e.Resta(e.Numero(0), e.Prod(e.Numero(2), e.Prod(e.Atomo('x'), e.Atomo('x')))) // -2x^2
        ),
        e.Prod(e.Numero(1), e.Atomo('x')) // x
      ),
      e.Resta(e.Numero(0), e.Numero(6)) // -6
    )
    val raizFuture = e.raizNewton(expr, e.Atomo('x'), 124.015, buenaAprox)
    val raiz = Await.result(raizFuture, Duration.Inf)
    assert(buenaAprox(expr, e.Atomo('x'), raiz))
  }
}
