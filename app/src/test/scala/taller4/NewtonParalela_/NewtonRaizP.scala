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
import taller4.Newton


@RunWith(classOf[JUnitRunner])
class NewtonRaizP extends AnyFunSuite{

  val e = new Newton()

  def buenaAprox(f: e.Expr, a: e.Atomo, d: Double): Boolean = {
    e.evaluar(f, a, d).abs < 0.001
  }

  test("raizNewtonTest1") {
    val expr = e.Resta(e.Prod(e.Atomo('x'), e.Atomo('x')), e.Numero(4.0))
    val raiz = e.raizNewton(expr, e.Atomo('x'), 3425.1, buenaAprox)
    assert(math.abs(raiz - 2.0) < 0.001)
  }
  test("raizNewtonTest2") {
    val expr = e.Expo(e.Suma(e.Prod(e.Numero(3.0), e.Atomo('x')), e.Expo(e.Atomo('x'), e.Numero(2.0))), e.Numero(2.0))
    val raiz = e.raizNewton(expr, e.Atomo('x'), 1571.6, buenaAprox)
    assert(buenaAprox(expr, e.Atomo('x'), raiz))
  }
  test("raizNewtonTest3"){
    val expr = e.Resta(e.Expo(e.Atomo('x'), e.Numero(3.0)), e.Prod(e.Numero(7.0), e.Atomo('x')))
    val raiz = e.raizNewton(expr, e.Atomo('x'), 2895.8, buenaAprox)
    assert(buenaAprox(expr, e.Atomo('x'), raiz))
  }
  test("raizNewtonTest4"){
    val expr = e.Resta(e.Logaritmo(e.Atomo('x')), e.Numero(1.0))
    val raiz = e.raizNewton(expr, e.Atomo('x'), 61000.23, buenaAprox)
    assert(buenaAprox(expr, e.Atomo('x'), raiz))
  }
  test("raizNewtonTest5"){
    val expr = e.Resta(e.Prod(e.Atomo('x'), e.Logaritmo(e.Atomo('x'))), e.Numero(3.0))
    val raiz = e.raizNewton(expr, e.Atomo('x'), 80000.0, buenaAprox)
    assert(buenaAprox(expr, e.Atomo('x'), raiz))
  }

}
