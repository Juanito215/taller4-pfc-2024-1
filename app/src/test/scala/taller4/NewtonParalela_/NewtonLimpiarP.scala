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
class NewtonLimpiarP extends AnyFunSuite {
  val e = new Newton()

  test("LimpiarTest1") {
    assert(e.limpiar(e.Prod(e.Suma(e.Numero(0), e.Atomo('x')), e.Div(e.Atomo('y'), e.Numero(1)))) == e.Prod(e.Atomo('x'), e.Atomo('y')))
  }

  test("LimpiarTest2") {
    val expr = e.Logaritmo(e.Prod(e.Numero(1), e.Atomo('y')))
    val result = e.limpiar(expr)
    assert(result == e.Logaritmo(e.Atomo('y')))
  }

  test("LimpiarTest3") {
    val expr = e.Div(e.Prod(e.Numero(0), e.Suma(e.Atomo('x'), e.Numero(0))), e.Numero(1))
    val result = e.limpiar(expr)
    assert(result == e.Numero(0))
  }

  test("LimpiarTest4") {
    val expr = e.Suma(e.Logaritmo(e.Atomo('x')), e.Expo(e.Atomo('y'), e.Numero(2)))
    val derivada = e.limpiar(e.derivar(expr, e.Atomo('y')))
    val result = e.Prod(e.Numero(2), e.Atomo('y'))

    assert(derivada == result)
  }

  test("LimpiarTest5") {
    assert(e.limpiar(e.Logaritmo(e.Prod(e.Numero(1), e.Suma(e.Atomo('y'), e.Numero(0))))) == e.Logaritmo(e.Atomo('y')))
  }
}