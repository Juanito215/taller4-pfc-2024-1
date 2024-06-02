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

@RunWith(classOf[JUnitRunner])
class NewtonMostrar extends AnyFunSuite {
  val e = new NewtonParalela()

  test("Derivar x^2") {
    assert(e.mostrar(e.derivar(e.Prod(e.Atomo('x'), e.Atomo('x')), e.Atomo('x'))) == "((1.0 * x) + (x * 1.0))")
  }
  test("Derivar 2x^2 + 3x"){
    assert(e.mostrar(e.derivar(e.Suma(e.Prod(e.Numero(2), e.Prod(e.Atomo('x'), e.Atomo('x'))), e.Prod(e.Numero(3), e.Atomo('x'))), e.Atomo('x'))) == "(((0.0 * (x * x)) + (2.0 * ((1.0 * x) + (x * 1.0)))) + ((0.0 * x) + (3.0 * 1.0)))")
  }
  test("Derivar 2x"){
    assert(e.mostrar(e.derivar(e.Prod(e.Numero(2), e.Atomo('x')), e.Atomo('x'))) == "((0.0 * x) + (2.0 * 1.0))")
  }
  test("Derivar Ln(5x^2)") {
    assert(e.mostrar(e.derivar(e.Logaritmo(e.Prod(e.Numero(5), e.Prod(e.Atomo('x'), e.Atomo('x')))), e.Atomo('x'))) == "(((0.0 * (x * x)) + (5.0 * ((1.0 * x) + (x * 1.0)))) / (5.0 * (x * x)))")
  }
  test("Derivar (5x^2 / 10x^4)"){
    assert(e.mostrar(e.derivar(e.Div(e.Prod(e.Numero(5), e.Prod(e.Atomo('x'), e.Atomo('x'))), e.Prod(e.Numero(10), e.Prod(e.Atomo('x'), e.Prod(e.Atomo('x'), e.Atomo('x'))))), e.Atomo('x'))) == "(((((0.0 * (x * x)) + (5.0 * ((1.0 * x) + (x * 1.0)))) * (10.0 * (x * (x * x)))) - ((5.0 * (x * x)) * ((0.0 * (x * (x * x))) + (10.0 * ((1.0 * (x * x)) + (x * ((1.0 * x) + (x * 1.0)))))))) / ((10.0 * (x * (x * x))) * (10.0 * (x * (x * x)))))"
    )
  }
  test("EvaluarTest1") {
    val expr = e.Numero(5.0)
    val result = e.evaluar(expr, e.Atomo('x'), 1.0)
    assert(result == 5.0)
  }
  test("EvaluarTest2"){
    val expr = e.Logaritmo(e.Prod(e.Numero(2.0), e.Atomo('x')))
    val result = e.evaluar(expr, e.Atomo('x'), 5.0)
    assert(result == math.log(10.0))
  }
  test("EvaluarTest3") {
    val expr1 = e.Suma(e.Atomo('x'), e.Numero(2.0))
    val expr2 = e.Prod(e.Atomo('x'), e.Atomo('x'))
    val divExpr = e.Div(expr1, expr2)
    assert(e.evaluar(divExpr, e.Atomo('x'), 5.0) == 0.28)
  }
  test("EvaluarTest4") {
    val expr1 = e.Suma(e.Atomo('x'), e.Numero(2.0))
    val logExpr = e.Logaritmo(expr1)
    assert(e.mostrar(logExpr) == "(ln((x + 2.0)))")
  }
  test("EvaluarTest5") {
    val expr = e.Suma(
      e.Logaritmo(e.Atomo('x')),
      e.Div(
        e.Suma(
          e.Prod(e.Numero(3.0), e.Expo(e.Atomo('x'), e.Numero(2.0))),
          e.Prod(e.Numero(2.0), e.Atomo('x'))
        ),
        e.Resta(e.Atomo('x'), e.Numero(5.0))
      )
    )
    val result = e.evaluar(expr, e.Atomo('x'), 6.0)
    val expectedResult = Math.log(6) + ((3 * Math.pow(6, 2) + 2 * 6) / (6 - 5))
    assert(result === expectedResult)
  }
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
  test("Expresion1"){
    assert(e.mostrar(e.Expo(e.Atomo('x'), e.Numero(2.0))) == "(x ^ 2.0)")
  }
  test("Expresion2"){
    assert(e.mostrar(e.Prod(e.Prod(e.Numero(2.0), e.Expo(e.Atomo('x'), e.Numero(2.0))),e.Suma(e.Atomo('x'), e.Prod(e.Numero(4.0), e.Atomo('x'))))) ==
      "((2.0 * (x ^ 2.0)) * (x + (4.0 * x)))")
  }
  test("Espresion3"){
    assert(e.mostrar(e.Resta(e.Div(e.Numero(1.0), e.Atomo('x')),e.Div(e.Numero(1.0), e.Prod(e.Numero(2.0),e.Atomo('y'))))) ==
      "((1.0 / x) - (1.0 / (2.0 * y)))")
  }
  test("Espresion4"){
    assert(e.mostrar(e.Suma(e.Logaritmo(e.Prod(e.Numero(2.0), e.Atomo('x'))), e.Logaritmo(e.Prod(e.Numero(3.0), e.Atomo('x'))))) ==
      "((ln((2.0 * x))) + (ln((3.0 * x))))")
  }
  test("Expresion5"){
    assert(e.mostrar(e.Logaritmo(e.Expo(e.Atomo('e'), e.Prod(e.Numero(4.0), e.Atomo('x'))))) == "(ln((e ^ (4.0 * x))))")
  }
  def buenaAprox(f: e.Expr, a: e.Atomo, d: Double): Boolean = {
    e.evaluar(f, a, d).abs < 0.001
  }

}
