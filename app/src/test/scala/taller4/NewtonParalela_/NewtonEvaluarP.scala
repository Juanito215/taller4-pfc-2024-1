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
class NewtonEvaluarP extends AnyFunSuite{

  val e = new Newton()
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
}
