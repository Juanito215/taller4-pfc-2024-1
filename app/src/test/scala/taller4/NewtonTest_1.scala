/**
 * @author *Samuel Escobar Rivera - 2266363
 *         *Joseph David Herrera Libreros - 2266309
 *         *Juan David Cuellar Lopez - 2266087
 * @version 1.0
 * @note 22 de Noviembre de 2023
 */

package taller4

import org.apache.commons.math3.analysis.function.Exp
import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class NewtonTest_1 extends AnyFunSuite{
  val e = new Newton()
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

}
