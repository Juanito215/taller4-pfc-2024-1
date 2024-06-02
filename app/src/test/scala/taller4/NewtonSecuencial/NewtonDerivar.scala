package taller4.NewtonSecuencial

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner
import taller4.Newton

@RunWith(classOf[JUnitRunner])
class NewtonDerivar extends AnyFunSuite {
  val e = new Newton()

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
}
