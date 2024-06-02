package taller4

import org.scalameter._


object Benchmarking {
  val n = new Newton()
  val np = new NewtonParalela()


  def runNewtonBenchmark(): Unit = {
    val expr = n.Div(n.Prod(n.Atomo('x'), n.Resta(n.Numero(5), n.Atomo('y'))), n.Suma(n.Atomo('z'), n.Numero(2)))
    val atom = n.Atomo('x')
    val initialValue = 100.0

    val time = config(
      Key.exec.minWarmupRuns := 10,
      Key.exec.maxWarmupRuns := 10,
      Key.exec.benchRuns := 20,
      Key.verbose := true,
    ) withWarmer new Warmer.Default measure {
      n.raizNewton(expr, atom ,initialValue, n.buenaAprox)
    }

    println(s"Tiempo promedio de ejecución de raizNewton en Newton: $time")
  }


  def runNewtonParaleloBenchmark(): Unit = {
    val expr = np.Logaritmo(np.Prod(np.Suma(np.Atomo('x'), np.Numero(2)), np.Div(np.Expo(np.Atomo('x'), np.Numero(3)), np.Resta(np.Numero(7), np.Atomo('x')))))
    val atom = np.Atomo('x')
    val initialValue = 100.0

    val time = config(
      Key.exec.minWarmupRuns := 10,
      Key.exec.maxWarmupRuns := 10,
      Key.exec.benchRuns := 20,
      Key.verbose := true
    ) withWarmer new Warmer.Default measure {
      np.raizNewton(expr, atom, initialValue, np.buenaAprox)
    }

    println(s"Tiempo promedio de ejecución de raizNewton en NewtonParalela: $time")
  }
}
