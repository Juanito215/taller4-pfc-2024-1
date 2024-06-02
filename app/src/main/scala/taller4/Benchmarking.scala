package taller4

import org.scalameter._


object Benchmarking {
  val n = new Newton()
  val np = new NewtonParalela()


  def runNewtonBenchmark(): Unit = {
    val expr = n.Resta(n.Prod(n.Atomo('x'), n.Atomo('x')), n.Numero(2))
    val atom = n.Atomo('x')
    val initialValue = 1.0

    val time = config(
      Key.exec.minWarmupRuns := 10,
      Key.exec.maxWarmupRuns := 20,
      Key.exec.benchRuns := 10,
      Key.verbose := true
    ) withWarmer (new Warmer.Default) measure {
      n.raizNewton(expr, atom, initialValue, n.buenaAprox)
    }

    println(s"Tiempo promedio de ejecución de raizNewton en Newton: $time")
  }

 /*
  def runNewtonParaleloBenchmark(): Unit = {
    val expr = np.Resta(np.Prod(np.Atomo('x'), np.Atomo('x')), np.Numero(2))
    val atom = np.Atomo('x')
    val initialValue = 1.0

    val time = config(
      Key.exec.minWarmupRuns := 10,
      Key.exec.maxWarmupRuns := 10,
      Key.exec.benchRuns := 10,
      Key.verbose := true,
      Key.exec.benchRuns := 10
    ) withWarmer (new Warmer.Default) measure {
      new NewtonParalela().raizNewton(expr, atom, initialValue, new NewtonParalela().buenaAprox)
    }

    println(s"Tiempo promedio de ejecución de raizNewton en NewtonParalela: $time")
  }*/
}
