/**
  * Taller 3 - Programación Funcional
  * Autores: <Estudiantes>
  * Profesor: Carlos A Delgado
  */
package taller4

import org.scalameter.withWarmer
import org.scalameter.Warmer

object Taller4{

  def saludo() = "Taller 4"

  def main(args: Array[String]): Unit = {
    println(saludo())
    println(
      withWarmer(new Warmer.Default) measure {
        (1 to 100000000).toArray
      }

    )
  }
  println("Ejecutando pruebas de rendimiento desde Taller4.scala\n\n") //NO ejecuta expresiones algebraicas muy complejas, se demora minimo 20 minutos.
  Benchmarking.runNewtonBenchmark()

  println("Ejecutando pruebas de rendimiento en paralelo desde Taller4.scala") //Ejecuta all tipo de expresiones algebraicas.
  Benchmarking.runNewtonParaleloBenchmark()
 }
