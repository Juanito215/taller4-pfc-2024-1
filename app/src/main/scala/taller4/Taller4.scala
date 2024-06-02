/**
  * Taller 3 - Programación Funcional
  * Autores: <Estudiantes>
  * Profesor: Carlos A Delgado
  */
package taller4

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer
import org.scalameter._
import taller4.Benchmarking


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
  println("Ejecutando pruebas de rendimiento desde Taller4.scala")
  Benchmarking.runNewtonBenchmark()
  println("Ejecutando pruebas de rendimiento en paralelo desde Taller4.scala")
  //Benchmarking.runNewtonParaleloBenchmark()
 }
