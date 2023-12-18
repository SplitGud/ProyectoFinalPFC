/**
  * Proyecto Final - Programación Funcional y Concurrente
  * Autores: Santiago Villa Salazar (2259527-3743), Manuel Alexander Serba Jaraba (2259345-3743)
  * Profesor: Carlos Andres Delgado
  */

package ProyectoFinal

import scala.util.Random

object ProyectoFinal {

  // Función para comparar el rendimiento de dos algoritmos
  def compararAlgoritmos[T](funcion1: => T, funcion2: => T, numEjecuciones: Int = 1): (Double, Double, Double) = {

    // Función interna para medir el tiempo de ejecución de una función
    def medirTiempo(funcion: => T): Double = {
      // Realizar múltiples ejecuciones y medir el tiempo de cada una
      val tiempos = (1 to numEjecuciones).map { _ =>
        val inicio = System.nanoTime()
        try {
          funcion // Ejecutar la función
        } catch {
          case _: Throwable => // Manejar cualquier excepción
        }
        val fin = System.nanoTime()
        (fin - inicio).toDouble / 1e6 // Convertir a milisegundos
      }
      // Calcular el tiempo promedio de ejecución
      tiempos.sum / numEjecuciones
    }

    // Medir el tiempo de ejecución de las dos funciones
    val tiempoFuncion1 = medirTiempo(funcion1)
    val tiempoFuncion2 = medirTiempo(funcion2)

    // Calcular la aceleración relativa entre las dos funciones
    val aceleracion = tiempoFuncion1 / tiempoFuncion2

    // Devolver una tupla con los tiempos y la aceleración
    (tiempoFuncion1, tiempoFuncion2, aceleracion)
  }

  // Función para generar cadenas aleatorias
  def generarCadenaAleatoria(magnitud: Int, alfabeto: Seq[Char]): String = {
    val random = new Random
    (1 to magnitud).map(_ => alfabeto(random.nextInt(alfabeto.length))).mkString
  }

  def main(args: Array[String]): Unit = {

    val alfabeto = "acgt".toSeq

    // Comparaciones de rendimiento para cada función
    for (magnitudCadena <- 2 to 11) {
      val cadenaObjetivo = generarCadenaAleatoria(magnitudCadena, alfabeto)
      val buscarCadenaSec = new BuscadorCadenaS(cadenaObjetivo, alfabeto)
      val buscarCadenaPar = new BuscadorCadenaP(cadenaObjetivo, alfabeto)

      println(s"Pruebas con cadena de magnitud $magnitudCadena")

      println("Pruebas ingenua vs. ingenuaPar")
      val resultadosIngenuo = compararAlgoritmos(buscarCadenaSec.ingenua(), buscarCadenaPar.ingenua())
      println(resultadosIngenuo)

      println("Pruebas mejorada vs. mejoradaPar")
      val resultadosMejorado = compararAlgoritmos(buscarCadenaSec.mejorada(), buscarCadenaPar.mejorada())
      println(resultadosMejorado)

      println("Pruebas turbo vs. turboPar")
      val resultadosTurbo = compararAlgoritmos(buscarCadenaSec.turbo(), buscarCadenaPar.turbo())
      println(resultadosTurbo)

      println("Pruebas turboMejorada vs. turboMejoradaPar")
      val resultadosTurboMejorada = compararAlgoritmos(buscarCadenaSec.turboMejorada(), buscarCadenaPar.turboMejorada())
      println(resultadosTurboMejorada)

      println("Pruebas turboAcelerada vs. turboAceleradaPar")
      val resultadosTurboAcelerada = compararAlgoritmos(buscarCadenaSec.turboAcelerada(), buscarCadenaPar.turboAcelerada())
      println(resultadosTurboAcelerada)

      println()
    }
  }
}
