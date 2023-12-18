/**
 * TEST PROYECTO FINAL
 *
 * Proyecto Final - Programación Funcional y Concurrente
 * Autores: Santiago Villa Salazar (2259527-3743), Manuel Alexander Serba Jaraba (2259345-3743)
 * Profesor: Carlos Andres Delgado
 */

package ProyectoFinal

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

import ProyectoFinal._

@RunWith(classOf[JUnitRunner])
class testBuscaCadenas extends AnyFunSuite {
  test("2 a 10") {
    val alfabeto = "acgt".toSeq

    // prueba de funcionamiento
    for (magnitudCadena <- 2 to 10) {
      val cadenaObjetivo = generarCadenaAleatoria(magnitudCadena, alfabeto)
      val buscadorCadena1 = new BuscadorCadenaS(cadenaObjetivo, alfabeto)
      val buscadorCadenasParalelo = new BuscadorCadenaP(cadenaObjetivo,alfabeto)
      println(s"Pruebas con cadena de magnitud $magnitudCadena para hallar $cadenaObjetivo")

      // Caso de prueba 1: ingenua
      val resultadoIngenuo = buscadorCadena1.ingenua()
      assert(resultadoIngenuo == cadenaObjetivo.toSeq)


      // Caso de prueba 2: mejorada
      val resultadoMejorado = buscadorCadena1.mejorada()
      assert(resultadoMejorado == cadenaObjetivo.toSeq)


      // Caso de prueba 3: turbo
      val resultadoTurbo = buscadorCadena1.turbo()
      assert(resultadoTurbo == cadenaObjetivo.toSeq)


      // Caso de prueba 4: turboMejorada
      val resultadoTurboMejorada = buscadorCadena1.turboMejorada()
      assert(resultadoTurboMejorada == cadenaObjetivo.toSeq)


      // Caso de prueba 5: turboAcelerada
      val resultadoTurboAcelerada = buscadorCadena1.turboAcelerada()
      assert(resultadoTurboAcelerada == cadenaObjetivo.toSeq)


      // Caso de prueba 6: ingenuaPar
      val resultadoIngenuoPar = buscadorCadenasParalelo.ingenua()
      assert(resultadoIngenuoPar == cadenaObjetivo.toSeq)


      // Caso de prueba 7: mejoradaPar
      val resultadoMejoradoPar = buscadorCadenasParalelo.mejorada()
      assert(resultadoMejoradoPar == cadenaObjetivo.toSeq)


      // Caso de prueba 8: turboPar
      val resultadoTurboPar = buscadorCadenasParalelo.turbo()
      assert(resultadoTurboPar == cadenaObjetivo.toSeq)


      // Caso de prueba 9: turboMejoradaPar
      val resultadoTurboMejoradaPar = buscadorCadenasParalelo.turboMejorada()
      assert(resultadoTurboMejoradaPar == cadenaObjetivo.toSeq)


      // Caso de prueba 10: turboAceleradaPar
      val resultadoTurboAceleradaPar = buscadorCadenasParalelo.turboAcelerada()
      assert(resultadoTurboAceleradaPar == cadenaObjetivo.toSeq)

      println()

    }
  }
}