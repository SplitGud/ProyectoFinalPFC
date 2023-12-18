/**
 * Clase BuscadorCadenaP que define diferentes algoritmos para encontrar una cadena en otra.
 */

package ProyectoFinal
import Trie.ramaValida
import Trie.agregar_secuencias
import Trie.cadenas_del_arbol
import Trie.arbolDeSufijos
import common._

import scala.annotation.tailrec

/**
 * Clase que define diferentes algoritmos para encontrar una cadena en otra.
 *
 * @param cadenaObjetivo Cadena objetivo a buscar.
 * @param alfabeto Alfabeto de alfabeto disponibles.
 */
class BuscadorCadenaP(cadenaObjetivo: String, alfabeto: Seq[Char]) {
  // Definición de tipo para el oráculo que verifica la existencia de subcadenas.
  type Oraculo = Seq[Char] => Boolean

  // Oráculo que evalúa si una subcadena está presente en la cadena objetivo.
  val oraculo: Oraculo = (subcadena: Seq[Char]) => cadenaObjetivo.contains(subcadena.mkString)

  // Longitud de la cadena objetivo.
  val n: Int = cadenaObjetivo.length

/**
  * Función para obtener todas las combinaciones posibles de un n dado usando un alfabeto específico.
  *
  * @param alfabeto Alfabeto de alfabeto que forman las combinaciones.
  * @param longitud n de las combinaciones a obtener.
  * @return Una secuencia con todas las combinaciones obtenidas.
  */
  def generarSubcadenas(caracteres: Seq[Char], longitud: Int): Seq[Seq[Char]] = {
    @tailrec
    def iterarSubcadenas(subs: Seq[Seq[Char]], n: Int): Seq[Seq[Char]] = {
      if (n > longitud) subs
      else iterarSubcadenas(subs.flatMap(subcadena => caracteres.map(_ +: subcadena)), n + 1)
    }
    iterarSubcadenas(caracteres.map(Seq(_)), 1)
  }
/**
  * Encuentra la primera subcadena que se ajuste a una cadena objetivo, usando un método simple.
  *
  * @return Una opción que contiene la primera subcadena hallada, si existe.
  */
  def ingenua(): Seq[Char] = {
    // Obtiene todas las subcadenas posibles de longitud n usando el alfabeto
    val resultado = generarSubcadenas(alfabeto, n - 1)
    // Si el n es par, separa el resultado en dos partes y busca en paralelo la primera secuencia que cumpla el oráculo
    val (c1, c2) = parallel(resultado.take(n / 2).find(oraculo(_)), resultado.drop(n / 2).find(oraculo(_)))
    // Devuelve la primera secuencia encontrada, o None si no hay ninguna
    if (c1.isDefined) c1.head else c2.head
  }
/**
  * Algoritmo de búsqueda PRC (Pattern Recognition Code) Optimizado.
  *
  * @return Lista de subcadenas halladas.
  */
  def mejorada(): Seq[Char]  = {
    // Obtiene la n de la cadena objetivo
    // Crea la lista de subcadenas con las de n 1
    // Función auxiliar que encuentra secuencias optimizadas a partir de una lista de subsecuencias
    def encontrarSecuenciasMSubs(subsecuencias: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      // Añade un caracter del alfabeto a cada subsecuencia y selecciona las que cumplan el oráculo
      val c1 = combinar_dos_listas(subsecuencias.take(n / 2), Seq('a', 'c', 'g', 't').map(Seq(_))).filter(oraculo(_))
      // Devuelve la primera secuencia hallada, o una secuencia vacía si no hay ninguna
      if (c1.nonEmpty) c1 else Seq(Seq())
    }

    // Función auxiliar que encuentra secuencias optimizadas a partir de una lista de secuencias
    @tailrec
    def encontrarSecuenciasParAux(secuencias: Seq[Seq[Char]]): Seq[Char] = {
      // Si la lista de secuencias es vacía, devuelve una secuencia vacía
      if (secuencias.head.isEmpty) Seq()
      else {
        // Separa la lista de secuencias en dos partes y busca en paralelo secuencias optimizadas a partir de cada una
        val (c1, c2) = parallel(encontrarSecuenciasMSubs(secuencias.take(secuencias.length / 2)), encontrarSecuenciasMSubs(secuencias.drop(secuencias.length / 2)))
        // Une las secuencias halladas
        val total = (c1 ++ c2).filter(_ != Seq())
        // Si hay alguna secuencia de n n, devuelve el conjunto de secuencias
        if (total.head.size == n) total.head
        // Si no, llama recursivamente a la función auxiliar con el conjunto de secuencias
        else encontrarSecuenciasParAux(total)
      }
    }
    // Crea una lista de secuencias de un solo caracter con los alfabetos
    val alfabetoEnForma = alfabeto.map(cadena => Seq(cadena))
    // Llama a la función auxiliar con la lista de secuencias
    encontrarSecuenciasParAux(alfabetoEnForma)
  }
/**
  * Une dos listas de cadenas creando todas las combinaciones posibles.
  *
  * @param primeraLista Primera lista de cadenas.
  * @param segundaLista Segunda lista de cadenas.
  * @return Lista de todas las combinaciones posibles.
  */
  def combinar_dos_listas(primeraLista: Seq[Seq[Char]], segundaLista: Seq[Seq[Char]]): Seq[Seq[Char]] = {
    if (primeraLista.isEmpty) Nil
    else segundaLista.map { cadena => primeraLista.head ++ cadena } ++ combinar_dos_listas(primeraLista.tail, segundaLista)
  }
  /**
  * Algoritmo de búsqueda de subcadenas veloz.
  *
  * @return Lista de subcadenas halladas.
  */
  def turbo(): Seq[Char] = {
    // Crea la lista de subcadenas con las de n 1
    val alfabetoEnForma = alfabeto.map(Seq(_))
    // Define una función auxiliar recursiva que encuentra las subcadenas que cumplen el oráculo
    def encontrarSubcadenasVelozAux(sck: Seq[Seq[Char]], n: Int,combinacion_anterior: Seq[Seq[Char]] = Seq(Seq())): Seq[Seq[Char]]  = {
      if (sck.head.size + 1 == n) {
        val uniones = combinar_dos_listas(sck, sck.flatten.distinct.map(Seq(_)))
        uniones.filter(oraculo(_))
      } else if (sck.head.size >= n) sck
      else if( sck.head.size * 2 > n)  {
        val uniones = combinar_dos_listas(sck, combinacion_anterior).filter(oraculo(_))
        encontrarSubcadenasVelozAux(uniones, n,sck)
      }
      else {
        val uniones = combinar_dos_listas(sck, sck).filter(oraculo(_))
        encontrarSubcadenasVelozAux(uniones, n,sck)
      }
    }
    if (n == 2) encontrarSubcadenasVelozAux(alfabetoEnForma, n).head
    else if (n % 2 == 0) {
      val cadenas1 = task{encontrarSubcadenasVelozAux(alfabetoEnForma, n / 2)}
      val cadenas2 = task{encontrarSubcadenasVelozAux(alfabetoEnForma, (n - 1 ) / 2)} // se fuerza ser  impares
      val total = combinar_dos_listas(cadenas1.join, cadenas2.join).filter(oraculo(_))
      val uniones = combinar_dos_listas(total, alfabeto.map(Seq(_))).filter(oraculo(_))
      uniones.head
    }
    else {
      val cadenas1 = task {encontrarSubcadenasVelozAux(alfabetoEnForma, n / 2)}
      val cadenas2 = task {encontrarSubcadenasVelozAux(alfabetoEnForma, n - (n / 2))}
      val uniones = combinar_dos_listas(cadenas1.join, cadenas2.join).filter(oraculo(_))
      uniones.head
    }
  }
/**
  * Filtra las cadenas que cumplen con ciertos criterios.
  *
  * @param cadena Cadena a seleccionar.
  * @param subcadenas Lista de subcadenas.
  * @param n n de subcadena a comprobar.
  * @return Booleano que indica si la cadena cumple con los criterios.
  */
  def filtro_cadenas(cadena: Seq[Char], subcadenas: Seq[Seq[Char]], n: Int): Boolean = {
    // Define una función auxiliar recursiva que comprueba si una cadena está incluida en una lista de subcadenas
    def incluye(cadena: Seq[Char], subcadenas: Seq[Seq[Char]]): Boolean = {
      if (subcadenas.isEmpty) false
      else if (subcadenas.head == cadena) true
      else incluye(cadena, subcadenas.tail)
    }
    // Si la cadena tiene el mismo n que el requerido, comprueba si está incluida en la lista de subcadenas
    if (cadena.length == n) incluye(cadena.take(n), subcadenas)
    // Si no, toma una subcadena del mismo n y comprueba si está incluida, y luego llama recursivamente a la función con el resto de la cadena
    else {
      val segmento = cadena.take(n)
      if (incluye(segmento, subcadenas)) filtro_cadenas(cadena.tail, subcadenas, n)
      else false
    }
  }

  /**
  * Algoritmo de búsqueda de subcadenas turbo optimizado.
  *
  * @return Lista de subcadenas halladas.
  */
  def turboMejorada(): Seq[Char]  = {
    // Define una función auxiliar recursiva que encuentra las subcadenas que cumplen el oráculo
    def turboMejoradaAux(sck: Seq[Seq[Char]], n: Int, secuenciaAnterior: Seq[Seq[Char]] = Seq(Seq())): Seq[Seq[Char]]  = {
      if (sck.head.size + 1 == n) {
        val uniones = combinar_dos_listas(sck, sck.flatten.distinct.map(Seq(_)))
        uniones.filter(oraculo(_))
      } else if (sck.head.size >= n) sck
      else if( sck.head.size * 2 > n)  {
        val uniones = combinar_dos_listas(sck, secuenciaAnterior).filter(filtro_cadenas(_, secuenciaAnterior, secuenciaAnterior.head.size)).filter(oraculo(_))
        turboMejoradaAux(uniones, n,sck) 
      }
      else {
        val uniones = combinar_dos_listas(sck, sck).filter(filtro_cadenas(_, sck, sck.head.size)).filter(oraculo(_))
        turboMejoradaAux(uniones,n,sck) 
      }
    }
    val alfabetoEnForma = alfabeto.map(Seq(_))
    // Si el n es 1, selecciona los alfabeto por el oráculo
    if (n == 2) turboMejoradaAux(alfabetoEnForma, n).head
    // Si el n es par, encuentra los alfabeto de la mitad del n y luego los une
    else if (n % 2 == 0) {
      val cadenas1 = task{turboMejoradaAux(alfabetoEnForma, n / 2)} // se fuerza ser  impar
      val cadenas2 = task{turboMejoradaAux(alfabetoEnForma, (n-1)/ 2)}
      val total = combinar_dos_listas(cadenas1.join, cadenas2.join).filter(oraculo(_))
      val uniones = combinar_dos_listas(total, alfabeto.map(Seq(_))).filter(oraculo(_))
      uniones.head
    // Si el n es impar, encuentra los alfabeto de la mitad superior e inferior del n y luego los une
    } else {
      val cadenas1 = task{turboMejoradaAux(alfabetoEnForma, n / 2)}
      val cadenas2 = task{turboMejoradaAux(alfabetoEnForma, n-(n/2))}
      val uniones = combinar_dos_listas(cadenas1.join, cadenas2.join).filter(oraculo(_))
      uniones.head
    }
  }
/**
  * Algoritmo de búsqueda de subcadenas turbo rápida.
  *
  * @return Lista de subcadenas halladas.
  */
  def turboAcelerada(): Seq[Char] = {
  /**
    * Función auxiliar que realiza la reconstrucción de secuencias en paralelo.
    *
    * @param arbol Arbol de sufijos.
    * @param secuencias Secuencias a combinar_dos_listas.
    * @param acumulada Secuencia acumulada.
    * @return Arbol de sufijos actualizado.
    */

    def turboAceleradaAux(t: Trie, secuencias: Seq[Seq[Char]], acumulada: Seq[Char]): Trie = {
        t match {
          case Nodo(valor, esFinal, hijos) =>
            val nuevosHijos = hijos.map { hijo =>
              if (ramaValida(hijo)) turboAceleradaAux(hijo, secuencias, acumulada :+ valor)
              else hijo
            }
            Nodo(valor, esFinal, nuevosHijos)
          case Hoja(valor, esFinal) =>
            if (!esFinal) Nodo(valor, esFinal, List())
            else {
              val cadenasNuevas: Seq[Seq[Char]] = secuencias.map(cadena => (acumulada.filter(_ != ' ') :+ valor) ++ cadena).filter(filtro_cadenas(_, secuencias, secuencias.head.size)).filter(oraculo(_))
              if (cadenasNuevas.isEmpty) Nodo(valor, esFinal, List())
              else {
                agregar_secuencias(cadenasNuevas.map(_.drop((acumulada.filter(_ != ' ') :+ valor).length)), t)
              }
            }
      }
    }

    /**
      * Función auxiliar evaluar_arbol que evalúa y actualiza el árbol de sufijos.
      *
      * @param arbol Arbol de sufijos.
      * @param n n de las secuencias.
      * @return Seq[Seq[Char]] Secuencias encontradas que cumplen con el oráculo.
      */
    def evaluar_arbol(arbol: Trie, n: Int,combinacion_anterior: Seq[Seq[Char]] =Seq(Seq())): Seq[Seq[Char]] = {
      val todas_secuencias_arbol = cadenas_del_arbol(arbol)
      if (todas_secuencias_arbol.isEmpty) Seq(Seq())
      else if (todas_secuencias_arbol.head.length + 1 == n) {
        combinar_dos_listas(todas_secuencias_arbol, alfabeto.map(Seq(_))).filter(oraculo(_))
      }
      else if (todas_secuencias_arbol.head.size == n) todas_secuencias_arbol
      else if( todas_secuencias_arbol.head.size * 2 > n)  {
        val arbol_nuevo = turboAceleradaAux(arbol, combinacion_anterior, Seq())
        evaluar_arbol(arbol_nuevo,n,todas_secuencias_arbol)
      }
      else {
        val arbolNuevo = turboAceleradaAux(arbol, todas_secuencias_arbol, Seq())
        evaluar_arbol(arbolNuevo,n,todas_secuencias_arbol)
      }
    }
    val arbol_inicial = arbolDeSufijos(alfabeto.map(cadena => Seq(cadena)))
    if (n == 2) evaluar_arbol(arbol_inicial, n).head
    else
    if (n % 2 == 0) {
      val cadenas1 = task{evaluar_arbol(arbol_inicial, n / 2)} // se fuerza ser  impar
      val cadenas2 = task{evaluar_arbol(arbol_inicial, (n-1)/ 2)}
      val total = combinar_dos_listas(cadenas1.join, cadenas2.join).filter(oraculo(_))
      val uniones = combinar_dos_listas(total, alfabeto.map(Seq(_))).filter(oraculo(_))
      uniones.head
    } else {
      val c1 = task{evaluar_arbol(arbol_inicial,n/2)}
      val c2 = task{evaluar_arbol(arbol_inicial,n-(n/2))}
      val uniones = combinar_dos_listas(c1.join, c2.join).filter(oraculo(_))
      uniones.head
    }
  }
}


  

