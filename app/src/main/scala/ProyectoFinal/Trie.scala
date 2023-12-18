package ProyectoFinal

import scala.annotation.tailrec


// Definición abstracta de la clase Trie y sus casos Nodo y Hoja
abstract class Trie
case class Nodo(car: Char, marcada: Boolean, hijos: List[Trie]) extends Trie
case class Hoja(car: Char, marcada: Boolean) extends Trie

// Objeto Trie con métodos y funciones relacionadas
object Trie {

  // Obtiene el carácter de la raíz de un Trie
  def raiz(t: Trie): Char = {
    t match {
      case Nodo(c, _, _) => c
      case Hoja(c, _) => c
    }
  }


  // Obtiene los carácteres de las cabezas de los hijos de un Trie
  def cabezas(t: Trie): Seq[Char] = {
    t match {
      case Nodo(_, _, hijos) => hijos.map(t => raiz(t))
      case Hoja(c, _) => Seq[Char](c)
    }
  }



  /**
   * Verifica si una cadena pertenece a un árbol de sufijos.
   *
   * @param s Cadena a verificar.
   * @param t Árbol de sufijos.
   * @return Booleano que indica si la cadena pertenece al árbol.
   */
  @tailrec
  def pertenece(s: Seq[Char], t: Trie): Boolean = {
    t match {
      case Nodo(car, marcada, hijos) => {
        if (s.isEmpty) marcada
        else {
          val cabeza = s.head
          val cola = s.tail
          val hijosCabeza = hijos.filter(hijo => raiz(hijo) == cabeza)
          if (hijosCabeza.isEmpty) false
          else pertenece(cola, hijosCabeza.head)
        }
      }
      case Hoja(car, marcada) => {
        if (s.isEmpty) marcada
        else false
      }
    }
  }

  /**
   * Agrega una cadena al árbol de sufijos.
   *
   * @param c Cadena a agregar.
   * @param t Árbol de sufijos.
   * @return Árbol de sufijos modificado.
   */
   def agregar(c: Seq[Char], t: Trie): Trie = {
    t match {
      case Nodo(car, marcada, hijos) => {
        if (c.isEmpty) Nodo(car, true, hijos)
        else {
          val cabeza = c.head
          val cola = c.tail
          val hijosCabeza = hijos.filter(hijo => raiz(hijo) == cabeza)
          if (hijosCabeza.isEmpty) {
            val nuevoHijo = if (cola.isEmpty) Hoja(cabeza, true) else agregar(cola, Nodo(cabeza, false, List()))
            Nodo(car, marcada, nuevoHijo :: hijos)
          } else {
            val nuevoHijo = agregar(cola, hijosCabeza.head)
            val otrosHijos = hijos.filter(hijo => raiz(hijo) != cabeza)
            Nodo(car, marcada, nuevoHijo :: otrosHijos)
          }
        }
      }
      case Hoja(car, marcada) => {
        if (c.isEmpty) Hoja(car, true)
        else {
          val cabeza = c.head
          val cola = c.tail
          if (car == cabeza) Nodo(car, marcada, List(if (cola.isEmpty) Hoja(cabeza, true) else agregar(cola, Nodo(cabeza, false, List()))))
          else Nodo(car, marcada, List(if (cola.isEmpty) Hoja(cabeza, true) else agregar(cola, Nodo(cabeza, false, List()))))
        }
      }
    }
  }

  /**
   * Agrega una secuencia de cadenas al árbol de sufijos.
   *
   * @param ss Lista de secuencias a agregar.
   * @param t Árbol de sufijos.
   * @return Árbol de sufijos modificado.
   */
  @tailrec
  def agregar_secuencias(ss: Seq[Seq[Char]], t: Trie): Trie = {
    if (ss.isEmpty) t
    else agregar_secuencias(ss.tail, agregar(ss.head, t))
  }

  /**
   * Construye un árbol de sufijos a partir de una lista de secuencias de cadenas.
   *
   * @param ss Lista de secuencias de cadenas.
   * @return Árbol de sufijos construido.
   */
  def arbolDeSufijos(ss: Seq[Seq[Char]]): Trie = {
    val t: Trie = agregar_secuencias(ss, Nodo(' ', false, List()))
    t
  }

  /**
   * Función auxiliar ramaValida que verifica si una rama tiene hijos.
   *
   * @param t: Trie: Nodo o Hoja del árbol de sufijos.
   * @return Boolean: true si la rama tiene hijos, false en caso contrario.
   */
  def ramaValida(t: Trie): Boolean = {
    t match {
      case Nodo(_, _, hijos) => hijos.nonEmpty
      case Hoja(_, _) => true
    }
  }

  /**
   * Recorre un árbol de sufijos y obtiene todas las secuencias presentes.
   *
   * @param t Árbol de sufijos.
   * @return Lista de secuencias presentes en el árbol.
   */
  def cadenas_del_arbol(t: Trie): Seq[Seq[Char]] = {
    t match {
      case Nodo(c, _, hijos) =>
        val secuenciasHijos = hijos.flatMap(hijo => cadenas_del_arbol(hijo))
        secuenciasHijos.map(secuencia => c +: secuencia).map(_.filter(_ != ' '))
      case Hoja(c, _) => Seq(Seq(c))
    }
  }
}