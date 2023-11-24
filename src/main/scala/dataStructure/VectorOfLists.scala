package dataStructure

import scala.collection.mutable.ArrayBuffer
import java.io.{IOException, ObjectInputStream, ObjectOutputStream}


class VectorOfLists[T: Ordering](capacity: Int) extends Serializable {

  private var lists: ArrayBuffer[Node[T]] = ArrayBuffer(Node[T]())
  var listCapacity: Int = if (capacity <= 0) 1 else capacity
  var vectorSize: Int = 1
  var size: Int = 0


  def getCapacity: Int = capacity

  private def getIndexOfList(itemIndex: Int): Int = {
    if (itemIndex < 0)
      throw new IllegalArgumentException("Index cannot be negative")
    if (itemIndex >= size)
      throw new IllegalArgumentException("Invalid index")

    var n = 0
    while (n < vectorSize) {
      val maxSize = (math.pow(2, n + 1).toInt - 1) * listCapacity
      if (maxSize > itemIndex)
        return n
      n += 1
    }

    if (n == vectorSize)
      throw new IllegalArgumentException("Index out of range")

    n
  }

  def add(item: T): Boolean = {
    // Создаем новый узел
    val newNode = Node(Option(item))

    // Получаем максимальный размер текущего вектора
    val maxSize = getMaxSize(vectorSize)

    // Проверяем, если текущий размер равен максимальному размеру
    if (size == maxSize) {
      // Текущий список полон, добавляем новый
      increaseVectorSize()
      lists(vectorSize - 1) = newNode
    } else {
      // Добавляем в последний список
      val lastList = lists(vectorSize - 1)

      // Проверяем, если последний список пустой
      if (lastList.data.isEmpty) {
        // Первый элемент в последний список
        lists(vectorSize - 1) = lastList.copy(data = Option(item))
      } else {
        // Добавляем новый узел в конец последнего списка
        lastList.next match {
          case Some(existingNode) =>
            // Если есть, добавляем новый узел в конец текущего списка
            lists(vectorSize - 1) = lastList.copy(next = Option(addToNode(existingNode, newNode)))

          case None =>
            // Если у последнего списка нет следующего узла, добавляем новый узел как следующий
            lists(vectorSize - 1) = lastList.copy(next = Option(newNode))
        }

        // Рекурсивная функция для добавления нового узла в конец списка
        def addToNode(node: Node[T], newNode: Node[T]): Node[T] = {
          node.next match {
            case Some(existingNode) => node.copy(next = Option(addToNode(existingNode, newNode)))
            case None => node.copy(next = Option(newNode))
          }
        }
      }
    }
    // Увеличиваем общий размер
    size += 1
    true
  }

  private def increaseVectorSize(): Unit = {
    lists += Node[T](Option.empty[T])
    vectorSize += 1
  }

  private def getMaxSize(vectorSize: Int): Int = {
    (math.pow(2, vectorSize).toInt - 1) * listCapacity
  }

  def delete(index: Int): Option[T] = {
    val flattenedList = toList
    if (index >= 0 && index < flattenedList.length) {
      val (prefix, suffix) = flattenedList.splitAt(index)
      val element = suffix.head
      val updatedList = prefix ++ suffix.tail
      rebuildStructure(updatedList)
      Some(element)
    } else {
      None
    }
  }

  private def toList: List[T] = {
    lists.flatMap(nodeToList).toList
  }

  private def nodeToList(node: Node[T]): List[T] = {
    node.data.toList ++ node.next.map(nodeToList).getOrElse(List.empty)
  }

  private def rebuildStructure(list: Iterable[T]): Unit = {
    lists = ArrayBuffer(Node[T](Option.empty[T]))
    vectorSize = 1
    size = 0

    list.foreach(add)
  }

  def insert(index: Int, item: T): Boolean = {
    val flattenedList = toList
    if (index >= 0 && index <= flattenedList.length) {
      val (prefix, suffix) = flattenedList.splitAt(index)
      rebuildStructure(prefix ++ List(item) ++ suffix)
      true
    } else {
      false
    }
  }

  def forEach(action: Option[T] => Unit): Unit = {
    for {
      i <- 0 until vectorSize
      current = lists(i)
    } {
      var currentNode = current
      while (currentNode != null) {
        action(currentNode.data)
        currentNode = currentNode.next.getOrElse(null.asInstanceOf[Node[T]])
      }
    }
  }

  override def toString: String = {
    val builder = new StringBuilder

    for ((list, index) <- lists.zipWithIndex) {
      builder.append(s"$index = ${nodeToString(list)}\n")
    }
    builder.toString()
  }

  private def nodeToString(node: Node[T]): String = {
    val builder = new StringBuilder
    var current = Option(node)

    while (current.isDefined) {
      builder.append(s"${current.get.data.getOrElse("None")}")

      if (current.get.next.isDefined) {
        builder.append(" -> ")
        current = current.get.next
      } else {
        current = None
      }
    }
    builder.toString()
  }

  def get(index: Int): Option[T] = {
    val n: Int = getIndexOfList(index)
    val startIndex: Int = getMaxSize(n)
    var current: Node[T] = lists(n)
    for (i <- startIndex until index) {
      current = current.next.getOrElse(throw new NoSuchElementException("Index out of bounds"))
    }
    current.data
  }

  private def writeObject(out: ObjectOutputStream): Unit = {
    out.defaultWriteObject()
    out.writeObject(lists.toArray)
  }

  private def readObject(in: ObjectInputStream): Unit = {
    in.defaultReadObject()
    val array = in.readObject().asInstanceOf[Array[Node[T]]]
    lists = ArrayBuffer(array: _*)
    vectorSize = lists.length
  }

  def sort(): Unit = {
    val flattenedList = toList
    val sortedList = mergeSort(flattenedList)
    rebuildStructure(sortedList)
  }

  private def mergeSort(list: List[T]): List[T] = {
    val n = list.length
    if (n <= 1) {
      list
    } else {
      val mid = n / 2
      val (left, right) = list.splitAt(mid)
      merge(mergeSort(left), mergeSort(right))
    }
  }

  private def merge(left: List[T], right: List[T]): List[T] = (left, right) match {
    case (Nil, _) => right
    case (_, Nil) => left
    case (leftHead :: leftTail, rightHead :: rightTail) =>
      if (implicitly[Ordering[T]].lt(leftHead, rightHead)) {
        leftHead :: merge(leftTail, right)
      } else {
        rightHead :: merge(left, rightTail)
      }
  }
}
