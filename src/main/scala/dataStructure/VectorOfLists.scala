package dataStructure

import scala.collection.mutable.ArrayBuffer
import java.io.{IOException, ObjectInputStream, ObjectOutputStream}


class VectorOfLists[T: Ordering](capacity: Int) extends Serializable {

  private var lists: ArrayBuffer[Node[T]] = ArrayBuffer(Node[T]())
  private var listCapacity: Int = if (capacity <= 0) 1 else capacity
  private var vectorSize: Int = 1
  private var size: Int = 0

  def getSize: Int = size

  def getVectorSize: Int = vectorSize

  def getCapacity: Int = capacity

  private def getIndexOfList(itemIndex: Int): Int = {
    if (itemIndex < 0)
      throw new IllegalArgumentException("Index cannot be negative")
    if (itemIndex >= size)
      throw new IllegalArgumentException("Invalid index")

    var n = 0
    while (n < vectorSize) {
      val maxSize = math.pow(2, n + 1).toInt - 1 * listCapacity
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
    val n = getIndexOfList(index)
    val startIndex = getMaxSize(n)

    val data: Option[T] = if (startIndex == index) {
      // We need to delete the head item
      val headData = lists(n).data
      lists(n) = lists(n).next.getOrElse(Node(Option.empty[T]))
      headData
    } else {
      var current = lists(n)
      val indexOfPrevious = index - 1
      for (_ <- startIndex until indexOfPrevious) {
        current = current.next.getOrElse(Node(Option.empty[T]))
      }
      val deletedData = current.next.flatMap(_.data)
      val newNext = current.next.flatMap(_.next)
      current = current.copy(next = newNext)
      deletedData
    }

    // Now we need to shift left everything ahead by 1 element
    shiftLeft(n)
    size -= 1
    data
  }

  private def shiftLeft(n: Int): Unit = {
    var current: Node[T] = null

    for (i <- n until vectorSize - 1) {
      current = lists(i)

      while (current.next.isDefined) {
        current = current.next.get
      }

      current = current.copy(next = Some(lists(i + 1)))

      val nextNext = current.next.get.next
      current = current.copy(next = current.next.map(_.copy(next = None)))
      lists(i + 1) = nextNext.getOrElse(null.asInstanceOf[Node[T]])
    }

    if (lists(vectorSize - 1) == null)
      decreaseVectorSize()
  }

  private def decreaseVectorSize(): Unit = {
    val newVector = lists.dropRight(1)
    lists = newVector
    vectorSize -= 1
  }

  private def shiftRight(n: Int): Unit = {
    if (size == getMaxSize(vectorSize))
      increaseVectorSize()

    for (i <- (n until vectorSize - 1).reverse) {
      var current = lists(i)
      while (current.next.isDefined && current.next.get.next.isDefined) {
        current = current.next.get
      }

      val remainder = current.next.getOrElse(Node[T](Option.empty[T]))
      current = current.copy(next = None)
      lists(i + 1) = remainder.copy(next = Some(lists(i + 1)))
    }
  }

  def insert(index: Int, item: T): Unit = {
    var newNode = Node(Some(item))
    val n = getIndexOfList(index)
    val startIndex = getMaxSize(n)

    if (index == startIndex) {
      newNode = newNode.copy(next = Some(lists(n)))

      lists(n) = newNode
    } else {
      var current = lists(n)
      val indexOfPrevious = index - 1

      for (i <- startIndex until indexOfPrevious) {
        current = current.next.getOrElse(null.asInstanceOf[Node[T]])
      }
      newNode = newNode.copy(next = current.next)
      current = current.copy(next = Some(newNode))
    }

    shiftRight(n)
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
    val builder = new StringBuilder("[")
    for (i <- 0 until vectorSize) {
      builder.append("Doubles Vector: [")
      val currentList = lists(i)
      var current = currentList
      while (current != null) {
        builder.append(current.data)
        if (current.next != null) {
          builder.append(", ")
        }
        current.next match {
          case Some(nextNode) => current = nextNode
          case None => throw new NoSuchElementException("Next element is null")
        }
      }
      builder.append("]")
      if (i != vectorSize - 1) {
        builder.append("\n")
      }
    }
    builder.append("]")
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
    out.writeInt(listCapacity)
    out.writeInt(size)

    for {
      i <- 0 until vectorSize
      current = lists(i)
    } {
      var currentNode = current
      while (currentNode != null) {
        out.writeObject(currentNode.data)
        currentNode = currentNode.next.getOrElse(null.asInstanceOf[Node[T]])
      }
    }
    out.flush()
  }

/*  private def readObject(in: ObjectInputStream): Unit = {
    listCapacity = in.readInt()
    val size = in.readInt()
    lists = new ArrayBuffer[Node[T]](1)
    vectorSize = 1

    for (_ <- 0 until size) {
      add(in.readObject().asInstanceOf[T])
    }
  }*/

  private def combineLists(): Unit = {
    for (i <- 0 until vectorSize - 1) {
      var current = lists(i)

      while (current.next.isDefined)
        current = current.next.getOrElse(null.asInstanceOf[Node[T]])

      current = current.copy(next = Some(lists(i + 1)))
    }
  }

  private def uncombineLists(): Unit = {
    var current: Node[T] = null.asInstanceOf[Node[T]]
    var max: Int = 0

    for (i <- 0 until vectorSize - 1) {
      max = Math.pow(2, i).toInt * listCapacity
      current = lists(i)

      for (_ <- 0 until max - 1)
        current = current.next.getOrElse(null.asInstanceOf[Node[T]])

      lists(i + 1) = current.next.getOrElse(null.asInstanceOf[Node[T]])
      current = current.copy(next = None)
    }
  }

  private def split(list: Node[T], size: Int): (Node[T], Node[T]) = {
    if (size <= 1 || list == null)
      (list, null)
    else {
      var current = list
      for (_ <- 0 until size / 2 - 1)
        current = current.next.getOrElse(null.asInstanceOf[Node[T]])

      val secondHalf = current.next.getOrElse(null.asInstanceOf[Node[T]])
      current = current.copy(next = None)
      (list, secondHalf)
    }
  }

  private def merge(first: Option[Node[T]], second: Option[Node[T]]): Option[Node[T]] = {
    (first, second) match {
      case (Some(f), None) => Some(f)
      case (None, Some(s)) => Some(s)
      case (Some(f), Some(s)) =>
        (f.data, s.data) match {
          case (Some(fd), Some(sd)) =>
            if (Ordering[T].lt(fd, sd)) {
              val mergedNext = merge(f.next, second)
              Some(Node(Some(fd), mergedNext))
            } else {
              val mergedNext = merge(first, s.next)
              Some(Node(Some(sd), mergedNext))
            }
          case _ => None
        }
      case _ => None
    }
  }


  private def sortFunctional(list: Option[Node[T]]): Option[Node[T]] = {
    list.flatMap { l =>
      if (l.next.isEmpty) Some(l)
      else {
        val (first, second) = split(l, size)
        merge(sortFunctional(Some(first)), sortFunctional(Some(second)))
      }
    }
  }

  def sortFunctional(): Unit = {
    if (size > 1) {
      combineLists()
      lists(0) = sortFunctional(Some(lists(0))).getOrElse(null.asInstanceOf[Node[T]])
      uncombineLists()
    }
  }
}
