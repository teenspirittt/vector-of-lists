package dataStructure

class VectorOfLists[T <: Comparable[T]](capacity: Int) extends Serializable {

  private var lists: Array[Node[T]] = Array.fill(1)(null)
  private val listCapacity: Int = if (capacity <= 0) 1 else capacity
  private val vectorSize: Int = 1
  private var size: Int = 0

}
