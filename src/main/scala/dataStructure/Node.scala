package dataStructure

case class Node[T <: Comparable[T]](data: T, next: Option[Node[T]] = None)
