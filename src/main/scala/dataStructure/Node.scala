package dataStructure

case class Node[T](data: Option[T] = None, next: Option[Node[T]] = None)
