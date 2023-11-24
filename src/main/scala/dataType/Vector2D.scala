package dataType

import scala.math.sqrt

class Vector2D(var x: Double, var y: Double) extends Comparable[Vector2D] with Serializable {


  def calculateMagnitude: Double = sqrt(x * x + y * y)

  def normalize(): Unit = {
    val magnitude = calculateMagnitude
    if (magnitude != 0) {
      x /= magnitude
      y /= magnitude
    }
  }

  def add(other: Vector2D): Vector2D = new Vector2D(x + other.x, y + other.y)

  def subtract(other: Vector2D): Vector2D = new Vector2D(x - other.x, y - other.y)

  override def compareTo(other: Vector2D): Int = {
    val thisMagnitude = calculateMagnitude
    val otherMagnitude = other.calculateMagnitude
    if (thisMagnitude < otherMagnitude) -1
    else if (thisMagnitude > otherMagnitude) 1
    else 0
  }

  override def toString: String = s"($x, $y)"
}

object Vector2D {
  def parseVector2d(input: String): Vector2D = {
    try {
      val parts = input.split(",")

      if (parts.length != 2) {
        throw new IllegalArgumentException("Input must contain two values separated by a delimiter.")
      }

      val x = parts(0).trim.toDouble
      val y = parts(1).trim.toDouble

      new Vector2D(x, y)
    } catch {
      case _: NumberFormatException =>
        throw new IllegalArgumentException("Invalid input format. Must be in the format 'x, y'.")
    }
  }
}

