package Model

import dataStructure.VectorOfLists
import dataType.Vector2D



class DataModel private () {
  private var currentType: DataTypes = DataTypes.Double
  private var baseCapacity: Int = 2
  private var doubles: VectorOfLists[Double] = new VectorOfLists[Double](baseCapacity)
  private var vectors2D: VectorOfLists[Vector2D] = new VectorOfLists[Vector2D](baseCapacity)

  def getDoublesVector: VectorOfLists[Double] = doubles

  def getVectors2DVector: VectorOfLists[Vector2D] = vectors2D

  def clearVectors(): Unit = {
    doubles = new VectorOfLists[Double](baseCapacity)
    vectors2D = new VectorOfLists[Vector2D](baseCapacity)
  }

  def changeBaseCapacities(newCapacity: Int): Unit = {
    baseCapacity = newCapacity
    clearVectors()
  }

  def getCurrentType: DataTypes = currentType

  def setCurrentType(`type`: DataTypes): Unit = {
    currentType = `type`
    clearVectors()
  }

  def setDoublesVector(vector: VectorOfLists[Double]): Unit = {
    doubles = vector
    baseCapacity = doubles.getCapacity
  }

  def setVectors2DVector(vector: VectorOfLists[Vector2D]): Unit = {
    vectors2D = vector
    baseCapacity = vectors2D.getCapacity
  }
}

object DataModel {
  private var instance: DataModel = null

  def getInstance: DataModel = {
    if (instance == null)
      instance = new DataModel()
    instance
  }
}

