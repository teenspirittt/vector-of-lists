package Model

sealed trait DataTypes
object DataTypes {
  case object Double extends DataTypes
  case object Vector2D extends DataTypes

  def withName(name: String): Option[DataTypes] = {
    List(Double, Vector2D).find(_.toString == name)
  }
}
