package controller

import Model.{DataModel, DataTypes}
import dataStructure.VectorOfLists
import dataType.Vector2D
import javafx.collections.ObservableList
import javafx.fxml.{FXML, Initializable}
import javafx.scene.control.{Alert, ChoiceBox, Label, TextField}
import javafx.scene.control.Alert.AlertType
import javafx.scene.control.TextFormatter
import javafx.scene.control.TextFormatter.Change

import java.net.URL
import javafx.stage.FileChooser
import javafx.stage.Stage
import javafx.util.StringConverter

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import java.util.ResourceBundle
import scala.util.{Failure, Success, Try}

class AppController extends Initializable {

  @FXML
  var label: Label = _
  @FXML
  private var addInput: TextField = _
  @FXML
  private var addRandomInput: TextField = _
  @FXML
  private var deleteInput: TextField = _
  @FXML
  private var insertInput: TextField = _
  @FXML
  private var changeBaseCapacityInput: TextField = _
  @FXML
  private var dataTypeInput: ChoiceBox[String] = _

  private val model: DataModel = DataModel.getInstance

  override def initialize(location: URL, resources: ResourceBundle): Unit = {
    setFormatters()
    initializeDataTypeInput()
  }

  def add(): Unit = {
    val text = addInput.getText
    if (text.nonEmpty) {
      Try {
        if (model.getCurrentType == DataTypes.Double)
          model.getDoublesVector.add(text.toDouble)
        else
          model.getVectors2DVector.add(Vector2D.parseVector2d(text))
        updateText()
      } match {
        case Failure(exception) =>
          showErrorMessage(exception.getMessage)
        case Success(_) =>
      }
    }
  }

  def delete(): Unit = {
    val text = deleteInput.getText
    if (text.nonEmpty) {
      Try {
        if (model.getCurrentType == DataTypes.Double)
          model.getDoublesVector.delete(text.toInt)
        else
          model.getVectors2DVector.delete(text.toInt)
        updateText()
      } match {
        case Failure(exception) =>
          showErrorMessage(exception.getMessage)
        case Success(_) =>
      }
    }
  }

  def addRandom(): Unit = {
    val text = addRandomInput.getText
    if (text.nonEmpty) {
      Try {
        val n = text.toInt
        println(n, "n")
        val random = scala.util.Random

        for (_ <- 0 until n) {
          if (model.getCurrentType == DataTypes.Double)
            model.getDoublesVector.add(random.nextInt(100).toDouble)
          else
            model.getVectors2DVector.add(new Vector2D(random.nextInt(100).toDouble, random.nextInt(100).toDouble))
        }
        updateText()
      } match {
        case Failure(exception) =>
          showErrorMessage(exception.getMessage)
        case Success(_) =>
      }
    }
  }

  def insert(): Unit = {
    val posStr = insertInput.getText
    val valStr = addInput.getText
    if (posStr.nonEmpty && valStr.nonEmpty) {
      Try {
        if (model.getCurrentType == DataTypes.Double)
          model.getDoublesVector.insert(posStr.toInt, valStr.toDouble)
        else
          model.getVectors2DVector.insert(posStr.toInt, Vector2D.parseVector2d(valStr))
        updateText()
      } match {
        case Failure(exception) =>
          showErrorMessage(exception.getMessage)
        case Success(_) =>
      }
    }
  }

  def sort(): Unit = {
    if (model.getCurrentType == DataTypes.Double) {
      model.getDoublesVector.sort
    } else {
      model.getVectors2DVector.sort
    }
    updateText()
  }

  def clear(): Unit = {
    model.clearVectors()
    updateText()
  }

  def changeBaseCapacity(): Unit = {
    val text = changeBaseCapacityInput.getText
    if (text.nonEmpty) {
      Try {
        model.changeBaseCapacities(text.toInt)
        updateText()
      } match {
        case Failure(exception) =>
          showErrorMessage(exception.getMessage)
        case Success(_) =>
      }
    }
  }

  def saveVector(): Unit = {
    Try {
      val fileChooser = new FileChooser
      fileChooser.setTitle("Save Vector")
      val file = fileChooser.showSaveDialog(new Stage)
      if (file == null)
        return
      val fileOut = new FileOutputStream(file, false)
      val out = new ObjectOutputStream(fileOut)
      if (model.getCurrentType == DataTypes.Double)
        out.writeObject(model.getDoublesVector)
      else
        out.writeObject(model.getVectors2DVector)
      out.close()
    } match {
      case Failure(exception) =>
        showErrorMessage(exception.getMessage)
      case Success(_) =>
    }
  }

  def openVector(): Unit = {
    Try {
      val fileChooser = new FileChooser
      fileChooser.setTitle("Load Vector")
      val file = fileChooser.showOpenDialog(new Stage)
      if (file == null)
        return
      val fileIn = new FileInputStream(file)
      val in = new ObjectInputStream(fileIn)
      val obj = in.readObject
      in.close()

      model.getCurrentType match {
        case DataTypes.Double =>
          obj match {
            case vector: VectorOfLists[Double] =>
              model.setDoublesVector(vector)
            case _ =>
              throw new IllegalArgumentException("Invalid vector type for Doubles")
          }
        case DataTypes.Vector2D =>
          obj match {
            case vector: VectorOfLists[Vector2D] =>
              model.setVectors2DVector(vector)
            case _ =>
              throw new IllegalArgumentException("Invalid vector type for Vector2D")
          }
      }
      updateText()
    } match {
      case Failure(exception) =>
        showErrorMessage(exception.getMessage)
      case Success(_) =>
    }
  }

  private def updateText(): Unit = {
    if (model.getCurrentType == DataTypes.Double)
      label.setText(model.getDoublesVector.toString)
    else
      label.setText(model.getVectors2DVector.toString)
  }

  private def initializeDataTypeInput(): Unit = {
    val items: ObservableList[String] = dataTypeInput.getItems
    items.addAll(DataTypes.Double.toString, DataTypes.Vector2D.toString)
    dataTypeInput.setValue(model.getCurrentType.toString)
    dataTypeInput.valueProperty.addListener((_, oldValue, newValue) => {
      val newType: DataTypes = newValue match {
        case "Double" => DataTypes.Double
        case "Vector2D" => DataTypes.Vector2D
        case _ => throw new NoSuchElementException(s"No value found for $newValue")
      }
      if (newType == DataTypes.Double)
        addInput.setTextFormatter(createDoubleTextFormatter())
      else
        addInput.setTextFormatter(createVector2DTextFormatter())
      model.setCurrentType(newType)
      updateText()
    })
  }

  private def setFormatters(): Unit = {
    if (model.getCurrentType == DataTypes.Double)
      addInput.setTextFormatter(createDoubleTextFormatter())
    else
      addInput.setTextFormatter(createVector2DTextFormatter())
    insertInput.setTextFormatter(createNumberTextFormatter())
    deleteInput.setTextFormatter(createNumberTextFormatter())
    addRandomInput.setTextFormatter(createNumberTextFormatter())
    changeBaseCapacityInput.setTextFormatter(createNumberTextFormatter())
  }

  private def createDoubleTextFormatter(): TextFormatter[String] = {
    new TextFormatter(new StringConverter[String] {
      override def fromString(string: String): String = string

      override def toString(value: String): String = {
        if (value != null && value.matches("-?\\d*\\.?\\d*")) value else ""
      }
    })
  }

  private def createNumberTextFormatter(): TextFormatter[String] = {
    new TextFormatter(new StringConverter[String] {
      override def fromString(string: String): String = string

      override def toString(value: String): String = {
        if (value != null && value.matches("\\d*")) value else ""
      }
    })
  }

  private def createVector2DTextFormatter(): TextFormatter[String] = {
    new TextFormatter(new StringConverter[String] {
      override def fromString(string: String): String = string

      override def toString(value: String): String = {
        if (value != null && value.matches("-?\\d+(\\.\\d+)?,?-?\\d+(\\.\\d+)?")) value else ""
      }
    })
  }

  private def showErrorMessage(message: String): Unit = {
    val alert = new Alert(AlertType.INFORMATION)
    alert.setTitle("Error")
    alert.setHeaderText(message)
    alert.showAndWait()
  }
}