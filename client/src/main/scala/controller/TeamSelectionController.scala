package controller

import java.net.URL
import java.util.ResourceBundle

import javafx.event.ActionEvent
import javafx.fxml.{FXML, Initializable}
import view.ApplicationView
import ApplicationView.viewSelector._
import javafx.scene.control.Label

object TeamSelectionController extends Initializable with ViewController {
  val controller: ViewController = this

  @FXML var idLabel: Label = _
  var username: String = "guest"

  @FXML def handleLogout(event: ActionEvent) {
    println("Logout pressed")

    ApplicationView changeView LOGIN
  }

  override def initialize(location: URL, resources: ResourceBundle): Unit = {
    idLabel setText username
  }
}
