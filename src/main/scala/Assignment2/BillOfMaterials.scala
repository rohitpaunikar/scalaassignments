package Assignment2

import java.io.{File, PrintStream, PrintWriter}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import scala.util.{Failure, Success}

object BillOfMaterials extends App {

  /**
   * prints bill of materials based on the provided list of widgets
   *
   * @param listOfWidgets List of widgets. Its a list of a tuple. Each tuple representing a widget and
   *                      its properties.
   *                      1st Param(TypeOfWidget.Value) - a type of widget
   *                      2nd Param(Int) - position X
   *                      3rd Param(Int) - position Y
   *                      4th Param(Array[Any]) - an Array of any additional parameters required for building the widget
   *                              Below are the additional parameters required for each type of widget
   *                              RECTANGLE - width Int, height Int
   *                              SQUARE - width Int
   *                              ELLIPSE - Horizontal diameter Int, Vertical diameter Int
   *                              CIRCLE - Diameter Int
   *                              TEXTBOX - width Int, height Int, text String(Optional)
   *                              The parameters should be provided in the same sequence and should be of the same type in the array
   *                              as mentioned above for each type of Widget
   */
  def getBillOfMaterials(listOfWidgets: List[(TypeOfWidget.Value,Int,Int,Array[Any])])={
    val widgets = listOfWidgets.map(t => Widget.getWidget(t._1,t._2,t._3,t._4))
    if (widgets.exists(w => w.isFailure)) {
      widgets.filter(_.isFailure).foreach(widget => widget match {
        case Failure(exception) =>
          val errorLogsPrintStream =  new PrintStream(new File(System.getProperty("user.dir") + "\\target\\Error_" + LocalDateTime.now.format(DateTimeFormatter.ofPattern("YYYYMMdd_HHmmss")) + ".log"))
          exception.printStackTrace(errorLogsPrintStream)
          errorLogsPrintStream.close()
      });
      println("+++++Abort+++++")
    }
    else{
      println("----------------------------------------------------------------")
      println("Bill of Materials")
      println("----------------------------------------------------------------")
      widgets.foreach{
        widget =>
          widget match {
            case Success(value) => println(value.toString)
          }
      }
      println("----------------------------------------------------------------")
    }
  }

  getBillOfMaterials(List((TypeOfWidget.RECTANGLE,10,10, Array(30,40)),
                          (TypeOfWidget.SQUARE,15,30, Array(35)),
                          (TypeOfWidget.ELLIPSE,100,150, Array(300,200)),
                          (TypeOfWidget.CIRCLE,1,1, Array(300)),
                          (TypeOfWidget.TEXTBOX,5,5, Array(200,100,"Sample Text"))
                        )
                    )
}
