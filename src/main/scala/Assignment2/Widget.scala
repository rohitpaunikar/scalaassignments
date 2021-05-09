package Assignment2

import scala.util.{Failure, Success, Try}

/**
 * An interface for widgets
 * Each widget would have position X and position Y i.e. the start position on XY axis while building it
 * on the canvas
 */
trait Widget {
  val posX:Int
  val posY:Int

  override def toString: String = this.getClass.getSimpleName + s" (${posX},${posY}) "
}

/**
 * An helper class for Widget
 */
object Widget{

  val CANVAS_WIDTH = 1000
  val CANVAS_HEIGHT = 1000

  /**
   * checks if the size parameters are positive
   * @param size number of parameters
   * @param params
   * @return true if the parameters are valid
   *         false if the parameters are invalid
   */
  def IsSizeParamsValid(size:Int, params: Array[Any]):Boolean = {
    if (params == null || params.length < size) false
    else {
      val isValid =
        for (i <- 0 until size; if (params(i).asInstanceOf[Int] <= 0)) yield false
      !isValid.exists(_ == false)
    }
  }

  /**
   * Factory method for an instance of a widget
   * @param typeOfWidget an enum with the available types of widgets
   * @param posX Position X
   * @param posY Position Y
   * @param params an Array of any additional parameters required for building the widget
   *        Below are the additional parameters required for each type of widget
   *        RECTANGLE - width Int, height Int
   *        SQUARE - width Int
   *        ELLIPSE - Horizontal diameter Int, Vertical diameter Int
   *        CIRCLE - Diameter Int
   *        TEXTBOX - width Int, height Int, text String(Optional)
   *        The parameters should be provided in the same sequence and should be of the same type in the array
   *        as mentioned above for each type of Widget
   * @return an instance of the widget
   */
  def getWidget(typeOfWidget: TypeOfWidget.Value, posX:Int, posY:Int, params: Array[Any]): Try[Widget] = {
    if (posX < 0 || posY < 0) Failure(new IllegalArgumentException(s"Position X and Position Y should be positive. Provided Values Position X - ${posX}, Position Y - ${posY}"))
    try {
      typeOfWidget match {
        case TypeOfWidget.RECTANGLE =>
          Success(Rectangle(posX, posY, params(0).asInstanceOf[Int], params(1).asInstanceOf[Int]))

        case TypeOfWidget.SQUARE =>
          Success(Square(posX, posY, params(0).asInstanceOf[Int]))

        case TypeOfWidget.ELLIPSE =>
          Success(Ellipse(posX, posY, params(0).asInstanceOf[Int], params(1).asInstanceOf[Int]))

        case TypeOfWidget.CIRCLE =>
          Success(Circle(posX, posY, params(0).asInstanceOf[Int]))

        case TypeOfWidget.TEXTBOX =>
          Success(TextBox(posX, posY, params(0).asInstanceOf[Int], params(1).asInstanceOf[Int], params(2).asInstanceOf[String]))
      }
    } catch {
      case e:Exception => Failure(e)
    }
  }
}

/**
 * Enum containing the type of widgets
 */
object TypeOfWidget extends Enumeration{
  val RECTANGLE,SQUARE, ELLIPSE, CIRCLE, TEXTBOX = Value;
}

protected case class Rectangle(posX: Int, posY:Int, width:Int, height:Int) extends Widget{
  override def toString: String = super.toString + s"width=${width} height=${height}"
}
object Rectange{
  def apply(posX: Int, posY: Int, width: Int, height: Int): Rectangle = {
    if (((posX + width) > Widget.CANVAS_WIDTH ) || ((posY+height) > Widget.CANVAS_HEIGHT) || !Widget.IsSizeParamsValid(2, Array(width,height)))
      throw new IllegalArgumentException(s"Invalid values for Rectangle's parameters. Supplied values posX - ${posX}, posY - ${posY}, width - ${width}, height - ${height}")
    else new Rectangle(posX, posY, width, height)
  }
}


protected case class Square(posX: Int, posY:Int, size:Int) extends Widget{
  override def toString: String = super.toString + s"size=${size}"
}
object Square{
  def apply(posX: Int, posY: Int, size: Int): Square = {
    if (((posX + size) > Widget.CANVAS_WIDTH ) || ((posY+size) > Widget.CANVAS_HEIGHT) || !Widget.IsSizeParamsValid(1, Array(size)))
      throw new IllegalArgumentException(s"Invalid values for Square's parameters. Supplied values posX - ${posX}, posY - ${posY}, size - ${size}")
    else new Square(posX, posY, size)
  }
}


protected case class Ellipse(posX:Int, posY:Int,diameterH:Int, diameterV:Int) extends Widget{
  override def toString: String = super.toString + s"diameterH=${diameterH} diameterV=${diameterV}"
}
object Ellipse{
  def apply(posX: Int, posY: Int, diameterH: Int, diameterV: Int): Ellipse = {
    if (((posX + diameterH) > Widget.CANVAS_WIDTH )  || ((posY+diameterV) > Widget.CANVAS_HEIGHT) || !Widget.IsSizeParamsValid(2, Array(diameterH,diameterV)))
      throw new IllegalArgumentException(s"Invalid values for Ellipse's parameters. Supplied values posX - ${posX}, posY - ${posY}, diameterH - ${diameterH}, diameterV - ${diameterV}")
    new Ellipse(posX, posY, diameterH, diameterV)
  }
}


protected case class Circle(posX:Int, posY:Int,size:Int) extends Widget{
  override def toString: String = super.toString + s"size=${size}"
}
object Circle{
  def apply(posX: Int, posY: Int, size: Int): Circle = {
    if (((posX + size) > Widget.CANVAS_WIDTH ) || ((posY+size) > Widget.CANVAS_HEIGHT) || !Widget.IsSizeParamsValid(1, Array(size)))
      throw new IllegalArgumentException(s"Invalid values for Circle's parameters. Supplied values posX - ${posX}, posY - ${posY}, size - ${size}")
    else new Circle(posX, posY, size)
  }
}


protected case class TextBox(posX:Int, posY:Int, width:Int, height:Int, text:String) extends Widget{
  override def toString: String = super.toString + s"width=${width} height=${height}  text=" + "\"" + text + "\""
}
object TextBox{
  def apply(posX: Int, posY: Int, width: Int, height: Int, text: String): TextBox = {
    if (((posX + width) > Widget.CANVAS_WIDTH ) || ((posY+height) > Widget.CANVAS_HEIGHT) || !Widget.IsSizeParamsValid(2, Array(width,height)))
      throw new IllegalArgumentException(s"Invalid values for TextBox's parameters. Supplied values posX - ${posX}, posY - ${posY}, width - ${width}, height - ${height}")
    else new TextBox(posX, posY, width, height, text)
  }
}
