package Assignment1.Problem3

import java.io.File

import scala.io.Source


object MaxSumTriange extends App {

  /**
   * Returns the max total possible by starting at the top of the triangle and moving to adjacent numbers
   * on the row below
   * @param inputTriange Vector of rows of a triangle
   * @return
   */
  def getMaxTotal(inputTriange: Vector[Vector[Int]]): Int ={
    def sumElements(previousRowSum:Vector[Int], currentRow: Vector[Int], rowNum:Int): Vector[Int]  = {
        val currentRowSum =
          for (current <- 0 until currentRow.length;
               indexOfAdjacentElemsInPreviousRow = Array(current,current+1);
               maxElemInPrevRow = findMaxElement(indexOfAdjacentElemsInPreviousRow,previousRowSum)) yield currentRow(current) + maxElemInPrevRow
        if (rowNum == 0) currentRowSum.toVector
        else sumElements(currentRowSum.toVector, inputTriange(rowNum-1), rowNum-1)
    }

    def findMaxElement(adjacentIndexes:Array[Int], previousRowSum: Vector[Int]): Int = {
      if (previousRowSum(adjacentIndexes(0)) > previousRowSum(adjacentIndexes(1))) previousRowSum(adjacentIndexes(0))
      else previousRowSum(adjacentIndexes(1))
    }

    val numOfRowsOfTriangle = inputTriange.length
    sumElements(inputTriange(numOfRowsOfTriangle - 1), inputTriange(numOfRowsOfTriangle - 2), numOfRowsOfTriangle - 2).sortWith(_>_).head
  }

  val input = Source.fromFile(new File(System.getProperty("user.dir") + "\\src\\resources\\triangle.txt")).getLines().
              map(s => s.split(" ").map(s => s.toInt).toVector).toVector
  println(getMaxTotal(input))
}
