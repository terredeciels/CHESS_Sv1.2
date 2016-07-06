package model2

import scala.Array._
import scala.collection.immutable._

class Builders {
  //type Case = (Int, Int)
  type CBoard = (Array[Array[Int]], Array[Int], StringBuilder)

  def print(board: CBoard) = board._3

  def buildBoard(nbOfFile: Int, nbOfRank: Int): CBoard = {
    var index = 0
    val board: CBoard = (
      ofDim[Int](nbOfFile, nbOfRank), ofDim[Int](nbOfFile * nbOfRank), new StringBuilder)
    J(nbOfRank).foreach { rg => {
      board._3.append('\n')
      I(nbOfFile).foreach { col => {
        //  board._1(col)(rg) = (col, rg)
        //  index = col + rg * nbOfRank
        board._1(col)(rg) = index
        board._2(index) = index
        board._3.append(index).append(",")
        index += 1
      }
      }
    }
    }
    board
  }

  def J(n: Int): Range = I(n).end - 1 to I(n).start by -I(n).step

  def I(n: Int): Range = 0 until n

  println(buildBoard(nbOfFile = 10, nbOfRank = 12)._3)
  // println(buildBoard(nbOfFile = 8, nbOfRank = 8)._3)

}
