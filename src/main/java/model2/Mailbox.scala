package model2

import scala.collection.immutable.Vector
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks

class Mailbox {

  val m120 = Vector[Int](
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 0, 1, 2, 3, 4, 5, 6, 7, -1,
    -1, 8, 9, 10, 11, 12, 13, 14, 15, -1,
    -1, 16, 17, 18, 19, 20, 21, 22, 23, -1,
    -1, 24, 25, 26, 27, 28, 29, 30, 31, -1,
    -1, 32, 33, 34, 35, 36, 37, 38, 39, -1,
    -1, 40, 41, 42, 43, 44, 45, 46, 47, -1,
    -1, 48, 49, 50, 51, 52, 53, 54, 55, -1,
    -1, 56, 57, 58, 59, 60, 61, 62, 63, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
  )

  val m64 = Vector[Int](
    21, 22, 23, 24, 25, 26, 27, 28,
    31, 32, 33, 34, 35, 36, 37, 38,
    41, 42, 43, 44, 45, 46, 47, 48,
    51, 52, 53, 54, 55, 56, 57, 58,
    61, 62, 63, 64, 65, 66, 67, 68,
    71, 72, 73, 74, 75, 76, 77, 78,
    81, 82, 83, 84, 85, 86, 87, 88,
    91, 92, 93, 94, 95, 96, 97, 98
  )
  val slide = Vector(false, false, true, true, true, false)

  val directions = Vector(0, 8, 4, 4, 8, 8) /* knight or ray directions */

  val offset = Vector(
    Vector(0, 0, 0, 0, 0, 0, 0, 0),
    Vector(-21, -19, -12, -8, 8, 12, 19, 21), /* KNIGHT */
    Vector(-11, -9, 9, 11, 0, 0, 0, 0), /* BISHOP */
    Vector(-10, -1, 1, 10, 0, 0, 0, 0), /* ROOK */
    Vector(-11, -10, -9, -1, 1, 9, 10, 11), /* QUEEN */
    Vector(-11, -10, -9, -1, 1, 9, 10, 11) /* KING */
  )

  val nbCases: Int = 64

  val EMPTY = 0
  val PAWN = 1
  val KNIGHT = 2
  val BISHOP = 3
  val ROOK = 4
  val QUEEN = 5
  val KING = 6
  val NOTUSED = 7

  def search(color: ArrayBuffer[Int], pieces: ArrayBuffer[Int], side: Int): Unit = {
    val `xside` = -side
    (0 until nbCases).foreach(cO =>
      if (isColorSide(color, side, cO)) {
        val piece = pieces(cO)
        if (piece != PAWN)
          (0 until directions(piece)).foreach(dir => {
            var cX = cO
            val loop = new Breaks
            while (true) {
              cX = nextSquare(cX, piece, dir)
              (cX, color(cX)) match {
                case (-1, _) => loop.break
                case (_, EMPTY) => genMove(cO, cX, capture = 0)
                  if (!slide(piece)) loop.break
                case (_, `xside`) => genMove(cO, cX, capture = 1)
                  loop.break()
              }
            }
          }
          )
        else {
          genPawnMove(cO)
        }
      }
    )
  }

  def nextSquare(cX: Int, piece: Int, dir: Int): Int = m120(m64(cX) + offset(piece)(dir))

  def genMove(cO: Int, cX: Int, capture: Int) = {}

  def genPawnMove(cO: Int) = {}

  def isColorSide(color: ArrayBuffer[Int], side: Int, cO: Int) = color(cO) == side
}




