package model

import model.Move._

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks

class GenS(val pos: PositionS, val side: Int) extends TGenS {

  var moves: ArrayBuffer[Int] = ArrayBuffer.empty[Int]

  def search: ArrayBuffer[Int] = {
    val colors = pos.colors
    val pieces = pos.pieces
    val xside = if (side == -1) 1 else -1 // ou autre
    val loop = new Breaks // se place ici ?
    def opposingPiece(square: Int) = colors(square) == xside
    def emptySquare(square: Int) = colors(square) == EMPTY
    def isTrait(square: Int) = colors(square) == side

    (0 until 64).foreach { cO => {
      if (isTrait(cO)) {
        val piece = pieces(cO)
        if (isPawn(piece)) {
          genPawnMove(cO)
        } else {
          (0 until directions(piece - 1)).foreach { dir => {
            var cX = cO
            loop.breakable {
              while (true) {
                cX = nextSquare(cX, piece, dir)
                if (cX == -1) loop.break
                if (emptySquare(cX)) {
                  genMove(cO, cX, capture = 0)
                  if (!slide(piece - 1)) loop.break
                } else {
                  if (opposingPiece(cX)) genMove(cO, cX, capture = 1)
                  loop.break
                }

              }
            }
          }
          }
        }
      }
    }
    }
    moves
  }

  def genMove(cO: Int, cX: Int, capture: Int) = {
    // println("piece" + cO + "," + cX + "," + capture)
    val flag = if (capture == 0) 0 else 4
    val captpiece = if (capture == 0) 0 else pos.pieces(cX)
    moves += move(cO, cX, flag, captpiece)
  }

  def genPawnMove(cO: Int) = {
    // println("pion: " + cO)
    searchPawnMoves(cO, recherchePionAttaqueRoque = false)
  }

  def searchPawnMoves(caseO: Int, recherchePionAttaqueRoque: Boolean): ArrayBuffer[Int] = {
    // var caseX = caseO + NS.orientation(side)
    var caseX = mailbox(mailbox64(caseO) + NS.orientation)
    if (pos.colors(caseX) == EMPTY) {
      if (lastrank(side).contains(caseX)) {
        moves ++= addPromotionMove(caseO, caseX, 0) // sans prise
      }
      else {
        val flag = 0 // quiet move 0
        val captpiece = 0
        moves += move(caseO, caseX, flag, captpiece)
        // pseudoCoups.add(new GCoups(couleur * PION, caseO, caseX, 0, 0, 0, Deplacement, 0));
      }
      if (rank2(side).contains(caseO)) {
        caseX += NS.orientation
        if (pos.colors(caseX) == EMPTY) {
          val flag = 0 // quiet move 0
          val captpiece = 0
          moves += move(caseO, caseX, flag, captpiece)
          //  pseudoCoups.add(new GCoups(couleur * PION, caseO, caseX, 0, 0, 0, Deplacement, 0));
        }
      }
    }
    if (!recherchePionAttaqueRoque) {
      // caseX = caseO + NS.orientation(side) + est
      caseX = mailbox(mailbox64(caseO) + NS.orientation + est)
      if (caseX != OUT) {
        if (opposingPiece(caseX)) {
          if (lastrank(side).contains(caseX)) {
            moves ++= addPromotionMove(caseO, caseX, pos.pieces(caseX)) // avec prise
          }
          else {
            val flag = 4 // capture 0100
            val captpiece = pos.pieces(caseX)
            moves += move(caseO, caseX, flag, captpiece)
            // pseudoCoups.add(new GCoups(couleur * PION, caseO, caseX, 0, 0, etats[caseX], Prise, 0));
          }
        }
      }
      // caseX = caseO + NS.orientation(side) + ouest
      caseX = mailbox(mailbox64(caseO) + NS.orientation + ouest)
      if (caseX != OUT) {
        if (opposingPiece(caseX)) {
          if (lastrank(side).contains(caseX)) {
            moves ++= addPromotionMove(caseO, caseX, pos.pieces(caseX))
          }
          else {
            val flag = 4
            val captpiece = pos.pieces(caseX)
            moves += move(caseO, caseX, flag, captpiece)
            //   pseudoCoups.add(new GCoups(couleur * PION, caseO, caseX, 0, 0, etats[caseX], Prise, 0));
          }
        }
      }
    }
    //    else {
    //      diagonalePionAttaqueRoque(caseO, orientation, est)
    //      diagonalePionAttaqueRoque(caseO, orientation, ouest)
    //    }
    moves
  }

  def opposingPiece(square: Int) = pos.colors(square) == -side

  def addPromotionMove(caseO: Int, caseX: Int, captpiece: Int): ArrayBuffer[Int] = {
    var flag = 8 // 1000 a 1011(11)
    List(KNIGHT, BISHOP, ROOK, QUEEN).foreach(p => {
      moves += move(caseO, caseX, flag, captpiece)
      flag += 1
    })
    // pseudoCoups.add(new GCoups(couleur * PAWN, caseO, caseX, 0, 0, pieceprise, Promotion, couleur * p)))
    moves
  }

  def xside = if (side == -1) 1 else -1

  object NS {
    val orientation = if (side == white) nord else sud
  }

  //  def diagonalePionAttaqueRoque(caseO: Int, orientation: Int, estOuOuest: Int) {
  //    val caseX: Int = caseO + orientation + estOuOuest
  //    if (etats(caseX) != OUT) {
  //    }
  //  }


}
