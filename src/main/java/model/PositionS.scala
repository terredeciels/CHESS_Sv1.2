package model

import model.Castle._
import model.Move._

import scala.collection.mutable.ArrayBuffer

class PositionS(pos: PositionS, side: Int,
                var pawnFlag: Boolean) extends GenS(pos, side) {


  val R = new Castle
  var caseEP: Int = -1
  var castles: ArrayBuffer[Boolean] = ArrayBuffer.empty[Boolean]
  var colors: ArrayBuffer[Int] = ArrayBuffer.empty[Int]
  var pieces: ArrayBuffer[Int] = ArrayBuffer.empty[Int]

  //initialisation
  (0 until 64).foreach(cO => colors += EMPTY)
  (0 until 64).foreach(cO => pieces += EMPTY)
  (0 until 4).foreach(c => castles += true)

  def setPawnFlag(flag: Boolean) = pawnFlag = flag

  def validMoves(_side: Int) = {
    val generateur = new GenS(pos, _side)
    val allValidMoves = generateur.search()
    isCheck = generateur.isCheck
    allValidMoves
  }

  def unexec(ug: UndoMove) {
    //    copy(ug.etats, 0, etats, 0, NB_CELLULES)
    //    copy(ug.roques, 0, roques, 0, 4)
    (0 until 64).foreach(e => pos.pieces(e) = ug.pieces(e))
    (0 until 64).foreach(e => pos.colors(e) = ug.colors(e))
    (0 until 4).foreach(e => pos.castles(e) = ug.castles(e))
    pos.caseEP = ug.caseEP
    //side = -side
    setSide(-side)
  }


  def exec(m: Int, ug: UndoMove): Boolean = {
    // copy(etats, 0, ug.etats, 0, NB_CELLULES)
    (0 until 64).foreach(e => ug.pieces(e) = pos.pieces(e))
    (0 until 64).foreach(e => ug.colors(e) = pos.colors(e))
    ug.setKQkq(pos.castles)
    ug.caseEP = pos.caseEP
    val O = cO(m)
    val X = cX(m)

    //val p = m.piece

    var t = flag(m)
    val cpiece = captpiece(m)
    pos.caseEP = -1
    R.side = side
    if (pos.pieces(O) == PAWN && abs(X - O) == abs(nord - sud)) {
      pos.caseEP = if (side == black) X + 10 else X - 10
    }
    val piecePromotion = t

    val Deplacement = 0
    val KingCastle = 2
    val QueenCastle = 3
    val Prise = 4
    val EnPassant = 5
    val Promotion = 8
    t = if (t >= 8) Promotion else t
    t match {
      // ! attention repeter code pour Deplacement et Prise
      case Deplacement =>
        //        e(X, O)
        //        e(O)
        pos.pieces(O) = pos.pieces(X)
        pos.pieces(O) = EMPTY
        pos.colors(X) = side
        pos.colors(O) = EMPTY
        valideDroitRoque(m)
      case Prise =>
        //        e(X, O)
        //        e(O)
        pos.pieces(O) = pos.pieces(X)
        pos.pieces(O) = EMPTY
        pos.colors(X) = side
        pos.colors(O) = EMPTY
        valideDroitRoque(m)
      case EnPassant =>
        //        e(X, O)
        //        e(O)
        pos.pieces(O) = pos.pieces(X)
        pos.pieces(O) = EMPTY
        pos.colors(X) = side
        pos.colors(O) = EMPTY

        pos.pieces(X + nord * side) = EMPTY
        pos.colors(X + nord * side) = EMPTY
      // e(X + nord * side)
      case Promotion =>
        piecePromotion match {
          case 8 => pos.pieces(X) = KNIGHT
          case 9 => pos.pieces(X) = BISHOP
          case 10 => pos.pieces(X) = ROOK
          case 11 => pos.pieces(X) = QUEEN
        }
        pos.colors(X) = side
        //e(O)
        pos.pieces(O) = EMPTY
        pos.colors(O) = EMPTY
      case KingCastle | QueenCastle =>
        //        e(X, O)
        //        e(O)
        pos.pieces(O) = pos.pieces(X) // Roi
        pos.pieces(O) = EMPTY
        pos.colors(X) = side
        pos.colors(O) = EMPTY
        //        e(caseXTour(m), caseOTour(m))
        //        e(caseOTour(m))
        pos.pieces(caseOTour(m)) = pos.pieces(caseXTour(m))
        pos.colors(caseOTour(m)) = EMPTY
        R.unsetRoque()
      case _ =>
    }
    // side = -side
    setSide(-side)
    true
  }

  //TODO
  def caseOTour(m: Int): Int = if (cO(m) == e1 && flag(m) == 2) h1
  else if (cO(m) == e1 && flag(m) == 3) a1
  else if (cO(m) == e8 && flag(m) == 2) h8
  else if (cO(m) == e8 && flag(m) == 3) a8 else -1 //bug

  def caseXTour(m: Int): Int = if (cO(m) == e1 && flag(m) == 2) f1
  else if (cO(m) == e1 && flag(m) == 3) d1
  else if (cO(m) == e8 && flag(m) == 2) f8
  else if (cO(m) == e8 && flag(m) == 3) d8 else -1 // bug

  def valideDroitRoque(m: Int) {
    val caseO = cO(m)
    val piece = pos.pieces(caseO) // ?
    piece match {
      case KING =>
        R.unsetRoque()
      case ROOK =>
        if (caseO == caseTourH(side)) unsetK(side)
        if (caseO == caseTourA(side)) unsetQ(side)
      case _ =>
    }
    if (pos.colors(caseTourA(side)) != side && pos.pieces(caseTourA(side)) != ROOK ||
      pos.colors(caseRoi(side)) != side && pos.pieces(caseRoi(side)) != KING) unsetQ(side)
    if (pos.colors(caseTourH(side)) != side && pos.pieces(caseTourH(side)) != ROOK ||
      pos.colors(caseRoi(side)) != side && pos.pieces(caseRoi(side)) != KING) unsetK(side)
  }
}

object PositionS {

  val PosInit = new PositionS(false) {

    castles = ArrayBuffer(true, true, true, true)

    lastrank(black).foreach(cO => colors(cO) = black)
    rank2(black).foreach(cO => colors(cO) = black)

    lastrank(white).foreach(cO => colors(cO) = white)
    rank2(white).foreach(cO => colors(cO) = white)

    rank2(black).foreach(cO => pieces(cO) = PAWN) // couleur donnée par color !
    rank2(white).foreach(cO => pieces(cO) = PAWN)
    List(a1, a8, h1, h8).foreach(cO => pieces(cO) = ROOK)
    List(b1, g1, b8, g8).foreach(cO => pieces(cO) = KNIGHT)
    List(c1, f1, c8, f8).foreach(cO => pieces(cO) = BISHOP)
    List(d1, d8).foreach(cO => pieces(cO) = QUEEN)
    List(e1, e8).foreach(cO => pieces(cO) = KING)
  }
  val PosEmpty = new PositionS(false) {

    castles = ArrayBuffer(true, true, true, true)


  }
}

