package model

import model.Castle._
import model.Move._

class GenExecS(pos: PositionS, side: Int) extends GenS(pos, side) {

  var estEnEchec : Boolean =_
  val R = new Castle


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
