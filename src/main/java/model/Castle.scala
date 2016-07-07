package model

import scala.collection.mutable.ArrayBuffer

object Castle extends TGenS{
  var castles =  ArrayBuffer.empty[Boolean]
 // val itRoque = List(0, 1, 2, 3)
 // var o_o = Array(Array(e1, g1, h1, f1), Array(e1, c1, a1, d1, b1), Array(e8, g8, h8, f8), Array(e8, c8, a8, d8, b8))

  def unsetKQ() = {
    castles(0) = false
    castles(1) = false
  }

  def unsetkq() {
    castles(2) = false
    castles(3) = false
  }

  def unsetK(color: Int) {
    if (color == white) unsetK()
    else if (color == black) unsetk()
  }

  def unsetK() = {
    castles(0) = false
  }

  def unsetk() {
    castles(2) = false
  }

  def unsetQ(color: Int) {
    if (color == white) unsetQ()
    else if (color == black) unsetq()
  }

  def unsetQ() {
    castles(1) = false
  }

  def unsetq() {
    castles(3) = false
  }

  def caseTourH(couleur: Int) = if (couleur == white) h1 else h8

  def caseTourA(couleur: Int) = if (couleur == white) a1 else a8

  def caseRoi(couleur: Int) = if (couleur == white) e1 else e8
}

class Castle extends TGenS{

  var side = 0

  def unsetRoque() = {
    if (side == white) Castle.unsetKQ()
    else if (side == black) Castle.unsetkq()
  }
}