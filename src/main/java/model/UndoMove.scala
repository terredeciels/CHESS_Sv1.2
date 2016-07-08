package model

import scala.collection.mutable.ArrayBuffer

class UndoMove extends TGenS{
  var pieces= ArrayBuffer.empty[Int]
  var colors= ArrayBuffer.empty[Int]
  var castles =  ArrayBuffer.empty[Boolean]
  var caseEP: Int = _

  (0 until 64).foreach(cO => colors += EMPTY)
  (0 until 64).foreach(cO => pieces += EMPTY)
  (0 until 4).foreach(c => castles += true)

  def setKQkq(roq: ArrayBuffer[Boolean]) {
    castles(0) = roq(0)
    castles(1) = roq(1)
    castles(2) = roq(2)
    castles(3) = roq(3)
  }
}