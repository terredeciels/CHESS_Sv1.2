import model.PositionS._
import model.{FenToPositionS, PositionS}

class FenToPositionSTest {
}

object FenToPositionSTest {
  def main(args: Array[String]): Unit = {
    val feninit = PosInit.fen
    val pos = FenToPositionS.toPositionS(feninit)
    val attendu = toString(PosInit)
    val resultat = toString(pos)
    assert(attendu == resultat)
    println("test pass")
    println(resultat)
  }

  def toString(p: PositionS): StringBuilder = {
    var index = 0
    val a: StringBuilder = new StringBuilder
    val b: StringBuilder = new StringBuilder
    (0 until 8).foreach { rg => {
      a.append('\n')
      b.append('\n')
      (0 until 8).foreach { col => {
        a.append(p.pieces(index)).append(" ")
        b.append(p.colors(index)).append(" ")
        index += 1
      }
      }
    }
    }
    a.append('\n').append('\n').append(b)
  }
}