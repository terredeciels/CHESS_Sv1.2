import model.TGenS

class TGenSTest extends TGenS {

  println("file")
  (0 until 64).foreach(square => {
    val f = file(square)
    print(if (f == 7) square + "," + f + " " + "\n" else square + "," + f + " ")
  }
  )
  println("rank")
  (0 until 64).foreach(square => {
    val f = rank(square)
    print(if (square % 8 == 0) "\n" else square + "," + f + " ")
  }
  )
  println("echq")
  (0 until 64).foreach(square => {

   // print(if (square % 8 == 0) "\n" else getString(square) )
    print(getString(square)+" " )
  }
  )
}

object TGenSTest {
  def main(args: Array[String]): Unit = {
    new TGenSTest
  }
}
