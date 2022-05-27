package som

object test {
  def main(args: Array[String]): Unit = {
    val prov = new provider
    val mod = new modifier

    prov.printArr()

    mod.modify(prov.giveArr)

    prov.printArr()
  }
}

class modifier {
  def modify (some: Array[Double]): Unit = {
    for (i <- some.indices) {
      some.update(i, some(i) + 1)
    }
  }
}

class provider {
  private val arr = new Array[Double](10)

  def giveArr: Array[Double] = arr

  def printArr (): Unit = {
    arr.foreach(x => println(x))
  }
}
