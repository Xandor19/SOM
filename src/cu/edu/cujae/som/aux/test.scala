package cu.edu.cujae.som.aux

import cu.edu.cujae.som.io.{ExperimentData, MapConfig, ReaderWriter}

object test {
  def main(args: Array[String]): Unit = {
    /*val pop = ReaderWriter.loadSetFromCSV("/mnt/D80C76380C76122C/Mis Programas/Repos/SOM/Datasets/creditcardfraud_normalised.csv", ',')._3
    var popProps = Utils.classCount(pop).map(x => (x._1, x._2, x._2.toDouble / pop.size)).toList

    popProps.sortBy(e => e._1).foreach(x => println("Class " + x._1 + " with " + x._2 + " occurrences represents a proportion of " + x._3))
    println("Size of the population: " + pop.size)

    val sample = Utils.stratified(pop, 0.01)
    val samProps = Utils.classCount(sample).map(x => (x._1, x._2, x._2.toDouble / sample.size)).toList

    samProps.sortBy(e => e._1).foreach(x => println("Class " + x._1 + " with " + x._2 + " occurrences represents a proportion of " + x._3))
    println("Size of the sample: " + sample.size)

    (popProps zip samProps).foreach(x => {
      println("Prop difference in class " + x._1._1 + " is " + x._2._3 / x._1._3)
    })*/
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
