package cu.edu.cujae.som.map

/**
 * Class to represent a rectangular-shape lattice
 *
 * @param width Width of the lattice
 * @param height Height of the lattice
 */
class RectLattice (width: Int, height: Int) extends Lattice (width, height) {

  /**
   * Lattice construction function
   * @param vectors Vectors to place in the neurons
   */
  override def constructLattice (vectors: Iterable[Array[Double]]): Unit = {
    val it = vectors.iterator
    // Initializes every neuron in the lattice
    for (i <- 0 until width; j <- 0 until height) {
      neurons(i)(j) = new Neuron(i, j, it.next)

      // Assigns left-side adjacent nodes as neighbors of the current neuron
      // As neurons implement symmetrical neighboring, its only necessary to add 2 neighbors manually
      if (i > 0) neurons(i)(j) addNeighbor neurons(i - 1)(j)
      if (j > 0) neurons(i)(j) addNeighbor neurons(i)(j - 1)
    }
  }


  /**
   * Prints this map distribution, e.g, the neurons with the number of inputs
   * that represents
   */
  override def printMap (): Unit = {
    for (i <- 0 until width) {
      for (j <- 0 until height) {
        print(neurons(i)(j).representedInputs.size + "\t")
      }
      println()
    }
  }


  /**
   * Prints each neuron as in printMap but adding how many classes it represents
   */
  override def printClassesBalance (): Unit = {
    for (i <- 0 until width) {
      for (j <- 0 until height) {
        print(neurons(i)(j).representedInputs.size + " : " + neurons(i)(j).representedClasses.size + "\t")
      }
      println()
    }
  }


  /**
   * Prints the name of the class that each neuron represents (the class from which
   * the neuron has most instances)
   */
  override def printMainClasses (): Unit = {
    for (i <- 0 until width) {
      for (j <- 0 until height) {
        printf("%20s\t", neurons(i)(j).mainClass)
      }
      println()
    }
  }


  /**
   * Provides the distribution of the neurons in this lattice
   * @return LatticeDistribution constant for this lattice's distribution
   */
  override def latticeType: String = LatticeDistribution.rectangular
}
