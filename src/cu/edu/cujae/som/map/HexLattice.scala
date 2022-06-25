package cu.edu.cujae.som.map

/**
 * Class to represent a hexagonal-shape lattice
 *
 * @param width Width of the lattice
 * @param height Height of the lattice
 */
class HexLattice (width: Int, height: Int) extends Lattice (width, height) {

  /**
   * Lattice construction function
   * @param vectors Vectors to place in the neurons
   */
  override def constructLattice (vectors: Iterable[Array[Double]]): Unit = {
    // Iterator over the vectors
    val it = vectors.iterator

    // Sin and cos of 60º, used to coordinate conversion
    val c60 = math.cos(math.Pi / 3)
    val s60 = math.sin(math.Pi / 3)

    // Initializes every neuron in the lattice
    for (i <- 0 until width; j <- 0 until height) {
      if (i % 2 == 0) {
        // Transformation formula por even row
        neurons(i)(j) = new Neuron(j, i * s60.toFloat, it.next)

        // Assigns left-side adjacent nodes as neighbors of the current neuron using the even rows distribution
        // As neurons implement symmetrical neighboring, its only necessary to add 2 neighbors manually
        if (j > 0) neurons(i)(j) addNeighbor neurons(i)(j - 1)
        if (i > 0 && j > 0) neurons(i)(j) addNeighbor neurons(i - 1)(j - 1)
        if (i > 0) neurons(i)(j) addNeighbor neurons(i - 1)(j)
      }
      else {
        // Transformation formula for odd row
        neurons(i)(j) = new Neuron(j + c60.toFloat, i *s60.toFloat, it.next)

        // Assigns left-side adjacent nodes as neighbors of the current neuron using the odd rows distribution
        // As neurons implement symmetrical neighboring, its only necessary to add 2 neighbors manually
        if (j > 0) neurons(i)(j) addNeighbor neurons(i)(j-1)
        if (i > 0) neurons(i)(j) addNeighbor neurons(i-1)(j)
        if (i > 0 && j < height - 1) neurons(i)(j) addNeighbor neurons(i-1)(j+1)
      }
    }
  }


  /**
   * Prints this map distribution, e.g, the neurons with the number of inputs
   * that represents
   */
  override def printMap (): Unit = {
    for (i <- 0 until width) {
      for (j <- 0 until height) {
        if (i % 2 == 0) print(neurons(i)(j).representedInputs.size + "\t\t")
        else print("\t" + neurons(i)(j).representedInputs.size + "\t")
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
        if (i % 2 == 0) printf("%3d : %3d\t\t", neurons(i)(j).representedInputs.size,
                                                    neurons(i)(j).representedClasses.size)
        else printf("\t\t%3d : %3d", neurons(i)(j).representedInputs.size, neurons(i)(j).representedClasses.size)
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
        if (i % 2 == 0) printf("%20s\t\t\t\t", neurons(i)(j).mainClass)
        else printf("\t\t\t\t%20s", neurons(i)(j).mainClass)
      }
      println()
    }
  }


  /**
   * Provides the distribution of the neurons in this lattice
   * @return LatticeDistribution constant for this lattice's distribution
   */
  override def latticeType: String = LatticeDistribution.hexagonal
}
