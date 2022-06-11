package som

/**
 * Class to represent a hexagonal-shape lattice
 * @param width Width of the lattice
 * @param height Height of the lattice
 * @param learningFactor Learning rate of the rough training stage
 * @param tuningFactor Initial factor for the tuning stage
 * @param neighRadius Initial neighborhood radius
 * @param radiusController Controller for the neighborhood radius shrinking
 * @param distanceFn Function used to determinate the similarity with an input
 * @param neighborhoodFn Function to control the learning of the BMU's neighbors
 * @param neighborhoodRadiusUpdateFn Time-based function to update the neighborhood radius
 */
class HexLattice (width: Int, height: Int, learningFactor: Double, tuningFactor: Double,
                  neighRadius: Int, radiusController: Double,
                  distanceFn: (Array[Double], Array[Double]) => Double,
                  neighborhoodFn: (Float, Float, Float, Float, Double) => Double,
                  neighborhoodRadiusUpdateFn: (Int, Int, Double) => Double)
                  extends Lattice (width, height, learningFactor, tuningFactor, neighRadius, radiusController,
                                   distanceFn, neighborhoodFn, neighborhoodRadiusUpdateFn) {

  /**
   *
   * @param vectorDim Dimensionality of the weight vector
   */
  override def constructLattice(vectorDim: Int): Unit = {
    dimensionality = vectorDim

    val c60 = math.cos(math.Pi / 3)
    val s60 = math.sin(math.Pi / 3)

    for (i <- 0 until width; j <- 0 until height) {
      if (i % 2 == 0) {
        neurons(i)(j) = new Neuron(j, i * s60.toFloat, new Array[Double](dimensionality), tuningFactor)

        if (j > 0) neurons(i)(j).addNeighbor(neurons(i)(j - 1))
        if (i > 0 && j > 0) neurons(i)(j).addNeighbor(neurons(i - 1)(j - 1))
        if (i > 0) neurons(i)(j).addNeighbor(neurons(i - 1)(j))
      }
      else {
        //neurons(i)(j) = new SomNode(j + c60, i *s60, d, Array.ofDim[Double](_).map(_ => rand.nextDouble))
        neurons(i)(j) = new Neuron(j + c60.toFloat, i *s60.toFloat, new Array[Double](dimensionality), tuningFactor)

        if (j > 0) neurons(i)(j).addNeighbor(neurons(i)(j-1))
        if (i > 0) neurons(i)(j).addNeighbor(neurons(i-1)(j))
        if (i > 0 && j < height - 1) neurons(i)(j).addNeighbor(neurons(i-1)(j+1))
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
   * Prints each neuron as in printMap but adding how many classes are represented
   * by it
   */
  def printClassesBalance (): Unit = {
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
   * Prints each neuron as in printMap but adding how many classes are represented
   * by it
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
}
