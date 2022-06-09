package som

/**
 * Class to represent a rectangular-shape lattice
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
class RectLattice (width: Int, height: Int, learningFactor: Double, tuningFactor: Double,
                   neighRadius: Int, radiusController: Double,
                   distanceFn: (Array[Double], Array[Double]) => Double,
                   neighborhoodFn: (Int, Int, Int, Int, Double) => Double,
                   neighborhoodRadiusUpdateFn: (Int, Int, Double) => Double)
                   extends Lattice (width, height, learningFactor, tuningFactor, neighRadius, radiusController,
                                    distanceFn, neighborhoodFn, neighborhoodRadiusUpdateFn) {

  override def constructLattice (vectorDim: Int): Unit = {
    dimensionality = vectorDim

    for (i <- 0 until width; j <- 0 until height) {
      neurons(i)(j) = new Neuron(i, j, new Array[Double](dimensionality), tuningFactor)

      if (i > 0) neurons(i)(j).addNeighbor(neurons(i - 1)(j))
      if (j > 0) neurons(i)(j).addNeighbor(neurons(i)(j - 1))
    }
  }

  /**
   * So far, it prints how many times each neuron was BMU for the final order
   */
  def printMap (): Unit = {
    for (i <- 0 until width) {
      for (j <- 0 until height) {
        print(neurons(i)(j).representedInputs.size + "\t")
      }
      println()
    }
  }
}
