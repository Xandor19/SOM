package cu.edu.cujae.som.map

/**
 * Class to represent a bi-dimensional SOM Lattice
 *
 * @param width Width of the lattice
 * @param height Height of the lattice
 */
abstract class Lattice (val width: Int, val height: Int) {

  /*
   * Class fields
   */
  val neurons: Array[Array[Neuron]] = Array.ofDim[Neuron](width, height)


  /**
   * Abstract construction function
   * @param vectors Vectors to place in the neurons
   */
  def constructLattice (vectors: Iterable[Array[Double]]): Unit


  /**
   * Provides the weight vectors of the neurons of the lattice, in horizontal order
   * @return Array of vectors (array of float) with each row representing a weight vector
   */
  def vectorSet: Array[Array[Double]] = neurons.flatten.map(x => x.weights)


  /**
   * Provides a lattice representation with the number of hits of each neuron (inputs
   * that it represents)
   * @return Bi-dimensional Int array in which each (i, j) represents the amount
   *         of inputs clustered in the neuron in that position's
   */
  def neuronHits: Array[Array[Int]] = neurons.map(x => x.map(y => y.representedInputs.size))


  /**
   * Provides a lattice representation with the classes balance of each neuron (total of
   * inputs and the amount of different classes of the inputs
   * @return Bi-dimensional array of (Int, Int) tuples in which the first number represents
   *         the number of hits in the neuron and the second the amount of different classes
   *         of those hits
   */
  def classesBalance: Array[Array[(Int, Int)]] = {
    neurons.map(x => x.map(y => (y.representedInputs.size, y.representedClasses.size)))
  }


  /**
   * Provides a lattice representation with the main class of each neuron (e.g, the
   * class with more inputs represented by the neuron
   * @return Bi-dimensional String array in which each (i, j) represents the main
   *         class of the neuron in that position
   */
  def mainClasses: Array[Array[String]] = neurons.map(x => x.map(y => y.findMainClass._1))


  /**
   * Provides the weight vectors of the lattice with the coordinates of their
   * respective neuron
   * @return Array of tuples with (x coord, y coord, weight vector)
   */
  def indexedVectors: Array[(Float, Float, Array[Double])] = neurons.flatten.map(x => (x.xPos, x.yPos, x.weightVector))


  /**
   * Provides the distribution of the neurons in this lattice
   * @return LatticeDistribution constant for this lattice's distribution
   */
  def latticeType: String
}


/**
 * Factory object for creating lattices with the specified distribution
 */
object LatticeFactory {
  /**
   * Creates a lattice of the received distribution with the specified parameters
   * @param latDistrib Id representing the desired distribution
   * @param width Width of the lattice
   * @param height Height of the lattice
   * @return A initial state lattice of the given distribution
   */
  def createLattice (latDistrib: String, width: Int, height: Int): Lattice = {

    if (latDistrib == LatticeDistribution.rectangular) {
      // Rectangular lattice
      new RectLattice(width, height)
    }
    else if (latDistrib == LatticeDistribution.hexagonal) {
      // Hexagonal lattice
      new HexLattice(width, height)
    }
    else null
  }
}


/**
 * Lattice distributions codes for creation
 */
object LatticeDistribution {
  val rectangular = "Rectangular"
  val hexagonal = "Hexagonal"
}
