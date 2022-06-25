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
   * Prints the set of weights of this map's neurons
   */
  def printSet (): Unit = {
    neurons.flatten.foreach(x => {
      print("Neuron at pos (" + x.xPos + ", " + x.yPos + "): [")
      x.weights.foreach(v => print(v + ", "))
      println("]")
    })
  }


  /**
   * Prints this map distribution, e.g, the neurons with the number of inputs
   * that represents
   */
  def printMap (): Unit


  /**
   * Prints each neuron as in printMap but adding how many classes it represents
   */
  def printClassesBalance (): Unit


  /**
   * Prints the name of the class that each neuron represents (the class from which
   * the neuron has most instances)
   */
  def printMainClasses (): Unit


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
