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
        // As neurons implement symmetrical neighboring, its only necessary to add 3 neighbors manually
        if (j > 0) neurons(i)(j) addNeighbor neurons(i)(j - 1)
        if (i > 0 && j > 0) neurons(i)(j) addNeighbor neurons(i - 1)(j - 1)
        if (i > 0) neurons(i)(j) addNeighbor neurons(i - 1)(j)
      }
      else {
        // Transformation formula for odd row
        neurons(i)(j) = new Neuron(j + c60.toFloat, i *s60.toFloat, it.next)

        // Neighbor assigment
        if (j > 0) neurons(i)(j) addNeighbor neurons(i)(j-1)
        if (i > 0) neurons(i)(j) addNeighbor neurons(i-1)(j)
        if (i > 0 && j < height - 1) neurons(i)(j) addNeighbor neurons(i-1)(j+1)
      }
    }
  }


  /**
   * Provides the distribution of the neurons in this lattice
   * @return LatticeDistribution constant for this lattice's distribution
   */
  override def latticeType: String = LatticeDistribution.hexagonal
}
