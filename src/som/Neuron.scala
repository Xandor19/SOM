package som

/**
 * Class to represent a Self-Organizing Map neuron
 * @param xPos This neuron's x coordinate in the grid
 * @param yPos This neuron's y coordinate in the grid
 * @param weightVector This neuron's weight vector
 * @param distanceFn Function to determine the distance between two weight vectors
 */
class Neuron (val xPos: Int, val yPos: Int, val weightVector: Array[Double],
              val distanceFn: (Array[Double], Array[Double]) => Double) {
  /*
   * Class fields
   */
  private var vectorDim = weightVector.length
  private var representedInputs = List.empty[InputVector]
  private var neighbors = Map.empty[Neuron, Int]
  /**var bias = 0*/


  /**
   * Encapsulates the application of this neuron's weight vector initialization
   * function
   * @param vectorInitFn Function to initialize this neuron's weights vector
   * @param bounds Set of lower and upper bounds for each dimension
   */
  def initializeWeights (vectorInitFn: (Array[Double], Array[(Double, Double)]) => Array[Double],
                         bounds: Array[(Double, Double)]): Unit = {
    vectorInitFn(weightVector, bounds)
  }


  /**
   * Encapsulates the application of this neuron's specific distance function
   * to a given input
   * @param inputVector Vector containing the input data
   * @return Distance between the input vector and this neuron's weight vector according
   *         to the defined distanceFn
   */
  def distanceTo (inputVector: Array[Double]): Double = {
    distanceFn(inputVector, weightVector) /**+ bias*/
  }


  /**
   * Applies the distance function to each dimension of the vectors individually
   * @param inputVector Vector containing the input data
   * @return Array containing the resulting distance of each dimension of the
   *         data in its corresponding index
   */
  def individualDimensionDistance (inputVector: InputVector): Array[Double] = {
    new Array[Double](0)
  }


  /**
   * Gets this neuron's current weights vector
   * @return
   */
  def weights: Array[Double] = weightVector


  /**
   * Adds the received input as represented by this neuron (it's the input's BMU)
   * @param inputVector Vector to be represented by this neuron
   */
  def adoptInput (inputVector: InputVector): Unit = {
    representedInputs = representedInputs.appended(inputVector)
  }


  def matches: List[InputVector] = {
    representedInputs
  }


  def restartRepresented (): Unit = representedInputs = representedInputs.empty


  /**
   * Adds received neuron as neighbor of this neuron with the
   * received neighborhood depth
   * @param neigh Neuron to be added as neighbor
   * @param depth Neighborhood depth in which the received neuron
   *              is corresponding to this neuron
   * @return True if the operation was completed, false otherwise
   */
  def addNeighbor (neigh: Neuron, depth: Int /*, radius: Int*/): Boolean = {
    // Checks if not null neuron is already defined as neighbor of this neuron
    if (neigh != null && !neighbors.keys.exists(x => x.equals(neigh))) {
      // Adds received neuron as 1st party neighbor
      neighbors = neighbors.updated(neigh, depth)
      true
    }
    // Unsuccessful addition
    else false
  }


  /**
   * Removes all neighbors that are out of range after a
   * neighborhood reduction, namely, those whose depth level
   * is greater than or equal to the previous radius
   * @param oldRadius Previous neighborhood radius
   */
  def shrinkNeighborhood (oldRadius: Int): Unit = {
    neighbors = neighbors.filter((c => c._2 < oldRadius))
  }


  /**
   * Provides the neighbors of this neuron
   * @return Array containing only the neighbors of the neuron
   */
  def allNeighbors: Array[Neuron] = neighbors.keys.toArray


  /**
   * Provides the neighbors with their neighborhood depth
   * related to this neuron
   * @return Map containing the neighbors as keys and their depth as values
   */
  def neighborsWithDepth: Map[Neuron, Int] = neighbors


  /**
   * Provides the neighbors that are in a given neighborhood depth in
   * relation to this neuron
   * @param depth Neighborhood depth of the desired neighbors
   * @return Array containing only the neighbors in the given depth
   */
  def neighborsInDepth (depth: Int) : Array[Neuron] = neighbors.filter(c => c._2 == depth).keys.toArray
}