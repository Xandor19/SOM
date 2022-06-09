package som

/**
 * Class to represent a Self-Organizing Map neuron
 * @param xPos This neuron's x coordinate in the grid
 * @param yPos This neuron's y coordinate in the grid
 * @param weightVector This neuron's weight vector
 */
class Neuron (val xPos: Int, val yPos: Int, val weightVector: Array[Double], var tuningRate: Double) {
  /*
   * Class fields
   */
  var representedInputs = List.empty[InputVector]
  var neighbors = List.empty[Neuron]


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


  def restartRepresented (): Unit = representedInputs = representedInputs.empty


  /**
   * Adds received neuron as neighbor of this neuron
   * Neighboring is symmetrical (received neighbor also gets this neuron as neighbor
   * @param neigh Neuron to be added as neighbor
   * @return True if the operation was completed, false otherwise
   */
  def addNeighbor (neigh: Neuron): Boolean = {
    // Checks if not null neuron is already defined as neighbor of this neuron
    if (neigh != null && !neighbors.contains(neigh)) {
      // Adds received neuron as 1st party neighbor
      neighbors = neighbors.appended(neigh)
      neigh.addNeighbor(this)
      true
    }
    // Unsuccessful addition
    else false
  }


  /**
   * Updates this neuron's tuning factor when is selected as BMU
   * during the tuning stage
   */
  def updateTuningRate (): Unit = {
    tuningRate = tuningRate / (1 + tuningRate)
  }
}