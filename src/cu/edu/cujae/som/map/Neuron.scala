package cu.edu.cujae.som.map

import cu.edu.cujae.som.aux.Utils
import cu.edu.cujae.som.data.InputVector

/**
 * Class to represent a Self-Organizing Map neuron
 * @param xPos This neuron's x coordinate in the grid
 * @param yPos This neuron's y coordinate in the grid
 * @param weightVector This neuron's weight vector
 * @param hits Amount of inputs that were represented by the neuron, default to 0
 * @param balance Amount of inputs and amount of different classes represented by the neuron, default to (0, 0)
 * @param representedClass Main class of the neuron, default to ("None", 0)
 */
class Neuron (val xPos: Float, val yPos: Float, var weightVector: Array[Double], var hits: Int = 0,
              var balance: (Int, Int) = (0, 0), var representedClass: String = "None") {
  /*
   * Class fields
   */
  var tuningRate = 0.0
  var representedInputs = Map.empty[InputVector, Double]
  var neighbors = List.empty[Neuron]


  /**
   * Gets this neuron's current weights vector
   * @return
   */
  def weights: Array[Double] = weightVector


  /**
   * Adds the received input as represented by this neuron (it's the input's BMU)
   * @param inputVector Vector to be represented by this neuron
   */
  def representInput (inputVector: InputVector, qe: Double): Unit = {
    representedInputs = representedInputs.updated(inputVector, qe)
  }


  /**
   * Obtains the most similar input, e.g, input with a local MQE from those
   * represented by this neuron
   * @return InputVector with the local MQE
   */
  def bestMatch: (InputVector, Double) = {
    representedInputs.minBy(x => x._2)
  }


  /**
   * Obtains the generalized median of this neuron represented inputs,
   * e.g, the mean of the input vectors
   * @return Average vector of the inputs
   */
  def generalizedMedian: Array[Double] = {
    var mean = new Array[Double](weightVector.length)

    if (representedInputs.nonEmpty) {
      representedInputs.keys.map(x => x.vector).foreach(vector => {
        mean = mean.zip(vector).map(x => x._1 + x._2)
      })
      mean = mean.map(x => x / representedInputs.size)
    }
    mean
  }


  /**
   * Obtains the average of quantification errors with which this
   * neuron represents its inputs
   * @return Value of the average QE
   */
  def averageQE: Double = {
    if (representedInputs.nonEmpty) representedInputs.values.sum / representedInputs.size
    else Double.NaN
  }


  /**
   * Updates the hit count of this neuron, the number of inputs that it represents
   * @param memory Optional parameter to indicate if previous hits must be accumulated
   *               to current (true) or are discarded (false). False by default
   */
  def updateHits (memory: Boolean = false): Unit = {
    if (memory) hits += representedInputs.size
    else hits = representedInputs.size
  }


  /**
   * Provides the amount of inputs represented by this neuron
   * @return
   */
  def hitCount: Int = hits


  /**
   * Counts how many classes (e.g, how many different "classification" string attribute)
   * and how many instances of each one currently this neuron physically represents
   * @return Map with the classes' names as the keys and the amount of inputs as values
   */
  def representedClasses: Map[String, Int] = {
    // Counts how many inputs are of each class
    Utils.classCount(representedInputs.keys)
  }


  /**
   * Provides the classes balance of the neuron
   * @return Tuple of (amount of inputs, amount of classes)
   */
  def classesBalance: (Int, Int) = {
    balance
  }


  /**
   * Updates this neurons classes balance
   */
  def updateBalance (): Unit = {
    // Obtains current pair of amount of inputs and amount of classes
    balance = (representedInputs.size, representedClasses.size)
  }


  /**
   * Provides the current main class of this neuron
   * @return Tuple of (class name, amount of inputs)
   */
  def mainClass: String = {
    if (representedClass == "None") {
      // If no class is represented, attempts to update main class state
      findMainClass()
    }
    representedClass
  }


  /**
   * Updates the most frequent class of those represented in this neuron
   */
  def findMainClass (): Unit = {
    // Looks for the most frequent class
    if (representedInputs.nonEmpty) {
      representedClass = representedClasses.toList.maxBy(x => x._2)._1
    }
    // This neuron does not represent any input
    else {
      representedClass = "None"
    }
  }


  /**
   * Restarts the represented inputs of this neuron
   */
  def clearRepresented(): Unit = representedInputs = representedInputs.empty


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


  def setTuningRate (tune: Double): Unit = {
    if (tune > 0 && tune <= 1) tuningRate = tune
  }
}