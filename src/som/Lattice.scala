package som

import scala.util.Random


/**
 * Class to represent a bi-dimensional SOM Lattice
 *
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
abstract class Lattice (val width: Int, val height: Int,
                        var learningFactor: Double, val tuningFactor: Double,
                        var neighRadius: Int, val radiusController: Double,
                        distanceFn: (Array[Double], Array[Double]) => Double,
                        neighborhoodFn: (Int, Int, Int, Int, Double) => Double,
                        neighborhoodRadiusUpdateFn: (Int, Int, Double) => Double) {

  /*
   * Class fields
   */
  val neurons: Array[Array[Neuron]] = Array.ofDim[Neuron](width, height)
  var dimensionality = 0
  var roughTrainingIters = 0


  /**
   * Abstract construction function
   * @param vectorDim Dimensionality of the weight vector
   */
  def constructLattice (vectorDim: Int): Unit


  /**
   * Initializes the neurons of the lattice with
   * the weight vectors received from an external source
   *
   * Weight vector are ordered in width-then-height indexation,
   * namely, each row is presented sequentially
   *
   * @param weightsSet The array of weight vectors to place in
   *                   the map
   */
  def importLattice (weightsSet: List[Array[Double]], vectorDim: Int): Unit = {
    dimensionality = vectorDim
    // Checks wetter there are as much weight vectors as neurons
    if (weightsSet.size == width * height) {
      // Iterator for the received vectors
      val it = weightsSet.iterator

      // Assigns each weight vector to the neuron in the corresponding position
      for (i <- 0 until width; j <- 0 until height) {
        neurons(i)(j) = new Neuron(i, j, it.next(), tuningFactor)
      }
    }
    // An invalid vector set was received
    else throw new IllegalStateException("Amount of weight vectors differs from total amount of neurons in the lattice")
  }


  /**
   * Initializes the neuron's weight vectors with random values
   * normalized between the bounds of each input dimension
   * @param bounds Set of lower and upper bounds for each dimension
   * @return The created random vector
   */
  def normalizedRandomInit (bounds: Array[(Double, Double)]): Unit = {
    neurons.flatten.foreach(x => {
      // Generates value for each dimension
      for (i <- bounds.indices) {
        val dimMin = bounds(i)._1
        val dimMax = bounds(i)._2

        x.weights.update(i, (Random.between(dimMin, dimMax) - dimMin) / (dimMax - dimMin) )
      }
    })
  }


  /**
   * Self-organization process, which lasts for a maximum number
   * of iterations or until a tolerable QE is reached
   * @param vectorSet Set of vectors that will be used for training
   * @param roughIters Maximum number of iterations for rough training stage
   * @param tuningIters Maximum number of iterations for tuning stage
   * @param tolerance MQE tolerance level
   */
  def organizeMap (vectorSet: VectorSet, roughIters: Int, tuningIters: Int, tolerance: Double): Unit = {
    roughTrainingIters = roughIters
    // Rough-train the network for a number of iterations
    //TODO modify loop to add tolerance stop-condition
    for (t <- 0 until roughIters) {
      // Present all inputs to the map
      while (vectorSet.hasNext) {
        // Obtain next vector to analyze
        val currentVector = vectorSet.next
        // Obtain BMU for current input
        val bmu = findBMU(currentVector.vector)

        // Applies learning cycle around the BMU
        neurons.flatten.foreach(x => {
          roughTraining(bmu.xPos, bmu.yPos, x, currentVector.vector, t)
        })
      }
      // Reset iteration process over the inputs
      vectorSet.reset()
      //TODO update iteration-defined learning factor and neighborhood radius after each
      //            iteration, not in each calculation
    }
    // Fixes neighborhood radius to only the adjacent neurons of the BMU
    neighRadius = 1

    // Tune the network for a number of iterations
    for (_ <- 0 until tuningIters) {
      // Present all inputs to the map
      while (vectorSet.hasNext) {
        // Obtain next vector to analyze
        val currentVector = vectorSet.next
        // Send BMU of current input for tuning
        tuning(findBMU(currentVector.vector), currentVector.vector)
      }
      // Reset iteration process over the inputs
      vectorSet.reset()
    }

    // Once the map is organized, present inputs one last time to form clusters
    vectorSet.vectors.foreach(x => clusterInput(x))
  }


  /**
   * Assigns the received input to a neuron of this map
   * @param inputVector Input vector to cluster in the map
   */
  def clusterInput (inputVector: InputVector): Unit = findBMU(inputVector.vector).adoptInput(inputVector)


  /**
   * Finds the best matching unit of a given input, that is,
   * the neuron for which the quantification error (QE), namely
   * distance, is minimum to the input vector
   * @param input The input vector to find its BMU
   * @return The neuron that has the MQE for that input
   */
  def findBMU (input: Array[Double]): Neuron = neurons.flatten.minBy[Double](x => distanceFn(x.weightVector, input))


  /**
   * Applies the training function to all neurons in the lattice,
   * applying a neighboring function to reduce the changes as the
   * neurons get father from the BMU
   *
   * Uses the formula:
   * wi (t + 1) = wi (t) + a(t) * hci(t) * dist(wi, vi)
   *
   * Where hci is the neighboring function which max value is
   * reached in the BMU and smoothly reduces with distance
   *
   * @param bmuX Value of x position of the BMU on the lattice
   * @param bmuY Value of the y position of the BMU on the lattice
   * @param unit Current neuron to train
   * @param inputVector Input vector represented by the BMU
   * @param epoch Current time value
   */
  def roughTraining(bmuX: Int, bmuY: Int, unit: Neuron,
                    inputVector: Array[Double], epoch: Int): Unit = {
    //Gets neuron's weight vector
    val weights = unit.weights

    for (i <- weights.indices) {
      val currentDim = weights(i)

      // Updates current dimension of the weight vector
      weights.update(i,  currentDim + learningFactor * neighborhoodFn(bmuX, bmuY, unit.xPos,
                         unit.yPos, neighborhoodRadiusUpdateFn(neighRadius, epoch, radiusController)) *
                         (inputVector(i) - currentDim))
    }
  }


  /**
   * Applies the training function to a BMU in the tuning stage,
   * namely, when only the BMU and its immediate neighbors are updated
   *
   * Uses the formula:
   * wi (t + 1) = wi (t) + a(t) * dist(wi, vi)
   * for the BMU
   *
   * And the formula:
   * wi (t + 1) = wi (t) + a(t) * hci * dist(wi, vi)
   * for the neighbors, where hci is invariant in time (only depends on
   * distance)
   * t stands for the epochs
   *
   * @param bmu Current BMU
   * @param inputVector Input vector represented by the BMU
   */
  def tuning(bmu: Neuron, inputVector: Array[Double]): Unit = {
    val vector = bmu.weightVector
    // Updates each dimension
    for (i <- vector.indices) {
      val currentDim = vector(i)

      // Updates current dimension of the weight vector
      vector.update(i, currentDim + bmu.tuningRate * (currentDim - inputVector(i)))
    }
    bmu.updateTuningRate()

    // Applies tuning to each neighbor
    bmu.neighbors.foreach(x => {
      val weights = x.weights

      for (i <- weights.indices) {
        val currentDim = weights(i)

        // Updates current dimension of the weight vector
        weights.update(i, currentDim + x.tuningRate * neighborhoodFn(bmu.xPos, bmu.yPos, x.xPos, x.yPos,
                          neighRadius) * (inputVector(i) - currentDim))
      }
    })
  }


  /**
   * Updates the learning factor of rough training by inverse-of-the-time
   * function
   * @param iter Current iteration
   * @return Factor for current iteration
   */
  def updateFactor(iter: Int): Unit = {
    learningFactor = learningFactor * (1 - iter/roughTrainingIters)
  }


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
}
