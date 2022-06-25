package cu.edu.cujae.som.map

import cu.edu.cujae.som.data.VectorSet
import cu.edu.cujae.som.io.MapConfig

/**
 * Class to represent a on-line training Self-Organizing Map
 * @param lattice The lattice of neurons
 * @param initialLearningFactor Learning factor for training
 * @param tuningFactor Factor for tuning stage
 * @param neighRadius Neighborhood radius
 * @param distanceFn Distance metric to use
 * @param neighborhoodFn Neighborhood function
 */
class OnlineSOM (lattice: Lattice, val initialLearningFactor: Double, val tuningFactor: Double,
                 neighRadius: Double, distanceFn: (Array[Double], Array[Double]) => Double,
                 neighborhoodFn: (Float, Float, Float, Float, Double) => Double)
                 extends SOM (lattice, neighRadius, distanceFn, neighborhoodFn) {

  var learningFactor: Double = initialLearningFactor

  /**
   * Self-organization process, which lasts for a maximum number
   * of iterations or until a tolerable QE is reached
   * @param vectorSet Set of vectors that will be used for training
   * @param mapConfig Configuration of the SOM which includes the training
   *                  settings
   */
  def organizeMap (vectorSet: VectorSet, mapConfig: MapConfig): Unit = {
    roughTrainingIters = mapConfig.trainIter
    // Rough-train the network for a number of iterations
    roughTraining(vectorSet)

    // Fixes neighborhood radius to only the adjacent neurons of the BMU
    decRadius = 1

    // Sets tuning factor for neurons
    lattice.neurons.flatten.foreach(x => x.setTuningRate(tuningFactor))

    // Tune the network for a number of iterations
    tuning(vectorSet, mapConfig.tuneIter, mapConfig.tolerance)

    // Once the map is organized, present inputs one last time to form clusters
    vectorSet.vectors.foreach(x => clusterInput(x))

    // Updates the trained map MQE standard of deviation
    updateMQEDeviation()
  }


  /**
   * Rough training stage of the map: globally orders the network from a initial
   * (possibly random) state
   * @param vectorSet The set used for the training
   */
  def roughTraining (vectorSet: VectorSet): Unit = {
    // Applies training for number of iters
    for (t <- 0 until roughTrainingIters) {
      // Iterator over the set
      val setIt = vectorSet.iterator
      // Present all inputs to the map
      while (setIt.hasNext) {
        // Obtain next vector to analyze
        val currentVector = setIt.next
        // Obtain BMU for current input
        val bmu = findBMU(currentVector.vector)

        // Applies learning cycle around the BMU
        lattice.neurons.flatten.foreach(x => {
          applySingleTraining(bmu._1.xPos, bmu._1.yPos, x, currentVector.vector, t)
        })
      }
      // Updates factors after iteration
      updateFactor(t)
      updateRadius(t)
    }
  }


  /**
   * Tuning stage of the map: after general order has been achieved, it refines
   * the network for a maximum number of iters or until an acceptable average
   * MQE has been reached
   * @param vectorSet The set used for the training
   * @param tuningIters The maximum number of iters in the stage
   * @param avMQETol The tolerable average MQE of the map
   */
  def tuning (vectorSet: VectorSet, tuningIters: Int, avMQETol: Double): Unit = {
    var i = 0

    do {
      // Obtains iterator over the inputs
      val setIt = vectorSet.iterator
      mapAvgMQE = 0

      // Present all inputs to the map
      while (setIt.hasNext) {
        // Obtain next vector to analyze
        val currentVector = setIt.next
        // Obtain BMY for current input
        val bmu = findBMU(currentVector.vector)

        // Accumulates the MQE of each BMU
        mapAvgMQE += bmu._2

        // Send BMU of current input for tuning
        applySingleTuning(bmu._1, currentVector.vector)
      }
      // Obtains average MQE
      mapAvgMQE /= vectorSet.vectors.size

      i += 1
    } while (mapAvgMQE > avMQETol && i < tuningIters)
  }


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
  def applySingleTraining (bmuX: Float, bmuY: Float, unit: Neuron,
                           inputVector: Array[Double], epoch: Int): Unit = {
    //Gets neuron's weight vector
    val weights = unit.weights

    for (i <- weights.indices) {
      val currentDim = weights(i)

      // Updates current dimension of the weight vector
      weights.update(i,  currentDim + learningFactor * neighborhoodFn(bmuX, bmuY, unit.xPos,
        unit.yPos, decRadius) *
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
  def applySingleTuning (bmu: Neuron, inputVector: Array[Double]): Unit = {
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
          decRadius) * (inputVector(i) - currentDim))
      }
      x.updateTuningRate()
    })
  }


  /**
   * Updates the learning factor of rough training by inverse-of-the-time
   * function
   * @param iter Current iteration
   * @return Factor for current iteration
   */
  def updateFactor (iter: Int): Unit = {
    learningFactor = initialLearningFactor * (1 - iter/roughTrainingIters.toDouble)
  }
}
