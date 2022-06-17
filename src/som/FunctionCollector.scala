package som

import scala.util.Random

object FunctionCollector {

  /**
   * Initializes the neuron's weight vectors with random values
   * between the bounds of each input dimension
   * @param neurons Neurons to initialize weights
   * @param vectorSet Input domain for initialization
   * @param seed Seed for random initialization
   */
  def randomInit (neurons: Array[Array[Neuron]], vectorSet: VectorSet, seed: Long): Unit = {
    // Obtains bounds of the input's dimensions
    val bounds = vectorSet.dimBounds
    val rand = new Random()
    rand.setSeed(seed)

    neurons.flatten.foreach(x => {
      // Generates value for each dimension
      for (i <- bounds.indices) x.weights.update(i, rand.between(bounds(i)._1, bounds(i)._2))
    })
  }


  /**
   * Initializes the neuron's weight vectors with random values
   * normalized between the bounds of each input dimension
   * @param neurons Neurons to initialize weights
   * @param vectorSet Input domain for initialization
   * @param seed Seed for random initialization
   */
  def normalizedRandomInit (neurons: Array[Array[Neuron]], vectorSet: VectorSet, seed: Long): Unit = {
    // Obtains bounds of the input's dimensions
    val bounds = vectorSet.dimBounds
    val rand = new Random()
    rand.setSeed(seed)

    neurons.flatten.foreach(x => {
      // Generates value for each dimension
      for (i <- bounds.indices) {
        val dimMin = bounds(i)._1
        val dimMax = bounds(i)._2

        x.weights.update(i, (rand.between(dimMin, dimMax) - dimMin) / (dimMax - dimMin) )
      }
    })
  }


  /**
   * Computes the simple euclidean distance between two vectors
   * @param arr1 1st vector
   * @param arr2 2do vector
   * @return
   */
  def euclideanDistance (arr1: Array[Double], arr2: Array[Double]): Double = {
    math.sqrt((arr1 zip arr2).map(x => math.pow(x._1 - x._2, 2)).sum)
  }


  /**
   * Neighborhood function, which gradually reduces the impact of an input
   * on the neuron depending of its relative position to the BMU
   * Uses the gaussian function:
   * exp(squaredDistance(BMU, neighbor) / 2 * currentRadius**2
   *
   * @param bmuX Value of x position of the BMU on the lattice
   * @param bmuY Value of the y position of the BMU on the lattice
   * @param neighX Value of x position of the neighbor on the lattice
   * @param neighY Value of the y position of the neighbor on the lattice
   * @param neighRadius Current neighborhood radius
   * @return
   */
  def gaussianNeighborhood (bmuX: Float, bmuY: Float, neighX: Float, neighY: Float, neighRadius: Double): Double = {
    val distance = math.pow(bmuX - neighX, 2) + math.pow(bmuY - neighY, 2)

    if (distance == 0) 1
    else {
      val value = math.exp( (-distance) / (2 * math.pow(neighRadius, 2)))

      if (value.isNaN) 0 else value
    }
  }


  /**
   * Adapts the learning factor to a given time status
   * @param epoch Current time value
   * @return Learning factor to apply at given time
   */
  def exponentialFactorDecrease (learningFactor: Double, epoch: Int, factorController: Double): Double = {
    // Exponentially decreases the learning factor depending of epoch
    val value = learningFactor * math.exp(-epoch / factorController)
    if (value.isNaN) 0 else value
  }


  /**
   * Adapts the neighborhood radius to a given time status
   * @param epoch Current time value
   * @return Learning factor to apply at given time
   */
  def exponentialRadiusDecrease (neighRadius: Int, epoch: Int, radiusController: Double): Double = {
    // Exponentially decreases the neighborhood radius depending of epoch
    val value = neighRadius * math.exp(-epoch / radiusController)

    if (value.isNaN) 0 else value
  }
}
