package som

import scala.util.Random

object FunctionCollector {
  /**
   * Creates a vector with random values in the range provided
   * @param vector Vector to initialize
   * @param bounds Set of lower and upper bounds for each dimension
   * @return The created random vector
   */
  def normalizedRandomInit (vector: Array[Double], bounds: Array[(Double, Double)]): Unit = {
    val dim = vector.length

    for (i <- 0 until dim) {
      val dimMin = bounds(i)._1
      val dimMax = bounds(i)._2

      vector.update(i, (Random.between(dimMin, dimMax) - dimMin) / (dimMax - dimMin) )
    }
    /**print("Vector: ")
    x.foreach(x => print(x + ", "))
    println()*/
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
   * @param epoch Current time value
   * @return
   */
  def gaussianNeighborhood (bmuX: Int, bmuY: Int, neighX: Int, neighY: Int, radiusDecrease: Double, epoch: Int): Double = {
    val distance = math.pow(bmuX - neighX, 2) + math.pow(bmuY - neighY, 2)

    val value  = math.exp( (-distance) / (2 * math.pow(radiusDecrease, 2)))

    if (value.isNaN) 0 else value
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
