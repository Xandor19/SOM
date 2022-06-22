package som

import scala.util.Random

/**
 * Contains multiple functions (distance, neighboring, radius decrease)
 * used by the SOM
 * Provides factory methods for obtaining the desired function
 */
object FunctionCollector {

  /**
   * Provides a specific initialization function given its id
   * @param init The initialization function's id
   * @return Desired initialization function or null if the id does not exists
   */
  def initFactory (init: Int): (Array[Array[Neuron]], VectorSet, Long) => Unit = {
    if (init == InitFns.randomInit) randomInit
    else if (init == InitFns.normalizedRandomInit) normalizedRandomInit
    else null
  }


  /**
   * Provides a specific distance function given its id
   * @param dist The distance function's id
   * @return Desired distance function or null if the id does not exists
   */
  def distanceFactory (dist: Int): (Array[Double], Array[Double]) => Double = {
    if (dist == DistanceFns.simpleEuclidean) euclideanDistance
    else if (dist == DistanceFns.squaredEuclidean) squaredEuclideanDistance
    else if (dist == DistanceFns.manhattan) manhattanDistance
    else null
  }


  /**
   * Provides a specific neighboring function given its id
   * @param neigh The neighboring function's id
   * @return Desired neighboring function or null if the id does not exists
   */
  def neighboringFactory (neigh: Int): (Float, Float, Float, Float, Double) => Double = {
    if (neigh == NeighboringFns.gaussian) gaussianNeighborhood
    else if (neigh == NeighboringFns.inverse) inverseNeighborhood
    else if (neigh == NeighboringFns.proportional) proportionalNeighborhood
    else null
  }


  /**
   * Provides a specific neighboring radius decrease function given its id
   * @param rad The decrease function's id
   * @return Desired decrease function or null if the id does not exists
   */
  def radiusDecreaseFactory (rad: Int): (Int, Int, Double) => Double = {
    if (rad == RadiusDecreaseFns.exponential) exponentialRadiusDecrease
    else null
  }


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
    math.sqrt(squaredEuclideanDistance(arr1, arr2))
  }


  /**
   * Computes the manhattan distance between two vectors
   * @param arr1 1st vector
   * @param arr2 2do vector
   * @return
   */
  def manhattanDistance (arr1: Array[Double], arr2: Array[Double]): Double = {
    (arr1 zip arr2).map(x => math.abs(x._1 - x._2)).sum
  }


  /**
   * Computes the squared euclidean distance between two vectors
   * @param arr1 1st vector
   * @param arr2 2do vector
   * @return
   */
  def squaredEuclideanDistance (arr1: Array[Double], arr2: Array[Double]): Double = {
    (arr1 zip arr2).map(x => math.pow(x._1 - x._2, 2)).sum
  }


  /**
   * Neighborhood function which gradually reduces the impact of an input
   * on the neuron depending of its relative position to the BMU
   * Uses the gaussian function:
   * exp(squaredDistance(BMU, neighbor) / 2 * currentRadius**2
   *
   * @param bmuX Value of x position of the BMU on the lattice
   * @param bmuY Value of the y position of the BMU on the lattice
   * @param neighX Value of x position of the neighbor on the lattice
   * @param neighY Value of the y position of the neighbor on the lattice
   * @param neighRadius Current neighborhood radius
   * @return Effect of the function in the neuron learning
   */
  def gaussianNeighborhood (bmuX: Float, bmuY: Float, neighX: Float, neighY: Float, neighRadius: Double): Double = {
    val distance = squaredEuclideanDistance(Array(bmuX, bmuY), Array(neighX, neighY))

    if (distance == 0) 1
    else {
      val value = math.exp( (-distance) / (2 * math.pow(neighRadius, 2)))

      if (value.isNaN) 0 else value
    }
  }


  /**
   * Neighborhood function which reduces the impact of an input on the neuron depending
   * on the inverse proportionality of the distance between the neuron and the BMU
   * @param bmuX Value of x position of the BMU on the lattice
   * @param bmuY Value of the y position of the BMU on the lattice
   * @param neighX Value of x position of the neighbor on the lattice
   * @param neighY Value of the y position of the neighbor on the lattice
   * @param neighRadius Current neighborhood radius
   * @return Effect of the function in the neuron learning
   */
  def proportionalNeighborhood (bmuX: Float, bmuY: Float, neighX: Float, neighY: Float, neighRadius: Double): Double = {
    val distance = squaredEuclideanDistance(Array(bmuX, bmuY), Array(neighX, neighY))

    if (distance == 0) 1
    else 1 / distance
  }


  /**
   * Neighborhood function which reduces the impact of an input on the neuron depending
   * on the inverse of the distance between the neuron and the BMU
   * @param bmuX Value of x position of the BMU on the lattice
   * @param bmuY Value of the y position of the BMU on the lattice
   * @param neighX Value of x position of the neighbor on the lattice
   * @param neighY Value of the y position of the neighbor on the lattice
   * @param neighRadius Current neighborhood radius
   * @return Effect of the function in the neuron learning
   */
  def inverseNeighborhood (bmuX: Float, bmuY: Float, neighX: Float, neighY: Float, neighRadius: Double): Double = {
    val distance = squaredEuclideanDistance(Array(bmuX, bmuY), Array(neighX, neighY))

    if (distance == 0) 1
    else 1 - distance / neighRadius
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


object InitFns {
  val randomInit = 0
  val normalizedRandomInit = 1
}


/**
 * Object for representing the distance functions
 */
object DistanceFns {
  val simpleEuclidean = 0
  val squaredEuclidean = 1
  val manhattan = 2
}


/**
 * Object for representing the neighboring functions
 */
object NeighboringFns {
  val gaussian = 0
  val proportional = 1
  val inverse = 2
}


/**
 * Object for representing the neighborhood radius
 * decrease functions
 */
object RadiusDecreaseFns {
  val exponential = 0
}