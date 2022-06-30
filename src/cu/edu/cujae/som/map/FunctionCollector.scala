package cu.edu.cujae.som.map

import cu.edu.cujae.som.data.VectorSet

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
  def initFactory (init: String): (Iterable[Array[Double]], VectorSet, Long) => Unit = {
    if (init == InitFns.randomInit) randomInit
    else if (init == InitFns.normalizedRandomInit) normalizedRandomInit
    else null
  }


  /**
   * Provides a specific distance function given its id
   * @param dist The distance function's id
   * @return Desired distance function or null if the id does not exists
   */
  def distanceFactory (dist: String): (Array[Double], Array[Double]) => Double = {
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
  def neighboringFactory (neigh: String): (Float, Float, Float, Float, Double) => Double = {
    if (neigh == NeighboringFns.gaussian) gaussianNeighborhood
    else null
  }


  /**
   * Initializes the neuron's weight vectors with random values
   * between the bounds of each input dimension
   * @param vectors Neurons to initialize weights
   * @param vectorSet Input domain for initialization
   * @param seed Seed for random initialization
   */
  def randomInit (vectors: Iterable[Array[Double]], vectorSet: VectorSet, seed: Long): Unit = {
    // Obtains bounds of the input's dimensions
    val bounds = vectorSet.dimBounds
    val rand = new Random()
    rand.setSeed(seed)

    vectors.foreach(x => {
      // Generates value for each dimension
      for (i <- bounds.indices) x.update(i, rand.between(bounds(i)._1, bounds(i)._2))
    })
  }


  /**
   * Initializes the neuron's weight vectors with random values
   * normalized between the bounds of each input dimension
   * @param vectors Neurons to initialize weights
   * @param vectorSet Input domain for initialization
   * @param seed Seed for random initialization
   */
  def normalizedRandomInit (vectors: Iterable[Array[Double]], vectorSet: VectorSet, seed: Long): Unit = {
    // Obtains bounds of the input's dimensions
    val bounds = vectorSet.dimBounds
    val rand = new Random()
    rand.setSeed(seed)

    vectors.foreach(x => {
      // Generates value for each dimension
      for (i <- bounds.indices) {
        val dimMin = bounds(i)._1
        val dimMax = bounds(i)._2

        x.update(i, (rand.between(dimMin, dimMax) - dimMin) / (dimMax - dimMin) )
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
   * Computes the squared euclidean distance between two vectors
   * @param arr1 1st vector
   * @param arr2 2do vector
   * @return
   */
  def squaredEuclideanDistance (arr1: Array[Double], arr2: Array[Double]): Double = {
    (arr1 zip arr2).map(x => math.pow(x._1 - x._2, 2)).sum
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
}


object InitFns {
  val randomInit = "Random"
  val normalizedRandomInit = "Normalized Random"
}


/**
 * Object for representing the distance functions
 */
object DistanceFns {
  val simpleEuclidean = "Euclidean"
  val squaredEuclidean = "Squared Euclidean"
  val manhattan = "Manhattan"
}


/**
 * Object for representing the neighboring functions
 */
object NeighboringFns {
  val gaussian = "Gaussian"
}