package cu.edu.cujae.som.map

import cu.edu.cujae.som.data.{InputVector, VectorSet}
import cu.edu.cujae.som.io.MapConfig

import scala.util.Random

/**
 * Abstract class to represent general Self-Organizing Map
 * @param lattice The lattice of neurons
 * @param neighRadius Neighborhood radius
 * @param distanceFn Distance metric to use
 * @param neighborhoodFn Neighborhood function
 */
abstract class SOM (val lattice: Lattice, var neighRadius: Double,
                    val distanceFn: (Array[Double], Array[Double]) => Double,
                    val neighborhoodFn: (Float, Float, Float, Float, Double) => Double) {

  /*
   * Class fields
   */
  var dimensionality: Int = 0
  var decRadius: Double = neighRadius
  var roughTrainingIters: Int = 0
  var mapAvgMQE: Double = 0
  var mapMQEDeviation: Double = 0


  /**
   * Sets the SOM to a initial state
   *
   * @param trainingSet Input space that will be used for training
   * @param initFn Function for initializing the weights
   * @param seed Seed for random initialization
   */
  def initSOM (trainingSet: VectorSet, initFn: (Iterable[Array[Double]], VectorSet, Long) => Unit,
              seed: Long = Random.nextInt()): Unit = {
    // Sets the SOM dimensionality
    dimensionality = trainingSet.dimensionality

    // Creates a set of vectors with the given initialization function
    val initVectors = List.fill(lattice.width * lattice.height) {
      new Array[Double](trainingSet.dimensionality)
    }
    initFn(initVectors, trainingSet, seed)

    // Initializes the lattice with the obtained set
    lattice.constructLattice(initVectors)
  }


  /**
   * Finds the best matching unit of a given input, that is,
   * the neuron for which the quantification error (QE), namely
   * distance, is minimum to the input vector
   *
   * @param input The input vector to find its BMU
   * @return The neuron that has the MQE for that input and the error value
   */
  def findBMU (input: Array[Double]): (Neuron, Double) = {
    lattice.neurons.flatten.map(x => (x, distanceFn(input, x.weightVector))).minBy(x => x._2)
  }


  /**
   * Assigns the received input to a neuron of this map
   *
   * @param inputVector Input vector to cluster in the map
   * @return BMU for the input
   */
  def clusterInput (inputVector: InputVector): Neuron = {
    // Find the BMU and cluster the input in it
    val bmu = findBMU(inputVector.vector)
    bmu._1.adoptInput(inputVector, bmu._2)

    bmu._1
  }


  /**
   * Self-organization process, which lasts for a maximum number
   * of iterations or until a tolerable QE is reached
   *
   * @param vectorSet Set of vectors that will be used for training
   * @param mapConfig Configuration of the SOM which includes the training
   *                  settings
   */
  def organizeMap (vectorSet: VectorSet, mapConfig: MapConfig): Unit


  /**
   * Updates the average MQE of the network by accumulating the QE of each input
   * represented in the map with its BMU
   */
  def updateAvMQE (): Unit = {
    mapAvgMQE = lattice.neurons.flatten.flatMap(x => x.representedInputs.values).sum /
      lattice.neurons.flatten.map(z => z.representedInputs.size).sum
  }


  /**
   * Updates the MQE standard deviation of the network by accumulating the difference
   * between the QE of each input with its BMU and the average MQE
   */
  def updateMQEDeviation (): Unit = {
    mapMQEDeviation = math.sqrt(lattice.neurons.flatten.filter(n => n.representedInputs.nonEmpty).
      flatMap(x => x.representedInputs.map(y => math.pow(y._2 - mapAvgMQE, 2))).sum /
             (lattice.neurons.flatten.map(z => z.representedInputs.size).sum - 1))
  }


  /**
   * Provides the threshold above which an instance mus be considered abnormal
   * Threshold is obtained as 3 times the upper deviation of the MQE
   *
   * @return Value of the threshold
   */
  def normalityThreshold: Double = {
    3 * (mapAvgMQE + mapMQEDeviation)
  }


  /**
   * Provides the neuron in the specified array index
   *
   * @param x Row position in the array
   * @param y Column position in the array
   * @return The neuron
   */
  def neuronAt (x: Int, y: Int): Neuron = {
    lattice.neurons(x)(y)
  }


  /**
   * Updates neighborhood radius in inverse-of-time function
   * @param iter Current iteration
   */
  def updateRadius (iter: Int): Unit = {
    decRadius = neighRadius * (1 - iter / roughTrainingIters.toFloat)
  }
}

/**
 * Factory object for creating a SOM with the specified training approach
 */
object SOMFactory {
  /**
   * Creates the specified type of SOM with the received parameters
   * @param config Configuration parameters of the SOM
   * @return Created SOM
   */
  def createSOM (config: MapConfig): SOM = {
    // Obtains specified functions
    val distFn = FunctionCollector.distanceFactory(config.distanceFn)
    val neighFn = FunctionCollector.neighboringFactory(config.neighFn)

    if (config.somType == SOMType.onlineSOM) {
      // Online training SOM
      new OnlineSOM(LatticeFactory.createLattice(config.latDistrib, config.width, config.height), config.learnFactor,
                    config.tuneFactor, config.neighRadius, distFn, neighFn)
    }
    else {
      // Batch learning SOM
      new BatchSOM(LatticeFactory.createLattice(config.latDistrib, config.width, config.height), config.neighRadius,
                   distFn, neighFn)
    }
  }
}


/**
 * SOM types (by training approach) codes
 */
object SOMType {
  val onlineSOM = "On-line"
  val batchSOM = "Batch"
}
