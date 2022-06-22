package som

import java.util.concurrent.TimeUnit

import scala.util.Random


/**
 * Class to represent a bi-dimensional SOM Lattice
 *
 * @param width Width of the lattice
 * @param height Height of the lattice
 * @param initialLearningFactor Learning rate of the rough training stage
 * @param tuningFactor Initial factor for the tuning stage
 * @param neighRadius Initial neighborhood radius
 * @param radiusController Controller for the neighborhood radius shrinking
 * @param distanceFn Function used to determinate the similarity with an input
 * @param neighborhoodFn Function to control the learning of the BMU's neighbors
 * @param neighborhoodRadiusUpdateFn Time-based function to update the neighborhood radius
 */
abstract class Lattice (val width: Int, val height: Int,
                        val initialLearningFactor: Double, val tuningFactor: Double,
                        var neighRadius: Int, val radiusController: Double,
                        distanceFn: (Array[Double], Array[Double]) => Double,
                        neighborhoodFn: (Float, Float, Float, Float, Double) => Double,
                        neighborhoodRadiusUpdateFn: (Int, Int, Double) => Double) {

  /*
   * Class fields
   */
  val neurons: Array[Array[Neuron]] = Array.ofDim[Neuron](width, height)
  var learningFactor: Double = initialLearningFactor
  var dimensionality: Int = 0
  var roughTrainingIters: Int = 0
  var mapAvgMQE: Double = 0
  var mapMQEDeviation: Double = 0


  /**
   * Abstract construction function
   * @param vectorDim Dimensionality of the weights vectors
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


  def initLattice (baseSet: VectorSet, initFn: (Array[Array[Neuron]], VectorSet, Long) => Unit, randomSeed: Long = 0): Unit = {
    initFn(neurons, baseSet, randomSeed)
  }


  /**
   * Self-organization process, which lasts for a maximum number
   * of iterations or until a tolerable QE is reached
   * @param vectorSet Set of vectors that will be used for training
   * @param roughIters Maximum number of iterations for rough training stage
   * @param tuningIters Maximum number of iterations for tuning stage
   * @param avgMQE Average MQE tolerance level for tuning stage
   */
  def organizeMap (vectorSet: VectorSet, roughIters: Int, tuningIters: Int, avgMQE: Double): Unit = {
    roughTrainingIters = roughIters
    // Rough-train the network for a number of iterations
    roughTraining(vectorSet, roughIters)

    // Fixes neighborhood radius to only the adjacent neurons of the BMU
    neighRadius = 1

    // Tune the network for a number of iterations
    tuning(vectorSet, tuningIters, avgMQE)

    // Once the map is organized, present inputs one last time to form clusters
    vectorSet.vectors.foreach(x => clusterInput(x))

    updateMQEDeviation()
  }


  /**
   * Rough training stage of the map: globally orders the network from a initial
   * (possibly random) state
   * @param vectorSet The set used for the training
   * @param roughIters Number of iters that the stage will last
   */
  def roughTraining (vectorSet: VectorSet, roughIters: Int): Unit = {
    //TODO modify loop to add variation-driven stop-condition
    for (t <- 0 until roughIters) {
      // Present all inputs to the map
      while (vectorSet.hasNext) {
        // Obtain next vector to analyze
        val currentVector = vectorSet.next
        // Obtain BMU for current input
        val bmu = findBMU(currentVector.vector)

        // Applies learning cycle around the BMU
        neurons.flatten.foreach(x => {
          applySingleTraining(bmu._1.xPos, bmu._1.yPos, x, currentVector.vector, t)
        })
      }
      // Reset iteration process over the inputs
      vectorSet.reset()
      updateFactor(t)
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
      mapAvgMQE = 0

      // Present all inputs to the map
      while (vectorSet.hasNext) {
        // Obtain next vector to analyze
        val currentVector = vectorSet.next
        // Obtain BMY for current input
        val bmu = findBMU(currentVector.vector)

        // Accumulates the MQE of each BMU
        mapAvgMQE += bmu._2

        // Send BMU of current input for tuning
        applySingleTuning(bmu._1, currentVector.vector)
      }
      // Obtains average MQE
      mapAvgMQE /= vectorSet.vectors.size

      // Reset iteration process over the inputs
      vectorSet.reset()

      i += 1
    } while (mapAvgMQE > avMQETol && i < tuningIters)
  }


  /**
   * Assigns the received input to a neuron of this map
   * @param inputVector Input vector to cluster in the map
   * @return (Int, Int) pair with the indices of the BMU in the
   *         array
   */
  def clusterInput (inputVector: InputVector): (Int, Int) = {
    // Find the BMU and cluster the input in it
    val bmu = findBMU(inputVector.vector)
    bmu._1.adoptInput (inputVector, bmu._2)

    // Indices of the BMU
    neuronCoord (bmu._1.xPos, bmu._1.yPos)
  }


  /**
   * Finds the best matching unit of a given input, that is,
   * the neuron for which the quantification error (QE), namely
   * distance, is minimum to the input vector
   * @param input The input vector to find its BMU
   * @return The neuron that has the MQE for that input
   */
  def findBMU (input: Array[Double]): (Neuron, Double) = {
    neurons.flatten.map(x => (x, distanceFn(input, x.weightVector))).minBy(x => x._2)
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
                          neighRadius) * (inputVector(i) - currentDim))
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


  /**
   * Updates the average MQE of the network by accumulating the QE of each input
   * represented in the map with its BMU
   */
  def updateAvMQE (): Unit = {
    mapAvgMQE =  neurons.flatten.flatMap(x => x.representedInputs.map(y => y._2)).sum /
                 neurons.flatten.map(z => z.representedInputs.size).sum
  }


  /**
   * Updates the MQE standard deviation of the network by accumulating the difference
   * between the QE of each input with its BMU and the average MQE
   */
  def updateMQEDeviation (): Unit = {
    mapMQEDeviation = math.sqrt(neurons.flatten.filter(n => n.representedInputs.nonEmpty).
                      flatMap(x => x.representedInputs.map(y => math.pow(y._2 - mapAvgMQE, 2))).sum /
                      (neurons.flatten.map(z => z.representedInputs.size).sum - 1))
  }


  /**
   * Provides the threshold above which an instance mus be considered abnormal
   * Threshold is obtained as 3 times the upper deviation of the MQE
   * @return Value of the threshold
   */
  def normalityThreshold: Double = {
    3 * (mapAvgMQE + mapMQEDeviation)
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


  /**
   * Prints each neuron as in printMap but adding how many classes it represents
   */
  def printClassesBalance (): Unit


  /**
   * Prints the name of the class that each neuron represents (the class from which
   * the neuron has most instances)
   */
  def printMainClasses (): Unit


  /**
   * Provides the distribution of the neurons in this lattice
   * @return LatticeDistribution constant for this lattice's distribution
   */
  def latticeType: Int


  /**
   * Obtains the location in the 2-dimensional array of a neuron
   * depending of the coordinate system of the lattice distribution
   * @param xPos X coordinate in the specific distribution
   * @param yPos Y coordinate in the specific distribution
   * @return (Int, Int) pair with the X and Y coordinates as array indices
   */
  def neuronCoord (xPos: Float, yPos: Float): (Int, Int)
}


/**
 * Factory object for creating lattices with the specified distribution
 */
object LatticeFactory {
  /**
   * Creates a lattice of the received distribution with the specified parameters
   * @param latDistrib
   * @param width
   * @param height
   * @param learningFactor
   * @param tuningFactor
   * @param neighRadius
   * @param somRadiusController
   * @param distanceFn
   * @param neighborhoodFn
   * @param radiusDecreaseFn
   * @return A initial state lattice of the given distribution
   */
  def createLattice (latDistrib: Int, width: Int, height: Int, learningFactor: Double, tuningFactor: Double,
                     neighRadius: Int, somRadiusController: Double, distanceFn: (Array[Double], Array[Double]) => Double,
                     neighborhoodFn: (Float, Float, Float, Float, Double) => Double,
                     radiusDecreaseFn: (Int, Int, Double) => Double): Lattice = {

    if (latDistrib == LatticeDistribution.squared) {
      // Rectangular lattice
      new RectLattice(width, height, learningFactor, tuningFactor, neighRadius, somRadiusController,
        distanceFn, neighborhoodFn, radiusDecreaseFn)
    }
    else if (latDistrib == LatticeDistribution.hexagonal) {
      // Hexagonal lattice
      new HexLattice(width, height, learningFactor, tuningFactor, neighRadius, somRadiusController,
        distanceFn, neighborhoodFn, radiusDecreaseFn)
    }
    else null
  }
}


/**
 * Lattice distributions codes for creation
 */
object LatticeDistribution {
  val squared = 0
  val hexagonal = 1
}
