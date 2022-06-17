package som

import java.util.concurrent.TimeUnit

import scala.util.Random

object SOMRunner {
  def main(args: Array[String]): Unit = {
    val path = "/mnt/D80C76380C76122C/Mis Programas/Repos/SOM/Datasets/iris.csv"
    val sep = ','
    val trainingSetProp = 0.8
    val latDistrib = LatticeDistribution.squared
    val somLearningFactor = 0.8
    val somTuningFactor = 0.2
    val somNeighRadius = 5
    val somRadiusController = 0.5
    val roughIters = 500
    val tuningIters = 54000
    val tolerance = 0
    val initFn: (Array[Array[Neuron]], VectorSet, Long) => Unit = FunctionCollector.normalizedRandomInit
    val distanceFn: (Array[Double], Array[Double]) => Double = FunctionCollector.euclideanDistance
    val neighborhoodFn: (Float, Float, Float, Float, Double) => Double = FunctionCollector.gaussianNeighborhood
    val radiusFn: (Int, Int, Double) => Double = FunctionCollector.exponentialRadiusDecrease
    val initSeed = 78
    val shuffleSeed = 43

    val initTime = System.nanoTime()
    flowFromScratch(path, sep, trainingSetProp, latDistrib, somLearningFactor, somTuningFactor, somNeighRadius,
              somRadiusController, roughIters, tuningIters, tolerance, initFn, distanceFn, neighborhoodFn, radiusFn,
      initSeed, shuffleSeed)
    val endTime = System.nanoTime()
    val seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    println("Total elapsed time: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")
  }


  /**
   * Control flow for creating an training a new SOM
   * @param datasetPath
   * @param datasetSeparator
   * @param trainingSetProp
   * @param latDistrib
   * @param somLearningFactor
   * @param somTuningFactor
   * @param somNeighRadius
   * @param somRadiusController
   * @param roughIters
   * @param tuningIters
   * @param tolerance
   * @param initFn
   * @param distanceFn
   * @param neighborhoodFn
   * @param radiusDecreaseFn
   * @param initSeed
   * @param shuffleSeed
   */
  def flowFromScratch (datasetPath: String, datasetSeparator: Char, trainingSetProp: Double,
                       latDistrib: Int, somLearningFactor: Double, somTuningFactor: Double, somNeighRadius: Int,
                       somRadiusController: Double, roughIters: Int, tuningIters: Int, tolerance: Double,
                       initFn: (Array[Array[Neuron]], VectorSet, Long) => Unit, distanceFn: (Array[Double], Array[Double]) => Double,
                       neighborhoodFn: (Float, Float, Float, Float, Double) => Double,
                       radiusDecreaseFn: (Int, Int, Double) => Double, initSeed: Long, shuffleSeed: Long): Unit = {

    val sets = loadSets(datasetPath, datasetSeparator, trainingSetProp, shuffleSeed)
    val trainingSet = sets._1
    val testSet = sets._2

    prepareTraining(trainingSet)

    val width = 9
    val height = 6

    // Train a new SOM
    val som = createSOM(latDistrib, width, height, somLearningFactor, somTuningFactor, somNeighRadius,
                             somRadiusController, roughIters, tuningIters, tolerance, initFn, distanceFn,
                             neighborhoodFn, radiusDecreaseFn, trainingSet, trainingSet.dimensionality, initSeed)

    printSOMState(som)

    //ReaderWriter.exportTrainingToCSV("/home/xandor19/training.csv", som)
  }


  /**
   * Loads the dataset into a training and a test set
   * @param datasetPath
   * @param datasetSeparator
   * @param trainingSetProp
   * @param shuffleSeed
   * @return
   */
  def loadSets (datasetPath: String, datasetSeparator: Char, trainingSetProp: Double, shuffleSeed: Long):
               (VectorSet, VectorSet) = {
    //Random instance
    val rand = new Random()
    rand.setSeed(shuffleSeed)

    // Loads dataset
    var initTime = System.nanoTime()
    val data = ReaderWriter.loadSetFromCSV(datasetPath, datasetSeparator)
    var endTime = System.nanoTime()
    var seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    println("File loaded in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    // Gets inputs' dimensionality
    val dimensionality = data._1
    println("Dataset dimensionality is " + dimensionality)

    // Defines training set as the 80% of the dataset
    val trainingSetSize = (data._3.length * trainingSetProp).toInt
    println("Training set size: " + trainingSetSize)

    // Shuffles dataset to ensure random segmentation
    initTime = System.nanoTime()
    val inputVectors = rand shuffle data._3
    endTime = System.nanoTime()
    seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    println("Inputs shuffled in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    // Obtains training set from the shuffled input
    val trainingSet = new RandomVectorSet(data._2, inputVectors.slice(0, trainingSetSize), shuffleSeed)
    println("Training set created")

    // Obtains test set as the remaining inputs
    val testSet = new SequentialVectorSet(data._2, inputVectors.slice(trainingSetSize, data._3.size))
    println("Test set created")

    (trainingSet, testSet)
  }


  /**
   * Prepares the dataset for training
   * //TODO parameters for training configuration
   * @param trainingSet
   */
  def prepareTraining (trainingSet: VectorSet): Unit = {
    // Obtains the input's dimensions bounds
    var initTime = System.nanoTime()
    trainingSet.findBounds()
    var endTime = System.nanoTime()
    var seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    println("Dimensionality bounds found in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    // Normalizes the input space
    initTime = System.nanoTime()
    trainingSet.normalize()
    endTime = System.nanoTime()
    seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    println("Input space normalized in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")
  }


  /**
   * Creates and trains a new SOM with the specified parameters and training set
   * @param latDistrib Distribution of the lattice according to LatticeDistribution constants
   * @param width Width of the lattice
   * @param height Height of the lattice
   * @param somLearningFactor Learning factor for the rough training stage
   * @param somTuningFactor Learning factor for the tuning stage
   * @param somNeighRadius Neighborhood radius for the rough training stage
   * @param somRadiusController Controller of the radius decrement
   * @param roughIters Total iterations of the rough training stage
   * @param tuningIters Maximum iterations of the tuning stage
   * @param tolerance Tolerable average MQE of the network
   * @param initFn Function to initialize the weights
   * @param distanceFn Distance metric to use
   * @param neighborhoodFn Function for smooth neighborhood
   * @param radiusDecreaseFn Function to decrease the neighborhood radius
   * @param trainingSet Set of inputs for training
   * @param dimensionality Dimensionality of the inputs
   * @param initSeed Seed for random initialization
   * @return A trained SOM
   */
  def createSOM (latDistrib: Int, width: Int, height: Int, somLearningFactor: Double, somTuningFactor: Double,
                 somNeighRadius: Int, somRadiusController: Double, roughIters: Int, tuningIters: Int,
                 tolerance: Double, initFn: (Array[Array[Neuron]], VectorSet, Long) => Unit,
                 distanceFn: (Array[Double], Array[Double]) => Double,
                 neighborhoodFn: (Float, Float, Float, Float, Double) => Double,
                 radiusDecreaseFn: (Int, Int, Double) => Double, trainingSet: VectorSet, dimensionality: Int,
                 initSeed: Long): Lattice = {
    // Creates the SOM lattice with given distribution
    var initTime = System.nanoTime()
    val som = Factory.createLattice(latDistrib, width, height, somLearningFactor, somTuningFactor, somNeighRadius,
                                    somRadiusController, distanceFn, neighborhoodFn, radiusDecreaseFn)

    // Sets the initial state of the lattice by initializing neurons and setting the distance function
    som.constructLattice(dimensionality)
    var endTime = System.nanoTime()
    var seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    println("Lattice created in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    // Initializes the weight vectors with the specified function
    initTime = System.nanoTime()
    som.initLattice(trainingSet, initFn, initSeed)
    //som.initLattice(trainingSet, FunctionCollector.randomInit, initSeed)
    endTime = System.nanoTime()
    seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    println("Lattice initialized in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    // SOM's training process
    initTime = System.nanoTime()
    som.organizeMap(trainingSet, roughIters, tuningIters, tolerance)
    endTime = System.nanoTime()
    seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    println("SOM trained in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    som
  }


  /**
   * Loads a pre-trained SOM from an external source
   */
  def loadSOM (somPath: String, somSeparator: Char): Unit = {
    /**val load = ReaderWriter.loadTrainingFromCSV("/home/xandor19/training.csv", ',')
    // Creates the SOM lattice with specified parameters
    val som = new RectLattice(load._1(0), load._1(1), somLearningFactor, somFactorController, somNeighRadius,
                          somRadiusController, FunctionCollector.euclideanDistance,
                          FunctionCollector.gaussianNeighborhood, FunctionCollector.exponentialRadiusDecrease)

    // Imports the received training information to the SOM
    som.importLattice(load._2, load._1(2))

    // Clusters the training set in the pre-trained SOM
    trainingSet.vectors.foreach(x => som.clusterInput(x))*/
  }


  /**
   * Summarizes the state of a given SOM
   * @param som The SOM
   */
  def printSOMState (som: Lattice): Unit = {
    som.printSet()
    println()
    println()
    som.printMap()
    println()
    println()
    som.printClassesBalance()
    println()
    println()
    som.printMainClasses()
    println()
    println()

    println("AVG MQE: " + som.mapAvgMQE)
    println("MQE standard deviation: " + som.mapMQEDeviation)
    println("Normality threshold: " + som.normalityThreshold)
  }
}
