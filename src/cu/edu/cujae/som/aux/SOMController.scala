package cu.edu.cujae.som.aux

import java.util.concurrent.TimeUnit

import cu.edu.cujae.som.data.{InputVector, RandomVectorSet, SequentialVectorSet, VectorSet}
import cu.edu.cujae.som.io.{ExperimentData, MapConfig, ReaderWriter}
import cu.edu.cujae.som.map.{FunctionCollector, Lattice, SOM, SOMFactory}

import scala.util.Random

/**
 * Singleton object to control the life cycle of a SOM
 */
object SOMController {
  /**
   * Controls the creation, training and results saving of a specified number of SOM
   * @param config Configuration parameters for the execution
   */
  def newSOM (config: MapConfig): Unit = {
    // Loads the main dataset
    val dataset = ReaderWriter.loadSetFromCSV(config.dataset, config.setSep)
    val datasetName = config.dataset.slice(config.dataset.lastIndexOf("/"), config.dataset.length)
    val exportName = "results_of_" + datasetName
    // Number of SOM to create
    val runs = config.runs

    // Measurement variables
    var trainingAvMQE = 0.0
    var trainingMQEDeviation = 0.0
    var avgCorrect = 0.0
    var avgIncorrect = 0.0
    var avgPrecision = 0.0
    var testAvMQE = 0.0
    var testMQEDeviation = 0.0

    val expInitTime = System.nanoTime()
    for (test <- 0 until runs) {
      // Divides the dataset into training and testing (if desired)
      val dividedSets = prepareSet(dataset, config.setProp, config.trainingProp, config.shuffleSeed)

      // Obtains sets
      val trainingSet = dividedSets._1
      val testSet = dividedSets._2

      // If the parameters were not specified, runs auto-configuration
      if (config.width == 0 || config.height == 0) autoDistribute(trainingSet, config)

      // Prepares training
      prepareTraining(trainingSet, config.normalize)

      // Train a new SOM
      val som = createSOM(config, trainingSet)

      // Shows the state of the SOM after training
      printSOMState(som.lattice)

      // Obtains and accumulates resulting average MQE and its standard of deviation of the training
      val runTrainAvMQE = som.mapAvgMQE
      val runTrainMQEDeviation = som.mapMQEDeviation

      trainingAvMQE += runTrainAvMQE
      trainingMQEDeviation += runTrainMQEDeviation

      println("\nTraining for run " + test + " ended with average MQE of " + runTrainAvMQE + " and standard deviation of " +
        runTrainMQEDeviation)

      if (config.trainingProp < 1) {
        // Tests the clustering capacities of the SOM
        val results = clusterTest(som, testSet, config.normalize)

        printSOMState(som.lattice)

        // Accumulates the results of the clustering test
        avgCorrect += results._1
        avgIncorrect += results._2
        avgPrecision += results._3

        // Updates network error with new inputs
        som.updateAvMQE()
        som.updateMQEDeviation()

        // Accumulates the new error
        val runTestAvMQE = som.mapAvgMQE
        val runTestMQEDeviation = som.mapMQEDeviation

        testAvMQE += runTestAvMQE
        testMQEDeviation += runTestMQEDeviation

        println("\nTest " + test + " average MQE is " + runTestAvMQE + " with a standard deviation of " + runTestMQEDeviation)
      }
    }
    if (runs > 1) {
      // Obtains measures averages if more than one run was made
      trainingAvMQE /= runs
      trainingMQEDeviation /= runs
      avgCorrect /= runs
      avgIncorrect /= runs
      avgPrecision /= runs
      testAvMQE /= runs
      testMQEDeviation /= runs
    }
    val expEndTime = System.nanoTime()
    val seconds = TimeUnit.SECONDS.convert(expEndTime - expInitTime, TimeUnit.NANOSECONDS)
    println("\nAll runs were completed in " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    // Exports the run results
    ReaderWriter.exportExperimentResult(config.resultsExportPath + exportName, config.setSep,
                                        List[ExperimentData](new ExperimentData(config, trainingAvMQE, trainingMQEDeviation,
                                                                                avgCorrect, avgIncorrect, avgPrecision,
                                                                                testAvMQE, testMQEDeviation)))
    //TODO bests models trained export
    //ReaderWriter.exportTrainingToCSV("/home/xandor19/training.csv", som)
  }


  /**
   * Prepares the received dataset and divides it into training and test set
   * @param dataset Source dataset
   * @param setProp Proportion of the dataset to use, if only a sample is desired
   * @param trainingSetProp Proportion of the final dataset to use as training set
   * @param shuffleSeed Seed for traceable shuffling
   * @return Tuple of two VectorSet objects, namely, the training and test sets
   */
  private def prepareSet (dataset: (Array[String], List[InputVector]), setProp: Double, trainingSetProp: Double,
                          shuffleSeed: Long): (VectorSet, VectorSet) = {
    //Random instance
    val rand = new Random()
    rand.setSeed(shuffleSeed)

    print("\nPreparing dataset")
    // Prepares the dataset, performing a stratified sampling if desired
    val initTime = System.nanoTime()
    val inputVectors = if (setProp < 1) Utils.stratified(dataset._2, setProp, shuffleSeed)._1
                       else rand shuffle dataset._2
    val endTime = System.nanoTime()
    val seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    print("\nInputs " + (if (setProp < 1) "stratified and " else "") + "shuffled in: " +
      seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    print("\nDataset " + (if (setProp < 1) "stratified sample " else "") + "size: " + inputVectors.size)
    // Defines training set as the given percent of the datasetset
    val trainingSetSize = (inputVectors.size * trainingSetProp).toInt
    print("\nTraining set size: " + trainingSetSize)

    // Obtains stratified training and test sets
    val split = Utils.stratified(inputVectors, trainingSetProp, shuffleSeed)

    // Obtains training set from stratified slice of the inputs
    val trainingSet = new RandomVectorSet(dataset._1, split._1, shuffleSeed)
    print("\nTraining set created with size " + trainingSet.sampleSize)

    // Obtains test set as the remaining inputs
    val testSet = new SequentialVectorSet(dataset._1, split._2)
    print("\nTest set created with size " + testSet.sampleSize)

    (trainingSet, testSet)
  }


  /**
   * Obtains the SOM distribution (width, height, neighborhood radius and stages iterations
   * based on the training set characteristics
   * Updates the SOM configuration object with the distribution results
   * @param from Training set
   * @param config Incomplete configuration parameters of the SOM
   */
  private def autoDistribute (from: VectorSet, config: MapConfig): Unit = {
    val neurons = math.sqrt(from.sampleSize) * 5
    var height = math.sqrt(neurons).toInt
    val width = if (neurons - math.pow(height, 2) > height / 2) height + 1
                else if (neurons - math.pow(height, 2) > height * 2) { height += 1; height }
                else height
    val neighRadius = width / 2 + 1
    val roughIters = (neurons * 50).toInt
    val tuningIters = (neurons * 500).toInt

    config.completeConfig(width, height, neighRadius, roughIters, tuningIters)
    print("\nSOM distribution auto-configured")
  }


  /**
   * Prepares the dataset for training
   * //TODO parameters for training configuration
   */
  private def prepareTraining (trainingSet: VectorSet, normalize: Boolean): Unit = {
    // Obtains the input's dimensions bounds
    var initTime = System.nanoTime()
    trainingSet.findBounds()
    var endTime = System.nanoTime()
    var seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    print("\nDimensionality bounds found in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    if (normalize) {
      // Normalizes the input space
      initTime = System.nanoTime()
      trainingSet.normalize()
      endTime = System.nanoTime()
      seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
      print("\nInput space normalized in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")
    }
  }


  /**
   * Creates a new SOM with given configuration
   * @param config Configuration parameters of the SOM
   * @param trainingSet Set for SOM training
   * @return The created and trained SOM
   */
  private def createSOM (config: MapConfig, trainingSet: VectorSet): SOM = {
    // Creates the SOM lattice with given distribution
    var initTime = System.nanoTime()
    val som = SOMFactory.createSOM(config)

    // Sets the initial state of the lattice by initializing neurons
    som.initSOM(trainingSet, FunctionCollector.initFactory(config.initFn), config.initSeed)
    var endTime = System.nanoTime()
    var seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    print("\nLattice initialized in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    // SOM's training process
    initTime = System.nanoTime()
    som.organizeMap(trainingSet, config)
    endTime = System.nanoTime()
    seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    print("\nSOM trained in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    som
  }


  /**
   * Presents the test set to the received SOM and evaluates its clustering precision
   * @return Values for correct and incorrect classifications and the precision
   */
  private def clusterTest (som: SOM, testSet: VectorSet, normalize: Boolean): (Int, Int, Double) = {
    var right = 0
    var wrong = 0

    // Normalizes input space if required
    if (normalize) testSet.normalize()

    // Presents the test instances to the map
    val initTime = System.nanoTime()
    while (testSet.hasNext) {
      val vector = testSet.next
      // Clusters the test input onto the map
      val pair = som.clusterInput(vector)
      // Obtains the class represented by the input's BMU
      val neuronClass = som.neuronAt(pair._1, pair._2).mainClass

      if (vector.classification == neuronClass) {
        // Correct classification
        print("\nInput of class " + vector.classification + " correctly clustered in a " + neuronClass + " type neuron")
        right += 1
      }
      else {
        // Incorrect classification
        print("\nInput of class " + vector.classification + " incorrectly clustered in a " + neuronClass + " type neuron")
        wrong += 1
      }
    }
    val endTime = System.nanoTime()
    val seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    print("\nTests completed in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    // Obtains test precision
    val precision = (right / testSet.sampleSize.toDouble) * 100

    println("\nResults: " + right + " inputs were classified correctly and " + wrong +
      " incorrectly for a precision of " + precision + "%")

    (right, wrong, precision)
  }


  /**
   * Summarizes the state of a given SOM
   */
  def printSOMState (som: Lattice): Unit = {
    //som.printSet()
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
  }
}
