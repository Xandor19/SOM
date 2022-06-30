package cu.edu.cujae.som.aux

import java.util.concurrent.TimeUnit

import cu.edu.cujae.som.data.{InputVector, RandomVectorSet, VectorSet}
import cu.edu.cujae.som.io.{ClusteringData, DetectionData, ExperimentData, MapConfig, ReaderWriter, Tasks}
import cu.edu.cujae.som.map.{FunctionCollector, LatticeDistribution, SOM, SOMFactory}

import scala.util.Random

/**
 * Singleton object to control the life cycle of a SOM
 */
object SOMController {

  /**
   * Controls the creation, training and results saving of a specified number of SOM
   * @param config Configuration parameters for the execution
   */
  def newSOMFlow(config: MapConfig): Unit = {
    // Loads the main dataset
    val initTime = System.nanoTime()
    val dataset = ReaderWriter.loadSet(config.dataset, config.setSep)
    val endTime = System.nanoTime()
    val seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    print("\nDataset loaded in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    // Gets dataset name
    config.dataset = config.dataset.slice(config.dataset.lastIndexOf("/") + 1, config.dataset.lastIndexOf("."))

    if (config.task == Tasks.classification) newClusterFlow(dataset, config)
    else if (config.task == Tasks.anomaly) newAnomalyFlow(dataset, config)
  }


  /**
   * Flow for the creation of a set of SOM for clustering test purposes
   * @param dataset Dataset to use
   * @param config Configuration parameters
   */
  def newClusterFlow (dataset: (Array[String], List[InputVector]), config: MapConfig): Unit = {
    // Number of experiments
    val runs = config.runs

    // Created SOMs
    val created = new Array[(SOM, Double)](runs)

    // Measurement variables
    var trainingAvMQE = 0.0
    var trainingMQEDeviation = 0.0
    var avgCorrect = 0.0
    var avgIncorrect = 0.0
    var avgPrecision = 0.0

    val expInitTime = System.nanoTime()
    for (test <- 0 until runs) {
      // Divides the dataset into training and testing (if desired)
      val (trainingSet, testSet) = prepareForClust(dataset._1, dataset._2, config)

      // Runs auto-configuration
      config.completeConfig(trainingSet.sampleSize)

      // Normalizes the sets if required
      if (config.normalize) { trainingSet.normalize(); testSet.normalize() }

      // Train a new SOM
      val som = createSOM(config, trainingSet)

      // Shows the state of the SOM after training
      printSOMState(som)

      // Obtains and accumulates resulting average MQE and its standard of deviation of the training
      val runTrainAvMQE = som.avgMQE
      val runTrainMQEDeviation = som.sdMQE

      trainingAvMQE += runTrainAvMQE
      trainingMQEDeviation += runTrainMQEDeviation

      println("\nTraining for run " + test + " ended with average MQE of " + runTrainAvMQE + " and standard deviation of " +
              runTrainMQEDeviation)

      // Tests the clustering capacities of the SOM
      val results = clusterTest(som, testSet)

      created.update(test, (som, results._3))

      // Accumulates the results of the clustering test
      avgCorrect += results._1
      avgIncorrect += results._2
      avgPrecision += results._3
    }
    if (runs > 1) {
      // Obtains measures averages if more than one run was made
      trainingAvMQE /= runs
      trainingMQEDeviation /= runs
      avgCorrect /= runs.toFloat
      avgIncorrect /= runs.toFloat
      avgPrecision /= runs
    }
    val expEndTime = System.nanoTime()
    val seconds = TimeUnit.SECONDS.convert(expEndTime - expInitTime, TimeUnit.NANOSECONDS)
    println("\nAll runs were completed in " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")


    if (config.exportTraining) {
      // Exports best trained SOM if desired
      config.trainingExportPath += "clust_less_mqe_som_for_" + config.dataset + ".csv"
      ReaderWriter.exportTraining(config, created.minBy(x => x._1.avgMQE)._1)
      config.trainingExportPath += "clust_great_prec_som_for_" + config.dataset + ".csv"
      ReaderWriter.exportTraining(config, created.minBy(x => x._2)._1)
    }

    // Constructs the name of the results export file
    val exportName = "clust_results_of_" + config.dataset + ".csv"

    // Exports the experiment average results
    ReaderWriter.exportExperimentResult(config.resultsExportPath + exportName, config.setSep,
                                        List[ExperimentData](new ClusteringData(config, trainingAvMQE, trainingMQEDeviation,
                                                             avgCorrect, avgIncorrect, avgPrecision,
                                                             String.format(seconds/60 + ":" + seconds%60))))
  }


  /**
   * Flow for the creation of a set of SOM for anomaly detection test purposes
   * @param dataset Dataset to use
   * @param config Configuration parameters
   */
  def newAnomalyFlow (dataset: (Array[String], List[InputVector]), config: MapConfig): Unit ={
    // Number of experiments
    val runs = config.runs

    // Created SOMs
    val created = new Array[(SOM, Double)](runs)

    // Measurement variables
    var trainingAvMQE = 0.0
    var trainingMQEDeviation = 0.0
    var avgTruePos = 0.0
    var avgFalseNeg = 0.0
    var avgTrueNeg = 0.0
    var avgFalsePos = 0.0

    val expInitTime = System.nanoTime()
    for (test <- 0 until runs) {
      // Divides the dataset into training and testing (if desired)
      val (trainingSet, testSet) = prepareForAnom(dataset._1, dataset._2, config)

      // Runs auto-configuration
      config.completeConfig(trainingSet.sampleSize)

      // Normalizes the sets if required
      if (config.normalize) { trainingSet.normalize(); testSet.normalize() }

      // Train a new SOM
      val som = createSOM(config, trainingSet)

      // Shows the state of the SOM after training
      printSOMState(som)

      // Obtains and accumulates resulting average MQE and its standard of deviation of the training
      val runTrainAvMQE = som.avgMQE
      val runTrainMQEDeviation = som.sdMQE

      trainingAvMQE += runTrainAvMQE
      trainingMQEDeviation += runTrainMQEDeviation

      println("\nTraining for run " + test + " ended with average MQE of " + runTrainAvMQE + " and standard deviation of " +
        runTrainMQEDeviation)

      // Tests the anomaly detection capacities of the SOM
      val results = anomalyTest(som, testSet)

      created.update(test, (som, results._1))

      // Accumulates the results of the clustering test
      avgTruePos += results._1
      avgFalseNeg += results._2
      avgTrueNeg += results._3
      avgFalsePos += results._4
    }
    if (runs > 1) {
      // Obtains measures averages if more than one run was made
      trainingAvMQE /= runs
      trainingMQEDeviation /= runs
      avgTruePos /= runs.toFloat
      avgFalseNeg /= runs.toFloat
      avgTrueNeg /= runs.toFloat
      avgFalsePos /= runs.toFloat
    }
    val expEndTime = System.nanoTime()
    val seconds = TimeUnit.SECONDS.convert(expEndTime - expInitTime, TimeUnit.NANOSECONDS)
    println("\nAll runs were completed in " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    if (config.exportTraining) {
      // Exports best trained SOM if desired
      config.trainingExportPath += "anom_less_mqe_som_for_" + config.dataset + ".csv"
      ReaderWriter.exportTraining(config, created.minBy(x => x._1.avgMQE)._1)
      config.trainingExportPath += "anom_great_prec_som_for_" + config.dataset + ".csv"
      ReaderWriter.exportTraining(config, created.minBy(x => x._2)._1)
    }

    // Constructs the name of the results export file
    val exportName = "anom_results_of_" + config.dataset + ".csv"

    // Exports the run results
    ReaderWriter.exportExperimentResult(config.resultsExportPath + exportName, config.setSep,
                                        List[ExperimentData](new DetectionData(config, trainingAvMQE, trainingMQEDeviation,
                                                             avgTruePos, avgFalseNeg, avgTrueNeg, avgFalsePos,
                                                             String.format(seconds/60 + ":" + seconds%60))))
  }


  /**
   * Prepares the training and test set for a cluster test, obtaining a sample of the original
   * dataset if desired and proportionally dividing it.
   * Both operations are performed via stratified sampling
   * @param features Features of the dataset
   * @param inputVectors Instances of the dataset
   * @param config Configuration parameters
   * @return Tuple of two VectorSet objects, namely, the training and test sets
   */
  def prepareForClust (features: Array[String], inputVectors: List[InputVector], config: MapConfig): (VectorSet, VectorSet) = {
    //Random instance
    val rand = new Random()
    rand.setSeed(config.shuffleSeed)

    print("\nPreparing dataset for clustering")
    // Prepares the dataset, performing a stratified sampling if desired
    val initTime = System.nanoTime()
    val subSet = if (config.setProp < 1) Utils.stratified(inputVectors, config.setProp, config.shuffleSeed)._1
                 else rand.shuffle(inputVectors)
    val endTime = System.nanoTime()
    val seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    print("\nInputs " + (if (config.setProp < 1) "stratified and " else "") + "shuffled in: " +
      seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    // Obtains stratified training and test sets
    val split = Utils.stratified(subSet, config.trainingProp, config.shuffleSeed)

    // Obtains training set from stratified slice of the inputs
    val trainingSet = new RandomVectorSet(features, split._1, config.shuffleSeed)
    print("\nTraining set created with size " + trainingSet.sampleSize)

    // Obtains test set as the remaining inputs
    val testSet = new VectorSet(features, split._2)
    print("\nTest set created with size " + testSet.sampleSize)

    (trainingSet, testSet)
  }


  /**
   * Prepares the training and test set for a anomaly detection test
   * Training set is composed by dividing the dataset into normal and abnormal classes
   * and selecting a random subset of the first with the desired proportion
   * Test set is created with the remaining normal instances and a random subset (using
   * the same proportion value) of the abnormal classes
   * @param features Features of the dataset
   * @param inputVectors Instances of the dataset
   * @param config Configuration parameters of the SOM
   * @return Tuple of two VectorSet objects, namely, the training and test sets
   */
  def prepareForAnom (features: Array[String], inputVectors: List[InputVector], config: MapConfig): (VectorSet, VectorSet) = {
    //Random instance
    val rand = new Random()
    rand.setSeed(config.shuffleSeed)

    print("\nPreparing dataset for anomaly detection")
    // Divides dataset into normal and anomalous instances
    val initTime = System.nanoTime()
    val classes = Utils.splitByClasses(inputVectors).sortWith((x, y) => x.head.classification < y.head.classification)
    val endTime = System.nanoTime()
    val seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    print("\nNormal and abnormal classes split in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    // Obtains training set size
    val normalProp = (classes.head.size * config.trainingProp).toInt
    // Shuffles the normal instances and splits it for training/testing
    val normal = rand.shuffle(classes.head).splitAt(normalProp)

    // Creates training set as the specified proportion subset of the normal instances
    val trainingSet = new RandomVectorSet(features, normal._1)
    print("\nTraining set created with subset of normal instances of size: " + normalProp)

    // Obtains amount of anomalies
    val abnormalProp = (classes.last.size * config.trainingProp).toInt
    // Shuffles the abnormal instances for splitting
    val abnormal = rand.shuffle(classes.last).slice(0, abnormalProp)

    // Creates test set with the remaining normal instances and the anomalous instances
    val testSet = new VectorSet(features, rand.shuffle(abnormal.appendedAll(normal._2)))
    println("\nTest set created with remaining normal instances and anomalous instances with size: " + testSet.sampleSize)

    (trainingSet, testSet)
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
   * @param som Trained model to evaluate
   * @param testSet Set to use in the test
   * @return Values for correct and incorrect classifications and precision
   */
  private def clusterTest (som: SOM, testSet: VectorSet): (Int, Int, Double) = {
    var right = 0
    var wrong = 0

    val testIt = testSet.iterator

    // Presents the test instances to the map
    val initTime = System.nanoTime()
    while (testIt.hasNext) {
      val vector = testIt.next
      // Clusters the test input onto the map
      val bmu = som.findBMU(vector.vector)._1
      // Obtains the class represented by the input's BMU
      val neuronClass = bmu.findMainClass

      if (vector.classification == neuronClass._1) {
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
   * Presents the test set to the created SOM and evaluates its performance in anomaly detection
   * @param som Trained model to evaluate
   * @param testSet Set to use in the test
   * @return Values for the amount of correctly detected, missed, normal instances ignored and
   *         normal instaces incorrectly marked as anomalies
   */
  private def anomalyTest (som: SOM, testSet: VectorSet): (Int, Int, Int, Int) = {
    val anomAm = testSet.vectors.count(x => x.classification == "1")
    val normAm = testSet.sampleSize - anomAm
    val threshold = som.normalityThreshold

    var detected = 0
    var falsePos = 0

    val it = testSet.iterator

    while (it.hasNext) {
      val vector = it.next

      if (som.findBMU(vector.vector)._2 > threshold) {
        if (vector.classification == "0") falsePos += 1
        else if (vector.classification == "1") detected += 1
      }
    }
    print("\n" + detected + " of " + anomAm + " anomalies were correctly detected (" +
         (detected / anomAm.toFloat) * 100 + "%)")
    print("\n" + falsePos + " of " + normAm + " normal instances were incorrectly detected as anomalies (" +
         (falsePos / normAm.toFloat) * 100 + "%)")

    val anomMiss = anomAm - detected
    val trueNeg = normAm - falsePos

    (detected, anomMiss, trueNeg, falsePos)
  }


  /**
   * Summarizes the state of a given SOM
   */
  def printSOMState (som: SOM): Unit = {
    println()
    println()
    printNeuronHits(som.lattice.neuronHits, som.lattice.latticeType)
    println()
    println()
    printClassesBalance(som.lattice.classesBalance, som.lattice.latticeType)
    println()
    println()
    printMainClasses(som.lattice.mainClasses, som.lattice.latticeType)
    println()
    println()
    printUMatrix(som.uMatrix, som.lattice.latticeType)
  }


  /**
   * Prints this map distribution, e.g, the neurons with the number of inputs
   * that represents
   */
  def printNeuronHits (lat: Array[Array[Int]], latType: String): Unit = {
    val width = lat.length
    val height = lat.head.length

    if (latType == LatticeDistribution.rectangular) {
      for (i <- 0 until width) {
        for (j <- 0 until height) {
          print(lat(i)(j) + "\t")
        }
        println()
      }
    }
    else if (latType == LatticeDistribution.hexagonal) {
      for (i <- 0 until width) {
        for (j <- 0 until height) {
          if (i % 2 == 0) print(lat(i)(j) + "\t\t")
          else print("\t" + lat(i)(j) + "\t")
        }
        println()
      }
    }
  }


  /**
   * Prints each neuron as in printMap but adding how many classes it represents
   */
  def printClassesBalance (lat: Array[Array[(Int, Int)]], latType: String): Unit = {
    val width = lat.length
    val height = lat.head.length

    if (latType == LatticeDistribution.rectangular) {
      for (i <- 0 until width) {
        for (j <- 0 until height) {
          printf("%3d : %3d\t", lat(i)(j)._1, lat(i)(j)._2)
        }
        println()
      }
    }
    else if (latType == LatticeDistribution.hexagonal) {
      for (i <- 0 until width) {
        for (j <- 0 until height) {
          if (i % 2 == 0) printf("%3d : %3d\t\t", lat(i)(j)._1, lat(i)(j)._2)
          else printf("\t\t%3d : %3d", lat(i)(j)._1, lat(i)(j)._2)
        }
        println()
      }
    }
  }


  /**
   * Prints the name of the class that each neuron represents (the class from which
   * the neuron has most instances)
   */
  def printMainClasses (lat: Array[Array[String]], latType: String): Unit = {
    val width = lat.length
    val height = lat.head.length

    if (latType == LatticeDistribution.rectangular) {
      for (i <- 0 until width) {
        for (j <- 0 until height) {
          printf("%20s\t", lat(i)(j))
        }
        println()
      }
    }
    else if (latType == LatticeDistribution.hexagonal) {
      for (i <- 0 until width) {
        for (j <- 0 until height) {
          if (i % 2 == 0) printf("%20s\t\t\t\t", lat(i)(j))
          else printf("\t\t\t\t%20s", lat(i)(j))
        }
        println()
      }
    }
  }


  /**
   * Prints the u-matrix of the SOM (the average distance between a neuron
   * and its neighbors
   */
  def printUMatrix (lat: Array[Array[Double]], latType: String): Unit = {
    val width = lat.length
    val height = lat.head.length

    if (latType == LatticeDistribution.rectangular) {
      for (i <- 0 until width) {
        for (j <- 0 until height) {
          printf("%3.4f\t", lat(i)(j))
        }
        println()
      }
    }
    else if (latType == LatticeDistribution.hexagonal) {
      for (i <- 0 until width) {
        for (j <- 0 until height) {
          if (i % 2 == 0) printf("%3.4f\t\t", lat(i)(j))
          else printf("\t%3.4f\t", lat(i)(j))
        }
        println()
      }
    }
  }


  /**
   * Prints the values of the weight vectors of all neurons specifying
   * their index in the lattice's coordinate system
   * @param vectors Array of weight vectors with their neuron's indices
   */
  def printVectors (vectors: Array[(Float, Float, Array[Double])]): Unit = {
    vectors.foreach(x => {
      printf("Index (%.2f;%.2f): %s\n", x._1, x._2, x._3.mkString(","))
    })
  }
}
