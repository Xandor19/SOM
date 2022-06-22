package som

import java.util.concurrent.TimeUnit

import scala.util.Random

object SOMRunner {

  def main(args: Array[String]): Unit = {
    var path = "/mnt/D80C76380C76122C/Mis Programas/Repos/SOM/Datasets/iris.csv"
    val sep = ','
    val exportPath = "/home/xandor19/export.csv"
    val trainingSetProp = 0.8
    var latDistrib = LatticeDistribution.squared
    var somLearningFactor = 0.8
    var somTuningFactor = 0.2
    var somNeighRadius = 5
    var somRadiusController = 0.5
    var roughIters = 5000
    var tuningIters = 0
    var tolerance = 0
    var initFn: (Array[Array[Neuron]], VectorSet, Long) => Unit = FunctionCollector.normalizedRandomInit
    var initName = "Norm rand"
    val normalize = true
    var distanceFn: (Array[Double], Array[Double]) => Double = FunctionCollector.euclideanDistance
    var distName = "Euc"
    var neighborhoodFn: (Float, Float, Float, Float, Double) => Double = FunctionCollector.gaussianNeighborhood
    var neighName = "Gauss"
    var radiusFn: (Int, Int, Double) => Double = FunctionCollector.exponentialRadiusDecrease
    var radiusDecName = "Exp"
    var initSeed = 500
    var shuffleSeed = 250
    var multiple = false
    var experiments = 30

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.6

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.4

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.6
    tuningIters = 27000

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.4

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.8
    somTuningFactor = 0.4

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.6

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.4

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somTuningFactor = 0.2
    somLearningFactor = 0.8
    roughIters = 500

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    distanceFn = FunctionCollector.manhattanDistance
    distName = "Manh"
    roughIters = 5000
    tuningIters = 0

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.6

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.4

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.6
    tuningIters = 27000

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.4

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.8
    somTuningFactor = 0.4

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.6

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.4

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somTuningFactor = 0.2
    somLearningFactor = 0.8
    roughIters = 500

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    distanceFn = FunctionCollector.euclideanDistance
    distName = "Euc"
    neighborhoodFn = FunctionCollector.proportionalNeighborhood
    neighName = "Prop"
    tuningIters = 0
    roughIters = 5000

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.6

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.4

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.6
    tuningIters = 27000

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.4

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.8
    somTuningFactor = 0.4

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.6

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.4

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somTuningFactor = 0.2
    somLearningFactor = 0.8
    roughIters = 500

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    distanceFn = FunctionCollector.manhattanDistance
    distName = "Manh"
    roughIters = 5000
    tuningIters = 0

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.6

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.4

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.6
    tuningIters = 27000

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.4

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.8
    somTuningFactor = 0.4

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.6

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    somLearningFactor = 0.4

    ReaderWriter.exportExperimentResult(exportPath, sep, List(multipleTests(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      experiments)))

    /*ReaderWriter.exportExperimentResult(exportPath, sep, List(singleTest(path, sep, trainingSetProp, latDistrib,
      somLearningFactor, somTuningFactor, somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance,
      initFn, initName, normalize, distanceFn, distName, neighborhoodFn, neighName, radiusFn, radiusDecName,
      initSeed, shuffleSeed)*/

  }


  /**
   * Performs a single experiment over a new trained SOM
   * @return ExperimentData object with the results of this experiment
   */
  def singleTest (path: String, sep: Char, trainingSetProp: Double, latDistrib: Int, somLearningFactor: Double,
                  somTuningFactor: Double, somNeighRadius: Int, somRadiusController: Double, roughIters: Int,
                  tuningIters: Int, tolerance: Double, initFn: (Array[Array[Neuron]], VectorSet, Long) => Unit,
                  initName: String, normalize: Boolean, distanceFn: (Array[Double], Array[Double]) => Double,
                  distName: String, neighborhoodFn: (Float, Float, Float, Float, Double) => Double, neighName: String,
                  radiusFn: (Int, Int, Double) => Double, radiusDecName: String, initSeed: Long, shuffleSeed: Long):
                  ExperimentData = {

    // Obtain dataset used
    val datasetName = path.substring(path.lastIndexOf("/") + 1)

    // Train and test a new SOM over the dataset
    val initTime = System.nanoTime()
    val results = flowFromScratch(path, sep, trainingSetProp, normalize, latDistrib, somLearningFactor, somTuningFactor,
                                  somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance, initFn,
                                  distanceFn, neighborhoodFn, radiusFn, initSeed, shuffleSeed)
    val endTime = System.nanoTime()
    val seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    println("Total elapsed time: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    // Collect this experiment's results
    new ExperimentData(datasetName, trainingSetProp, latDistrib, results._1, results._2, somNeighRadius,
                       somLearningFactor, somTuningFactor, initName, distName, neighName, radiusDecName,
                       somRadiusController, roughIters, tuningIters, tolerance, 1, initSeed, shuffleSeed,
                       results._3, results._4, results._5, results._6, results._7, results._8, results._9)
  }


  /**
   * Perform a series of experiments over the specified number of new SOMs
   * @return ExperimentData object with the averages of the results of the experiments
   */
  def multipleTests (path: String, sep: Char, trainingSetProp: Double, latDistrib: Int, somLearningFactor: Double,
                     somTuningFactor: Double, somNeighRadius: Int, somRadiusController: Double, roughIters: Int,
                     tuningIters: Int, tolerance: Double, initFn: (Array[Array[Neuron]], VectorSet, Long) => Unit,
                     initName: String, normalize: Boolean, distanceFn: (Array[Double], Array[Double]) => Double,
                     distName: String, neighborhoodFn: (Float, Float, Float, Float, Double) => Double, neighName: String,
                     radiusFn: (Int, Int, Double) => Double, radiusDecName: String, experiments: Int): ExperimentData = {

    // Obtains dataset used
    val datasetName = path.substring(path.lastIndexOf("/") + 1)
    // List for store each run's data
    var accum = List.empty[(Int, Int, Double, Double, Int, Int, Double, Double, Double)]

    // Performs received amount of experiments
    val expInitTime = System.nanoTime()
    for (i <- 1 to experiments) {
      // Train and test a new SOM over the dataset
      val initTime = System.nanoTime()
      val results = flowFromScratch(path, sep, trainingSetProp, normalize, latDistrib, somLearningFactor, somTuningFactor,
                                    somNeighRadius, somRadiusController, roughIters, tuningIters, tolerance, initFn,
                                    distanceFn, neighborhoodFn, radiusFn, Random.nextInt(), Random.nextInt(), i)
      val endTime = System.nanoTime()
      val seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
      println("Total elapsed time for test " + i + ": " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

      // Saves this model's results
      accum = accum.appended(results)
    }
    val expEndTime = System.nanoTime()
    val seconds = TimeUnit.SECONDS.convert(expEndTime - expInitTime, TimeUnit.NANOSECONDS)
    println("Total experiment time: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    // Obtains averages of all the model's results
    val width = accum.head._1
    val height = accum.head._2
    val trainAvgMQE = accum.map(x => x._3).sum / experiments
    val trainAvgDeviation = accum.map(x => x._4).sum / experiments
    val avgRight = accum.map(x => x._5).sum / experiments.toDouble
    val avgWrong = accum.map(x => x._6).sum / experiments.toDouble
    val avgPrec = accum.map(x => x._7).sum / experiments
    val avgMQE = accum.map(x => x._8).sum / experiments
    val avgDeviation = accum.map(x => x._9).sum / experiments

    println("Test finished with average of:\n" + avgRight + " successful\n" + avgWrong + " unsuccessful\n" + avgPrec +
            "% of precision\n" + avgMQE + " average MQE\n" + avgDeviation + " standard deviation\nTraining average" +
            "MQE was " + trainAvgMQE + " with a standard deviation of " + trainAvgDeviation)

    new ExperimentData(datasetName, trainingSetProp, latDistrib, width, height, somNeighRadius,
                       somLearningFactor, somTuningFactor, initName, distName, neighName,
                       radiusDecName, somRadiusController, roughIters, tuningIters, tolerance,
                       experiments, -1, -1, trainAvgMQE,
                       trainAvgDeviation, avgRight, avgWrong, avgPrec, avgMQE, avgDeviation)
  }


  /**
   * Control flow for creating, training and testing a new SOM using specified proportions of the dataset
   * @return Results of the created model
   */
  def flowFromScratch (datasetPath: String, datasetSeparator: Char, trainingSetProp: Double, normalize: Boolean,
                       latDistrib: Int, somLearningFactor: Double, somTuningFactor: Double, somNeighRadius: Int,
                       somRadiusController: Double, roughIters: Int, tuningIters: Int, tolerance: Double,
                       initFn: (Array[Array[Neuron]], VectorSet, Long) => Unit, distanceFn: (Array[Double], Array[Double]) => Double,
                       neighborhoodFn: (Float, Float, Float, Float, Double) => Double,
                       radiusDecreaseFn: (Int, Int, Double) => Double, initSeed: Long, shuffleSeed: Long, test: Int = 0):
                      (Int, Int, Double, Double, Int, Int, Double, Double, Double) = {

    // Loads the main dataset and splits it into training and test sets
    val sets = loadSets(datasetPath, datasetSeparator, trainingSetProp, shuffleSeed)
    val trainingSet = sets._1
    val testSet = sets._2
    val neurons = math.sqrt(trainingSet.sampleSize) * 5
    var height = math.sqrt(neurons).toInt
    val width = if (neurons - math.pow(height, 2) > height / 2) height + 1
                //else if (neurons - math.pow(height, 2) > height * 2) { height += 1; height }
                else height
    //val somNeighRadius = width / 2

    // Prepares training set
    prepareTraining(trainingSet, normalize)

    // Train a new SOM
    val som = createSOM(latDistrib, width, height, somLearningFactor, somTuningFactor, somNeighRadius,
                             somRadiusController, roughIters, tuningIters, tolerance, initFn, distanceFn,
                             neighborhoodFn, radiusDecreaseFn, trainingSet, trainingSet.dimensionality, initSeed)

    // Shows the state of the SOM after training
    //printSOMState(som)

    // Obtains resulting average MQE and its standard of deviation of the training
    val trainingAvMQE = som.mapAvgMQE
    val trainingMQEDeviation = som.mapMQEDeviation

    println("Training for test " + test + " ended with average MQE of " + trainingAvMQE + " and standard deviation of " +
            trainingMQEDeviation)

    // Tests the clustering capacities of the SOM
    val results = clusterTest(som, testSet, normalize)

    som.updateAvMQE()
    som.updateMQEDeviation()

    println("Test " + test + " average MQE is " + som.mapAvgMQE + " with a standard deviation of " + som.mapMQEDeviation)

    //ReaderWriter.exportTrainingToCSV("/home/xandor19/training.csv", som)

    (width, height, trainingAvMQE, trainingMQEDeviation, results._1, results._2, results._3, som.mapAvgMQE,
     som.mapMQEDeviation)
  }


  /**
   * Loads the dataset into a training and a test set
   * @return The training and test sets as two VectorSet objects
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
    //println("File loaded in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    // Gets inputs' dimensionality
    val dimensionality = data._1
    //println("Dataset dimensionality is " + dimensionality)

    // Defines training set as the 80% of the dataset
    val trainingSetSize = (data._3.length * trainingSetProp).toInt
    //println("Training set size: " + trainingSetSize)

    // Shuffles dataset to ensure random segmentation
    initTime = System.nanoTime()
    val inputVectors = rand shuffle data._3
    endTime = System.nanoTime()
    seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    //println("Inputs shuffled in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    // Obtains training set from the shuffled input
    val trainingSet = new RandomVectorSet(data._2, inputVectors.slice(0, trainingSetSize), shuffleSeed)
    //println("Training set created with size " + trainingSet.sampleSize)

    // Obtains test set as the remaining inputs
    val testSet = new SequentialVectorSet(data._2, inputVectors.slice(trainingSetSize, data._3.size))
    //println("Test set created with size " + testSet.sampleSize)

    (trainingSet, testSet)
  }


  /**
   * Prepares the dataset for training
   * //TODO parameters for training configuration
   */
  def prepareTraining (trainingSet: VectorSet, normalize: Boolean): Unit = {
    // Obtains the input's dimensions bounds
    var initTime = System.nanoTime()
    trainingSet.findBounds()
    var endTime = System.nanoTime()
    var seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    //println("Dimensionality bounds found in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    if (normalize) {
      // Normalizes the input space
      initTime = System.nanoTime()
      trainingSet.normalize()
      endTime = System.nanoTime()
      seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
      //println("Input space normalized in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")
    }
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
    val som = LatticeFactory.createLattice(latDistrib, width, height, somLearningFactor, somTuningFactor, somNeighRadius,
                                    somRadiusController, distanceFn, neighborhoodFn, radiusDecreaseFn)

    // Sets the initial state of the lattice by initializing neurons and setting the distance function
    som.constructLattice(dimensionality)
    var endTime = System.nanoTime()
    var seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    //println("Lattice created in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    // Initializes the weight vectors with the specified function
    initTime = System.nanoTime()
    som.initLattice(trainingSet, initFn, initSeed)
    //som.initLattice(trainingSet, FunctionCollector.randomInit, initSeed)
    endTime = System.nanoTime()
    seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    //println("Lattice initialized in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    // SOM's training process
    initTime = System.nanoTime()
    som.organizeMap(trainingSet, roughIters, tuningIters, tolerance)
    endTime = System.nanoTime()
    seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    //println("SOM trained in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    som
  }


  /**
   * Presents the test set to the received SOM and evaluates its clustering precision
   * @return Values for correct and incorrect classifications and the precision
   */
  def clusterTest (som: Lattice, testSet: VectorSet, normalize: Boolean): (Int, Int, Double) = {
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
      val neuronClass = som.neurons(pair._1)(pair._2).mainClass

      if (vector.classification == neuronClass) {
        // Correct classification
        //println("Input of class " + vector.classification + " correctly clustered in a " + neuronClass + " type neuron")
        right += 1
      }
      else {
        // Incorrect classification
        //println("Input of class " + vector.classification + " incorrectly clustered in a " + neuronClass + " type neuron")
        wrong += 1
      }
    }
    val endTime = System.nanoTime()
    val seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    //println("Tests completed in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    // Obtains test precision
    val precision = (right / testSet.sampleSize.toDouble) * 100

    println("Test " + test + ": " + right + " inputs were classified correctly and " + wrong +
      " incorrectly for a precision of " + precision + "%")

    (right, wrong, precision)
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
   */
  def printSOMState (som: Lattice): Unit = {
    som.printSet()
    //println()
    //println()
    som.printMap()
    //println()
    //println()
    som.printClassesBalance()
    //println()
    //println()
    som.printMainClasses()
    //println()
    //println()

    //println("AVG MQE: " + som.mapAvgMQE)
    //println("MQE standard deviation: " + som.mapMQEDeviation)
    //println("Normality threshold: " + som.normalityThreshold)
  }
}
