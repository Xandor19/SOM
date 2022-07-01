package cu.edu.cujae.som.aux

import java.util.concurrent.TimeUnit

import cu.edu.cujae.som.data.{InputVector, RandomVectorSet, VectorSet}
import cu.edu.cujae.som.io.{ClusteringData, DetectionData, ExperimentData, MapConfig, MapIO, ReaderWriter, Tasks}
import cu.edu.cujae.som.map.{FunctionCollector, SOM, SOMFactory}

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
    val dataset = ReaderWriter.loadSet(config.dataset, config.setSep)

    // Gets dataset name
    config.dataset = config.dataset.slice(config.dataset.lastIndexOf("/") + 1, config.dataset.lastIndexOf("."))

    if (config.task == Tasks.clustering) newClusterFlow(dataset, config)
    else if (config.task == Tasks.anomaly) newAnomalyFlow(dataset, config)
  }


  /**
   * Flow for the creation of a set of SOM for clustering test purposes
   * @param dataset Dataset to use
   * @param config Configuration parameters
   */
  private def newClusterFlow (dataset: (Array[String], List[InputVector]), config: MapConfig): Unit = {
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

      // Obtains and accumulates resulting average MQE and its standard of deviation of the training
      val runTrainAvMQE = som.avgMQE
      val runTrainMQEDeviation = som.sdMQE

      trainingAvMQE += runTrainAvMQE
      trainingMQEDeviation += runTrainMQEDeviation

      // Tests the clustering capacities of the SOM
      val results = clusterTest(som, testSet)

      // Stores created model
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


    if (config.exportTraining) {
      // Exports best trained SOM if desired
      val origPath = config.trainingExportPath

      val lessMQE = origPath + "clust_less_mqe_som_for_" + config.dataset + ".csv"
      ReaderWriter.exportTraining(lessMQE, prepareExport(config, created.minBy(x => x._1.avgMQE)._1))
      val greatPrec = origPath + "clust_great_prec_som_for_" + config.dataset + ".csv"
      ReaderWriter.exportTraining(greatPrec, prepareExport(config, created.minBy(x => x._2)._1))
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
  private def newAnomalyFlow (dataset: (Array[String], List[InputVector]), config: MapConfig): Unit ={
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
    var avgConf = 0.0
    var avgSens = 0.0
    var avgAcc = 0.0

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

      // Obtains and accumulates resulting average MQE and its standard of deviation of the training
      val runTrainAvMQE = som.avgMQE
      val runTrainMQEDeviation = som.sdMQE

      trainingAvMQE += runTrainAvMQE
      trainingMQEDeviation += runTrainMQEDeviation

      // Tests the anomaly detection capacities of the SOM
      val results = anomalyTest(som, testSet)

      // Obtains this experiment's parameters
      val currentConf = results._1 / (results._1 + results._4).toFloat
      val currentSens = results._1 / (results._1 + results._2).toFloat
      val currentAcc = (results._1 + results._3) / (results._1 + results._2 + results._3 + results._4).toFloat

      // Stores created model
      created.update(test, (som, currentAcc))

      // Accumulates the results of the clustering test
      avgTruePos += results._1
      avgFalseNeg += results._2
      avgTrueNeg += results._3
      avgFalsePos += results._4
      avgConf += currentConf
      avgSens += currentSens
      avgAcc += currentAcc
    }
    if (runs > 1) {
      // Obtains measures averages if more than one run was made
      trainingAvMQE /= runs
      trainingMQEDeviation /= runs
      avgTruePos /= runs.toFloat
      avgFalseNeg /= runs.toFloat
      avgTrueNeg /= runs.toFloat
      avgFalsePos /= runs.toFloat
      avgConf /= runs
      avgSens /= runs
      avgAcc /= runs
    }
    val expEndTime = System.nanoTime()
    val seconds = TimeUnit.SECONDS.convert(expEndTime - expInitTime, TimeUnit.NANOSECONDS)

    if (config.exportTraining) {
      // Exports best trained SOM if desired
      val origPath = config.trainingExportPath

      val lessMQE = origPath + "anom_less_mqe_som_for_" + config.dataset + ".json"
      ReaderWriter.exportTraining(lessMQE, prepareExport(config, created.minBy(x => x._1.avgMQE)._1))
      val greatAcc = origPath + "anom_great_acc_som_for_" + config.dataset + ".json"
      ReaderWriter.exportTraining(greatAcc, prepareExport(config, created.minBy(x => x._2)._1))
    }

    // Constructs the name of the results export file
    val exportName = "anom_results_of_" + config.dataset + ".csv"

    // Exports the run results
    ReaderWriter.exportExperimentResult(config.resultsExportPath + exportName, config.setSep,
                                        List[ExperimentData](new DetectionData(config, trainingAvMQE, trainingMQEDeviation,
                                                             avgTruePos, avgFalseNeg, avgTrueNeg, avgFalsePos, avgConf,
                                                             avgSens, avgAcc, String.format(seconds/60 + ":" + seconds%60))))
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

    // Prepares the dataset, performing a stratified sampling if desired
    val subSet = if (config.setProp < 1) Utils.stratified(inputVectors, config.setProp, config.shuffleSeed)._1
                 else rand.shuffle(inputVectors)

    // Obtains stratified training and test sets
    val split = Utils.stratified(subSet, config.trainingProp, config.shuffleSeed)

    // Obtains training set from stratified slice of the inputs
    val trainingSet = new RandomVectorSet(features, split._1, config.shuffleSeed)
    // Obtains test set as the remaining inputs
    val testSet = new VectorSet(features, split._2)

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

    // Divides dataset into normal and anomalous instances
    val classes = Utils.splitByClasses(inputVectors).sortWith((x, y) => x.head.classification < y.head.classification)

    // Obtains training set size
    val normalProp = (classes.head.size * config.trainingProp).toInt
    // Shuffles the normal instances and splits it for training/testing
    val normal = rand.shuffle(classes.head).splitAt(normalProp)

    // Creates training set as the specified proportion subset of the normal instances
    val trainingSet = new RandomVectorSet(features, normal._1)

    // Obtains amount of anomalies
    val abnormalProp = (classes.last.size * config.trainingProp).toInt
    // Shuffles the abnormal instances for splitting
    val abnormal = rand.shuffle(classes.last).slice(0, abnormalProp)

    // Creates test set with the remaining normal instances and the anomalous instances
    val testSet = new VectorSet(features, rand.shuffle(abnormal.appendedAll(normal._2)))

    (trainingSet, testSet)
  }


  /**
   * Creates a new SOM with given configuration
   * @param config Configuration parameters of the SOM
   * @param trainingSet Set for SOM training
   * @return The created and trained SOM
   */
  def createSOM (config: MapConfig, trainingSet: VectorSet): SOM = {
    // Creates the SOM lattice with given distribution
    val som = SOMFactory.createSOM(config)

    // Sets the initial state of the lattice by initializing neurons
    som.initSOM(trainingSet, FunctionCollector.initFactory(config.initFn), config.initSeed)

    // SOM's training process
    som.organizeMap(trainingSet, config)

    som
  }


  /**
   * Presents the test set to the received SOM and evaluates its clustering precision
   * @param som Trained model to evaluate
   * @param testSet Set to use in the test
   * @return Values for correct and incorrect classifications and precision
   */
  def clusterTest (som: SOM, testSet: VectorSet): (Int, Int, Double) = {
    var right = 0
    var wrong = 0

    val testIt = testSet.iterator

    // Presents the test instances to the map
    while (testIt.hasNext) {
      val vector = testIt.next
      // Clusters the test input onto the map
      val bmu = som.findBMU(vector.vector)._1
      // Obtains the class represented by the input's BMU
      val neuronClass = bmu.mainClass

      // Correct classification
      if (vector.classification == neuronClass) right += 1
      // Incorrect classification
      else wrong += 1
    }
    // Obtains test precision
    val precision = (right / testSet.sampleSize.toDouble) * 100

    (right, wrong, precision)
  }


  /**
   * Presents the test set to the created SOM and evaluates its performance in anomaly detection
   * @param som Trained model to evaluate
   * @param testSet Set to use in the test
   * @return Values for the amount of correctly detected, missed, normal instances ignored and
   *         normal instaces incorrectly marked as anomalies
   */
  def anomalyTest (som: SOM, testSet: VectorSet): (Int, Int, Int, Int) = {
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
    val anomMiss = anomAm - detected
    val trueNeg = normAm - falsePos

    (detected, anomMiss, trueNeg, falsePos)
  }


  /**
   * Prepara un objeto MapIO con la configuracion de un SOM entrenado para exportar
   * @param config Parametros de configuracion inicial del SOM
   * @param som SOM entrenado
   * @return Objeto MapIO con los datos de exportacion
   */
  def prepareExport (config: MapConfig, som: SOM): MapIO = {
    new MapIO(config.dataset, config.task, config.somType, config.latDistrib, som.lattice.width, som.lattice.height,
              config.normalize, config.distanceFn, som.avgMQE, som.sdMQE, som.lattice.neurons.flatten.map(x =>
              (x.weightVector.mkString(","), x.hitCount, x.classesBalance.toString(), x.mainClass)))
  }
}
