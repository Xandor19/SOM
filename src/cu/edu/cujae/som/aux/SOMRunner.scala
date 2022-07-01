package cu.edu.cujae.som.aux

import cu.edu.cujae.som.io.{MapConfig, Tasks}
import cu.edu.cujae.som.map._

import scala.util.Random

object SOMRunner {

  def main(args: Array[String]): Unit = {
    val trainNew = false
    val datasetPath = "./Datasets/reduced_card_fraud_normalised_less_anomalies.csv"
    val modelImportPath = "./Pre-trained/anom_great_acc_som_for_reduced_card_fraud_normalised_less_anomalies.json"
    val modelExportPath = "./Pre-trained/"
    val resultsExportPath = "./Results/"
    val setProp = 1
    val trainingSetProp = 0.6
    var normalize = false
    val task = Tasks.anomaly
    var experiments = 1
    var somType = SOMType.batchSOM
    var latDistrib = LatticeDistribution.rectangular
    var latWidth = 0
    var latHeight = 0
    var latNeighRadius = 0
    var onlineLearningFactor = 0.8
    var onlineTuningFactor = 0.2
    var trainingIters = 200
    var onlineTuningIters = 27000
    var initFn = InitFns.randomInit
    var distanceFn = DistanceFns.squaredEuclidean
    var neighborhoodFn = NeighboringFns.gaussian
    var initSeed = Random.nextInt()
    var shuffleSeed = Random.nextInt()


    if (trainNew) {
      SOMController.newSOMFlow(new MapConfig(dataset = datasetPath, trainingExportPath = modelExportPath,
        resultsExportPath = resultsExportPath, setProp = setProp, trainingProp = trainingSetProp,
        normalize = normalize, task = task, runs = experiments, somType = somType,
        latDistrib = latDistrib, width = latWidth, height = latHeight, neighRadius = latNeighRadius,
        learnFactor = onlineLearningFactor, tuneFactor = onlineTuningFactor,
        trainIter = trainingIters, tuneIter = onlineTuningIters, initFn = initFn,
        distanceFn = distanceFn, neighFn = neighborhoodFn, randInitSeed = initSeed,
        randShuffleSeed = shuffleSeed))
    }
    else {
      SOMController.importSOMFlow(modelImportPath, datasetPath, resultsExportPath)
    }
  }
}
