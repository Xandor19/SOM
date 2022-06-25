package cu.edu.cujae.som.aux

import cu.edu.cujae.som.io.MapConfig
import cu.edu.cujae.som.map._

object SOMRunner {

  def main(args: Array[String]): Unit = {
    var path = "./Datasets/wine.csv"
    val sep = ','
    val exportPath = "./Results/"
    val setProp = 1
    val trainingSetProp = 0.8
    var latDistrib = LatticeDistribution.hexagonal
    var somTye = SOMType.batchSOM
    var somLearningFactor = 0.8
    var somTuningFactor = 0.2
    var width = 9
    var height = 6
    var somNeighRadius = 5
    var roughIters = 200
    var tuningIters = 54000
    var tolerance = 0
    var initFn = InitFns.normalizedRandomInit
    val normalize = true
    var distanceFn = DistanceFns.simpleEuclidean
    var neighborhoodFn = NeighboringFns.gaussian
    var initSeed = 500
    var shuffleSeed = 250
    var experiments = 30

    SOMController.newSOM(new MapConfig(path, sep, setProp, trainingSetProp, normalize, somTye, latDistrib,
                         learnFactor = somLearningFactor, tuneFactor = somTuningFactor, initFn = initFn,
                         distanceFn = distanceFn, neighFn = neighborhoodFn, trainIter = roughIters,
                         /*tuneIter = tuningIters, */runs = experiments, resultsExportPath = exportPath,
                         trainingExportPath = ""
      //,width = width, height = height, neighRadius = somNeighRadius, initSeed = initSeed, shuffleSeed = shuffleSeed
    ))

  }
}
