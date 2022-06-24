package cu.edu.cujae.som.aux

import cu.edu.cujae.som.io.MapConfig
import cu.edu.cujae.som.map._

import scala.util.Random

object SOMRunner {

  def main(args: Array[String]): Unit = {
    var path = "/mnt/D80C76380C76122C/Mis Programas/Repos/SOM/Datasets/iris.csv"
    val sep = ','
    val exportPath = "/home/xandor19/export.csv"
    val setProp = 1
    val trainingSetProp = 0.8
    var latDistrib = LatticeDistribution.squared
    var somTye = SOMType.onlineSOM
    var somLearningFactor = 0.8
    var somTuningFactor = 0.2
    var width = 9
    var height = 6
    var somNeighRadius = 5
    var somRadiusController = 0.5
    var roughIters = 1000
    var tuningIters = 54000
    var tolerance = 0
    var initFn = InitFns.normalizedRandomInit
    val normalize = true
    var distanceFn = DistanceFns.simpleEuclidean
    var neighborhoodFn = NeighboringFns.gaussian
    var radiusFn = RadiusDecreaseFns.exponential
    var initSeed = 500
    var shuffleSeed = 250
    var experiments = 1

    SOMController.newSOM(new MapConfig(path, sep, setProp, trainingSetProp, normalize, somTye, latDistrib,
                         learnFactor = somLearningFactor, tuneFactor = somTuningFactor, initFn = initFn,
                         distanceFn = distanceFn, neighFn = neighborhoodFn, neighDecreaseFn = radiusFn,
                         decFactor = somRadiusController, trainIter = roughIters, tuneIter = tuningIters,
                         runs = experiments, resultsExportPath = "", trainingExportPath = "",
      width = width, height = height, neighRadius = somNeighRadius, initSeed = initSeed, shuffleSeed = shuffleSeed))

  }






}
