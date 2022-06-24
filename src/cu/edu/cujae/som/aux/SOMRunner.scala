package cu.edu.cujae.som.aux

import cu.edu.cujae.som.io.MapConfig
import cu.edu.cujae.som.map._

import scala.util.Random

object SOMRunner {

  def main(args: Array[String]): Unit = {
    var path = "/mnt/D80C76380C76122C/Mis Programas/Repos/SOM/Datasets/wine.csv"
    val sep = ','
    val exportPath = "/home/xandor19/"
    val setProp = 1
    val trainingSetProp = 0.8
    var latDistrib = LatticeDistribution.squared
    var somTye = SOMType.onlineSOM
    var somLearningFactor = 0.8
    var somTuningFactor = 0.2
    var width = 9
    var height = 6
    var somNeighRadius = 5
    var somRadiusController = 0.9
    var roughIters = 5000
    var tuningIters = 0
    var tolerance = 0
    var initFn = InitFns.normalizedRandomInit
    val normalize = true
    var distanceFn = DistanceFns.manhattan
    var neighborhoodFn = NeighboringFns.gaussian
    var radiusFn = RadiusDecreaseFns.exponential
    var initSeed = 500
    var shuffleSeed = 250
    var experiments = 30

    SOMController.newSOM(new MapConfig(path, sep, setProp, trainingSetProp, normalize, somTye, latDistrib,
                         learnFactor = somLearningFactor, tuneFactor = somTuningFactor, initFn = initFn,
                         distanceFn = distanceFn, neighFn = neighborhoodFn, neighDecreaseFn = radiusFn,
                         decFactor = somRadiusController, trainIter = roughIters, tuneIter = tuningIters,
                         runs = experiments, resultsExportPath = "", trainingExportPath = "",
      ))

  }






}
