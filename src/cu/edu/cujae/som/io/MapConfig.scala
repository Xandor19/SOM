package cu.edu.cujae.som.io

import scala.util.Random

class MapConfig (val dataset: String, val setSep: Char = ',', val setProp: Double, val trainingProp: Double,
                 val normalize: Boolean = false, val latDistrib: Int, var width: Int = 0, var height: Int = 0,
                 var neighRadius: Int = 0, val learnFactor: Double, val tuneFactor: Double = 0, val initFn: Int,
                 val distanceFn: Int, val neighFn: Int, val neighDecreaseFn: Int, val decFactor: Double,
                 var trainIter: Int = 0, var tuneIter: Int = 0, var tolerance: Double = 0, val runs: Int = 1,
                 val initSeed: Long = Random.nextInt(), val shuffleSeed: Long = Random.nextInt(),
                 val resultsExportPath: String, val trainingExportPath: String = "") {

  val atributes: String = "Dataset,Dataset sep, Dataset prop,Training Prop,Data normalized,Lattice distribution," +
                          "Lattice width,Lattice height,Neighborhood radius,Learning factor,Tuning factor," +
                          "Initialization,Distance,Neighborhood function,Neighborhood decrease,Radius Factor," +
                          "Training iters,Tuning iters,Tolerance,Models created,Init seed,Set shuffling seed"

  val parameters: String = List(dataset, setSep, setProp, trainingProp, normalize, latDistrib, width, height,
                                neighRadius, learnFactor, tuneFactor, initFn, distanceFn, neighFn, neighDecreaseFn,
                                decFactor, trainIter, tuneIter, tolerance, runs, initSeed, shuffleSeed).mkString(",")


  def completeConfig (width: Int, height: Int, rad: Int, trainIter: Int, tuneIter: Int): Unit = {
    this.width = width
    this.height = height
    this.neighRadius = rad
    if (this.trainIter == 0) this.trainIter = trainIter
    if (tuneFactor != 0 && tuneIter == 0) this.tuneIter = tuneIter
  }
}
