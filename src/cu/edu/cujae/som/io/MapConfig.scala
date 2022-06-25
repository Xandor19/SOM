package cu.edu.cujae.som.io

import scala.util.Random

/**
 * Class to transfer the configuration parameters of a SOM life cycle
 * @param dataset Path to the dataset that will be applied to the SOM
 * @param setSep Separator of the dataset's csv
 * @param setProp Proportion of the dataset to use as sample
 * @param trainingProp Proportion of the sample to use as training set
 * @param normalize Specifies if data will be normalized
 * @param somType Type of SOM (by training approach) to create
 * @param latDistrib Distribution of the lattice of the SOM
 * @param width Width of the lattice
 * @param height Height of the lattice
 * @param neighRadius Initial neighborhood radius
 * @param learnFactor Learning factor for on-line SOM
 * @param tuneFactor Tuning factor for on-line SOM
 * @param initFn Initialization function to use
 * @param distanceFn Distance function to use
 * @param neighFn Neighborhood function to use
 * @param trainIter Number of rough training iters
 * @param tuneIter Number of iters for tuning stage (on-line SOM)
 * @param tolerance Tolerable average MQE error (on-line SOM) or average variation
 *                  ratio of a weight vector (batch SOM) to reach
 * @param runs Number of models to create and evaluate
 * @param initSeed Seed for traceable random initialization
 * @param shuffleSeed Seed for traceable sets shuffling
 * @param resultsExportPath Path to export the results of the training/testing
 * @param trainingExportPath Path to export the results of a new trained SOM
 */
class MapConfig (var dataset: String, val setSep: Char = ',', val setProp: Double, val trainingProp: Double,
                 val normalize: Boolean = false, val somType: String, val latDistrib: String, var width: Int = 0,
                 var height: Int = 0, var neighRadius: Int = 0, val learnFactor: Double, val tuneFactor: Double = 0,
                 val initFn: String, val distanceFn: String, val neighFn: String,
                 var trainIter: Int = 0, var tuneIter: Int = 0, var tolerance: Double = 0, val runs: Int = 1,
                 val initSeed: Long = Random.nextInt(), val shuffleSeed: Long = Random.nextInt(),
                 val resultsExportPath: String, val trainingExportPath: String = "") {

  /*
   * Provides a string with the field names for new export files
   */
  val attributes: String = "Dataset,Dataset prop,Training Prop,Data normalized,SOM type,Lattice " +
                          "distribution,Lattice width,Lattice height,Neighborhood radius,Learning factor,Tuning " +
                          "factor,Initialization,Distance,Neighborhood function," +
                          "Training iters,Tuning iters,Tolerance,Models created,Init seed,Set shuffling seed"

  /*
   * Provides a string with current instance values to use
   */
  def parameters: String = List(dataset, setProp, trainingProp, normalize, somType, latDistrib, width, height,
                                neighRadius, learnFactor, tuneFactor, initFn, distanceFn, neighFn,
                                trainIter, tuneIter, tolerance, runs, initSeed, shuffleSeed).mkString(",")


  /**
   * Completes the configuration parameters which could be automatically
   * generated
   * Params are assumed to be left for auto setup if width and/or height
   * are left as default (0)
   *
   * @param width Width of the lattice
   * @param height Height of the lattice
   * @param rad Neighborhood radius
   * @param trainIter Amount of rough training iters
   * @param tuneIter Amount of tuning iters (on-line SOM)
   */
  def completeConfig (width: Int, height: Int, rad: Int, trainIter: Int, tuneIter: Int): Unit = {
    this.width = width
    this.height = height
    this.neighRadius = rad
    if (this.trainIter == 0) this.trainIter = trainIter
    if (tuneFactor != 0 && tuneIter == 0) this.tuneIter = tuneIter
  }
}
