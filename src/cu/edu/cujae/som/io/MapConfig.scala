package cu.edu.cujae.som.io

import cu.edu.cujae.som.map.{SOM, SOMType}

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
 * @param runs Number of models to create and evaluate
 * @param randInitSeed Seed for traceable random initialization
 * @param randShuffleSeed Seed for traceable sets shuffling
 * @param task Task to perform with loaded config
 * @param resultsExportPath Path to export the results of the training/testing
 * @param trainingExportPath Path to export the results of a new trained SOM
 */
class MapConfig (var dataset: String, val setSep: Char = ',', val setProp: Double, val trainingProp: Double,
                 val normalize: Boolean = true, val somType: String, val latDistrib: String, var width: Int = 0,
                 var height: Int = 0, var neighRadius: Int = 0, val learnFactor: Double = 0,
                 val tuneFactor: Double = 0, val initFn: String, val distanceFn: String, val neighFn: String,
                 var trainIter: Int = 0, var tuneIter: Int = 0, val runs: Int = 1,
                 val randInitSeed: Long = Random.nextInt(), val randShuffleSeed: Long = Random.nextInt(), val task: String,
                 var resultsExportPath: String, var trainingExportPath: String = "") {

  /*
   * Provides a string with the field names for new export files
   */
  val attributes: String = "Dataset,Dataset prop,Training Prop,Data normalized,SOM type,Lattice " +
                          "distribution,Lattice width,Lattice height,Neighborhood radius,Learning factor,Tuning " +
                          "factor,Initialization,Distance,Neighborhood function," +
                          "Training iters,Tuning iters,Models created,Init seed,Set shuffling seed"

  /*
   * Class fields
   */
  private val randomizedInitSeed = new Random()
  randomizedInitSeed.setSeed(randInitSeed)

  private val randomizedShuffledSeed = new Random()
  randomizedShuffledSeed.setSeed(randShuffleSeed)


  /**
   * Provides a string with current instance values to use
   */
  def parameters: String = List(dataset, setProp, trainingProp, normalize, somType, latDistrib, width, height,
                                neighRadius, learnFactor, tuneFactor, initFn, distanceFn, neighFn,
                                trainIter, tuneIter, runs, randInitSeed, randShuffleSeed).mkString(",")


  /**
   * Provides a new seed for random initialization from the
   * random sequence of the seed for random inits
   * @return Random int
   */
  def initSeed: Int = randomizedInitSeed.nextInt


  /**
   * Provides a new seed for shuffling from the random sequence of the seed
   * for shuffling
   * @return Random int
   */
  def shuffleSeed: Int = randomizedShuffledSeed.nextInt

  /**
   * Completes the configuration parameters which could be automatically
   * generated
   * Params are assumed to be left for auto setup if width and/or height
   * are left as default (0)
   *
   * @param setSize Amount of instances of the dataset
   */
  def completeConfig (setSize: Int): Unit = {
    var neurons: Double = width * height
    // Size auto configuration is required
    if (width == 0 || height == 0) {
      // Obtains amount of neurons from input
      neurons = math.sqrt(setSize) * 5
      // Adapts lattice distribution to the number of neurons
      height = math.sqrt(neurons).toInt
      width = if (neurons - math.pow(height, 2) > height / 2) height + 1
              else if (neurons - math.pow(height, 2) > height * 2) { height += 1; height }
              else height
      // Obtains neighborhood radius from the distribution
      neighRadius = width / 2 + 1
    }
    // Number of training iters is required
    if (trainIter == 0) {
      if (somType == SOMType.onlineSOM) trainIter = 1000
      // Value for batch training
      else trainIter = 200
    }
    if (tuneIter == 0 && tuneFactor > 0) {
      // Tuning stage adjust for on-line training
      tuneIter = (neurons * 500).toInt
    }
  }


  def exportTraining: Boolean = resultsExportPath != ""
}


object Tasks {
  val clustering = "clustering"
  val anomaly = "anomaly"
}
