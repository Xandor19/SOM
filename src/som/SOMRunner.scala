package som

import scala.util.Random

object SOMRunner {
  def main(args: Array[String]): Unit = {
    val path = "/home/xandor19/iris.csv"
    val sep = ','
    val trainingSetSize = 0.8
    val somLearningFactor = 0.8
    val somFactorController = 0.5
    val somNeighRadius = 5
    val somRadiusController = 0.5
    val maxTrainingIter = 108000

    createSOM(path, sep, trainingSetSize, somLearningFactor, somFactorController, somNeighRadius,
              somRadiusController, maxTrainingIter)
  }


  def createSOM (datasetPath: String, datasetSeparator: Char, trainingSetProp: Double,
                 somLearningFactor: Double, somFactorController: Double, somNeighRadius: Int,
                 somRadiusController: Double, maxTrainingIter: Int): Unit = {
    // Loads dataset
    val data = VectorLoader.loadFromCSV(datasetPath, datasetSeparator)
    // Gets inputs' dimensionality
    val dimensionality = data._1
    // Defines training set as the 80% of the dataset
    val trainingSetSize = (data._3.length * trainingSetProp).toInt
    // Shuffles the input to make random selection of the subsets
    val shuffledVectors = Random.shuffle(data._3)
    // Obtains training set from the shuffled input
    val trainingSet = new RandomVectorSet(data._2, shuffledVectors.slice(0, trainingSetSize))
    // Obtains test set as the remaining inputs
    val testSet = new SequentialVectorSet(data._2, shuffledVectors.slice(trainingSetSize, data._3.size))

    //TODO how to define neurons amount and how to distribute it
    trainingSet.findBounds()
    trainingSet.normalize()

    // Creates the SOM lattice with specified parameters
    val som = new Lattice(9, 6, somLearningFactor, somFactorController, somNeighRadius, somRadiusController)

    // Sets the initial state of the lattice by initializing neurons and setting the distance function
    som.constructLattice(dimensionality, vectorInitFn, trainingSet.dimBounds, distanceFn)

    //som.printSet()

    // SOM's training process
    som.organizeMap(trainingSet, maxTrainingIter, 0)

    som.printMap()
  }


  /**
   * Creates a vector with random values in the range provided
   * @param bounds Set of lower and upper bounds for each dimension
   * @param dim Dimensionality of the vector
   * @return The created random vector
   */
  def vectorInitFn (bounds: Array[(Double, Double)], dim: Int): Array[Double] = {
    val x = new Array[Double](dim)

    for (i <- 0 until dim) {
      val dimMin = bounds(i)._1
      val dimMax = bounds(i)._2

      x.update(i, (Random.between(dimMin, dimMax) - dimMin) / (dimMax - dimMin) )
    }
    /**print("Vector: ")
    x.foreach(x => print(x + ", "))
    println()*/
    x
  }


  /**
   * Computes the simple euclidean distance between two vectors
   * @param arr1 1st vector
   * @param arr2 2do vector
   * @return
   */
  def distanceFn (arr1: Array[Double], arr2: Array[Double]): Double = {
    math.sqrt((arr1 zip arr2).map(x => math.pow(x._1 - x._2, 2)).sum)
  }
}
