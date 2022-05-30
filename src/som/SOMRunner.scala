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
    val data = ReaderWriter.loadSetFromCSV(datasetPath, datasetSeparator)
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

    //TODO This code loads a pre-trained SOM from a file
    val load = ReaderWriter.loadTrainingFromCSV("/home/xandor19/training.csv", ',')
    // Creates the SOM lattice with specified parameters
    val som = new Lattice(load._1(0), load._1(1), somLearningFactor, somFactorController, somNeighRadius,
                          somRadiusController, FunctionCollector.euclideanDistance,
                          FunctionCollector.gaussianNeighborhood, FunctionCollector.exponentialFactorDecrease,
                          FunctionCollector.exponentialRadiusDecrease)

    // Imports the received training information to the SOM
    som.importLattice(load._2, load._1(2))

    // Clusters the training set in the pre-trained SOM
    trainingSet.vectors.foreach(x => som.clusterInput(x))

    //TODO this code trains a new SOM
    /**
    // Creates the SOM lattice with specified parameters
    val som = new Lattice(9, 6, somLearningFactor, somFactorController, somNeighRadius,
                          somRadiusController, FunctionCollector.euclideanDistance, FunctionCollector.gaussianNeighborhood,
                          FunctionCollector.exponentialFactorDecrease, FunctionCollector.exponentialRadiusDecrease)

    // Sets the initial state of the lattice by initializing neurons and setting the distance function
    som.constructLattice(dimensionality, FunctionCollector.normalizedRandomInit, trainingSet.dimBounds)

    // SOM's training process
    som.organizeMap(trainingSet, maxTrainingIter, 0)

    ReaderWriter.exportTrainingToCSV("/home/xandor19/training.csv", som)*/

    som.printSet()
    som.printMap()
  }
}
