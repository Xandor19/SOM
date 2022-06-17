package som

import java.util.concurrent.TimeUnit

import scala.util.Random

object SOMRunner {
  def main(args: Array[String]): Unit = {
    val path = "/mnt/D80C76380C76122C/Mis Programas/Repos/SOM/Datasets/iris.csv"
    val sep = ','
    val trainingSetSize = 0.8
    val somLearningFactor = 0.85
    val somTuningFactor = 0.2
    val somNeighRadius = 5
    val somRadiusController = 0.5
    val roughIters = 500
    val tuningIters = 54000
    val initSeed = 78
    val shuffleSeed = 43

    val initTime = System.nanoTime()
    createSOM(path, sep, trainingSetSize, somLearningFactor, somTuningFactor, somNeighRadius,
              somRadiusController, roughIters, tuningIters, initSeed, shuffleSeed)
    val endTime = System.nanoTime()
    val seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    println("Total elapsed time: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")
  }


  def createSOM (datasetPath: String, datasetSeparator: Char, trainingSetProp: Double,
                 somLearningFactor: Double, somTuningFactor: Double, somNeighRadius: Int,
                 somRadiusController: Double, roughIters: Int, tuningIters: Int, initSeed: Long,
                 shuffleSeed: Long): Unit = {
    //Random instance
    val rand = new Random()
    rand.setSeed(shuffleSeed)

    // Loads dataset
    var initTime = System.nanoTime()
    val data = ReaderWriter.loadSetFromCSV(datasetPath, datasetSeparator)
    var endTime = System.nanoTime()
    var seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    println("File loaded in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    // Gets inputs' dimensionality
    val dimensionality = data._1
    println("Dataset dimensionality is " + dimensionality)

    // Defines training set as the 80% of the dataset
    val trainingSetSize = (data._3.length * trainingSetProp).toInt
    println("Training set size: " + trainingSetSize)

    // Shuffles dataset to ensure random segmentation
    initTime = System.nanoTime()
    val inputVectors = rand shuffle data._3
    endTime = System.nanoTime()
    seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    println("Inputs shuffled in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    // Obtains training set from the shuffled input
    val trainingSet = new RandomVectorSet(data._2, inputVectors.slice(0, trainingSetSize), shuffleSeed)
    println("Training set created")

    // Obtains test set as the remaining inputs
    // val testSet = new SequentialVectorSet(data._2, inputVectors.slice(trainingSetSize, data._3.size))
    // println("Test set created)

    // Obtains the input's dimensions bounds
    initTime = System.nanoTime()
    trainingSet.findBounds()
    endTime = System.nanoTime()
    seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    println("Dimensionality bounds found in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    // Normalizes the input space
    initTime = System.nanoTime()
    trainingSet.normalize()
    endTime = System.nanoTime()
    seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    println("Input space normalized in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    //TODO This code loads a pre-trained SOM from a file
    /**val load = ReaderWriter.loadTrainingFromCSV("/home/xandor19/training.csv", ',')
    // Creates the SOM lattice with specified parameters
    val som = new RectLattice(load._1(0), load._1(1), somLearningFactor, somFactorController, somNeighRadius,
                          somRadiusController, FunctionCollector.euclideanDistance,
                          FunctionCollector.gaussianNeighborhood, FunctionCollector.exponentialRadiusDecrease)

    // Imports the received training information to the SOM
    som.importLattice(load._2, load._1(2))

    // Clusters the training set in the pre-trained SOM
    trainingSet.vectors.foreach(x => som.clusterInput(x))*/

    //TODO this code trains a new SOM

    // Creates the SOM lattice with specified parameters
    initTime = System.nanoTime()
    val som = new RectLattice(9, 6, somLearningFactor, somTuningFactor, somNeighRadius,
                              somRadiusController, FunctionCollector.euclideanDistance,
                              FunctionCollector.gaussianNeighborhood, FunctionCollector.exponentialRadiusDecrease)

    // Sets the initial state of the lattice by initializing neurons and setting the distance function
    som.constructLattice(dimensionality)
    endTime = System.nanoTime()
    seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    println("Lattice created in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    // Initializes the weight vectors with the specified function
    initTime = System.nanoTime()
    som.initLattice(trainingSet, FunctionCollector.normalizedRandomInit, initSeed)
    //som.initLattice(trainingSet, FunctionCollector.randomInit, initSeed)
    endTime = System.nanoTime()
    seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    println("Lattice initialized in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    // SOM's training process
    initTime = System.nanoTime()
    som.organizeMap(trainingSet, roughIters, tuningIters, 0)
    endTime = System.nanoTime()
    seconds = TimeUnit.SECONDS.convert(endTime - initTime, TimeUnit.NANOSECONDS)
    println("SOM trained in: " + seconds / 60 + " minutes and " + seconds % 60 + " seconds")

    //ReaderWriter.exportTrainingToCSV("/home/xandor19/training.csv", som)

    // Prints training results
    //som.printSet()
    println()
    println()
    som.printMap()
    println()
    println()
    som.printClassesBalance()
    println()
    println()
    som.printMainClasses()
    println()
    println()

    println("AVG MQE: " + som.mapAvgMQE + " in " + (roughIters + tuningIters) + " iters")

    som.updateMQEDeviation()

    println(som.mapMQEDeviation)
  }
}
