package som

class Lattice (val width: Int, val height: Int,
               val learningFactor: Double, val factorController: Double,
               val neighRadius: Int, val radiusController: Double,
               distanceFn: (Array[Double], Array[Double]) => Double,
               neighborhoodFn: (Int, Int, Int, Int, Double, Int) => Double,
               learningFactorUpdateFn: (Double, Int, Double) => Double,
               neighborhoodRadiusUpdateFn: (Int, Int, Double) => Double) {

  /*
   * Class fields
   */
  val neurons: Array[Array[Neuron]] = Array.ofDim[Neuron](width, height)
  private var dimensionality = 0
  private var tuningStage = false


  /**
   * Temporal construction function
   * @param vectorDim Dimensionality of the weight vector
   * @param vectorInitFn Function to initialize the weights
   * @param dimBounds Set of lower and upper bounds for each dimension
   */
  def constructLattice (vectorDim: Int, vectorInitFn: (Array[Double], Array[(Double, Double)]) => Array[Double],
                        dimBounds: Array[(Double, Double)]): Unit = {
    //TODO change to abstract when rectangular and hexagonal lattices are implemented
    dimensionality = vectorDim

    for (i <- 0 until width; j <- 0 until height) {
      neurons(i)(j) = new Neuron(i, j, new Array[Double](dimensionality), distanceFn)
      neurons(i)(j).initializeWeights(vectorInitFn, dimBounds)
    }
  }


  /**
   * Initializes the neurons of the lattice with
   * the weight vectors received from an external source
   *
   * Weight vector are ordered in width-then-height indexation,
   * namely, each row is presented sequentially
   *
   * @param weightsSet The array of weight vectors to place in
   *                   the map
   */
  def importLattice (weightsSet: Array[Array[Double]]): Unit = {
    // Checks wetter there are as much weight vectors as neurons
    if (weightsSet.length == width * height) {
      // Iterator for the received vectors
      val it = weightsSet.iterator

      // Assigns each weight vector to the neuron in the corresponding position
      for (i <- 0 until width; j <- 0 until height) {
        neurons(i)(j) = new Neuron(i, j, it.next(), distanceFn)
      }
    }
    // An invalid vector set was received
    else throw new IllegalStateException("Amount of weight vectors differs from total amount of neurons in the lattice")
  }


  /**
   * Self-organization process, which lasts for a maximum number
   * of iterations or until a tolerable QE is reached
   * @param vectorSet Set of vectors that will be used for training
   * @param maxIter Maximum number of iterations
   * @param tolerance MQE tolerance level
   */
  def organizeMap (vectorSet: VectorSet, maxIter: Int, tolerance: Double): Unit = {
    // Train for a predefined number of iterations
    //TODO modify loop to add tolerance stop-condition
    for (t <- 0 until maxIter) {
      neurons.flatten.foreach(x => x.restartRepresented())
      // Present all inputs to the map
      while (vectorSet.hasNext) {
        // Obtain next vector to analyze
        val currentVector = vectorSet.next

        // Applies learning cycle around the found BMU
        learnFromInput(findBMU(currentVector.vector), currentVector, t)
      }
    // Reset iteration process over the inputs
      vectorSet.reset()
    }
    // Once the map is organized, present inputs one last time to form clusters
    vectorSet.vectors.foreach(x => {
      val bmu = findBMU(x.vector)
      bmu.adoptInput(x)
    })
  }


  /**
   * Finds the best matching unit of a given input, that is,
   * the neuron for which the quantification error (QE), namely
   * distance, is minimum to the input vector
   * @param input The input vector to find its BMU
   * @return The neuron that has the MQE for that input
   */
  def findBMU (input: Array[Double]): Neuron = neurons.flatten.minBy[Double](_.distanceTo(input))


  /**
   * Applies corresponding training functions to the BMU
   * and its neighbors
   * @param bmu The neuron with the MQE for given input
   * @param input The input vector
   * @param epoch Current time value
   */
  def learnFromInput (bmu: Neuron, input: InputVector, epoch: Int): Unit = {
    /**println("BMU at: (" + bmu.xPos + ", " + bmu.yPos + ")")*/
    //TODO uncomment when differentiation between training stage and tuning stage (radius = 0) is made
    /**if (tuningStage) {
      TODO Apply single tuning function
    }
    else { */
      //TODO uncomment when differential learning is implemented (definition of neighborhood) and remove last line
      /**trainBMU(bmu.weights, input, epoch)
      bmu.allNeighbors.foreach(n => trainNeighbor(bmu.xPos, bmu.yPos, n, input, epoch))*/
      neurons.flatten.foreach(x => {
        trainNeighbor(bmu.xPos, bmu.yPos, x, input.vector, epoch)
      })
    /**}*/
  }


  /**
   * Applies default training function to a BMU
   * Uses the formula:
   * wi (t + 1) = wi (t) + a(t) * dist(wi, vi)
   *
   * @param bmuWeights Weight vector of the BMU
   * @param inputVector Input vector represented by the BMU
   * @param epoch Current time value
   */
  def trainBMU (bmuWeights: Array[Double], inputVector: Array[Double], epoch: Int): Unit = {
    // Updates each dimension
    for (i <- bmuWeights.indices) {
      val currentWeight = bmuWeights(i)

      // Updates current dimension of the weight vector
      bmuWeights.update(i,  currentWeight + learningFactorUpdateFn(learningFactor, epoch, factorController) *
                            (currentWeight - inputVector(i)))
    }
  }


  /**
   * Applies the training function to a BMU in
   * the tuning stage, namely, when the neighborhood
   * radius is 0
   * @param bmuWeights Weight vector of the BMU
   * @param inputVector Input vector represented by the BMU
   * @param epoch Current time value
   */
  def tuneBMU (bmuWeights: Array[Double], inputVector: Array[Double], epoch: Int): Unit = {
    //TODO define tuning function
  }


  /**
   * Applies the neighborhood function extended training function
   * to reduce impact of a input vector on a neighbor of the BMU
   * Uses the formula:
   * wi (t + 1) = wi (t) + a(t) * hci(t) * dist(wi, vi)
   *
   * @param bmuX Value of x position of the BMU on the lattice
   * @param bmuY Value of the y position of the BMU on the lattice
   * @param neighbor BMU's neighbor neuron
   * @param inputVector Input vector represented by the BMU
   * @param epoch Current time value
   */
  def trainNeighbor (bmuX: Int, bmuY: Int, neighbor: Neuron,
                    inputVector: Array[Double], epoch: Int): Unit = {
    //Gets neighbor's weight vector
    val weights = neighbor.weights

    for (i <- weights.indices) {
      val currentWeight = weights(i)

      // Updates current dimension of the weight vector
      weights.update(i,  currentWeight + learningFactorUpdateFn(learningFactor, epoch, factorController) *
                         neighborhoodFn(bmuX, bmuY, neighbor.xPos, neighbor.yPos,
                         neighborhoodRadiusUpdateFn(neighRadius, epoch, radiusController), epoch) *
                         (inputVector(i) - currentWeight))
    }
  }


  def somDimensionality: Int = dimensionality


  /**
   * Prints the set of weights of this map's neurons
   */
  def printSet (): Unit = {
    neurons.flatten.foreach(x => {
      print("Neuron at pos (" + x.xPos + ", " + x.yPos + "): [")
      x.weights.foreach(v => print(v + ", "))
      println("]")
    })
  }


  /**
   * So far, it prints how many times each neuron was BMU for the final order
   */
  def printMap (): Unit = {
    for (i <- 0 until width) {
      for (j <- 0 until height) {
        print(neurons(i)(j).matches.size + "\t")
      }
      println()
    }
  }
}
