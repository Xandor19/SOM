package cu.edu.cujae.som.map

import cu.edu.cujae.som.aux.SOMController
import cu.edu.cujae.som.data.VectorSet
import cu.edu.cujae.som.io.MapConfig

/**
 * Class to represent a batch training Self-Organizing Map
 * @param lattice The lattice of neurons
 * @param neighRadius Neighborhood radius
 * @param distanceFn Distance metric to use
 * @param neighborhoodFn Neighborhood function
 */
class BatchSOM (lattice: Lattice, neighRadius: Double, distanceFn: (Array[Double], Array[Double]) => Double,
                neighborhoodFn: (Float, Float, Float, Float, Double) => Double)
               extends SOM (lattice, neighRadius, distanceFn, neighborhoodFn) {

  /**
   * Self-organization process, which lasts for a maximum number
   * of iterations or the variation of the map is tolerable
   *
   * @param vectorSet Set of vectors that will be used for training
   * @param mapConfig Configuration of the SOM which includes the training
   *                  settings
   */
  override def organizeMap(vectorSet: VectorSet, mapConfig: MapConfig): Unit = {
    val trainIters = mapConfig.trainIter
    // Obtains stop condition
    //var stopTol = mapConfig.tolerance
    // Prepares iteration flags
    var t = 0
    var prevAvMQE = 0D

    do {
      // Obtains previous average MQE of the map
      //prevAvMQE = avgMQE
      // Obtains set iterator
      val setIt = vectorSet.iterator

      // Assigns each input to its BMU
      setIt.foreach(x => clusterInput(x))

      // Applies learning to each neuron of the map
      lattice.neurons.flatten.foreach(x => applySingleBatch(x, t))

      // Updates values for next iteration
      decRadius = updateRadius(t, trainIters)
      //updateAvMQE()

      // Resets assigned inputs
      lattice.neurons.flatten.foreach(x => x.clearRepresented())
      t += 1
    } while (/**(prevAvMQE - mapAvgMQE) > stopTol &&*/ t < trainIters )

    // Assigns each input to its BMU
    vectorSet.iterator.foreach(x => clusterInput(x))

    updateSdMQE()
  }


  /**
   * Applies batch training to a individual neuron in the lattice,
   * applying a neighboring function to reduce the changes as the
   * neurons get father from the BMU
   *
   * Computes the weighted mean of the inputs clustered in the map
   *
   * Uses the formula
   *
   * wi(t + 1) = (sum from j (nj * hji(t) xmj)) / (sum from j (nj * hji(t)))
   *
   * Where nj is the amount of vectors represented by a neuron j, hji is the
   * neighborhood function between neuron to update i and current neuron j
   * and xmj is the mean of the vectors represented by neuron j
   * @param current Current neuron to train
   * @param epoch Current time value
   */
  def applySingleBatch (current: Neuron, epoch: Int): Unit = {
    // Partial results accumulators
    var accVector = new Array[Double](dimensionality)
    var accNeigh = 0D

    // Traverses the map
    lattice.neurons.flatten.foreach(bmu => {
      // Amount of inputs represented by current BMU
      val repAmm = bmu.representedInputs.size

      if (repAmm > 0) {
        // The neuron is actually a BMU
        // Obtains neighborhood rank between current neuron and BMU
        val neighRank = neighborhoodFn(bmu.xPos, bmu.yPos, current.xPos, current.yPos, decRadius)

        // Computes the mean vector of its represented inputs
        accVector = accVector.zip(bmu.generalizedMedian).map(x => x._1 + (neighRank * repAmm * x._2))
        accNeigh += repAmm * neighRank
      }
    })
    // Updates the weight vector with the weighted mean of the inputs
    current.weightVector = accVector.map(x => x / accNeigh)
  }
}
