package cu.edu.cujae.som.map

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
    roughTrainingIters = mapConfig.trainIter
    // Obtains stop condition
    var stopTol = mapConfig.tolerance
    // Prepares iteration flags
    var t = 0
    var prevAvMQE = 0D

    do {
      // Obtains previous average MQE of the map
      prevAvMQE = mapAvgMQE
      // Obtains set iterator
      val setIt = vectorSet.iterator

      // Assigns each input to its BMU
      setIt.foreach(x => clusterInput(x))

      // Applies learning to each neuron of the map
      lattice.neurons.flatten.foreach(current => {
        var accVector = new Array[Double](dimensionality)
        var accNeigh = 0D

        lattice.neurons.flatten.foreach(bmu => {
          val repAmm = bmu.representedInputs.size

          if (repAmm > 0) {
            val neighRank = neighborhoodFn(bmu.xPos, bmu.yPos, current.xPos, current.yPos, decRadius)

            accVector = accVector.zip(bmu.generalizedMedian).map(x => x._1 + (neighRank * repAmm * x._2))
            accNeigh += repAmm * neighRank
          }
        })
        current.weightVector = accVector.map(x => x / accNeigh)
      })
      // Updates values for next iteration
      updateRadius(t)
      updateAvMQE()

      // Resets assigned inputs
      lattice.neurons.flatten.foreach(x => x.restartRepresented())

      t += 1
    } while (/**(prevAvMQE - mapAvgMQE) > stopTol &&*/ t < roughTrainingIters )

    // Assigns each input to its BMU
    vectorSet.iterator.foreach(x => clusterInput(x))

    updateMQEDeviation()
  }
}
