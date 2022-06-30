package cu.edu.cujae.som.data

/**
 * Class to represent an input vector, useful for training or testing stages
 * @param vector The input data
 * @param dim The dimension of the vector
 */
class InputVector (val datasetIndex: Int, val dim: Int, val vector: Array[Double], val classification: String) {
}
