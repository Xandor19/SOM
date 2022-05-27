package som

/**
 * Class to represent an input vector, useful for training or testing stages
 * @param vector The input data
 * @param dim The dimension of the vector
 */
class InputVector (val dim: Int, val vector: Array[Double], val classification: String) {
}
