package cu.edu.cujae.som.data

/**
 * Class to represent a set of vectors that is accessed sequentially
 *
 * @param features Definition of each dimension of the vectors
 * @param vectors Input vectors
 */
class SequentialVectorSet (features: Array[String], vectors: List[InputVector])
                          extends VectorSet (features, vectors) {

  /**
   * Provides the next input vector in sequential order
   * @return Vector in current index
   */
  override def next: InputVector = {
    vectors(accessIndex)
  }


  /**
   * Resets the iteration parameters, returning to the start index
   */
  override def reset (): Unit = accessIndex = -1
}

