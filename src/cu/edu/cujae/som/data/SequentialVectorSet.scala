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
   * Provides an iterator over the vector set
   * @return Iterator in the original sequential order
   */
  override def iterator: Iterator[InputVector] = new SetIterator(vectors)
}

