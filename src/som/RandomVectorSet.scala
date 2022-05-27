package som

import scala.util.Random

/**
 * Class to represent a set of vectors that is randomized before access
 * @param features Definition of each dimension of the vectors
 * @param vectors Input vectors
 */
class RandomVectorSet (override val features: Array[String], override val vectors: List[InputVector])
                      extends VectorSet (features, vectors) {

  /*
   * Class fields
   */
  private var randomized = Random.shuffle(vectors)

  /**
   * Provides next input vector in the order in which where shuffled
   * @return Vector in current index
   */
  override def next: InputVector = {
    randomized(accessIndex)
  }


  /**
   * Resets the iteration parameters, returning to the start index and
   * re-shuffling the vectors to a new random order
   */
  override def reset (): Unit = {
    accessIndex = -1
    randomized = Random.shuffle(vectors)
  }
}
