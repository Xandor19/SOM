package cu.edu.cujae.som.data

import scala.util.Random

/**
 * Class to represent a set of vectors that is randomized before access
 * @param features Definition of each dimension of the vectors
 * @param vectors Input vectors
 */
class RandomVectorSet (features: Array[String], vectors: List[InputVector], seed: Long)
                      extends VectorSet (features, vectors) {

  /*
   * Class fields
   */
  val rand = new Random()
  rand.setSeed(seed)

  var randomized: List[InputVector] = rand.shuffle(vectors)

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
    randomized = rand.shuffle(vectors)
  }
}
