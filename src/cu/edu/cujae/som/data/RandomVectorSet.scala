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


  /**
   * Provides an iterator over a randomized order of the vectors
   * @return Randomized SetIterator
   */
  override def iterator: Iterator[InputVector] = new SetIterator(rand.shuffle(vectors))
}
