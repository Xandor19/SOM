package cu.edu.cujae.som.data

/**
 * Class to represent a set of input vectors in a tabular view
 * Provides methods to iterate over this vectors in an ordered
 * or randomized way
 *
 * @param features Definition of each dimension of the vectors
 * @param vectors Input vectors
 */
class VectorSet (val features: Array[String], val vectors: List[InputVector]) extends Iterable[InputVector] {

  /*
   * Class fields
   */
  val dimensionality: Int = features.length - 1
  val sampleSize: Int = vectors.size
  protected val bounds: Array[(Double, Double)] = new Array[(Double, Double)](dimensionality)
  protected var boundsFound = false


  /**
   * Calculates the low and top bounds of each dimension in the set
   */
  def findBounds (): Unit = {
    for (i <- 0 until dimensionality) {
      // Sets initial minimum and maximum as the dimension's value in the first vector
      var minInDim = vectors.head.vector(i)
      var maxInDim = minInDim

      // Traverses the rest of the vectors
      vectors.tail.foreach(x => {
        val current = x.vector(i)

        // A new minimum was found, record is updated
        if (current < minInDim) {
          minInDim = current
        }
        // A new maximum was found, record is updated
        else if (current > maxInDim) {
          maxInDim = current
        }
      })
      // Sets this dimension's minimum and maximum values
      bounds.update(i, (minInDim, maxInDim))
    }
    boundsFound = true
  }


  /**
   * Provides the dimension's bounds of this set
   * @return Array of tuples (Double, Double) with the lower and
   *         upper bounds respectively
   */
  def dimBounds: Array[(Double, Double)] = {
    if (!boundsFound) findBounds()
    bounds
  }


  /**
   * Normalizes this set's vectors
   * Dimension's bounds must be found with findBounds method
   * before using this method
   */
  def normalize (): Unit = {
    // Checks wetter the dimensions's bound have been found
    if (!boundsFound) findBounds()

    vectors.foreach(x => {
      // Applies normalization for each dimension individually
      for (i <- 0 until dimensionality) {
        val currentLow = bounds(i)._1
        val currentHigh = bounds(i)._2

        // Normalization formula
        x.vector.update(i, (x.vector(i) - currentLow) / (currentHigh - currentLow))
      }
    })
  }


  /**
   * Provides this vector set as a table, specifying the dimensions
   * names and the class of each vector, if known
   */
  def asTable (): Unit = {
    features.foreach(x => print(x + ", "))
    println()
    vectors.foreach(x => {
      x.vector.foreach(y => print(y + ", "))
      println(x.classification)
    })
    println(vectors.length)
  }


  /**
   * Provides an iterator for the original order of the inputs
   * @return SetIterator
   */
  override def iterator: Iterator[InputVector] = new SetIterator(vectors)


  /**
   * Iterator for a set of input vectors
   * @param vectors The input vectors to iterate over
   */
  class SetIterator (val vectors: List[InputVector]) extends Iterator[InputVector] {

    /*
     * Class fields
     */
    private var accessIndex = -1
    private val sampleSize = vectors.size


    /**
     * Checks wetter there are input vectors remaining
     * @return True if the sequential access has not reached
     *         the amount of vectors, false otherwise
     */
    override def hasNext: Boolean = {
      accessIndex += 1

      if (accessIndex < sampleSize) true
      else false
    }


    /**
     * Provides next vector in the iteration order
     * @return
     */
    override def next: InputVector = vectors(accessIndex)
  }
}
