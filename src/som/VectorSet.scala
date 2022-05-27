package som

/**
 * Class to represent a set of input vectors in a tabular view
 * * Provides methods to iterate over this vectors in an ordered
 * * or randomized way
 * @param features Definition of each dimension of the vectors
 * @param vectors Input vectors
 */
abstract class VectorSet (val features: Array[String], val vectors: List[InputVector]) {

  /*
   * Class fields
   */
  val dimensionality:Int = features.length - 1
  val dimBounds: Array[(Double, Double)] = new Array[(Double, Double)](dimensionality)
  protected var accessIndex: Int = -1


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
      dimBounds.update(i, (minInDim, maxInDim))
    }
  }


  /**
   * Normalizes this set's vectors
   */
  def normalize (): Unit = {
    vectors.foreach(x => {
      // Applies normalization for each dimension individually
      for (i <- 0 until dimensionality) {
        val currentLow = dimBounds(i)._1
        val currentHigh = dimBounds(i)._2

        // Normalization formula
        x.vector.update(i, (x.vector(i) - currentLow) / (currentHigh - currentLow))
      }
    })
  }

  /**
   * Checks wetter there are input vectors remaining
   * @return True if the sequential access has not reached
   *         the amount of vectors, false otherwise
   */
  def hasNext: Boolean = {
    accessIndex += 1

    if (accessIndex < vectors.size) true
    else false
  }


  /**
   * Prints this vector set as a table, specifying the dimensions
   * names and the class of each vector, if known
   */
  def printSet (): Unit = {
    features.foreach(x => print(x + ", "))
    println()
    vectors.foreach(x => {
      x.vector.foreach(y => print(y + ", "))
      println(x.classification)
    })
    println(vectors.length)
  }


  /*
   * Abstract methods for iteration
   */
  def next: InputVector
  def reset (): Unit
}
