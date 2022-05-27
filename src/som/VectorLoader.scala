package som

/**
 * Provides functions to load input vectors from different sources
 */
object VectorLoader {
  /**
   * Loads dataset from csv
   * @param path Path to csv file
   * @param separator Columns separator of the csv
   * @return Tuple (inputs dimension, input features, inputs)
   */
  def loadFromCSV (path: String, separator: Char): (Int, Array[String], List[InputVector]) = {
    // Opens file
    val bufferedSource = io.Source.fromFile(path)
    // Read all lines from file
    val records = bufferedSource.getLines().toList
    // Obtains the input's features names
    val features = records.head.split(separator).map(_.trim)
    // Obtains the input's dimensionality
    val dimensionality = features.length - 1
    // Empty vectors list
    var vectors = List.empty[InputVector]

    // Iterates over all inputs
    for (i <- records.tail) {
      // Clears current input into separated no-spaced columns
      val cleared = i.split(separator).map(_.trim)

      // Creates this input's vector by splitting the features values from the labeled class
      vectors = vectors.appended(new InputVector(dimensionality,
                                                 cleared.slice(0, dimensionality).map(_.toDouble), cleared.last))
    }
    // Closes file
    bufferedSource.close()

    (dimensionality, features, vectors)
  }


  def loadFromJSON (): (Int, List[String], List[InputVector]) = {
    (0, List.empty[String], List.empty[InputVector])
  }


  def loadFromXML (): (Int, List[String], List[InputVector]) = {
    (0, List.empty[String], List.empty[InputVector])
  }
}
