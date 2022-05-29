package som

import java.util.Formatter

/**
 * Provides functions to load input vectors from different sources
 */
object ReaderWriter {
  /**
   * Loads dataset from csv
   *
   * csv must be composed by real numbers with the last
   * column being the class to which the instance belongs:
   * value1, value2, value3, ..., valueN, class (str)
   *
   * For unclassified instances, leave class field as " "
   *
   * @param path Path to csv file
   * @param separator Columns separator of the csv
   * @return Tuple (inputs dimension, input features, inputs)
   */
  def loadSetFromCSV(path: String, separator: Char): (Int, Array[String], List[InputVector]) = {
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


  def loadSetFromJSON(): (Int, List[String], List[InputVector]) = {
    (0, List.empty[String], List.empty[InputVector])
  }


  def loadSetFromXML(): (Int, List[String], List[InputVector]) = {
    (0, List.empty[String], List.empty[InputVector])
  }


  def exportTrainingToCSV (path: String, som: Lattice): Unit = {
    val writer = new Formatter(path)

    writer.format("%d, %d, %d\n", som.width, som.height, som.somDimensionality)
    som.neurons.flatten.foreach(n => {
      writer.format("%s\n", n.weights.mkString(", "))
      writer.flush()
    })
  }
}
