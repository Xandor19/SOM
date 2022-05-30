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
   * csv must always have class column, for unclassified instances,
   * leave class field as " "
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


  /**
   * Loads a set of pre-trained weight vectors from a csv file
   *
   * Weight vector must be ordered in width-then-height indexation,
   * namely, each row is presented sequentially
   *
   * First row must be the properties of the map: width, height
   * and vector dimensionality, in that order
   *
   * @param path Path to csv file
   * @param separator Columns separator of the csv
   * @return
   */
  def loadTrainingFromCSV (path: String, separator: Char): (Array[Int], List[Array[Double]]) = {
    val bufferedSource = io.Source.fromFile(path)
    // Read all lines from file
    val records = bufferedSource.getLines().toList
    // Obtains the input's features names
    val data = records.head.split(separator).map(_.trim.toInt)
    // Set of weight vectors
    var vectors = List.empty[Array[Double]]

    for (i <- records.tail) {
      // Clears current input to a real-values vector
      vectors = vectors.appended(i.split(separator).map(_.trim.toDouble))
    }
    (data, vectors)
  }


  /**
   * Export a pre-trained set of weight vectors to a csv file
   *
   * The first row of the csv contains the SOM's distribution data,
   * that is, wight, height and vector dimensionality, in that order
   *
   * Vectors are write in wight-then-height order, that is, each
   * row is exported sequentially before going to the next row
   *
   * @param path Path to csv file
   * @param som Trained Self-Organizing Map to export
   */
  def exportTrainingToCSV (path: String, som: Lattice): Unit = {
    val writer = new Formatter(path)

    // Writes the SOM's distribution data
    writer.format("%d, %d, %d\n", som.width, som.height, som.somDimensionality)
    // Traverses each neuron obtaining its weight vector
    som.neurons.flatten.foreach(n => {
      // Converts this weight vector to a csv line by adding commas between the dimensions
      writer.format("%s\n", n.weights.mkString(", "))
      writer.flush()
    })
    writer.close()
  }
}
