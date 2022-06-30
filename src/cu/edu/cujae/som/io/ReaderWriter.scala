package cu.edu.cujae.som.io

import java.io.FileNotFoundException
import java.util.Formatter

import cu.edu.cujae.som.data.InputVector
import cu.edu.cujae.som.map.SOM

/**
 * Provides functions to load input vectors from different sources
 */
object ReaderWriter {

  /**
   * Loads a generic csv file into a list of records
   * @param path Path to csv file
   * @param separator Columns separator of the csv
   * @return List with all the csv's lines
   */
  def loadCSV (path: String, separator: Char): List[String] = {
    // Opens file
    val bufferedSource = io.Source.fromFile(path)
    // Read all lines from file
    val lines = bufferedSource.getLines().toList
    // Closes file
    bufferedSource.close()

    lines
  }


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
   * @return Tuple of (inputs dimension, input features, inputs)
   */
  def loadSet (path: String, separator: Char): (Array[String], List[InputVector]) = {
    // Reads csv
    val records = loadCSV (path, separator)
    // Obtains features names
    val features = records.head.split(separator).map(_.trim)
    // Obtains the input's dimensionality
    val dimensionality = features.length - 1
    // Empty vectors list
    var vectors = List.empty[InputVector]
    //
    var rowIndex = 1

    // Iterates over all inputs
    records.tail.foreach(i => {
      // Clears current input into separated no-spaced columns
      val cleared = i.split(separator).map(_.trim)

      // Creates this input's vector by splitting the features values from the labeled class
      vectors = vectors.appended(new InputVector(rowIndex, dimensionality,
                                                 cleared.slice(0, dimensionality).map(_.toDouble), cleared.last))
      rowIndex += 1
    })
    (features, vectors)
  }


  /**
   * Loads a set of pre-trained weight vectors from a json file
   *
   * @param path Path to configuration file
   * @return
   */
  def loadTraining (path: String): SOM = {
    null
  }


  /**
   * Export a pre-trained set of weight vectors to a json file
   *
   * The first row of the csv contains the SOM's distribution data,
   * that is, wight, height and vector dimensionality, in that order
   *
   * Vectors are write in wight-then-height order, that is, each
   * row is exported sequentially before going to the next row
   *
   * //@param path Path to csv file
   * //@param som Trained Self-Organizing Map to export
   */
  def exportTraining (config: MapConfig, som: SOM): Unit = {
  }


  /**
   * Exports the results of a experiment to a csv
   * @param path Destination file of the export
   * @param sep Csv separator
   * @param data Resulting parameters to export
   */
  def exportExperimentResult (path: String, sep: Char, data: List[ExperimentData]): Unit = {
    // List to store old file's content
    var existing = List.empty[String]
    var prev = true

    try {
      // Tries to load the destination file
      existing = existing.appendedAll(loadCSV(path, sep))
    }
    catch {
      // The destination file has not been created
      case _: FileNotFoundException => prev = false
    }
    val writer = new Formatter(path)

    // Creates the destination file and sets the headers
    if (!prev) writer.format("%s\n", data.head.attributes)

    // Appends current export data to the existing one (if exists)
    existing = existing appendedAll data.map(x => x.data)

    // Writes the data
    existing.foreach(x => {
      writer.format("%s\n", x)
    })
    writer.flush()
    // Closes file
    writer.close()
  }
}
