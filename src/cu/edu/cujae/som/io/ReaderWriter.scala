package cu.edu.cujae.som.io

import java.io.{File, FileInputStream, FileNotFoundException, FileOutputStream}
import java.util.Formatter

import cu.edu.cujae.som.data.InputVector
import net.maritimecloud.internal.core.javax.json.{Json, JsonObject}
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
  def loadTraining (path: String): MapIO = {
    val reader = Json.createReader(new FileInputStream(path)).readObject()

    val dataset = reader.getString("dataset")
    val task = reader.getString("task")
    val somType = reader.getString("type")
    val latDistrib = reader.getString("distrib")
    val width = reader.getInt("width")
    val height = reader.getInt("height")
    val normalized = reader.getBoolean("normalized")
    val distFn = reader.getString("dist_funct")
    val avMQE = reader.getString("av_mqe").toDouble
    val sdMQE = reader.getString("mqe_sd").toDouble
    val neurons = reader.getJsonArray("neurons").toArray.
                                                 map(_.asInstanceOf[JsonObject]).
                                                 map(y => (y.getString("vector"), y.getInt("hits"),
                                                           y.getString("balance"), y.getString("class")))

    new MapIO(dataset, task, somType, latDistrib, width, height, normalized, distFn, avMQE, sdMQE, neurons)
  }


  /**
   * Export a pre-trained set of weight vectors to a json file
   * @param path Path of the export file
   * @param data Data of the model to export
   */
  def exportTraining (path: String, data: MapIO): Unit = {
    val factory = Json.createBuilderFactory(null)
    val neurons = factory.createArrayBuilder()

    data.neurons.foreach(x => {
      neurons.add(factory.createObjectBuilder().
                  add("vector", x._1).
                  add("hits", x._2).
                  add("balance", x._3).
                  add("class", x._4).
                  build())
    })
    val export = factory.createObjectBuilder().
                 add("dataset", data.dataset).
                 add("task", data.task).
                 add("type", data.somType).
                 add("distrib", data.latDistrib).
                 add("width", data.width).
                 add("height", data.height).
                 add("normalized", data.normalized).
                 add("dist_funct", data.distFn).
                 add("av_mqe", data.avMQE).
                 add("mqe_sd", data.sdMQE).
                 add("neurons", neurons.build()).
                 build()

    val writer = Json.createWriter(new FileOutputStream(new File(path)))
    writer.write(`export`)
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
