package cu.edu.cujae.som.io

/**
 * Class to store/transfer the results of a clustering test
 * @param config Configuration parameters of the flow
 * @param trainAvMQE Average MQE obtained after training
 * @param trainAvSD Standard of deviation of the training average MQE
 * @param avCorrect Average of correctly classified instances
 * @param avIncorrect Average of incorrectly classified instances
 * @param avPrecision Average of classification precision
 * @param elapsedTime Running time of the test
 */
class ClusteringData(config: MapConfig, trainAvMQE: Double, trainAvSD: Double, avCorrect: Double, avIncorrect: Double,
                     avPrecision: Double, elapsedTime: String)
                    extends ExperimentData (config, trainAvMQE, trainAvSD, elapsedTime) {

  /**
   * Provides a string with the field names for new export files
   */
  override def attributes: String = super.attributes + "," + "Av success,Av failure,Av Precision"


  /**
   * Provides a string with current instance values to transfer
   * Coma separated string with the result values in the attributes order
   */
  override def data: String = super.data + "," + List(avCorrect, avIncorrect, avPrecision).mkString(",")

}
