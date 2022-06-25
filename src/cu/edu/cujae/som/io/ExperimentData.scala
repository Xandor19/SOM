package cu.edu.cujae.som.io

/**
 * Class to store and transfer the results of a experiment (training/testing)
 * @param config Configuration parameters of the flow
 * @param trainAvMQE Average MQE obtained after training
 * @param trainAvSD Standard of deviation of the training average MQE
 * @param avCorrect Average of correctly classified instances
 * @param avIncorrect Average of incorrectly classified instances
 * @param avPrecision Average of classification precision
 * @param testAvMQE Average MQE obtained after clustering test
 * @param testAvSD Standard of deviation of the testing average MQE
 */
class ExperimentData (config: MapConfig, trainAvMQE: Double, trainAvSD: Double, avCorrect: Double,
                      avIncorrect: Double, avPrecision: Double, testAvMQE: Double, testAvSD: Double,
                      elapsedTime: String) {

  /*
   * Provides a string with the field names for new export files
   */
   val attributes: String = config.attributes + "," + "Av MQE after training,MQE SD after training,Av success,Av " +
                                                      "failure,Av Precision,Av MQE after test,MQE SD after test," +
                                                      "Elapsed time"


  /*
   * Provides a string with current instance values to transfer
   */
   def data: String = config.parameters + "," + List(trainAvMQE, trainAvSD, avCorrect, avIncorrect, avPrecision,
                                                     testAvMQE, testAvSD, elapsedTime).mkString(",")
}
