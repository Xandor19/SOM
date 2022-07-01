package cu.edu.cujae.som.io

/**
 * Class to store and transfer the results of a experiment (training/testing)
 * @param config Configuration parameters of the flow
 * @param trainAvMQE Average MQE obtained after training
 * @param trainAvSD Standard of deviation of the training average MQE
 * @param elapsedTime Running time of the test
 */
abstract class ExperimentData (config: MapConfig, trainAvMQE: Double, trainAvSD: Double, elapsedTime: String) {

  /*
   * Provides a string with the field names for new export files
   */
   def attributes: String = (if (config != null) config.attributes + "," else "") +
                            "Elapsed time,Av MQE after training,MQE SD after training"


  /**
   * Provides a string with current instance values to transfer
   * Coma separated string with the result values in the attributes order
   */
   def data: String = (if (config != null) config.parameters + "," else "") +
                      List(elapsedTime, trainAvMQE, trainAvSD).mkString(",")
}
