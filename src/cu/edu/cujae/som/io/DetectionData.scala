package cu.edu.cujae.som.io

/**
 * Class to store/transfer the results of a anomaly detection test
 * @param config Configuration parameters of the flow
 * @param trainAvMQE Average MQE obtained after training
 * @param trainAvSD Standard of deviation of the training average MQE
 * @param avTruePos Amount of anomalies correctly detected
 * @param avFalseNeg Amount of anomalies missed
 * @param avTrueNeg Amount of normal instances correctly ignored
 * @param avFalsePos Amount of normal instances incorrectly marked as anomalies
 * @param confidence Confidence value of the SOM decision
 * @param sensibility How sensible the SOM is to anomalies
 * @param accuracy Overall correctness of the SOM decisions
 * @param elapsedTime Running time of the test
 */
class DetectionData (config: MapConfig, trainAvMQE: Double, trainAvSD: Double, avTruePos: Double,
                     avFalseNeg: Double, avTrueNeg: Double, avFalsePos: Double, confidence: Double, sensibility: Double,
                     accuracy: Double, elapsedTime: String)
                    extends ExperimentData (config, trainAvMQE, trainAvSD, elapsedTime) {
  private val totAnom = avTruePos + avFalseNeg
  private val totNorm = avTrueNeg + avFalsePos

  /**
   * Provides a string with the field names for new export files
   */
  override def attributes: String = super.attributes + "," + "Total anomalies,Total normal,Av true positives," +
                                                             "Av false negatives,Av true negatives," + "Av false " +
                                                             "positives,Av confidence,Av sensibility,Av accuracy"


  /**
   * Provides a string with current instance values to transfer
   * Coma separated string with the result values in the attributes order
   */
  override def data: String = super.data + "," + List(totAnom, totNorm, avTruePos, avFalseNeg, avTrueNeg, avFalsePos,
                                                      confidence, sensibility, accuracy).mkString(",")
}
