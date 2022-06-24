package cu.edu.cujae.som.io

class ExperimentData (config: MapConfig, trainAvMQE: Double, trainAvSD: Double, avCorrect: Double,
                      avIncorrect: Double, avPrecision: Double, testAvMQE: Double, testAvSD: Double) {

   val attributes: String = config.attributes + "," + "Av MQE after training,MQE SD after training,Av success,Av " +
                                                     "failure,Av Precision,Av MQE after test,MQE SD after test"

   val data: String = config.parameters + "," + List(trainAvMQE, trainAvSD, avCorrect, avIncorrect, avPrecision,
                                                     testAvMQE, testAvSD).mkString(",")
}
