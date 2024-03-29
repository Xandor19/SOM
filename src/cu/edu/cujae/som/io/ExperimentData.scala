package cu.edu.cujae.som.io

/**
 * Clase para almacenar y transferir los resultados generales de una prueba
 *
 * @param config Parametros de configuracion empleados en el flujo
 * @param trainAvMQE Error de Cuantificacion Minimo medio obtenido tras el entrenamiento
 * @param trainAvSD Desviacion estandar del Error de Cuantificacion Minimo
 * @param elapsedTime Tiempo de ejecucion de la prueba
 */
abstract class ExperimentData (config: MapConfig, trainAvMQE: Double, trainAvSD: Double, elapsedTime: String) {

   /**
    * Proporciona una string con los nombres de los datos contenidos en la clase
    */
   def attributes: String = (if (config != null) config.attributes + "," else "") +
                            "Elapsed time,Av MQE after training,MQE SD after training"


  /**
   * Proporciona una string con los valores de resultados almacenados actualmente
   */
   def data: String = (if (config != null) config.parameters + "," else "") +
                      List(elapsedTime, trainAvMQE, trainAvSD).mkString(",")
}
