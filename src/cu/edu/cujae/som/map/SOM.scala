package cu.edu.cujae.som.map

import cu.edu.cujae.som.data.{InputVector, VectorSet}
import cu.edu.cujae.som.io.{MapConfig, MapIO}

import scala.util.Random

/**
 * Clase abstracta para representar los elementos generales de un SOM
 *
 * @param _lattice Grilla del mapa
 * @param _neighRadius Radio de vecindad inicial
 * @param _distanceFn Funcion de distancia a utilizar
 * @param _neighborhoodFn Funcion de vecindad a utilizar en el entrenamiento
 */
abstract class SOM (private val _lattice: Lattice, private val _neighRadius: Double,
                    private val _distanceFn: (Array[Double], Array[Double]) => Double,
                    private val _neighborhoodFn: (Float, Float, Float, Float, Double) => Double) {

  /*
   * Atributos de clase
   */
  private var _dimensionality: Int = 0
  private var _avgMQE: Double = 0
  private var _sdMQE: Double = 0


  /**
   * Establece el estado inicial del SOM a partir de un espacio de entrada
   *
   * @param trainingSet Espacio de entrada que se utilizara en el entrenamiento
   * @param initFn Funcion de inicializacion de los vectores de peso del SOM
   * @param seed Semilla para la inicializacion aleatoria
   */
  def initSOM (trainingSet: VectorSet, initFn: (Iterable[Array[Double]], VectorSet, Long) => Unit,
              seed: Long = Random.nextInt()): Unit = {
    // Fija la dimensionalidad del SOM
    _dimensionality = trainingSet.dimensionality

    // Crea e inicializa los vectores de pesos para las neuronas
    val initVectors = List.fill(_lattice.width * _lattice.height) {
      new Array[Double](trainingSet.dimensionality)
    }
    initFn(initVectors, trainingSet, seed)

    // Construye la grilla con los vectores de pesos obtenidos
    _lattice.constructLattice(initVectors)
  }


  /**
   * Establece el estado de un SOM a partir de la configuracion de
   * un modelo previamente entrenado
   *
   * @param parameters Configuracion del modelo importado
   */
  def importSOM (parameters: MapIO): Unit = {
    // Carga el estado de la grilla
    _lattice.loadLattice(parameters)
    // Establece las metricas de error de la red
    avMQE = parameters.avMQE
    sdMQE = parameters.sdMQE
  }


  /**
   * Proceso de auto-organizacion
   *
   * @param vectorSet Conjunto de vectores de entrada a emplear en el entrenamiento
   * @param mapConfig Parametros de configuracion del entrenamiento
   */
  def organizeMap (vectorSet: VectorSet, mapConfig: MapConfig): Unit


  /**
   * Completa el estado de la red tras el entrenamiento
   */
  def somReady (): Unit = {
    lattice.neurons.flatten.foreach(x => {
      x.updateHits()
      x.updateBalance()
      x.updateMainClass()
    })
    updateAvMQE()
    updateSdMQE()
  }


  /**
   * Obtiene la neurona mas representativa (BMU) de un vector de entrada,
   * aquella neurona cuyo error de cuantificacion (QE), digase distancia,
   * es el minimo (MQE)
   *
   * @param input Vector de entrada
   * @return Tupla (BMU, MQE) para el vector
   */
  def findBMU (input: Array[Double]): (Neuron, Double) = {
    _lattice.neurons.flatten.map(x => (x, _distanceFn(input, x.weightVector))).minBy(x => x._2)
  }


  /**
   * Asigna el vector de entrada recibido a su BMU
   *
   * @param inputVector Vector de entrada para agrupar en el mapa
   * @return BMU obtenida para el vector
   */
  def clusterInput (inputVector: InputVector): Neuron = {
    // Find the BMU and cluster the input in it
    val bmu = findBMU(inputVector.vector)
    bmu._1.representInput(inputVector, bmu._2)

    bmu._1
  }


  /**
   * Reduce el radio de vecindad de manera lineal, inversa del
   * tiempo (iteraciones)
   *
   * @param iter Iteracion actual
   * @param totIters Total de iteraciones a realizar
   * @return Valor del radio para el instante actual
   */
  def updateRadius (iter: Int, totIters: Float): Double = {
    _neighRadius * (1 - iter / totIters)
  }


  /**
   * Actualiza el error medio del mapa obteniendo la media de
   * error con que cada BMU representa a sus entradas
   */
  def updateAvMQE (): Unit = {
    _avgMQE = _lattice.neurons.flatten.flatMap(x => x.representedInputs.values).sum /
      _lattice.neurons.flatten.map(z => z.representedInputs.size).sum
  }


  /**
   * Actualiza la desviacion estandar del error medio mapa
   */
  def updateSdMQE(): Unit = {
    _sdMQE = math.sqrt(_lattice.neurons.flatten.filter(n => n.representedInputs.nonEmpty).
                       flatMap(x => x.representedInputs.map(y => math.pow(y._2 - _avgMQE, 2))).sum /
                       (_lattice.neurons.flatten.map(z => z.representedInputs.size).sum - 1))
  }


  /**
   * Obtiene el umbral de normalidad (valor de error) a partir
   * del cual un registro es considerado anomalo
   *
   * Este umbral se considera como el valor de error 3 veces
   * mas alla de la desviacion estandar
   *
   * @return Valor del umbral
   */
  def normalityThreshold: Double = {
    _avgMQE + 3 * _sdMQE
  }


  /**
   * Proporciona la U-Matrix del mapa, grilla en la que se
   * representa la distancia media del vector de pesos de
   * cada neurona a los de sus vecinas
   * @return Array bi-dimensional con las medias de distancia
   */
  def uMatrix: Array[Array[Double]] = {
    _lattice.neurons.map(x => x.map(neuron => {
      neuron.neighbors.map(neigh => _distanceFn(neuron.weightVector, neigh.weightVector)).sum / neuron.neighbors.size
    }))
  }


  /*
   * Gets y Sets
   */

  def lattice: Lattice = _lattice


  def dimensionality: Int = _dimensionality
  def dimensionality_=: (dim: Int): Unit = _dimensionality = dim


  def avMQE: Double = _avgMQE
  def avMQE_= (av: Double): Unit = _avgMQE = av


  def sdMQE: Double = _sdMQE
  def sdMQE_= (sd: Double): Unit = _sdMQE = sd
}


/**
 * Objeto para la creacion de un SOM segun la configuracion especificada
 */
object SOMFactory {
  /**
   * Crea el tipo de SOM (por enfoque de entrenamiento) especificado
   * en los parametros de configuracion
   *
   * @param config Parametros de configuracion para crear un SOM
   * @return SOM creado
   */
  def createSOM (config: MapConfig): SOM = {
    // Obtiene las funciones a emplear
    val distFn = FunctionCollector.distanceFactory(config.distanceFn)
    val neighFn = FunctionCollector.neighboringFactory(config.neighFn)

    if (config.somType == SOMType.onlineSOM) {
      // SOM de entrenamiento on-line
      new OnlineSOM(LatticeFactory.createLattice(config.latDistrib, config.width, config.height), config.learnFactor,
                    config.tuneFactor, config.neighRadius, distFn, neighFn)
    }
    else {
      // SOM de entrenamiento en batch
      new BatchSOM(LatticeFactory.createLattice(config.latDistrib, config.width, config.height), config.neighRadius,
                   distFn, neighFn)
    }
  }


  /**
   * Crea un SOM a partir de los datos de un modelo pre-entrenado recibidos
   *
   * @param parameters Configuracion del modelo importado
   * @return SOM creado
   */
  def importSOM (parameters: MapIO): SOM = {
    // Obtiene las funciones a emplear
    val distFn = FunctionCollector.distanceFactory(parameters.distFn)
    // Crea el SOM
    val som = new BatchSOM(LatticeFactory.createLattice(parameters.latDistrib, parameters.width, parameters.height),
                           0, distFn, null)
    // Importa la configuracion de entrenamiento en el SOM
    som.importSOM(parameters)
    som
  }
}


/**
 * Identificadores para los tipos de SOM segun enfoque de entrenamiento
 */
object SOMType {
  val onlineSOM = "On-line"
  val batchSOM = "Batch"
}
