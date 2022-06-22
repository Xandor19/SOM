package som

class ExperimentData ( dataset: String,  datasetProp: Double,  latDistrib: Int,  width: Int,  height: Int,
                       neighRadius: Int,  learnFactor: Double,  tuneFactor: Double,  initFn: String,
                       distanceFn: String,  neighFn: String,  neighDecreaseFn: String,  decFactor: Double, 
                       trainIter: Int,  tuneIter: Int,  tolerance: Double,  runs: Int,  initSeed: Long,
                       shuffleSeed: Long,  trainAvMQE: Double,  trainAvSD: Double,  avCorrect: Double,
                       avIncorrect: Double,  avPrecision: Double,  testAvMQE: Double,  testAvSD: Double) {
  
   val attributes: String = "Dataset,Dataset prop,Lattice distribution,Lattice width,Lattice height,Neighborhood " +
     "radius,Learning factor,Tuning factor,Initialization,Distance,Neighborhood function,Neighborhood decrease," +
     "Radius Factor,Training iters,Tuning iters,Tolerance,Models created,Initialization seed,Set shuffle seed,Av " +
     "MQE after training,MQE SD after training,Av success,Av failure,Av Precision,Av MQE after test,MQE SD after test"
  
   val data: String = List(dataset, datasetProp, latDistrib, width, height, neighRadius, learnFactor, tuneFactor,
                           initFn, distanceFn, neighFn, neighDecreaseFn, decFactor, trainIter, tuneIter, tolerance,
                           runs, initSeed, shuffleSeed, trainAvMQE, trainAvSD, avCorrect, avIncorrect, avPrecision,
                           testAvMQE, testAvSD).mkString(",")
}
