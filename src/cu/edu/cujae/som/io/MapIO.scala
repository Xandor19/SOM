package cu.edu.cujae.som.io

class MapIO (val dataset: String, val task: String, val somType: String, val latDistrib: String, val width: Int,
             val height: Int, val normalized: Boolean, val distFn: String, val avMQE: Double, val sdMQE: Double,
             val neurons: Array[(String, Int, String, String)]) {
}