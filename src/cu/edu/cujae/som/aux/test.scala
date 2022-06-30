package cu.edu.cujae.som.aux

import java.util.Formatter

import cu.edu.cujae.som.io.ReaderWriter

object test {
  def main(args: Array[String]): Unit = {
    val dataset = ReaderWriter.loadSet("/mnt/D80C76380C76122C/Mis Programas/Repos/SOM/Datasets/creditcardfraud_normalised.csv", ',')
    val classes = Utils.splitByClasses(dataset._2).tail.head
    val reduced = Utils.stratified(dataset._2, 0.003)._1
    val newDataset = reduced.appendedAll(classes.filter(x => x.classification == "1").slice(0, 50))

    val writer = new Formatter("/home/xandor19/reduced_card_fraud_normalised.csv")

    val records = newDataset.map(x => String.format("%s,%s", x.vector.mkString(","), x.classification))

    writer.format(dataset._1.mkString(",") + "\n")
    writer.flush()

    records.foreach(x => {writer.format(x + "\n"); writer.flush()})
  }
}
