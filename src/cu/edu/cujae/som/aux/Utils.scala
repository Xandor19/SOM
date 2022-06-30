package cu.edu.cujae.som.aux

import cu.edu.cujae.som.data.InputVector

import scala.util.Random

object Utils {
  /**
   * Provides the different classes of a dataset with their number
   * of occurrences
   * @param inputs Set of input vectors to analyze
   * @return Map of String and Int with each class and is respective
   *         number of instances
   */
  def classCount (inputs: Iterable[InputVector]): Map[String, Int] = {
    inputs.map(x => x.classification).foldLeft(Map.empty[String, Int]) {
      (count, word) => count + (word -> (count.getOrElse(word, 0) + 1))
    }
  }


  /**
   * Provides a proportional stratified random sampling from a given population
   * (set of input vectors)
   * @param population Whole dataset
   * @param prop Sample proportion to obtain from the dataset
   * @param shuffleSeed Optional seed for traceable shuffling
   * @return
   */
  def stratified (population: List[InputVector], prop: Double, shuffleSeed: Long = Random.nextInt):
                 (List[InputVector], List[InputVector]) = {
    // Random instance
    val rand = new Random()
    rand.setSeed(shuffleSeed)
    // Sizes of received population and desired sample
    val popSize = population.size
    val sampleSize = (popSize * prop).toInt
    // Classes with their occurrences
    val classes = classCount(population).map(x => (x._1, x._2.toDouble / popSize))
    // Sample ready
    var sample = List.empty[InputVector]
    var rest = List.empty[InputVector]
    // Shuffles the input
    val shuffled = rand.shuffle(population)

    // Adds the corresponding amount of inputs of each class to the sample
    classes.foreach(x => {
      // Amount calculation
      val top = /*math.ceil*/(sampleSize * x._2).toInt
      // Obtaining only the current class
      val ofClass = shuffled.filter(i => i.classification == x._1)

      // Sample updating
      sample = sample.appendedAll(ofClass.slice(0, top))
      // Collecting remaining instances
      rest = rest.appendedAll(ofClass.slice(top, ofClass.size))
    })
    // Shuffles remainder
    rest = rand.shuffle(rest)

    (sample, rest)
  }


  /**
   * Splits a set of input vectors in the different classes that composes it
   * @param dataset Elements of the dataset to split
   * @return List of lists of vectors of each class
   */
  def splitByClasses (dataset: List[InputVector]): List[List[InputVector]] = {
    var distributed = List.empty[List[InputVector]]

    // Maps dataset by its classes
    dataset.map(x => x.classification).distinct.foreach(x => {
      // Appends all instances of current class
      distributed = distributed.appended(dataset.filter(v => v.classification == x))
    })
    distributed
  }
}
