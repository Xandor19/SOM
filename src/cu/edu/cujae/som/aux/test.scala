package cu.edu.cujae.som.aux

import net.maritimecloud.internal.core.javax.json.Json

object test {
  def main(args: Array[String]): Unit = {
    print(mainClass.toString())
  }

  def mainClass: (Int, Int) = {
    var representedClass = (0, 0)
    if (representedClass == (0, 0)) {
      representedClass = (3, 1)
    }
    else representedClass = (4, 3)
    representedClass
  }
}
