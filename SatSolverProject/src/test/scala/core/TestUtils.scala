package core

import java.io.File

/**
  * Created by jan on 23.03.17.
  *
  * Testing utility methods
  */
object TestUtils {

  /**
    * Get a list of all the smt2 files in a given directory
    *
    * @param directoryPath directory where to look for smt2 files
    * @return list of paths of the smt2 files
    */
  def getListOfSmt2Files(directoryPath: String): List[String] = {
    val directory = new File(directoryPath)
    if (directory.exists && directory.isDirectory) {
      directory.listFiles
        .filter(f => f.isFile)
        .filter(f => f.getName.contains(".smt2"))
        .map(f => f.getName)
        .toList
    } else {
      List()
    }
  }

}
