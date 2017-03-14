package util

/**
  * Created by jan on 02.03.17.
  *
  * Util methods that belong to no other category of util methods...
  */
object Utils {

  /**
    * Get the name of the current method
    *
    * @return the method name as a string
    */
  def methodName(): String = {
    Thread.currentThread.getStackTrace()(2).getMethodName
  }

  /**
    * Measure and output the time it takes to execute a block in seconds.
    *
    * @param block Code to execute
    * @tparam R The return type of the code block to run.
    * @return The result of the executed code block.
    */
  def time[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block    // call-by-name
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0)/1000 + " s")
    result
  }

}

