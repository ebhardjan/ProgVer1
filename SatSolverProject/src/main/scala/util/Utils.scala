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

}

