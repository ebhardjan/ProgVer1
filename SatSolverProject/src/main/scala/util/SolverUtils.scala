package util

/**
  * Created by Severin on 2017-03-07.
  */
object SolverUtils {
  /**
    * Evaluate the given clause under the model.
    *
    * @param clause The clause to be evaluated.
    * @param model The model for the clause.
    * @return The truth value of the clause under the given model.
    */
  def evaluateClause(clause: InternalClause, model: Map[String, Boolean]): Boolean = {
    clause.disjuncts.foldLeft[Boolean](false)((b, disjunct: InternalDisjunct) => b || {
      model getOrElse(disjunct.literal.name, None) match {
        case None => false
        case value => disjunct.literal.polarity == value
      }
    })
  }
}
