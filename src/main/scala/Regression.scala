import scala.annotation.tailrec

object Regression {

  def regression(dataset_file: String,
                attribute_columns: List[String],
                value_column: String,
                test_percentage: Double,
                alpha: Double,
                gradient_descent_steps: Int): (Matrix, Double) = {
    // creating the dataset from the given file
    val datasetInitial: Dataset = Dataset.apply(dataset_file)

    // splitting the dataset into training + testing
    val (trainingSetAll, testingSetAll) = datasetInitial.split(test_percentage)
    
    // select the target columns for both datasets
    val trainingSet = trainingSetAll.selectColumns(attribute_columns)
    val testingSet = testingSetAll.selectColumns(attribute_columns)

    // X - input matrix
    val X: Matrix = Matrix(trainingSet).++(1.0)

    // tailrec function to create W
    @tailrec
    def createW(length: Int, acc: List[Double]): List[Double] = {
      if (length == 0) acc
      else createW(length - 1, 0.0 :: acc)
    }

    // gets the width of X
    val widthX: Int = X.width match {
      case None => 0
      case Some(width) => width
    }

    // creates W, initialised with 0 (W of type Matrix)
    val WHelper: List[Double] = createW(widthX, Nil)
    val W: Matrix = new Matrix(Some(List(WHelper))).transpose

    // Y contains the real values from the value_column column (training part)
    val YHelper: Dataset = trainingSetAll.selectColumn(value_column)
    val Y: Matrix = Matrix.apply(YHelper)

    // Z contains the real values from the value_column column (testing part)
    val ZHelper: Dataset = testingSetAll.selectColumn(value_column)
    val Z: Matrix = Matrix.apply(ZHelper)

    // Gradient Descent method
    @tailrec
    def gradientDescent(gradient_descent_steps: Int, W: Matrix): Matrix = {
      // stop condition
      if (gradient_descent_steps == 0) return W

      // calculates estimates
      val estimate: Matrix = X * W

      // calculates the error
      val error: Matrix = estimate - Y

      // gets the height of the error
      val m: Int = error.height match {
        case None => -1
        case Some(m2) => m2
      }

      // calculates the rated gradient
      val gradient: Matrix = (X.transpose * error).map(_ / m)
      val ratedGradient: Matrix = gradient.map(_ * alpha)

      // recursive call
      gradientDescent(gradient_descent_steps - 1, W - ratedGradient)
    }

    // form final W
    val finalW: Matrix = gradientDescent(gradient_descent_steps, W)

    // calculate the predictions for testing set
    val testingMatrix: Matrix = Matrix(testingSet).++(1)
    val testingPred: Matrix = testingMatrix * finalW

    // calculate the error
    val error = (testingPred - Z).map(e => math.abs(e))

    // gets the height of the error
    val noOfEntries = error.height match {
      case None => 0
      case Some(x) => x
    }

    // gets the values
    val errValues = error.data match {
      case None => List[List[Double]]()
      case Some(mat) => mat
    }

    // calculate the sum of the errors
    val sum = errValues.foldRight(0.0)((list, acc) => list.head + acc)

    // returns finalW and the average of the sum
    (finalW, sum / noOfEntries)
  }

  def main(args: Array[String]): Unit = {
    // Exemplu de utilizare
    print(regression("datasets/houseds.csv", List("GrLivArea", "YearBuilt"), "SalePrice", 0.1, 1e-7, 10000))
  }
}