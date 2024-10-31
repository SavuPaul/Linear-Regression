type Mat = List[List[Double]]

class Matrix(m: Option[List[List[Double]]]) {

  // TRANSPOSE method inspired from Course06
  def transpose: Matrix = {
    m match {
      case None => new Matrix(None)
      case Some(Nil) => new Matrix(Some(Nil))
      case Some(rows) =>
        val transposed = transposeHelper(rows)
        new Matrix(Some(transposed))
    }
  }
  private def transposeHelper(rows: List[List[Double]]): List[List[Double]] = {
    if (rows.exists(row => row.isEmpty)) Nil
    else rows.map(_.head) :: transposeHelper(rows.map(_.tail))
  }

  // MAP FOR MATRIX
  def map(f: Double => Double): Matrix = {
    m match {
      case None => Matrix(None)
      case Some(mat) => Matrix(mat.map(row => row.map(elem => f(elem))))
    }
  }

  // MULTIPLICATION
  def *(other: Matrix): Matrix = {
    val mat: Matrix = Matrix(m)

    // n x m matrix can only be multiplied with m x p matrix
    if (mat.width != other.height) {
      return Matrix(None)
    }

    m match {
      case None => Matrix(None)
      case Some(m1) =>
        other.data match {
          case None => Matrix(None)
          case Some(oth2) =>
            // Multiply operation is taken from Course06
            val result: List[List[Double]] = m1.map(line => oth2.transpose
              .map(col => line.zip(col).map(pair => pair._1 * pair._2).sum))
            new Matrix(Some(result))
        }
    }
  }

  // ADDING A COLUMN WITH VALUE x
  def ++(x: Double): Matrix = {
    val mat: Matrix = Matrix(m)
    mat.data match {
      case None => Matrix(None)
      case Some(mat2) =>
        val newMatrix = mat2.map(row => row :+ x)
        new Matrix(Some(newMatrix))
    }
  }

  // SUBTRACT TWO MATRIX
  def -(other: Matrix): Matrix = {
    val mat: Matrix = Matrix(m)
    if (mat.height != other.height
      || mat.width != other.width) {
      return Matrix(None)
    }

    m match {
      case None => Matrix(None)
      case Some(m2) =>
        other.data match {
          case None => Matrix(None)
          case Some(oth2) =>
            val resultData = mat.data.get.zip(other.data.get)
                            .map((row1, row2) => row1.zip(row2)
                              .map((e1, e2) => e1 - e2))
            new Matrix(Some(resultData))
        }
    }
  }

  def data: Option[Mat] = m

  // HEIGHT (number of rows)
  def height: Option[Int] = {
    m match {
      case None => None
      case Some(mat) => Some(mat.length)
    }
  }

  // WIDTH (number of columns)
  def width: Option[Int] = {
    m match {
      case None => None
      case Some(mat) => Some(mat.head.length)
    }
  }

  // toString method
  override def toString: String = {
    m match {
      case None => ""
      case Some(mat) =>
        m.map(_.mkString("\n")).mkString(" ")
    }
  }
}

object Matrix {
  def apply(data: Mat): Matrix = new Matrix(Some(data))
  def apply(data: Option[Mat]): Matrix = {
    data match {
      case None => new Matrix(None)
      case Some(d) => new Matrix(Some(d))
    }
  }
  def apply(dataset: Dataset): Matrix = {
    val rowsNoHeader = dataset.getRows.tail.map(rows => rows.map(_.toDouble))
    new Matrix(Some(rowsNoHeader))
  }
}
