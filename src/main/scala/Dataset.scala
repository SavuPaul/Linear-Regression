import scala.annotation.tailrec
import scala.collection.immutable
import scala.io.Source

class Dataset(m: List[List[String]]) {
  val data: List[List[String]] = m

  // toString method
  override def toString: String = {
    // mkString creates lists where the elements are split using ","
    // and then foldLeft is used to append new lines and create a single list
    data.map(lines => lines.mkString(","))
      .foldLeft("")((acc, str) => acc + str + "\n")
  }

  // SELECTS A COLUMN based on a given string input
  def selectColumn(col: String): Dataset = {
    // Transpose and filter the row (column originally)
    // after the row "col" and transpose back
    val column: List[List[String]]
    = data.transpose.filter(row => row.head == col).transpose

    new Dataset(column)
  }

  // SELECTS MULTIPLE COLUMNS based on a given list of strings
  def selectColumns(cols: List[String]): Dataset = {
    // creates individual datasets for each column (List[Dataset] = List[List[List[String])
    val datasets: List[Dataset] = cols.map(col => selectColumn(col))

    /*
     * Flatten the List[Dataset] into a List[List[String]] = Dataset
     * It needs to be transposed so that it's returned as columns and not as rows
    */
    val newDataset: List[List[String]] = datasets.map(set => set.data.flatten).transpose

    // Create a new Dataset with the concatenated columns
    new Dataset(newDataset)
  }

  // SPLIT FUNCTION (splits a dataset into 2 datasets: training + testing)
  def split(percentage: Double): (Dataset, Dataset) = {
    // creates an ascending list of consecutive integers up to a given length
    @tailrec
    def createIndexes(length: Int, acc: List[Int]): List[Int] = {
      if (length == 0) acc
      else createIndexes(length - 1, length :: acc)
    }

    // sort by first column, ignoring the headers
    val dataSort: List[List[String]] = data.tail.sortBy(row => row.head)

    // percentage splitting formula
    val k: Int = (math.ceil(1 / percentage)).toInt

    // creates the list of indexes
    val indexes: List[Int] = createIndexes(dataSort.length, Nil)

    // zip the dataset and indexes so that each row has an index
    val indexedData = dataSort.zip(indexes)

    // filter the rows so that the two sub-datasets are obtained
    val trainingList: List[List[String]] = indexedData.filter(p => p._2 % k != 0).map(_._1)
    val testingList: List[List[String]] = indexedData.filter(p => p._2 % k == 0).map(_._1)

    // add the original headers to the 2 sub-datasets
    val trainingSet: Dataset = new Dataset(data.head :: trainingList)
    val testingSet: Dataset = new Dataset(data.head :: testingList)

    // return the training dataset and the testing dataset
    (trainingSet, testingSet)
  }

  // SIZE of a dataset
  def size: Int = data.length

  // returns the rows of the dataset
  def getRows: List[List[String]] = {
    if (data.isEmpty) Nil
    else data
  }

  // returns the header of the dataset
  def getHeader: List[String] = {
    if (data.isEmpty) Nil
    else data.head
  }
}

object Dataset {
  def apply(csv_filename: String): Dataset = {
    val source = Source.fromFile(csv_filename)
    val data: List[List[String]] = source.getLines.toList
      .map(lines => lines.split(',').toList)
    new Dataset(data)
  }
  def apply(ds: List[List[String]]): Dataset = new Dataset(ds)
}
