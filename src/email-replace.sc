import $ivy.`com.lihaoyi::ammonite-ops:1.1.2`, ammonite.ops._

import scala.collection.breakOut

//Ref: https://stackoverflow.com/a/32445372/3096687
val emailRegex =
  """
    |^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}
    |[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$
  """.stripMargin.replaceAll("\n", "").trim.r

@main
def main(fileStrings: String*): Unit = {
  val cwd: Path = pwd
  val files: Array[Path] = fileStrings.map(pwd / RelPath(_))(breakOut)
  files.foreach{infile =>
    println(infile)
    val fileText: String = read! infile
    println(fileText)
    emailRegex.findAllMatchIn(fileText).foreach{ md =>
      println(md)
    }
    /*
    val fileTextOut = ???
      if (fileText == fileTextOut){

      }
      else{
        println()
      }
    */
  }
}