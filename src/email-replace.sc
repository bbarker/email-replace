import $ivy.`com.lihaoyi::ammonite-ops:1.1.2`
import ammonite.ops._

import scala.annotation.tailrec
import scala.collection.breakOut



val rand = new scala.util.Random()

// Ref: https://stackoverflow.com/q/2049502/3096687
val localChars: Array[Char] = (
  ('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9') ++
  "!#$%&'*+-/=?^_`{|}~".toCharArray
).toArray
//
val domainChars: Array[Char] = (
  ('a' to 'z') ++ ('0' to '9') ++ Seq('-')
).toArray

// Note: as can be seen from the examples included in test and
// likely at https://en.wikipedia.org/wiki/Email_address#Examples
// , the following is not a perfect matcher

//Ref: https://stackoverflow.com/a/32445372/3096687
val emailRegex =
  """
    |([a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+)@([a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}
    |[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?))*
  """.stripMargin.replaceAll("\n", "").trim.r

def isValidEmail(em: String): Boolean = em match {
  case null                                             => false
  case em if em.trim.isEmpty                            => false
  case em if emailRegex.findFirstMatchIn(em).isDefined  => true
  case _                                                => false
}

@main
def main(SEED_IN: Int, fileStrings: String*): Unit = {

  val seedMsg =
    """
      | !!! !!! !!! !!! !!!    Important   !!! !!! !!! !!! !!!
      | !!! Change and record the random seed privately !!!
    """.stripMargin


  println(seedMsg)


  @tailrec
  def randomStringTailRecursive(nn: Int, charPool: Array[Char], list: List[Char]): List[Char] = {
    val randIdx = Math.abs(rand.nextInt % charPool.length)
    if (nn == 1) util.Random.nextPrintableChar :: list
    else randomStringTailRecursive(nn-1, charPool, charPool(randIdx) :: list)
  }

  def randomString(nn: Int, charPool: Array[Char]): String = {
    randomStringTailRecursive(nn, charPool, Nil).mkString
  }

  /**
    *
    * @param local local part of email address
    * @param domain domain part of email address
    * @return Randomized email address; does not cover full
    *         spectrum of possible email addresses
    */
  def randEmail(local: String, domain: String): String = {
    @tailrec
    def loop(local: String, domain: String, seedIn: Int): String = {
      // Note the order of the following rand generation must be
      // preserved for reproducibility
      rand.setSeed(SEED_IN * local.hashCode - domain.hashCode)
      val d1 = Math.abs(rand.nextInt % 5)
      val d2 = Math.abs(rand.nextInt % 5)
      val local2 = randomString(local.length + d1, localChars)
      val domain2 = randomString(domain.length + d2, domainChars)
      val email = local2 + "@" + domain2
      if (isValidEmail(email)) email else loop(local, domain2, seedIn + 1)
    }

    loop(local, domain, SEED_IN)
  }

  val cwd: Path = pwd
  val files: Array[Path] = fileStrings.map(pwd / RelPath(_))(breakOut)
  files.foreach{infile =>
    println(s"*** Replacing in $infile ***")
    val fileText: String = read! infile
    val outText: String = emailRegex.replaceAllIn(fileText, md => {
      if (md.groupCount == 2)
        md.group(1) + "@" +md.group(2)
      else ""
    })
    write(infile, outText)

    /*
    => fileText).foreach{ md =>
      println(List(md.group(1), md.group(2)))
      if (md.groupCount == 3) { println(md.group(3))}
      ""
     */
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