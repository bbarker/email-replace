import $ivy.`com.lihaoyi::ammonite-ops:1.1.2`
import ammonite.ops._

import scala.annotation.tailrec
import scala.collection.breakOut
import scala.util.matching.Regex


// Ref: https://stackoverflow.com/q/2049502/3096687
val localChars: Array[Char] = (
  ('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9') ++
  "!#$%&*+-/=?^_`{|}~".toCharArray // Removed ' due to complexity in output
).toArray
//
val domainChars: Array[Char] = (
  ('a' to 'z') ++ ('0' to '9') ++ Seq('-')
).toArray

// Note: as can be seen from the examples included in test and
// likely at https://en.wikipedia.org/wiki/Email_address#Examples
// , the following is not a perfect matcher

//Ref: https://stackoverflow.com/a/32445372/3096687
val emailRegexBare =
  """
    |([a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+)@([a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}
    |[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)+)*
  """.stripMargin.replaceAll("\n", "").trim

lazy val emailRegexSingles = s"""'$emailRegexBare'"""
lazy val emailRegexDoubles = s""""$emailRegexBare""""

case class RandSeed(value: Int) extends AnyVal

@main
def main(/*quoteStyle: String,*/ SEED_IN: Int, fileStrings: String*): Unit = {
  val rand = new scala.util.Random()
  val seedMsg =
    """
      | !!! !!! !!! !!! !!!    Important   !!! !!! !!! !!! !!!
      | !!! Change and record the random seed privately !!!
    """.stripMargin
  println(seedMsg)


  //TODO: not really working as desired
  /*
  val bareOpt = "-bare"
  val sqOpt   = "-squotes"
  val dqOpt   = "-dquotes"
  val emailRegex = quoteStyle match {
    case opt if opt == bareOpt => emailRegexBare.r
    case opt if opt == sqOpt => emailRegexSingles.r
    case opt if opt == dqOpt => emailRegexDoubles.r
    case _ => println(
        s"Invalid quote matching option; please choose one of:" +
        s"$bareOpt, $sqOpt, or $dqOpt"
      )
      throw new RuntimeException("Invalid Option")

  }
  */
  val emailRegex = emailRegexBare.r


  def isValidEmail(em: String): Boolean = em match {
    case null                                             => false
    case em if em.trim.isEmpty                            => false
    case em if emailRegex.findFirstMatchIn(em).isDefined  => true
    case _                                                => false
  }

  def randomString(nn: Int, charPool: Array[Char])(implicit rseed: RandSeed): String = {
    rand.setSeed(rseed.value)
    @tailrec
    def loop(nn: Int, charPool: Array[Char], list: List[Char]): List[Char] = {
      val randIdx = Math.abs(rand.nextInt % charPool.length)
      if (nn == 1) charPool(randIdx) :: list
      else loop(nn-1, charPool, charPool(randIdx) :: list)
    }
    loop(nn, charPool, Nil).mkString
  }

  /**
    *
    * @param local local part of email address
    * @param domain domain part of email address
    * @return Randomized email address; does not cover full
    *         spectrum of possible email addresses
    */
  def randEmail(local: String, domain: String): String = {
    implicit val rseed: RandSeed = RandSeed(SEED_IN * local.hashCode - domain.hashCode)
    rand.setSeed(rseed.value)
    @tailrec
    def loop(local: String, domain: String, seedIn: Int): String = {
      // Note the order of the following rand generation must be
      // preserved for reproducibility
      val d1 = Math.abs(rand.nextInt % 5)
      val d2 = Math.abs(rand.nextInt % 5)
      val local2 = randomString(local.length + d1, localChars)
      val domain2 = randomString(domain.length + d2, domainChars)
      val email = local2 + "@" + domain2.stripSuffix("-")
      if (isValidEmail(email)) email
      else {
        val nextSeed = seedIn + 1
        rand.setSeed(nextSeed)
        loop(local, domain2, nextSeed)
      }
    }
    loop(local, domain, SEED_IN)
  }

  val cwd: Path = pwd
  val files: Array[Path] = fileStrings.map(pwd / RelPath(_))(breakOut)
  files.foreach{infile =>
    println(s"*** Replacing in $infile ***")
    val fileText: String = read! infile
    val outText: String = emailRegex.replaceAllIn(fileText, md => {
      Regex.quoteReplacement(
        if (md.groupCount > 1 && md.group(2) != null && md.group(1) != null) {
          println(s"${md.group(1)} AT ${md.group(2)}")
          val tmp = randEmail(md.group(1), md.group(2))
          println(s"new email is $tmp")
          tmp // DEBUG
        }
        else {
          println(s"Warning: partial match found")
          md.group(0)
        }
      )
    })
    write.over(infile, outText)

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