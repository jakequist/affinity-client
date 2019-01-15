package jakequist.affinity.util

import java.io.{ByteArrayInputStream, InputStream, StringBufferInputStream}
import java.math.BigInteger
import java.net.{URI, URLDecoder, URLEncoder}

import java.nio.charset.Charset
import java.sql.Date
import java.util.Base64
import java.util.regex.Pattern
import java.util.Locale.ENGLISH

import com.google.common.escape.Escapers
import com.google.common.hash.Hashing
import com.google.common.net.InternetDomainName
import org.apache.commons.lang3.StringUtils

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

trait StringImplicits {

  implicit class StringContextRegexImp(sc: StringContext) {
    def r = {
      val regex = new util.matching.Regex(
        sc.parts.mkString
      )
      regex
    }
  }

  implicit class RegexImp(base: Regex) {
    def matches(that: String): Boolean = {
      base.pattern.matcher(that).matches()
    }
  }


  implicit class StringImp(base: String) {

    //    def hash: Hash = {
    //      Hash(base)
    //    }
    //
    //    def md5: String = {
    //      Hashing.md5().hashString(base, Charset.defaultCharset()).toString
    //    }
    //
    //    def jaroWinklerDistance(that: String): Double = {
    //      StringMetric.compareWithJaroWinkler(base, that).getOrElse(0.0)
    //    }

    private[this] val acronymRegExpStr = "[A-Z]{2,}"
    private[this] val acronymRegExp = acronymRegExpStr.r
    private[this] val endsWithAcronymRegExpStr = "[A-Z]{2,}$"
    private[this] val singleUpperCaseRegExp = """[A-Z]""".r

    @transient private lazy val hasherMd5 = Hashing.md5()
    @transient private lazy val hasherMurmur32 = Hashing.murmur3_32()


    def toURI: URI = {
      URI.create(base)
    }

    def toDomain : InternetDomainName = {
      if (base.contains("http://") || base.contains("https://")) {
        InternetDomainName.from(base.toURI.getHost)
      } else {
        InternetDomainName.from(base)
      }
    }


    def isHex = {
      toLongHexOpt.isDefined
    }

    def toLongHexOpt : Option[Long] = {
      Try( (new BigInteger(base, 16)).longValue() ).toOption
    }

    def toLongHex : Long = {
      (new BigInteger(base, 16)).longValue()
    }

    def toSnakeCase: String = {
      // first, applies acronyms filter
      val acronymsFiltered = acronymRegExp.replaceAllIn(
        acronymRegExp
          .findFirstMatchIn(base)
          .map { m =>
            base.replaceFirst(endsWithAcronymRegExpStr, "_" + m.matched.toLowerCase(ENGLISH))
          }
          .getOrElse(base), // might end with an acronym
        { m =>
          "_" + m.matched.init.toLowerCase(ENGLISH) + "_" + m.matched.last.toString.toLowerCase(ENGLISH)
        }
      )
      // second, convert single upper case char to '_' + c.toLower
      val result = singleUpperCaseRegExp
        .replaceAllIn(acronymsFiltered, { m =>
          "_" + m.matched.toLowerCase(ENGLISH)
        })
        .replaceFirst("^_", "")
        .replaceFirst("_$", "")

      if (base.startsWith("_")) "_" + result
      else if (base.endsWith("_")) result + "_"
      else result
    }


    def toCamelCase = {
      Option(base)
        .map { s =>
          val result = s.toUpperCamelCase
          if (result.headOption.exists(c => c.isUpper)) result.head.toLower + result.tail
          else result
        }
        .orNull[String]
    }

    def toUpperCamelCase = {
      Option(base)
        .map { s =>
          val result = s
            .foldLeft((ListBuffer[Char](), false)) {
              case ((cs, prevIs_), c) =>
                if (c == '_') (cs, true)
                else if (prevIs_) (cs += c.toUpper, false)
                else (cs += c, false)
            }
            ._1
            .mkString
          if (result.headOption.exists(c => c.isLower)) result.head.toUpper + result.tail
          else result
        }
        .orNull[String]
    }


    def hashMD5 = {
      hasherMd5.hashString(base, Charset.defaultCharset)
    }

    def hashMurmer32 = {
      hasherMurmur32.hashString(base, Charset.defaultCharset)
    }

    def decodeUriComponent: String = {
      URLDecoder.decode(base, "UTF-8");
    }

    def encodeUriComponent: String = {
      URLEncoder.encode(base, "UTF-8")
        .replaceAll("\\+", "%20")
        .replaceAll("\\%21", "!")
        .replaceAll("\\%27", "'")
        .replaceAll("\\%28", "(")
        .replaceAll("\\%29", ")")
        .replaceAll("\\%7E", "~")
    }

    def decodeBase64: Array[Byte] = {
      Base64.getDecoder.decode(base)
    }

//    def encodeGzip: Array[Byte] = {
//      import commons.io.IOImplicits._
//      import commons.core.ByteArrayImplicits._
//      base.getBytes.encodeGzip
//    }

    def isAlphaNumeric : Boolean = {
      base.forall(_.isLetterOrDigit)
    }

    def toInputStream : InputStream = {
      new ByteArrayInputStream(base.getBytes)
    }

    def matchesGlob(glob: String): Boolean = {
      base.matches(Regex.quoteReplacement(glob).replaceAll("\\*", ".*"))
    }

    def toBool: Boolean = {
      toBoolOpt.get
    }

    def toBoolOpt: Option[Boolean] = {
      base.toLowerCase match {
        case "true" => Some(true)
        case "t" => Some(true)
        case "yes" => Some(true)
        case "false" => Some(false)
        case "f" => Some(false)
        case "no" => Some(false)
        case "0" => Some(false)
        case _ => None
      }
    }


    @transient lazy val SHELL_ESCAPER = {
      val builder = Escapers.builder();
      builder.addEscape(' ', "\\ ")
      builder.addEscape('\'', "\\\'")
      builder.addEscape('\"', "\\\"")
      builder.addEscape('$', "\\$")
      builder.addEscape('^', "\\^")
      builder.build()
    }

    def escapeShell:String = {
      SHELL_ESCAPER.escape(base)
    }

    def escapeRegex: String = {
      Pattern.quote(base)
    }

    def escapeDoubleQuotes: String = {
      base.replaceAllLiterally("\"", "\\\"")
    }

    def escapeSingleQuotes: String = {
      base.replaceAllLiterally("'", "\\\'")
    }

    def encodeUrlComponent: String = {
      java.net.URLEncoder.encode(base, "UTF-8")
    }

    def assertUrlComponentSafe: String = {
      base.matches("(\\w)*") match {
        case false =>
          throw new IllegalArgumentException(s"String is not url-component safe: ${base}")
        case _ =>
          base
      }
    }

    def newline: String = {
      base + "\n"
    }

    def emptyOption: Option[String] = {
      if (base.nonEmpty) Some(base)
      else None
    }

    def emptyNull: String = {
      if (base.nonEmpty) base
      else null
    }

    def containsIgnoreCase(s: CharSequence): Boolean = {
      base.toLowerCase.contains(s.toString.toLowerCase())
    }

    def containsAnyIgnoreCase(first:String, rest:String*): Boolean = {
      containsAnyIgnoreCase(first +: rest)
    }

    def containsAnyIgnoreCase(str: Iterable[String]): Boolean = {
      str.exists(v => base.toLowerCase().contains(v.toLowerCase()))
    }

    def equalsAnyIgnoreCase(str: Iterable[String]): Boolean = {
      str.exists(v => base.toLowerCase() equals v.toLowerCase())
    }


    def containsAnyRegex(reg: Seq[Regex]): Boolean = {
      reg.exists(v => v.findFirstIn(base).isDefined)
    }


    def containsRegex(regex: Regex): Boolean = {
      regex.findFirstIn(base).nonEmpty
    }

    def containsAny(other: Iterable[String]): Boolean = {
      other.exists(s => {
        base.contains(s)
      })
    }

    def equalsAny(those: Iterable[String]): Boolean = {
      those.exists(_ equals base)
    }

    def equalsAny(head: String, others: String*): Boolean = {
      equalsAny(Seq(head) ++ others.toSeq)
    }

    def in(others: Seq[String]): Boolean = {
      others.exists(v => {
        v equals base
      })
    }

    def in(head: String, others: String*): Boolean = {
      in(Seq(head) ++ others.toSeq)
    }

    def underscore: String = {
      base.replaceAll("\\W", "_")
    }

    def padRight(n: Int, c: Char = ' '): String = {
      StringUtils.rightPad(base, n, c)
    }

    def padLeft(n: Int, c: Char = ' '): String = {
      StringUtils.leftPad(base, n, c)
    }

    def singleline: String = {
      base.replaceAll("[\n\r]", " ")
    }

    def remove(str: String): String = {
      base.replace(str, "")
    }

    def removeAll(str: String): String = {
      base.replaceAll(Regex.quote(str), "")
    }

    def removeLeading(str: String): String = {
      val r = new Regex("^" + Pattern.quote(str))
      val ret = r.replaceFirstIn(base, "")
      ret
    }

    def removeTrailing(str: String): String = {
      val r = new Regex(Pattern.quote(str) + "$")
      r.replaceAllIn(base, "")
    }

    def removeLeadingTrailing(head: String, tail: String): String = {
      removeLeading(head).removeTrailing(tail)
    }


    def truncateMiddle(max: Int): String = {
      base match {
        case v if v.length > max => "\u2026" + v.take(max) + "\u2026"
        case v => v
      }
    }

    def truncateLeft(max: Int): String = {
      base match {
        case v if v.length > max => v.take(max) + "\u2026"
        case v => v
      }
    }

    def truncate = truncateLeft(20)

    def truncate(max: Int) = truncateLeft(max)

    def extract(r: Regex): Option[String] = extractAny(r)

    def extractAny(regs: Regex*): Option[String] = {
      for (r <- regs) {
        r.findFirstMatchIn(base) match {
          case Some(m) =>
            if (m.groupCount == 0) throw new IllegalArgumentException("regex does not contain group")
            return Some(m.group(1))
          case _ =>
        }
      }
      None
    }

    def blockSplit(width: Int): String = {
      base
        .grouped(width)
        .mkString("\n")
    }

    def blockIndent(num: Int): String = {
      base.split("\n").map(v => (" " * num) + v).mkString("\n")
    }

    def blockIndent: String = blockIndent(2)

    def blockPrefix(str: String): String = {
      base.split("\n").map(v => str + v).mkString("\n")
    }

    def blockIndentWithPrefix(lhs: Int, pfx: String, rhs: Int = 1): String = {
      base
        .blockIndent(rhs)
        .blockPrefix(pfx)
        .blockIndent(lhs)
    }

    def blockIndentWithPrefix: String = blockIndentWithPrefix(2, "|")

    def blockMap(fn: (String) => String): String = {
      base.split("\n").map(v => fn(v)).mkString("\n")
    }

    def blockTrim: String = {
      blockMap(_.trim)
    }

    def isBlank: Boolean = {
      base.trim.isEmpty
    }

    def nonBlank: Boolean = {
      !isBlank
    }

//    def jaroWinklerDistance(that: String): Double = {
//      StringMetric.compareWithJaroWinkler(base, that).getOrElse(0.0)
//    }

//
//    def distanceAbove(that: String, theshold: Double): Boolean = {
//      distanceAboveOpt(that, theshold).isDefined
//    }
//
//    def distanceAboveOpt(that: String, threshold: Double): Option[Double] = {
//
//      if (base == null || base.isEmpty) return None
//      if (that == null || that.isEmpty) return None
//      val baseLC = base.toLowerCase()
//      val thatLC = that.toLowerCase()
//
//      // Performance hack:
//      // JaroWinkler is the difference between two strings, accounting for 'fat finger' entries.
//      // We take a shortcut here and say: "if the two strings have more than N delta in stringlength,
//      // then obviously they will also fail at JaroWrinkler"
//      val delta = Math.min(baseLC.length, thatLC.length).toDouble / Math.max(baseLC.length, thatLC.length).toDouble
//      if (delta > threshold) {
//        // Perform JaroWrinker
//        val s = baseLC.distanceTo(thatLC)
//        if (s > threshold) {
//          return Some(s)
//        } else {
//          return None
//        }
//      } else {
//        // Strings are of different length, so will obviously fail at JaroWrinker
//        return None
//      }
//
//    }

//    def distanceTo(that: String): Double = jaroWinklerDistance(that)

    def isLong: Boolean = {
      Try(base.toLong) match {
        case Success(_) => true
        case Failure(_) => false
      }
    }

    def toIntOpt: Option[Int] = {
      Try(base.toInt).toOption
    }

    def toDoubleOpt: Option[Double] = {
      Try(base.toDouble).toOption
    }

//    def toDateOpt: Option[Date] = {
//      Try(new Date(Clock.parse(base).getMillis)).toOption
//    }
//
//    def toDate: Date = toDateOpt.get


    /**
      * Turns a string of format "foo_bar" into camel case "FooBar"
      *
      * Functional code courtesy of Jamie Webb (j@jmawebb.cjb.net) 2006/11/28
      *
      * @return the CamelCased string
      */
    def camelCase: String = {
      def loop(x: List[Char]): List[Char] = (x: @unchecked) match {
        case '_' :: '_' :: rest => loop('_' :: rest)
        case '_' :: c :: rest => Character.toUpperCase(c) :: loop(rest)
        case '_' :: Nil => Nil
        case c :: rest => c :: loop(rest)
        case Nil => Nil
      }

      if (base == null)
        ""
      else
        loop('_' :: base.toList).mkString
    }

    /**
      * Turn a string of format "foo_bar" into camel case with the first letter in lower case: "fooBar"
      * This function is especially used to camelCase method names.
      *
      * @return the CamelCased string
      */
    def camelCaseTrailing: String = {
      val tmp: String = base.camelCase
      if (tmp.length == 0)
        ""
      else
        tmp.substring(0, 1).toLowerCase + tmp.substring(1)
    }


  }


  implicit class StringOptionImp(base: Option[String]) {

    def trimOption: Option[String] = {
      base.map(_.trim.emptyNull)
    }

    def trim: Option[String] = trimOption

    def emptyNone: Option[String] = {
      base match {
        case Some(s) if s == null => None
        case Some(s) if s.trim.isEmpty => None
        case Some(s) => Some(s)
        case None => None
      }
    }

    def contains(s: String): Boolean = {
      base match {
        case Some(ss) if ss equals s => true
        case _ => false
      }
    }

    def orEmpty: String = {
      base match {
        case Some(v) => v
        case _ => ""
      }
    }

    def isBlank: Boolean = {
      base.map(_.isBlank).getOrElse(true)
    }

    def blankNone: Option[String] = {
      base match {
        case Some(s) if s.isBlank => None
        case v => v
      }
    }

    def nonBlank: Boolean = {
      !isBlank
    }

  }


  implicit class IterableStringImp(base: Iterable[String]) {

    def containsAny(that: Iterable[String]): Boolean = {
      base.foreach(a => {
        that.foreach(b => {
          if (a equals b) return true
        })
      })
      return false
    }

    def containsAnyIgnoreCase(that: Iterable[String]): Boolean = {
      //import IterableImplicits._
      base.map(_.toLowerCase).containsAny(that.map(_.toLowerCase))
    }

    def containsAllIgnoreCase(that: Iterable[String]): Boolean = {
      that.map(_.toLowerCase).toSet.diff(base.map(_.toLowerCase).toSet).isEmpty
    }

    def containsIgnoreCase(that: String): Boolean = {
      base.exists(v => v.equalsIgnoreCase(that))
    }

//    def containsAnyDistanceAbove(that: Iterable[String], score: Double): Boolean = {
//      //import IterableImplicits._
//      base.crossExists(that)(StringImp(_).distanceAbove(_, score))
//    }

  }


}


object StringImplicits extends StringImplicits {

}