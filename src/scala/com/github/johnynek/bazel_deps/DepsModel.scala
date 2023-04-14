package com.github.johnynek.bazel_deps

import java.io.{BufferedReader, ByteArrayOutputStream, File, FileInputStream, FileReader, InputStream}
import java.security.MessageDigest
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal
import org.typelevel.paiges.Doc
import cats.kernel.{CommutativeMonoid, Monoid, Semigroup}
import cats.implicits._
import cats.{Applicative, Foldable, Functor, Id, SemigroupK}
import cats.data.{Ior, NonEmptyList, Validated, ValidatedNel}

/** These should be upstreamed to paiges
  */
object DocUtil {
  def packedKV(k: String, v: Doc): Doc =
    Doc.text(k) + Doc.text(":") + Doc.lineOrSpace.nested(2) + v

  def packedDocKV(k: Doc, v: Doc): Doc =
    k + Doc.text(":") + Doc.lineOrSpace.nested(2) + v

  def kv(k: String, v: Doc, tight: Boolean = false): Doc =
    Doc.text(k) + Doc.text(":") + ((Doc.line + v).nested(2))
  def quote(s: String): String = {
    val escape = s.flatMap {
      case '\\' => "\\\\"
      case '"'  => "\\\""
      case o    => o.toString
    }
    "\"%s\"".format(escape)
  }
  def quoteDoc(s: String): Doc = Doc.text(quote(s))

  def list[T](i: Iterable[T])(show: T => Doc): Doc = {
    val parts =
      Doc.intercalate(Doc.comma, i.map { j => (Doc.line + show(j)).grouped })
    "[" +: (parts :+ " ]").nested(2)
  }
  // Here is a vertical list of docs
  def vlist(ds: Iterable[Doc]): Doc = {
    val dash = Doc.text("- ")
    Doc.intercalate(Doc.line, ds.map { d => dash + d.nested(2) })
  }

  def yamlMap(kvs: List[(String, Doc)], lines: Int = 1): Doc = {
    def rep(x: Int): Doc =
      if (x <= 0) Doc.empty
      else Doc.line + rep(x - 1)

    if (kvs.isEmpty) Doc.text("{}")
    else Doc.intercalate(rep(lines), kvs.map { case (k, v) => kv(k, v) })
  }

  def packedYamlMap(kvs: List[(String, Doc)]): Doc =
    if (kvs.isEmpty) Doc.text("{}")
    else Doc.intercalate(Doc.line, kvs.map { case (k, v) => packedKV(k, v) })

  def packedDocYamlMap(kvs: List[(Doc, Doc)]): Doc =
    if (kvs.isEmpty) Doc.text("{}")
    else Doc.intercalate(Doc.line, kvs.map { case (k, v) => packedDocKV(k, v) })
}

import DocUtil._

case class Model(
    dependencies: Dependencies,
    replacements: Option[Replacements],
    options: Option[Options]
) {

  def flatten: Model = copy(dependencies = dependencies.flatten)

  def getOptions: Options =
    options.getOrElse(Options.default)

  def getReplacements: Replacements =
    replacements.getOrElse(Replacements.empty)

  def toDoc: Doc = {
    val deps = Some(("dependencies", dependencies.toDoc))
    val reps = replacements.map { r => ("replacements", r.toDoc) }
    val opts = options.map { o => ("options", o.toDoc) }

    yamlMap(
      List(opts, deps, reps).collect { case Some(kv) => kv },
      2
    ) + Doc.line
  }

  def hasAuthFile: Boolean = options.exists(_.authFile.nonEmpty)

  def getAuthFile: Option[String] =
    options.flatMap(_.authFile).map { auth =>
      if (auth.startsWith("$"))
        sys.env.getOrElse(auth.substring(1), s"env var ${auth} not found")
      else auth
    }
}

object Model {
  def readFile(f: File): Try[String] = Try {
    val fr = new FileReader(f)
    try {
      val buf = new BufferedReader(fr)
      val bldr = new java.lang.StringBuilder
      val cbuf = new Array[Char](1024)
      var read = 0
      while (read >= 0) {
        read = buf.read(cbuf, 0, 1024)
        if (read > 0) bldr.append(cbuf, 0, read)
      }
      Success(bldr.toString)
    } catch {
      case NonFatal(err) => Failure(err)
    } finally {
      fr.close
    }
  }.flatten

  def combine(a: Model, b: Model): ValidatedNel[String, Model] = {
    val oo = Monoid[Option[Options]].combine(a.options, b.options)

    val vcp = oo.getOrElse(Monoid[Options].empty).getVersionConflictPolicy

    def combineO[F[_]: Applicative, T](a: Option[T], b: Option[T])(
        fn: (T, T) => F[T]
    ): F[Option[T]] = {
      def p[A](a: A): F[A] = Applicative[F].pure(a)

      (a, b) match {
        case (None, right)      => p(right)
        case (left, None)       => p(left)
        case (Some(l), Some(r)) => fn(l, r).map(Some(_))
      }
    }

    type AE[T] = ValidatedNel[String, T]
    val validatedDeps =
      Dependencies.combine(vcp, a.dependencies, b.dependencies)
    val validatedOptR =
      combineO[AE, Replacements](a.replacements, b.replacements)(
        Replacements.combine
      )

    Applicative[AE].map2(validatedDeps, validatedOptR) { (deps, reps) =>
      Model(deps, reps, oo)
    }
  }

  def combine(ms: NonEmptyList[Model]): Either[NonEmptyList[String], Model] = {
    type M[T] = Either[NonEmptyList[String], T]

    Foldable[List].foldM[M, Model, Model](ms.tail, ms.head)(
      combine(_, _).toEither
    )
  }
}

case class MavenGroup(asString: String)
case class ArtifactOrProject(artifact: MavenArtifactId) {

  private val artifactId = artifact.artifactId
  private val packaging = artifact.packaging
  private val classifier = artifact.classifier

  def asString: String = artifact.asString

  val splitSubprojects: List[(ArtifactOrProject, Subproject)] =
    if (artifactId.contains('-')) {
      val indices = artifactId.iterator.zipWithIndex.collect {
        case (c, i) if c == '-' => i
      }
      indices.map { i =>
        (
          ArtifactOrProject(
            MavenArtifactId(artifactId.substring(0, i), packaging, classifier)
          ),
          Subproject(artifactId.substring(i + 1))
        )
      }.toList
    } else Nil

  // This is the same as splitSubprojects but also
  // includes the null split:
  def splitSubprojects1: NonEmptyList[(ArtifactOrProject, Subproject)] =
    NonEmptyList((this, Subproject("")), splitSubprojects)

  def split(a: ArtifactOrProject): Option[Subproject] =
    if (this == a) Some(Subproject(""))
    else if (
      artifactId.startsWith(a.artifactId) && artifactId.charAt(
        a.artifactId.length
      ) == '-'
    )
      Some {
        val sp = artifactId.substring(a.artifactId.length + 1) // skip the '-'
        Subproject(sp)
      }
    else None

  def toArtifact(sp: Subproject): ArtifactOrProject = {
    val str = sp.asString
    str match {
      case "" => this
      case _ =>
        ArtifactOrProject(
          MavenArtifactId(s"$artifactId-$str", packaging, classifier)
        )
    }
  }
}
object ArtifactOrProject {
  implicit val ordering: Ordering[ArtifactOrProject] = Ordering.by(_.asString)

  def apply(str: String): ArtifactOrProject = {
    ArtifactOrProject(MavenArtifactId(str))
  }
  def apply(
      artifactId: String,
      packaging: String,
      classifier: Option[String]
  ): ArtifactOrProject = {
    ArtifactOrProject(MavenArtifactId(artifactId, packaging, classifier))
  }
}

sealed trait DigestType {
  def getDigestInstance: MessageDigest
  def expectedHexLength: Int
  def name: String
}
object DigestType {
  case object Sha1 extends DigestType {
    def getDigestInstance: MessageDigest = MessageDigest.getInstance("SHA-1")
    def expectedHexLength: Int = 40
    def name = "SHA-1"
  }
  case object Sha256 extends DigestType {
    def getDigestInstance: MessageDigest = MessageDigest.getInstance("SHA-256")
    def expectedHexLength: Int = 64
    def name = "SHA-256"
  }
}

case class ShaValue(toHex: String, digestType: DigestType)

object ShaValue {

  def computeShaOf(digestType: DigestType, f: File): Try[ShaValue] = Try {
    val fis = new FileInputStream(f)
    try {
      val shaInstance = digestType.getDigestInstance
      withContent(fis) { (buffer, n) =>
        if (n > 0) shaInstance.update(buffer, 0, n) else ()
      }
      Success(
        ShaValue(
          shaInstance.digest.map("%02X".format(_)).mkString.toLowerCase,
          digestType
        )
      )
    } catch {
      case NonFatal(err) => Failure(err)
    } finally {
      fis.close
    }
  }.flatten

  private[this] def withContent(
      is: InputStream
  )(f: (Array[Byte], Int) => Unit): Unit = {
    val data = Array.ofDim[Byte](16384)
    var nRead = is.read(data, 0, data.length)
    while (nRead != -1) {
      f(data, nRead)
      nRead = is.read(data, 0, data.length)
    }
  }

  def parseFile(digestType: DigestType, file: File): Try[ShaValue] = {
    val fis = new FileInputStream(file)
    val baos = new ByteArrayOutputStream()
    withContent(fis) { (buffer, n) =>
      baos.write(buffer, 0, n)
    }
    val s = new String(baos.toByteArray, "UTF-8")
    parseData(digestType, s)
  }

  def parseData(digestType: DigestType, contents: String): Try[ShaValue] = {
    val hexString = contents
      .split("\\s") // some files have sha<whitespace>filename
      .dropWhile(_.isEmpty)
      .head
      .trim
      .toLowerCase
    if (
      hexString.length == digestType.expectedHexLength && hexString.matches(
        "[0-9A-Fa-f]*"
      )
    ) {
      Success(ShaValue(hexString, digestType))
    } else {
      Failure(
        new Exception(s"string: $hexString, not a valid ${digestType.name}")
      )
    }
  }
}

case class Subproject(asString: String)
case class Version(asString: String)
sealed trait DependencyServer {
  def toDoc: Doc
  def id: String
  def url: String
}
case class IvyServer(
    id: String,
    url: String,
    ivyPattern: String,
    ivyArtifactPattern: String
) extends DependencyServer {
  def toDoc: Doc =
    packedYamlMap(
      List(
        ("id", quoteDoc(id)),
        ("serverType", quoteDoc("ivy")),
        ("url", Doc.text(url)),
        ("ivyPattern", quoteDoc(ivyPattern)),
        ("ivyArtifactPattern", quoteDoc(ivyArtifactPattern))
      )
    )

}

case class StrictVisibility(enabled: Boolean)
object StrictVisibility {
  implicit val strictVisibilitySemiGroup: Semigroup[StrictVisibility] = Options.useRight.algebra[StrictVisibility]
}
case class MavenServer(id: String, contentType: String, url: String)
    extends DependencyServer {
  def toDoc: Doc =
    packedYamlMap(
      List(
        ("id", quoteDoc(id)),
        ("type", quoteDoc(contentType)),
        ("url", Doc.text(url))
      )
    )
}

object JarDescriptor {
  def computeShasOf(
      f: File,
      serverId: String,
      url: Option[String]
  ): Try[JarDescriptor] =
    for {
      sha1 <- ShaValue.computeShaOf(DigestType.Sha1, f)
      sha256 <- ShaValue.computeShaOf(DigestType.Sha256, f)
    } yield {
      JarDescriptor(
        url = url,
        sha1 = Some(sha1),
        sha256 = Some(sha256),
        fileSizeBytes = Some(f.length()),
        serverId = serverId
      )
    }
}

case class JarDescriptor(
    url: Option[String],
    sha1: Option[ShaValue],
    sha256: Option[ShaValue],
    fileSizeBytes: Option[Long],
    serverId: String
)

case class ResolvedShasValue(
    binaryJar: JarDescriptor,
    sourceJar: Option[JarDescriptor]
)

case class ProcessorClass(asString: String)

object Version {
  private def isNum(c: Char): Boolean =
    ('0' <= c) && (c <= '9')

  /** break a string into alternating runs of Longs and Strings
    */
  private def tokenize(s: String): List[Either[String, Long]] = {
    def append(
        a: List[Either[String, Long]],
        b: Either[List[Char], List[Char]]
    ): List[Either[String, Long]] =
      b match {
        case Right(thisAcc) =>
          Right(thisAcc.reverse.mkString.toLong) :: a
        case Left(thisAcc) =>
          Left(thisAcc.reverse.mkString) :: a
      }

    val (acc, toAdd) =
      s.foldLeft(
        (
          List.empty[Either[String, Long]],
          Option.empty[Either[List[Char], List[Char]]]
        )
      ) {
        // Here are the first characters
        case ((acc, None), c) if isNum(c) =>
          (acc, Some(Right(c :: Nil)))
        case ((acc, None), c) if !isNum(c) =>
          (acc, Some(Left(c :: Nil)))
        // Here we continue with the same type
        case ((acc, Some(Right(thisAcc))), c) if isNum(c) =>
          (acc, Some(Right(c :: thisAcc)))
        case ((acc, Some(Left(thisAcc))), c) if !isNum(c) =>
          (acc, Some(Left(c :: thisAcc)))
        // Here we switch type and add to the acc
        case ((acc, Some(r @ Right(thisAcc))), c) if !isNum(c) =>
          (append(acc, r), Some(Left(c :: Nil)))
        case ((acc, Some(l @ Left(thisAcc))), c) if isNum(c) =>
          (append(acc, l), Some(Right(c :: Nil)))
      }
    toAdd.fold(acc)(append(acc, _)).reverse
  }

  implicit def versionOrdering: Ordering[Version] = {
    implicit val strNumOrd: Ordering[Either[String, Long]] =
      new Ordering[Either[String, Long]] {
        def compare(
            left: Either[String, Long],
            right: Either[String, Long]
        ): Int = {
          (left, right) match {
            case (Right(a), Right(b)) => java.lang.Long.compare(a, b)
            case (Right(_), Left(_)) =>
              1 // put non number before number (eg, "-RC" comes before 2)
            case (Left(_), Right(_)) => -1
            case (Left(a), Left(b)) =>
              a.compareTo(b)
              val commonTokens = Set("alpha", "beta", "pre", "rc", "m")
              val al = a.toLowerCase
              val bl = b.toLowerCase
              if (commonTokens(al) && commonTokens(bl)) {
                al.compareTo(bl)
              } else a.compareTo(b)
          }
        }
      }
    // In versions, if one is a prefix of the other, and the next item is
    // not a number, it is bigger.
    @annotation.tailrec
    def prefixCompare[T: Ordering](a: List[T], b: List[T])(fn: T => Int): Int =
      (a, b) match {
        case (Nil, h :: tail) => fn(h)
        case (h :: tail, Nil) => -fn(h)
        case (Nil, Nil)       => 0
        case (ha :: taila, hb :: tailb) =>
          val c = Ordering[T].compare(ha, hb)
          if (c == 0) prefixCompare(taila, tailb)(fn)
          else c
      }
    Ordering.by { v: Version =>
      v.asString
        .split("\\.|\\-") // note this is a regex
        .flatMap(tokenize)
        .toList
    }(new Ordering[List[Either[String, Long]]] {
      def compare(
          a: List[Either[String, Long]],
          b: List[Either[String, Long]]
      ) =
        prefixCompare(a, b) {
          case Left(_)  => 1 // if see a string, the shorter one is larger
          case Right(_) => -1 // if we see a number, the shorter is smaller
        }
    })
  }
}

case class MavenArtifactId(
    artifactId: String,
    packaging: String,
    classifier: Option[String]
) {

  def asString: String = classifier match {
    case Some(c) => s"$artifactId:$packaging:$c"
    case None =>
      if (packaging == MavenArtifactId.defaultPackaging) {
        artifactId
      } else {
        s"$artifactId:$packaging"
      }
  }

  def addSuffix(s: String): MavenArtifactId =
    MavenArtifactId(s"$artifactId$s", packaging, classifier)
}

object MavenArtifactId {
  val defaultPackaging = "jar"

  def apply(a: ArtifactOrProject): MavenArtifactId = MavenArtifactId(a.asString)
  def apply(a: ArtifactOrProject, s: Subproject): MavenArtifactId =
    MavenArtifactId(a.toArtifact(s))

  // convenience: empty string classifier converted to None
  def apply(
      artifact: String,
      packaging: String,
      classifier: String
  ): MavenArtifactId = {
    assert(packaging != "")
    MavenArtifactId(
      artifact,
      packaging,
      classifier match {
        case "" => None
        case c  => Some(c)
      }
    )
  }

  def apply(str: String): MavenArtifactId =
    str.split(":") match {
      case Array(a, p, c) => MavenArtifactId(a, p, Some(c))
      case Array(a, p)    => MavenArtifactId(a, p, None)
      case Array(a)       => MavenArtifactId(a, defaultPackaging, None)
      case _ =>
        sys.error(
          s"$str did not match expected format <artifactId>[:<packaging>[:<classifier>]]"
        )
    }
}

case class MavenCoordinate(
    group: MavenGroup,
    artifact: MavenArtifactId,
    version: Version
) {
  def unversioned: UnversionedCoordinate =
    UnversionedCoordinate(group, artifact)
  def asString: String =
    s"${group.asString}:${artifact.asString}:${version.asString}"

  def toDependencies(l: Language): Dependencies =
    Dependencies(
      Map(
        group ->
          Map(
            ArtifactOrProject(artifact.asString) ->
              ProjectRecord(
                l,
                Some(version),
                None,
                None,
                None,
                None,
                None,
                None
              )
          )
      )
    )
}

object MavenCoordinate {
  def apply(s: String): MavenCoordinate =
    parse(s) match {
      case Validated.Valid(m)                        => m
      case Validated.Invalid(NonEmptyList(msg, Nil)) => sys.error(msg)
      case _ => sys.error("unreachable (we have only a single error)")
    }

  def parse(s: String): ValidatedNel[String, MavenCoordinate] =
    s.split(":") match {
      case Array(g, a, v) =>
        Validated.valid(
          MavenCoordinate(MavenGroup(g), MavenArtifactId(a), Version(v))
        )
      case other => Validated.invalidNel(s"expected exactly three :, got $s")
    }

  def apply(u: UnversionedCoordinate, v: Version): MavenCoordinate =
    MavenCoordinate(u.group, u.artifact, v)

  implicit def mvnCoordOrd: Ordering[MavenCoordinate] = Ordering.by {
    m: MavenCoordinate =>
      (m.group.asString, m.artifact.asString, m.version)
  }
}

sealed abstract class Language {
  def asString: String
  def asReversableString: String
  def asOptionsString: String
  def mavenCoord(
      g: MavenGroup,
      a: ArtifactOrProject,
      v: Version
  ): MavenCoordinate
  def mavenCoord(
      g: MavenGroup,
      a: ArtifactOrProject,
      sp: Subproject,
      v: Version
  ): MavenCoordinate
  def unversioned(g: MavenGroup, a: ArtifactOrProject): UnversionedCoordinate
  def unversioned(
      g: MavenGroup,
      a: ArtifactOrProject,
      sp: Subproject
  ): UnversionedCoordinate

  def unmangle(m: MavenCoordinate): MavenCoordinate
}

object Language {
  sealed trait JavaLike extends Language {
    def asString: String
    def asReversableString: String
    def asOptionsString = asString
    def mavenCoord(
        g: MavenGroup,
        a: ArtifactOrProject,
        v: Version
    ): MavenCoordinate =
      MavenCoordinate(g, MavenArtifactId(a), v)

    def mavenCoord(
        g: MavenGroup,
        a: ArtifactOrProject,
        sp: Subproject,
        v: Version
    ): MavenCoordinate =
      MavenCoordinate(g, MavenArtifactId(a, sp), v)

    def unversioned(
        g: MavenGroup,
        a: ArtifactOrProject
    ): UnversionedCoordinate =
      UnversionedCoordinate(g, MavenArtifactId(a))

    def unversioned(
        g: MavenGroup,
        a: ArtifactOrProject,
        sp: Subproject
    ): UnversionedCoordinate =
      UnversionedCoordinate(g, MavenArtifactId(a, sp))

    def unmangle(m: MavenCoordinate) = m
  }

  case object Java extends JavaLike {
    def asString = "java"
    def asReversableString = asString
  }

  case object Kotlin extends JavaLike {
    def asString = "kotlin"
    def asReversableString = asString
  }

  case class Scala(v: Version, mangle: Boolean) extends Language {
    def asString = if (mangle) "scala" else "scala/unmangled"
    def asOptionsString: String = s"scala:${v.asString}"
    def asReversableString = s"${asString}:${v.asString}"

    val major = v.asString.split('.') match {
      case Array("2", x) if (x.toInt >= 10)    => s"2.$x"
      case Array("2", x, _) if (x.toInt >= 10) => s"2.$x"
      case _ => sys.error(s"unsupported scala version: ${v.asString}")
    }
    private val suffix = s"_$major"
    private def add(a: MavenArtifactId): MavenArtifactId =
      if (mangle) a.addSuffix(suffix)
      else a

    def unversioned(
        g: MavenGroup,
        a: ArtifactOrProject
    ): UnversionedCoordinate =
      UnversionedCoordinate(g, add(MavenArtifactId(a)))

    def unversioned(
        g: MavenGroup,
        a: ArtifactOrProject,
        sp: Subproject
    ): UnversionedCoordinate =
      UnversionedCoordinate(g, add(MavenArtifactId(a, sp)))

    def mavenCoord(
        g: MavenGroup,
        a: ArtifactOrProject,
        v: Version
    ): MavenCoordinate =
      MavenCoordinate(g, add(MavenArtifactId(a)), v)

    def mavenCoord(
        g: MavenGroup,
        a: ArtifactOrProject,
        sp: Subproject,
        v: Version
    ): MavenCoordinate =
      MavenCoordinate(g, add(MavenArtifactId(a, sp)), v)

    def removeSuffix(s: String): Option[String] =
      if (s.endsWith(suffix)) Some(s.dropRight(suffix.size))
      else None

    def removeSuffix(uv: UnversionedCoordinate): UnversionedCoordinate = {
      val aid = uv.artifact
      removeSuffix(aid.artifactId) match {
        case None => uv
        case Some(a) =>
          UnversionedCoordinate(
            uv.group,
            MavenArtifactId(a, aid.packaging, aid.classifier)
          )
      }
    }

    def endsWithScalaVersion(uv: UnversionedCoordinate): Boolean =
      uv.artifact.artifactId.endsWith(suffix)

    def unmangle(m: MavenCoordinate) = {
      val uv = m.unversioned
      val uvWithRemoved = removeSuffix(uv)
      if (uv == uvWithRemoved) {
        m
      } else {
        MavenCoordinate(uvWithRemoved, v)
      }
    }
  }

  object Scala {
    val default: Scala = Scala(Version("2.11.11"), true)
  }

  implicit val ordering: Ordering[Language] = Ordering.by(_.asString)
}

case class UnversionedCoordinate(group: MavenGroup, artifact: MavenArtifactId) {
  def asString: String = s"${group.asString}:${artifact.asString}"

  /** This is a bazel-safe name to use as a remote repo name
    */
  def toBazelRepoName(namePrefix: NamePrefix): String =
    s"${namePrefix.asString}$asString".map {
      case '.' =>
        "_" // todo, we should have something such that if a != b this can't be equal, but this can
      case '-'   => "_"
      case ':'   => "_"
      case other => other
    }.mkString

  /** The bazel-safe target name
    */
  def toTargetName: String =
    artifact.asString.map {
      case ':' => '_'
      case o   => o
    }

  def toBindingName(namePrefix: NamePrefix): String = {
    val g = group.asString.map {
      case '.' => '/'
      case o   => o
    }
    s"jar/${namePrefix.asString}$g/${toTargetName}".map {
      case '.' | '-' => '_'
      case o         => o
    }
  }
  def bindTarget(namePrefix: NamePrefix): String =
    s"//external:${toBindingName(namePrefix)}"
}

case class ProjectRecord(
    lang: Language,
    version: Option[Version],
    modules: Option[Set[Subproject]],
    exports: Option[Set[(MavenGroup, ArtifactOrProject)]],
    exclude: Option[Set[(MavenGroup, ArtifactOrProject)]],
    generatesApi: Option[Boolean],
    processorClasses: Option[Set[ProcessorClass]],
    generateNeverlink: Option[Boolean]
) {

  // Cache this
  override lazy val hashCode: Int =
    (lang, version, modules, exports, exclude, processorClasses).hashCode

  def flatten(ap: ArtifactOrProject): List[(ArtifactOrProject, ProjectRecord)] =
    getModules match {
      case Nil => List((ap, copy(modules = None)))
      case mods =>
        mods.map { sp =>
          (ap.toArtifact(sp), copy(modules = None))
        }
    }

  def normalizeEmptyModule: ProjectRecord =
    getModules match {
      case Subproject("") :: Nil => copy(modules = None)
      case _                     => this
    }

  def withModule(m: Subproject): ProjectRecord = modules match {
    case None =>
      copy(modules = Some(Set(m)))
    case Some(subs) =>
      // otherwise add this subproject
      copy(modules = Some(subs + m))
  }

  def combineModules(that: ProjectRecord): Option[ProjectRecord] =
    if (
      (lang == that.lang) &&
      (version.flatMap { v => that.version.map(_ == v) }.forall(_ == true)) &&
      (exports == that.exports) &&
      (exclude == that.exclude)
    ) {
      val mods = (modules, that.modules) match {
        case (Some(a), Some(b)) => Some(a | b)
        case (None, s)          => s.map(_ + Subproject(""))
        case (s, None)          => s.map(_ + Subproject(""))
      }

      Some(copy(modules = mods))
    } else None

  def getModules: List[Subproject] =
    modules.getOrElse(Set.empty).toList.sortBy(_.asString)

  def versionedDependencies(
      g: MavenGroup,
      ap: ArtifactOrProject
  ): List[MavenCoordinate] =
    version.fold(List.empty[MavenCoordinate]) { v =>
      getModules match {
        case Nil  => List(lang.mavenCoord(g, ap, v))
        case mods => mods.map { m => lang.mavenCoord(g, ap, m, v) }
      }
    }

  def allDependencies(
      g: MavenGroup,
      ap: ArtifactOrProject
  ): List[UnversionedCoordinate] =
    getModules match {
      case Nil  => List(lang.unversioned(g, ap))
      case mods => mods.map { m => lang.unversioned(g, ap, m) }
    }

  private def toList(
      s: Set[(MavenGroup, ArtifactOrProject)]
  ): List[(MavenGroup, ArtifactOrProject)] =
    s.toList.sortBy { case (a, b) => (a.asString, b.asString) }

  def toDoc: Doc = {
    def colonPair(a: MavenGroup, b: ArtifactOrProject): Doc =
      quoteDoc(s"${a.asString}:${b.asString}")

    def exportsDoc(e: Set[(MavenGroup, ArtifactOrProject)]): Doc =
      if (e.isEmpty) Doc.text("[]")
      else
        (Doc.line + vlist(toList(e).map { case (a, b) => colonPair(a, b) }))
          .nested(2)

    def quoteEmpty(s: String): Doc =
      if (s.isEmpty) quoteDoc("") else Doc.text(s)

    val record = List(
      List(("lang", Doc.text(lang.asString))),
      version.toList.map { v => ("version", quoteDoc(v.asString)) },
      modules.toList.map { ms =>
        ("modules", list(ms.map(_.asString).toList.sorted)(quoteDoc))
      },
      exports.toList.map { ms => ("exports", exportsDoc(ms)) },
      exclude.toList.map { ms => ("exclude", exportsDoc(ms)) },
      processorClasses.toList.map { pcs =>
        ("processorClasses", list(pcs.map(_.asString).toList.sorted)(quoteDoc))
      },
      generateNeverlink.toList.map { v =>
        ("generateNeverlink", Doc.text(v.toString))
      }
    ).flatten
      .sortBy(_._1)
    packedYamlMap(record)
  }
}

object ProjectRecord {
  implicit val ordering: Ordering[ProjectRecord] = {
    implicit def ordList[T: Ordering]: Ordering[List[T]] =
      Ordering.by { l => (l: Iterable[T]) }

    Ordering.by { pr =>
      (
        pr.lang,
        pr.version,
        (
          pr.modules.fold(0)(_.size),
          pr.modules.map(_.map(_.asString).toList.sorted)
        ),
        (
          pr.exports.fold(0)(_.size),
          pr.exports.map(_.map { case (m, a) =>
            (m.asString, a.asString)
          }.toList.sorted)
        ),
        (
          pr.exclude.fold(0)(_.size),
          pr.exclude.map(_.map { case (m, a) =>
            (m.asString, a.asString)
          }.toList.sorted)
        )
      )
    }
  }
}

case class Dependencies(
    toMap: Map[MavenGroup, Map[ArtifactOrProject, ProjectRecord]]
) {

  def flatten: Dependencies =
    Dependencies(toMap.mapValues { map =>
      map.toList.flatMap { case (a, p) => p.flatten(a) }.toMap
    })

  def toDoc: Doc = {
    implicit val ordDoc: Ordering[Doc] = Ordering.by { d: Doc =>
      d.renderWideStream.mkString
    }
    val allDepDoc = toMap.toList.map { case (g, map) =>
      val parts = Dependencies.normalize(map.toList).sorted

      // This is invariant should be true at the end
      // assert(parts.flatMap { case (a, p) => p.flatten(a) }.sorted == allProj.sorted)

      val groupMap = yamlMap(parts.map { case (a, p) => (a.asString, p.toDoc) })

      (g.asString, groupMap)
    }.sorted

    yamlMap(allDepDoc, 2)
  }

  // Returns 1 if there is exactly one candidate that matches.
  def unversionedCoordinatesOf(
      g: MavenGroup,
      a: ArtifactOrProject
  ): Option[UnversionedCoordinate] =
    toMap.get(g).flatMap { ap =>
      a.splitSubprojects match {
        case Nil =>
          ap.get(a).map(_.allDependencies(g, a)) match {
            case Some(h :: Nil) => Some(h)
            case other          => None // 0 or more than one
          }
        case parts =>
          // This can be split, but may not be:
          val unsplit = ap.get(a).map(_.lang.unversioned(g, a)).toSet
          val uvcs = unsplit.union(parts.flatMap { case (proj, subproj) =>
            ap.get(proj)
              .map { pr =>
                pr.getModules.filter(_ == subproj).map((_, pr.lang))
              }
              .getOrElse(Nil)
              .map { case (m, lang) => lang.unversioned(g, proj, m) }
          }.toSet)
          if (uvcs.size == 1) Some(uvcs.head) else None
      }
    }

  def exportedUnversioned(
      u: UnversionedCoordinate,
      r: Replacements
  ): Either[List[(MavenGroup, ArtifactOrProject)], List[
    UnversionedCoordinate
  ]] =
    recordOf(u).flatMap(_.exports) match {
      case None => Right(Nil)
      case Some(l) =>
        def uv(
            g: MavenGroup,
            a: ArtifactOrProject
        ): Option[UnversionedCoordinate] =
          unversionedCoordinatesOf(g, a).orElse(
            r.unversionedCoordinatesOf(g, a)
          )

        val errs = l.filter { case (g, a) => uv(g, a).isEmpty }
        if (errs.nonEmpty) Left(l.toList)
        else Right(l.toList.flatMap { case (g, a) => uv(g, a) })
    }

  private val coordToProj: Map[MavenCoordinate, ProjectRecord] =
    (for {
      (g, m) <- toMap.iterator
      (a, p) <- m.iterator
      mcoord <- p.versionedDependencies(g, a)
    } yield (mcoord -> p)).toMap

  private val unversionedToProj: Map[UnversionedCoordinate, ProjectRecord] =
    (for {
      (g, m) <- toMap.iterator
      (a, p) <- m.iterator
      uv <- p.allDependencies(g, a)
    } yield (uv -> p)).toMap

  val roots: Set[MavenCoordinate] = coordToProj.keySet
  val unversionedRoots: Set[UnversionedCoordinate] =
    unversionedToProj.iterator.collect {
      case (uv, pr) if pr.version.isEmpty => uv
    }.toSet

  /** Note, if we implement this method with an unversioned coordinate, we need
    * to potentially remove the scala version to check the ArtifactOrProject key
    */
  private def recordOf(m: UnversionedCoordinate): Option[ProjectRecord] =
    unversionedToProj.get(m)

  def languageOf(m: UnversionedCoordinate): Option[Language] =
    recordOf(m).map(_.lang)

  def excludes(m: UnversionedCoordinate): Set[UnversionedCoordinate] =
    recordOf(m).flatMap(_.exclude) match {
      case None => Set.empty
      case Some(uvs) =>
        uvs.map { case (g, a) =>
          unversionedCoordinatesOf(g, a)
            .getOrElse(UnversionedCoordinate(g, MavenArtifactId(a)))
        }.toSet
    }
}

object Dependencies {
  def empty: Dependencies = Dependencies(
    Map.empty[MavenGroup, Map[ArtifactOrProject, ProjectRecord]]
  )

  /** Combine as many ProjectRecords as possible into a result
    */
  def normalize(
      candidates0: List[(ArtifactOrProject, ProjectRecord)]
  ): List[(ArtifactOrProject, ProjectRecord)] = {
    type AP = (ArtifactOrProject, ProjectRecord)

    def group[A, B](abs: List[(A, B)]): List[(A, List[B])] =
      abs.groupBy(_._1).map { case (k, vs) => k -> vs.map(_._2) }.toList

    def flatten(lp: List[AP]): List[AP] = lp.flatMap { case (a, p) =>
      p.flatten(a)
    }

    type CandidateGraph =
      List[(ArtifactOrProject, List[(ProjectRecord, List[(Subproject, AP)])])]

    def apsIn(cs: CandidateGraph): Set[AP] =
      (for {
        (a, ps) <- cs
        (p, saps) <- ps
        (s, ap) <- saps
      } yield ap).toSet

    // each Artifact-project record pair is either in the final result, or it isn't. We
    // just build all the cases now:
    def manyWorlds(
        candidates: CandidateGraph,
        acc: List[AP]
    ): List[List[AP]] = {
      candidates match {
        case Nil                => List(acc)
        case (art, Nil) :: tail => manyWorlds(tail, acc)
        case (art, (_, Nil) :: rest) :: tail =>
          manyWorlds((art, rest) :: tail, acc)
        case (art, (pr, subs) :: rest) :: tail =>
          // we consider taking (art, pr) and putting it in the result:
          val newPR = subs
            .foldLeft(pr) { case (pr, (sub, _)) => pr.withModule(sub) }
            .normalizeEmptyModule

          val finished = subs.map(_._2).toSet
          // this ArtifactOrProject has been used, so nothing in rest is legitimate
          // but we also need to filter to not consider items we have already added
          val newCand =
            tail
              .map { case (a, ps) =>
                val newPS =
                  ps.map { case (pr, subs) =>
                    (pr, subs.filterNot { case (_, ap) => finished(ap) })
                  }
                (a, newPS)
              }

          // this AP can't appear in others:
          val case1 = manyWorlds(newCand, (art, newPR) :: acc)

          // If we can still skip this (pr, subs) item and still
          // find homes for all the AP pairs in subs, then try
          def maybeRecurse(g: CandidateGraph): List[List[AP]] = {
            val aps = subs.iterator.map(_._2)
            val stillPaths = aps.filterNot(apsIn(g)).isEmpty
            if (stillPaths) manyWorlds(g, acc)
            else Nil
          }

          // or we could have not used this (art, pr) pair at all if there is
          // something else to use it in rest:
          // if any APs in subs don't appear in the rest this can't be successful
          // check that before we recurse
          val case2 = maybeRecurse((art, rest) :: tail)

          // or we could have just skipped using this art entirely
          // so that we don't exclude APs associated with it from
          // better matches
          // if any APs in subs don't appear in the rest this can't be successful
          // check that before we recurse
          val case3 = maybeRecurse(tail)

          case1 reverse_::: case2 reverse_::: case3
      }
    }

    def select(cs: CandidateGraph, inputs: Set[AP]): List[AP] = {
      def hasAllInputs(lp: List[AP]): Boolean =
        inputs == flatten(lp).toSet

      // We want the result that has all inputs and is smallest
      manyWorlds(cs, Nil) match {
        case Nil => Nil
        case nonEmpty =>
          val minimal = nonEmpty
            .filter(hasAllInputs _)
            .groupBy(
              _.size
            ) // there can be several variants with the same count
            .toList
            .minBy(_._1)
            ._2
          // after first taking the minimal number, we want
          // the lowest versions first, then we want the longest
          // prefixes
          implicit def ordList[T: Ordering]: Ordering[List[T]] =
            Ordering.Iterable[T].on[List[T]] { l => l }

          implicit val orderingArtP: Ordering[ArtifactOrProject] =
            Ordering.by { a: ArtifactOrProject =>
              val str = a.asString
              (-str.size, str)
            }
          // max of the sorted list gets us the longest strings, from last to front.
          minimal.map(_.sortBy(_.swap)).min
      }
    }
    // Each artifact or project has a minimal prefix
    // they can't conflict if that minimal prefix does not conflict
    val candidates = flatten(candidates0)
    val splitToOrig: List[(String, CandidateGraph)] = {
      val g0 = candidates.flatMap { case ap @ (a, p) =>
        require(p.modules == None) // this is an invariant true of candidates

        // Note that previously we allowed these splits to happen at any hyphen
        // in the artifact name, which looked nice, but resulted in terrible
        // runtime performance (literally hours to format the file) in cases
        // with lots of subprojects. The `take(2)` here restricts the split to
        // happening at the first hyphen (if at all).
        val subs = a.splitSubprojects1.toList.take(2)
        val prefix = subs.map {
          case (ArtifactOrProject(MavenArtifactId(artifact, _, _)), _) =>
            artifact
        }.min
        subs.map { case (a, sp) =>
          (prefix, (a, (p, (sp, ap))))
        }
      }
      group(g0).map { case (p, as) =>
        p -> (group(as).map { case (a, prsub) => a -> group(prsub) }.sortBy {
          case (_, prs) => -prs.size
        })
      }
    }

    // For each prefix apply the algo
    splitToOrig.flatMap { case (_, cg) =>
      select(cg, apsIn(cg))
    }
  }

  private[bazel_deps] def joinWith[F[_]: Applicative, K, A, B, C](
      m1: Map[K, A],
      m2: Map[K, B]
  )(fn: Ior[A, B] => F[C]): F[Map[K, C]] = {
    val allKeys = (m1.keySet | m2.keySet).toList
    def travFn(k: K): F[(K, C)] = {

      def withKey(f: F[C]): F[(K, C)] = f.map((k, _))

      (m1.get(k), m2.get(k)) match {
        case (Some(a), None)    => withKey(fn(Ior.left(a)))
        case (None, Some(b))    => withKey(fn(Ior.right(b)))
        case (Some(a), Some(b)) => withKey(fn(Ior.both(a, b)))
        case (None, None) => sys.error(s"somehow $k has no values in either")
      }
    }

    val fl: F[List[(K, C)]] = allKeys.traverse(travFn)
    fl.map(_.toMap)
  }

  private[bazel_deps] def onBoth[F[_]: Applicative, A](
      fn: (A, A) => F[A]
  ): Ior[A, A] => F[A] = {
    case Ior.Right(a)     => Applicative[F].pure(a)
    case Ior.Left(a)      => Applicative[F].pure(a)
    case Ior.Both(a1, a2) => fn(a1, a2)
  }

  def combine(
      vcp: VersionConflictPolicy,
      a: Dependencies,
      b: Dependencies
  ): ValidatedNel[String, Dependencies] = {

    type M1[T] = Map[MavenGroup, T]

    val functor1 = Functor[M1]
    def flatten(d: Dependencies): Dependencies = {
      val m: Map[MavenGroup, Map[ArtifactOrProject, ProjectRecord]] =
        functor1.map(d.toMap) { m: Map[ArtifactOrProject, ProjectRecord] =>
          m.iterator.flatMap { case (ap, pr) => pr.flatten(ap) }.toMap
        }
      Dependencies(m)
    }

    def mergeArtifact(
        p1: ProjectRecord,
        p2: ProjectRecord
    ): ValidatedNel[String, ProjectRecord] = {
      (p1.version, p2.version) match {
        case (None, None) => Validated.valid(p2) // right wins
        case (Some(v1), Some(v2)) if v1 == v2 =>
          Validated.valid(p2) // right wins
        case (Some(v1), Some(v2)) =>
          vcp.resolve(None, Set(v1, v2)).map { v =>
            if (v == v1) p1
            else p2
          }
        case (Some(v1), None) => Validated.valid(p1)
        case (None, Some(v2)) => Validated.valid(p2)
      }
    }

    type Artifacts = Map[ArtifactOrProject, ProjectRecord]
    type AE[T] = ValidatedNel[String, T]

    val mergeGroup: Ior[Artifacts, Artifacts] => AE[Artifacts] = {
      val fn1: Ior[ProjectRecord, ProjectRecord] => AE[ProjectRecord] =
        onBoth[AE, ProjectRecord](mergeArtifact(_, _))

      onBoth[AE, Artifacts](
        joinWith[
          AE,
          ArtifactOrProject,
          ProjectRecord,
          ProjectRecord,
          ProjectRecord
        ](_, _)(fn1)
      )
    }

    val flatA = flatten(a).toMap
    val flatB = flatten(b).toMap

    joinWith[AE, MavenGroup, Artifacts, Artifacts, Artifacts](flatA, flatB)(
      mergeGroup
    )
      .map { map => Dependencies(map.toList: _*) }
  }

  def apply(
      items: (MavenGroup, Map[ArtifactOrProject, ProjectRecord])*
  ): Dependencies =
    Dependencies(
      items
        .groupBy(_._1)
        .map { case (g, pairs) =>
          val finalMap = pairs.map(_._2).reduce(_ ++ _)
          (g, finalMap)
        }
        .toMap
    )
}

case class BazelTarget(asString: String)

case class ReplacementRecord(lang: Language, target: BazelTarget) {

  def toDoc: Doc =
    packedYamlMap(
      List(
        ("lang", Doc.text(lang.asString)),
        ("target", quoteDoc(target.asString))
      )
    )
}

case class Replacements(
    toMap: Map[MavenGroup, Map[ArtifactOrProject, ReplacementRecord]]
) {
  val unversionedToReplacementRecord
      : Map[UnversionedCoordinate, ReplacementRecord] =
    toMap.flatMap { case (g, projs) =>
      projs.map { case (a, r) =>
        r.lang.unversioned(g, a) -> r
      }
    }

  def unversionedCoordinatesOf(
      g: MavenGroup,
      a: ArtifactOrProject
  ): Option[UnversionedCoordinate] =
    for {
      m <- toMap.get(g)
      r <- m.get(a)
    } yield r.lang.unversioned(g, a)

  def get(uv: UnversionedCoordinate): Option[ReplacementRecord] =
    unversionedToReplacementRecord.get(uv)

  def toDoc: Doc = {
    implicit val ordDoc: Ordering[Doc] = Ordering.by { d: Doc =>
      d.renderWideStream.mkString
    }
    val allDepDoc = toMap.toList.map { case (g, map) =>
      val parts: List[(ArtifactOrProject, ReplacementRecord)] =
        map.toList
          .sortBy(_._1.asString)

      val groupMap = yamlMap(parts.map { case (a, rr) =>
        (a.asString, rr.toDoc)
      })

      (g.asString, groupMap)
    }.sorted

    yamlMap(allDepDoc, 2)
  }
}

object Replacements {
  def empty: Replacements = Replacements(Map.empty)

  /** Combine two replacements lists. Fail if there is a collision which is not
    * identical on both sides
    */
  def combine(
      a: Replacements,
      b: Replacements
  ): ValidatedNel[String, Replacements] = {
    import Dependencies.{joinWith, onBoth}

    def bothMatch[A](a: A, b: A): ValidatedNel[String, A] =
      if (a == b) Validated.valid(a)
      else Validated.invalidNel(s"in replacements combine: $a != $b")

    type AE[T] = ValidatedNel[String, T]
    val innerFn = onBoth[AE, ReplacementRecord](bothMatch(_, _))
    val outerFn = onBoth[AE, Map[ArtifactOrProject, ReplacementRecord]](
      joinWith(_, _)(innerFn)
    )
    joinWith(a.toMap, b.toMap)(outerFn)
      .map(Replacements(_))
  }
}

sealed abstract class VersionConflictPolicy(val asString: String) {

  /** TODO we currenly only have policies that always keep roots, if this
    * invariant changes, Normalizer will need to change the dead node
    * elimination step
    */
  def resolve(
      root: Option[Version],
      s: Set[Version]
  ): ValidatedNel[String, Version]
}
object VersionConflictPolicy {

  /** This is a way to combine VersionConflictPolicy taking the strictest of the
    * two it is actually a bounded semilattice (it is idempotent and
    * commutative).
    */
  implicit val vcpMonoid: CommutativeMonoid[VersionConflictPolicy] =
    new CommutativeMonoid[VersionConflictPolicy] {
      def empty = Highest
      def combine(a: VersionConflictPolicy, b: VersionConflictPolicy) =
        (a, b) match {
          case (Fail, _)          => Fail
          case (_, Fail)          => Fail
          case (Fixed, _)         => Fixed
          case (_, Fixed)         => Fixed
          case (Highest, Highest) => Highest
        }
    }

  def default: VersionConflictPolicy = Highest

  /** there must be only 1 version.
    */
  case object Fail extends VersionConflictPolicy("fail") {
    def resolve(root: Option[Version], s: Set[Version]) = root match {
      case Some(v) if s.size == 1 && s(v) => Validated.valid(v)
      case None if s.size == 1            => Validated.valid(s.head)
      case _ =>
        Validated.invalidNel(
          s"multiple versions found in Fail policy, root: $root, transitive: ${s.toList.sorted}"
        )
    }
  }

  /** It a version is explicitly declared, it is always used, otherwise there
    * must be only 1 version.
    */
  case object Fixed extends VersionConflictPolicy("fixed") {
    def resolve(root: Option[Version], s: Set[Version]) = root match {
      case Some(v)             => Validated.valid(v)
      case None if s.size == 1 => Validated.valid(s.head)
      case None =>
        Validated.invalidNel(
          s"fixed requires 1, or a declared version, found: ${s.toList.sorted}"
        )
    }
  }

  /** It a version is explicitly declared, it is always used, otherwise we take
    * the highest version.
    */
  case object Highest extends VersionConflictPolicy("highest") {
    def resolve(root: Option[Version], s: Set[Version]) = root match {
      case Some(v) => Validated.valid(v)
      case None =>
        Validated.valid(
          s.max
        ) // there must be at least one version, so this won't throw
    }
  }
}

case class DirectoryName(asString: String) {
  def parts: List[String] =
    asString.split('/').filter(_.nonEmpty).toList
}

object DirectoryName {
  def default: DirectoryName = DirectoryName("3rdparty/jvm")

  implicit val dirNameSemigroup: Semigroup[DirectoryName] =
    Options.useRight.algebra[DirectoryName]
}

sealed abstract class Transitivity(val asString: String)
object Transitivity {
  case object RuntimeDeps extends Transitivity("runtime_deps")
  case object Exports extends Transitivity("exports")

  implicit val transitivityMonoid: CommutativeMonoid[Transitivity] =
    new CommutativeMonoid[Transitivity] {
      def empty = RuntimeDeps
      def combine(a: Transitivity, b: Transitivity): Transitivity =
        (a, b) match {
          case (RuntimeDeps, t) => t
          case (t, RuntimeDeps) => t
          case (Exports, Exports) => Exports
        }
    }
}

sealed abstract class ResolverCache(val asString: String)
object ResolverCache {
  case object Local extends ResolverCache("local")
  case object BazelOutputBase extends ResolverCache("bazel_output_base")

  implicit val resolverCacheSemigroup: Semigroup[ResolverCache] =
    Options.useRight.algebra[ResolverCache]
}

case class NamePrefix(val asString: String)

object NamePrefix {

  def default: NamePrefix = NamePrefix("")

  implicit val namePrefixSemigroup: Semigroup[NamePrefix] =
    Options.useRight.algebra[NamePrefix]
}

sealed abstract class ResolverType(val asString: String) {
  def optionsDoc: Option[Doc]
}

object ResolverType {
  case object Aether extends ResolverType("aether") {
    override def optionsDoc: Option[Doc] = None
  }
  case object Coursier extends ResolverType("coursier") {
    override def optionsDoc: Option[Doc] = None
  }

  case class Gradle(
      lockFiles: Option[List[String]],
      noContentDeps: Option[List[String]],
      contentTypeOverride: Option[Map[String, String]],
      ignoreDependencyEdge: Option[Set[(String, String)]]
  ) extends ResolverType("gradle") {
    def getLockFiles: List[String] = lockFiles.getOrElse(Nil)
    def getNoContentDeps: Map[String, Option[Version]] = noContentDeps
      .getOrElse(Nil)
      .map { entry =>
        val indx = entry.indexOf('@')
        if (indx > 0) {
          require(indx < entry.length - 1, "Should never end on an @")
          (
            entry.substring(0, indx),
            Some(Version(entry.substring(indx + 1)))
          )
        } else {
          (entry, None)
        }
      }
      .toMap
    def getContentTypeOverride: Map[String, String] =
      contentTypeOverride.getOrElse(Map())

    override def optionsDoc: Option[Doc] = {

      val items = List(
        (
          "lockFiles",
          lockFiles.map {
            case Nil => Doc.text("[]")
            case fs  => (Doc.line + vlist(fs.map(quoteDoc(_)))).nested(2)
          }
        ),
        (
          "noContentDeps",
          noContentDeps.map {
            case Nil => Doc.text("[]")
            case fs  => (Doc.line + vlist(fs.map(quoteDoc(_)))).nested(2)
          }
        ),
        (
          "ignoreDependencyEdge",
          ignoreDependencyEdge.flatMap { m =>
            if (m.isEmpty) {
              None
            } else {
              Some(
                (Doc.line + vlist(
                  m.toList.sorted.map { case (k, v) =>
                    list(List(k, v)) { t: String => quoteDoc(t) }
                  }
                )).nested(2)
              )
            }
          }
        ),
        (
          "contentTypeOverride",
          contentTypeOverride.flatMap { m =>
            if (m.isEmpty) {
              None
            } else {
              Some(
                (Doc.line + packedDocYamlMap(
                  m.toList.sorted.map { case (k, v) =>
                    (quoteDoc(k), quoteDoc(v))
                  }
                )).nested(2)
              )
            }
          }
        )
      ).sortBy(_._1)
        .collect { case (k, Some(v)) => (k, v) }

      // we can't pack resolvers (yet)
      Some(packedYamlMap(items))
    }
  }

  object Gradle {
    def empty = Gradle(None, None, None, None)
    implicit val gradleMonoid: Monoid[Gradle] = new Monoid[Gradle] {
      val empty = Gradle.empty

      def combine(a: Gradle, b: Gradle): Gradle = {
        val lockFiles =
          Monoid[Option[List[String]]].combine(a.lockFiles, b.lockFiles)
        val noContentDeps =
          Monoid[Option[List[String]]].combine(a.noContentDeps, b.noContentDeps)
        val contentTypeOverride = Monoid[Option[Map[String, String]]]
          .combine(a.contentTypeOverride, b.contentTypeOverride)
        val ignoreDependencyEdge = Monoid[Option[Set[(String, String)]]]
          .combine(a.ignoreDependencyEdge, b.ignoreDependencyEdge)

        Gradle(
          lockFiles = lockFiles,
          noContentDeps = noContentDeps,
          contentTypeOverride = contentTypeOverride,
          ignoreDependencyEdge = ignoreDependencyEdge
        )
      }
    }
  }

  val default = Coursier

  implicit val resolverSemigroup: Semigroup[ResolverType] =
    new Semigroup[ResolverType] {
      override def combine(x: ResolverType, y: ResolverType): ResolverType = {
        (x, y) match {
          case (l: Gradle, r: Gradle) => Monoid.combine(l, r)
          case (_, r)                 => r
        }
      }
    }
}

case class Options(
    versionConflictPolicy: Option[VersionConflictPolicy],
    languages: Option[Set[Language]],
    resolvers: Option[List[DependencyServer]],
    resolverCache: Option[ResolverCache],
    namePrefix: Option[NamePrefix],
    licenses: Option[Set[String]],
    resolverType: Option[ResolverType],
    transitivity: Option[Transitivity],
    buildHeader: Option[List[String]],
    thirdPartyDirectory: Option[DirectoryName],
    strictVisibility: Option[StrictVisibility],
    buildFileName: Option[String],
    authFile: Option[String]
) {
  def isDefault: Boolean =
    versionConflictPolicy.isEmpty &&
      thirdPartyDirectory.isEmpty &&
      languages.isEmpty &&
      resolvers.isEmpty &&
      transitivity.isEmpty &&
      buildHeader.isEmpty &&
      resolverCache.isEmpty &&
      namePrefix.isEmpty &&
      licenses.isEmpty &&
      resolverType.isEmpty &&
      strictVisibility.isEmpty &&
      buildFileName.isEmpty &&
      authFile.isEmpty

  def getLicenses: Set[String] =
    licenses.getOrElse(Set.empty)


  def getThirdPartyDirectory: DirectoryName =
    thirdPartyDirectory.getOrElse(DirectoryName.default)

  def getVersionConflictPolicy: VersionConflictPolicy =
    versionConflictPolicy.getOrElse(VersionConflictPolicy.default)

  def replaceLang(l: Language): Language = l match {
    case Language.Java   => Language.Java
    case Language.Kotlin => Language.Kotlin
    case s @ Language.Scala(_, _) =>
      getLanguages
        .collectFirst { case scala: Language.Scala => scala }
        .getOrElse(s)
  }

  def getLanguages: List[Language] = languages match {
    case None        => List(Language.Java, Language.Scala.default)
    case Some(langs) => langs.toList.sortBy(_.asString)
  }
  def getResolvers: List[DependencyServer] =
    resolvers.getOrElse(
      List(
        MavenServer(
          "mavencentral",
          "default",
          "https://repo.maven.apache.org/maven2/"
        )
      )
    )

  def getResolverCache: ResolverCache =
    resolverCache.getOrElse(ResolverCache.Local)

  def getNamePrefix: NamePrefix =
    namePrefix.getOrElse(NamePrefix.default)

  def getResolverType: ResolverType =
    resolverType.getOrElse(ResolverType.default)

  def getTransitivity: Transitivity =
    transitivity.getOrElse(Transitivity.Exports)

  def getBuildHeader: String = buildHeader match {
    case Some(lines) => lines.mkString("\n")
    case None => ""
  }

  def getBuildFileName: String =
    buildFileName.getOrElse("BUILD")

  def toDoc: Doc = {
    val items = List(
      (
        "versionConflictPolicy",
        versionConflictPolicy.map { p => Doc.text(p.asString) }
      ),
      (
        "resolvers",
        resolvers.map {
          case Nil => Doc.text("[]")
          case ms  => (Doc.line + vlist(ms.map(_.toDoc))).nested(2)
        }
      ),
      (
        "languages",
        languages.map { ls =>
          list(ls.map(_.asOptionsString).toList.sorted)(quoteDoc)
        }
      ),
      ("resolverCache", resolverCache.map { rc => Doc.text(rc.asString) }),
      ("namePrefix", namePrefix.map { p => quoteDoc(p.asString) }),
      ("licenses", licenses.map { l => list(l.toList.sorted)(quoteDoc) }),
      ("resolverType", resolverType.map(r => quoteDoc(r.asString))),
      (
        "resolverOptions",
        resolverType.flatMap(_.optionsDoc).map(d => (Doc.line + d).nested(2))
      ),
      ("thirdPartyDirectory",
        thirdPartyDirectory.map { tpd => quoteDoc(tpd.asString) }),
      ("buildHeader",
        buildHeader.map(list(_) { s => quoteDoc(s) })),
      ("transitivity", transitivity.map { t => Doc.text(t.asString) }),
      ("strictVisibility", strictVisibility.map { x => Doc.text(x.enabled.toString) }),
      ("buildFileName", buildFileName.map(name => Doc.text(name))),
      ("authFile", authFile.map(name => Doc.text(name)))
    ).sortBy(_._1)
      .collect { case (k, Some(v)) => (k, v) }

    // we can't pack resolvers (yet)
    packedYamlMap(items)
  }
}

object Options {

  def default: Options = optionsMonoid.empty

  def useRight: SemigroupK[Id] =
    new SemigroupK[Id] {
      def combineK[A](x: A, y: A): A = y
    }

  /** A monoid on options that is just the point-wise monoid
    */
  implicit val optionsMonoid: Monoid[Options] = new Monoid[Options] {
    val empty = Options(
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None
    )

    def combine(a: Options, b: Options): Options = {
      val vcp = Monoid[Option[VersionConflictPolicy]]
        .combine(a.versionConflictPolicy, b.versionConflictPolicy)
      val langs =
        Monoid[Option[Set[Language]]].combine(a.languages, b.languages)
      val resolvers = Monoid[Option[List[DependencyServer]]]
        .combine(a.resolvers, b.resolvers)
        .map(_.distinct)
      val resolverCache =
        Monoid[Option[ResolverCache]].combine(a.resolverCache, b.resolverCache)
      val namePrefix =
        Monoid[Option[NamePrefix]].combine(a.namePrefix, b.namePrefix)
      val licenses = Monoid[Option[Set[String]]].combine(a.licenses, b.licenses)
      val resolverType =
        Monoid[Option[ResolverType]].combine(a.resolverType, b.resolverType)
      val trans = Monoid[Option[Transitivity]].combine(a.transitivity, b.transitivity)
      val headers = Monoid[Option[List[String]]].combine(a.buildHeader, b.buildHeader).map(_.distinct)
      val tpd = Monoid[Option[DirectoryName]].combine(a.thirdPartyDirectory, b.thirdPartyDirectory)
      val strictVisibility = Monoid[Option[StrictVisibility]].combine(a.strictVisibility, b.strictVisibility)
      val buildFileName = Monoid[Option[String]].combine(a.buildFileName, b.buildFileName)
      val authFile = Monoid[Option[String]].combine(a.authFile, b.authFile)
      Options(
        vcp,
        langs,
        resolvers,
        resolverCache,
        namePrefix,
        licenses,
        resolverType,
        trans,
        headers,
        tpd,
        strictVisibility,
        buildFileName,
        authFile
      )
    }
  }
}
