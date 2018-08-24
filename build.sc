// Using Mill 0.2.2
// Install: http://www.lihaoyi.com/mill/
// Run:     ./mill -i -watch html
import mill._, ammonite.ops._
import scala.annotation.tailrec
import java.util.Calendar
import java.text.SimpleDateFormat

val ELM_MAKE = "elm-make"

object SNS extends Module {
  def millSourcePath = pwd
  implicit def src: Path = pwd / "src"
  
  def elmStuffRoot = T.sources {  pwd / "elm-stuff" }
  def sourceRoot   = T.sources { src }
  def nativeRoot   = T.sources { src / "Native" }
  def examplesRoot = T.sources { pwd / 'examples }
  def examplesTemplate = T.input { (pwd / 'src / "ExamplesTemplate.elm").toNIO.toFile.lastModified() }
  def leoExamplesRoot = T.sources { pwd / 'examples / 'fromLeo }
  def preludeRoot = T.input { (pwd / 'examples / "preludeLeo.elm").toNIO.toFile.lastModified() }
  val outDir = pwd / "build" /"out"
  val outSNS = outDir / "sns.js"
  
  def packages = T{ //TODO 
  }
  
  def examples = T{
    allTimestampsOfIncludedExampleFiles()
    examplesTemplate()
    %%("python", pwd/'scripts/"expandTemplate.py", "Examples") //'
    println("Examples regenerated")
  }
  
  def prelude = T{
    preludeRoot()
    %%("python", pwd/'scripts/"expandTemplate.py", "Prelude") //'
    println("Prelude regenerated")
  }

  def currentDate = new SimpleDateFormat("HH:mm:ss  (yyyy-MM-dd)").format(Calendar.getInstance().getTime())
  
  def all = T{
    packages()
    examples()
    prelude()
    sourceRoot()
    val startTime =  Calendar.getInstance().getTime().getTime
    println("elm_make started at " + currentDate)
    stderr(%%(ELM_MAKE,"Main.elm", "--output", outSNS)) match {
      case Left(msg) =>
        System.out.print("\033[H\033[2J") // Clears the console to display the error
        println(buildSummary(fixpoint(insertLinks)(msg)))
      case Right(ok) =>
        val endTime =  Calendar.getInstance().getTime().getTime
        println("it took " + (endTime - startTime )/ 1000 + "s")
        val output = read(outSNS)
        write.over(outSNS, output.replace("""var Elm = {};""",
        """var Elm = {};
          |Elm["EvalUpdate"] = Elm["EvalUpdate"] || {};
          |Elm["EvalUpdate"].api = _user$project$EvalUpdate$api;
          |""".stripMargin))
    }
    true
  }

  def buildSummary(errorMsg: String): String = {
    errorMsg + """
    |Last errors by files:
    |""".stripMargin +
    """\.\((\w+\.elm):(\d+)\)""".r.findAllMatchIn(errorMsg).map(m =>
      (m.group(1), m.group(2))
    ).toList.groupBy(_._1).map{case (filename, filename_linenums) =>
      val lineNums = filename_linenums.map(_._2.toInt)
      val numErrors = lineNums.filter(x => !lineNums.exists(y => y == x + 1))
      "at .(" + filename + ":" + lineNums.sorted.last + ")" +
        (if(filename_linenums.length > 1) " and " + numErrors.length + " errors covering " + lineNums.length + " lines." else " and this is the last error!")
    }.mkString("\n")
  }
  
  def copyNative = T{
    nativeRoot()
    List("aceCodeBox.js",
         "outputCanvas.js",
         "aceTooltips.js",
         "animationLoop.js",
         "fileHandler.js",
         "deucePopupPanelInfo.js",
         "dotGraph.js",
         "colorScheme.js",
         "syntaxHighlight.js",
         "focus.js",
         "keyBlocker.js",
         "solverServer.js"
    ).map(src/'Native/_).foreach(copy(_, outDir))
    List("ace.js",
         "mode-little.js",
         "mode-elm.js",
         "theme-chrome.js"
    ).map(pwd/"ace-builds"/'src/_).foreach(copy(_, outDir))
    copy(pwd/"viz.js"/"viz.js", outDir)
    mkdir ! pwd/'build/'out/'img //'
    copy(pwd/'img/"light_logo.svg", outDir/'img)
    ls! pwd/'img |? (_.ext == "png") |! (copy(_, outDir / 'img))
  }

  def html = T{
    copyNative()
    all()
  }
  
  def insertLinks(msg: String) = {
    """((\w+\.elm)\r?\n(?:(?!\w+\.elm\r?\n)[\s\S])*?\r?\n) ?(\d+)\|([^\r\n]*\r?\n)(?=(\s*\^)?)""".r.replaceAllIn(msg,
      { m =>
        val link = "at .("+m.group(2)+":" +m.group(3)+")"
        val result = m.group(1)+link+m.group(4)+(if(m.group(5) != null) " " * (link.length - m.group(3).length - 1) else "")
        """\$""".r.replaceAllIn("""\\""".r.replaceAllIn(result, _ => """\\\\"""), _ => """\\\$""")
      })
  }
  @tailrec def fixpoint[A](f: A => A, max: Int = 100)(x: A): A = {
    var new_x = f(x)
    if(new_x == x || max <= 0) x else fixpoint(f, max - 1)(new_x)
  }
  
  def copy(file: Path, outDir: Path) = {
    val out = outDir/file.last
    if (exists! out) rm(out)
    mkdir! outDir
    cp(file, out)
  }

  def stderr(commandResult: =>CommandResult): Either[String, String] = {
    try {
      Right(commandResult.err.string)
    } catch {
      case ammonite.ops.ShelloutException(commandResult) =>
        Left(commandResult.err.string)
    }
  }

  val ActiveExampleFile = "(LEO_TO_ELM|LITTLE_TO_ELM) (.*)".r
  def extension(converter: String) = if(converter == "LEO_TO_ELM") ".elm" else ".little"

  def allTimestampsOfIncludedExampleFiles = T {
    examplesRoot()
    read(pwd / 'src / "ExamplesTemplate.elm").split("\\r?\\n").toList.flatMap{
      case ActiveExampleFile(converter, filename) =>
        val filepath = pwd / 'examples / RelPath(filename+extension(converter))
        List(filepath.toNIO.toFile.lastModified())
      case _ => Nil
    }
  }
}

object SNSTests extends Module {
  import SNS.{buildSummary,fixpoint,insertLinks,stderr, examples, prelude}
  def millSourcePath = pwd
  implicit def src: Path = pwd / "tests"

  def testRoot = T.sources { pwd / 'tests }

  def test = T {
    testRoot()
    examples()
    prelude()
    SNS.sourceRoot()
    stderr(%%(ELM_MAKE, "UpdateTests.elm", "--output", "build/test.js")) match {
      case Left(msg) =>
        System.out.print("\033[H\033[2J") // Clears the console to display the error
        println(buildSummary(fixpoint(insertLinks)(msg)))
      case Right(ok) =>
        println("Let's run the tests")
        try {
          %("node", "--stack_size=2048", "support/runner.js")
        } catch {
          case ammonite.ops.InteractiveShelloutException() =>
            println("Exception 1 caught")
            // Re-running to get the error:
            try {
              %%("node", "--stack_size=2048", "support/runner.js")
              println("Executed normally")
            } catch {
              case ammonite.ops.ShelloutException(commandResult) =>
                println("Exception caught")
                println(SNS.currentDate + ":" + commandResult.err.string)
            }
        }
        ()
    }
  }
}

def html = T{ SNS.html() }

def test =  T{ SNSTests.test() }
