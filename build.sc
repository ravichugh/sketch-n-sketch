// Using Mill 0.2.2
// Install: http://www.lihaoyi.com/mill/
// Run:     ./mill -i -watch html
import mill._, ammonite.ops._
import scala.annotation.tailrec

val ELM_MAKE = "elm-make"

object SNS extends Module {
  def millSourcePath = pwd
  implicit def src: Path = pwd / "src"
  
  def elmStuffRoot = T.sources {  pwd / "elm-stuff" }
  def sourceRoot   = T.sources { src }
  def nativeRoot   = T.sources { src / "Native" }
  def examplesRoot = T.sources { pwd / 'examples } //'
  def leoExamplesRoot = T.sources { pwd / 'examples / 'fromLeo } //'
  def preludeRoot = T.sources { pwd / 'examples / "preludeLeo.elm" } //'
  val outDir = pwd / "build" /"out"
  val outSNS = outDir / "sns.js"
  
  def packages = T{ //TODO 
  }
  
  def examples = T{
    examplesRoot()
    leoExamplesRoot()
    %%("python", pwd/'scripts/"expandTemplate.py", "Examples") //'
  }
  
  def prelude = T{
    preludeRoot()
    %%("python", pwd/'scripts/"expandTemplate.py", "Prelude") //'
  }
  
  def all = T{
    packages()
    examples()
    prelude()
    sourceRoot()
    stderr(%%(ELM_MAKE,"Main.elm", "--output", outSNS)) match {
      case Left(msg) =>
        System.out.print("\033[H\033[2J") // Clears the console to display the error
        println(fixpoint(insertLinks)(msg))
      case Right(ok) =>
        val output = read(outSNS)
        write.over(outSNS, output.replace("""var Elm = {};""",
        """var Elm = {};
          |Elm["EvalUpdate"] = Elm["EvalUpdate"] || {};
          |Elm["EvalUpdate"].api = _user$project$EvalUpdate$api;
          |""".stripMargin))
    }
    true
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
        m.group(1)+link+m.group(4)+(if(m.group(5) != null) " " * (link.length - m.group(3).length - 1) else "")})
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
}

def html = T{ SNS.html() }
