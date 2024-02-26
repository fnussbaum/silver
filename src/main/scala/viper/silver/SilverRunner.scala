package viper.silver


import scala.collection.immutable.ArraySeq
import viper.silver.frontend.DefaultStates 
import viper.silver.frontend.ViperAstProvider
import viper.silver.reporter.StdIOReporter
import viper.silver.logger.ViperStdOutLogger


object SilverRunner extends SilverRunnerInstance {
  def main(args: Array[String]): Unit = {
    runMain(args)
  }
}

class SilverRunnerInstance extends ViperAstProvider(StdIOReporter(), ViperStdOutLogger("SilverRunnerLogger").get) {
  def runMain(args: Array[String]): Unit = {
    var exitCode = 1 /* Only 0 indicates no error */

    execute(ArraySeq.unsafeWrapArray(Array("--plugin", "viper.silver.plugin.standard.doc.DocPlugin") ++ args))
    plugins.beforeMethodFilter(translationResult)
    
    if (state >= DefaultStates.Translation) {
      exitCode = 0
    }
    sys.exit(exitCode)
  }
}