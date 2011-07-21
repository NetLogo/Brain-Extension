import org.nlogo.api._
import org.nlogo.api.Syntax._
import org.nlogo.api.ScalaConversions._

object BrainExtension {
  var bc: Option[BrainConnection] = None

  def connect(host:String){ 
    bc = Some(BrainConnection(host))
  }

  def readAttention(): String = bc match {
    case Some(c) => c.readAttention()
    case None => sys.error("not connected")
  }
}

class BrainExtension extends DefaultClassManager {
  def load(manager: PrimitiveManager) {
    manager.addPrimitive("connect", new Connect)
    manager.addPrimitive("attention", new Attention)
  }
}

case class BrainConnection(host:String) {
  import java.io._
  import java.net._
  var socket = new Socket(host, 13854)
  var out = new PrintWriter(socket.getOutputStream, true)
  var in = new BufferedReader(new InputStreamReader(socket.getInputStream))
  def readAttention(): String = in.readLine() 
}

class Connect extends DefaultCommand {
  override def getSyntax = commandSyntax(Array(StringType))
  def perform(args: Array[Argument], context: Context){
    BrainExtension.connect(args(0).getString)
  }
}


class Attention extends DefaultReporter {
  override def getSyntax = reporterSyntax(Array(), StringType)
  def report(args: Array[Argument], context: Context): AnyRef = 
    BrainExtension.readAttention().asInstanceOf[AnyRef]
}
