import org.nlogo.api._
import org.nlogo.api.Syntax._
import org.nlogo.api.ScalaConversions._
import java.io._
import java.net._

object BrainExtension {
  var bc: Option[BrainConnection] = None

  def connect(host:String){ 
    bc = Some(BrainConnection(host))
  }

  def disconnect(){
    bc match {
      case Some(c) => 
        c.disconnect()
        bc = None
      case None => sys.error("not connected")
    }
  }

  def attention: Int = bc match {
    case Some(c) => c.attention
    case None => sys.error("not connected")
  }

  def meditation: Int = bc match {
    case Some(c) => c.meditation
    case None => sys.error("not connected")
  }
}

class BrainExtension extends DefaultClassManager {
  def load(manager: PrimitiveManager) {
    manager.addPrimitive("connect", new Connect)
    manager.addPrimitive("disconnect", new Disconnect)
    manager.addPrimitive("attention", new Attention)
    manager.addPrimitive("meditation", new Meditation)
  }
}

case class BrainConnection(host:String) {
  val port = 13854
  var socket = new Socket()
  try socket.connect(new InetSocketAddress(host, port), 3000)
  catch {
    case e:Exception => 
     throw new ExtensionException(new Exception("couldn't connect to " + (host, port), e))
  }

  @volatile var connected = true
  @volatile var attention = 0
  @volatile var meditation = 0

  new Thread(new Runnable(){
    def run(){
      while(connected){
        val (a,m) = BrainReader.read(socket.getInputStream, attention, meditation)
	attention = a
        meditation = m
      } 
    }
  }).start()

  def disconnect(){
    connected = false
    socket.close()
  }
}

object BrainReader{
  def read(in:InputStream, attention:Int, meditation:Int): (Int, Int) = {
    val b = in.read()
    if(b == 0x04) (in.read(), meditation)
    else if(b == 0x05) (attention, in.read())
    else (attention, meditation)
  }
}

class Connect extends DefaultCommand {
  override def getSyntax = commandSyntax(Array(TYPE_STRING))
  def perform(args: Array[Argument], context: Context){
    BrainExtension.connect(args(0).getString)
  }
}

class Disconnect extends DefaultCommand {
  override def getSyntax = commandSyntax(Array())
  def perform(args: Array[Argument], context: Context){
    BrainExtension.disconnect()
  }
}

class Attention extends DefaultReporter {
  override def getSyntax = reporterSyntax(Array(), TYPE_NUMBER)
  def report(args: Array[Argument], context: Context): AnyRef = 
    BrainExtension.attention.toDouble.asInstanceOf[AnyRef]
}

class Meditation extends DefaultReporter {
  override def getSyntax = reporterSyntax(Array(), TYPE_NUMBER)
  def report(args: Array[Argument], context: Context): AnyRef = 
    BrainExtension.meditation.toDouble.asInstanceOf[AnyRef]
}
