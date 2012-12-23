package com.npcode

import akka.actor._
import java.net.InetSocketAddress
import akka.util.ByteString
import java.io.File
import org.apache.tika.Tika
import java.io.FileInputStream
import scala.util.matching.Regex
import akka.actor.IO.ReadHandle
import java.nio.charset.Charset

trait HTTP {
  def ??? : Nothing = throw new Error("an implementation is missing")

  type ??? = Nothing

  val docroot = "."

  def mimeType(file: File) = new Tika().detect(file)

  // Get request line from request
  def requestLine(bytes: ByteString): String = bytes.decodeString("UTF-8")

  // Return "404 Not Found" response.
  def notFound: ByteString = ByteString("HTTP/1.1 404 Not Found\r\n\r\n")

  // Return "405 Method Not Allowed" response.
  def methodNotAllowed: ByteString = ByteString("HTTP/1.1 405 Method Not Allowed\r\n\r\n")

  // Return "400 Bad Request" response.
  def badRequest: ByteString = ByteString("HTTP/1.1 400 Bad Request\r\n\r\n")

  // Return 200 OK response with the given file.
  def ok(file: File): ByteString = ByteString("HTTP/1.1 200 OK\r\nContent-Length: " + file.length() + "\r\nContent-Type: " + mimeType(file)) ++ readFile(file)

  def serve(rHandle: ReadHandle, request: ByteString) = {
    rHandle.asSocket.write(response(request))
    rHandle.close()
  }

  def readFile(file: File): ByteString = {
    val resource = new Array[Byte](file.length.toInt)
    val in = new FileInputStream(file)
    in.read(resource)
    in.close()
    ByteString(resource)
  }

  // Read the given request and return appropriate response.
  def response(request: ByteString): ByteString = {
    val segments:Array[String] = request.utf8String.split(" ")
    if (segments.length < 2) return badRequest 
    val method = segments(0)
    val uri = segments(1)
    
    val file = new File(docroot + "/demo/" + uri)
    
    method match {
      case "GET" => {
    	  if (file.exists && file.isFile()) {
    	    return ok(file)
    	  }
    	  return notFound
      }
      case _ => return methodNotAllowed
    }
    badRequest
  } 

}

class TCPServer(port: Int) extends Actor with HTTP {

  override def preStart {
    IOManager(context.system).listen(new InetSocketAddress(port))
  }

  def receive = {
    case IO.NewClient(server) => server.accept()
    case IO.Read(rHandle, bytes) => serve(rHandle, bytes)
  }
}

object Application {
  def main(args: Array[String]) {
    val port = 8000
    ActorSystem().actorOf(Props(new TCPServer(port)))
  }
}
