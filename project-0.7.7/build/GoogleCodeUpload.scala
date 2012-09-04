/* Port of the ant task by Jonathan Fuerth (new BSD license) */

import java.io.BufferedInputStream
import java.io.FileInputStream
import java.io.IOException
import java.io.InputStream
import java.io.OutputStream
import java.net.HttpURLConnection
import java.net.MalformedURLException
import java.net.URL

import sbt.TaskManager

trait GoogleCodeTasks extends TaskManager {

  /**
   * Uploads the contents of the file {@link #fileName} to the project's
   * Google Code upload url. Performs the basic http authentication required
   * by Google Code.
   */
  def googleCodeUploadTask(
    userName:String,
    password:String,
    fileName:String, 
    projectName:String,
    targetFileName:String, 
    summary:String, 
    labels:String) = task {

      /**
       * Just sends an ASCII version of the given string, followed by a CRLF
       * line terminator, to the given output stream.
       */
      def sendLine(out: OutputStream, string:String) = {
        out.write(string.getBytes("ascii"))
        out.write("\r\n".getBytes("ascii"))
      }
  
      /**
       * Creates a (base64-encoded) HTTP basic authentication token for the
       * given user name and password.
       */
       def createAuthToken(userName:String, password:String) = {
         val string = (userName + ":" + password)
           try { Base64.encodeBytes(string.getBytes("UTF-8")) }
         catch {
           case notreached:java.io.UnsupportedEncodingException =>
           throw new InternalError(notreached.toString)
         }
       }

      /**
       * Creates the correct URL for uploading to the named google code project.
       * If uploadUrl is not set (this is the standard case), the correct URL will
       * be generated based on the {@link #projectName}.  Otherwise, if uploadUrl
       * is set, it will be used and the project name setting will be ignored.
       */
      def createUploadURL = {
        if (projectName == null) {
          throw new NullPointerException("projectName must be set")
        } else new URL("https", projectName + ".googlecode.com", "/files")
      }

      try {
        // fixes open-jdk-issue
        System.clearProperty("javax.net.ssl.trustStoreProvider") 
        System.clearProperty("javax.net.ssl.trustStoreType")
        
        val BOUNDARY = "CowMooCowMooCowCowCow"
        val url = createUploadURL
        
        println("The upload URL is " + url)
        
        var in : InputStream = 
          new BufferedInputStream(new FileInputStream(fileName))
        
        val conn = url.openConnection.asInstanceOf[HttpURLConnection]
        conn.setDoOutput(true)
        conn.setRequestProperty(
          "Authorization", 
          "Basic " + createAuthToken(userName, password))
        conn.setRequestProperty(
          "Content-Type", 
          "multipart/form-data; boundary=" + BOUNDARY)
        conn.setRequestProperty("User-Agent", "Google Code Upload sbt Task")
        
        println("Attempting to connect (username is " + userName + ")...")
        conn.connect
        
        System.out.println("Sending request parameters...")
        val out = conn.getOutputStream
        sendLine(out, "--" + BOUNDARY)
        sendLine(out, "content-disposition: form-data; name=\"summary\"")
        sendLine(out, "")
        sendLine(out, summary)
        
        if (labels != null) {
          val labelArray = labels.split("\\,")
          if (labelArray != null && labelArray.length > 0) {
            println("Setting "+labelArray.length+" label(s)")
            for (n <- 0 until labelArray.length) {
              sendLine(out, "--" + BOUNDARY);
              sendLine(out, "content-disposition: form-data; name=\"label\"")
              sendLine(out, "")
              sendLine(out, labelArray(n).trim)
            }
          }
        }

        println("Sending file... "+targetFileName)
        sendLine(out, "--" + BOUNDARY)
        sendLine(out, 
          "content-disposition: form-data; name=\"filename\"; filename=\""
            + targetFileName + "\"")
        sendLine(out, "Content-Type: application/octet-stream")
        sendLine(out, "")
        val buf = new Array[Byte](8192);
        var count = in.read(buf)
          while ( count >= 0 ) {
          out.write(buf, 0, count)
          count = in.read(buf)
        }
        in.close
        sendLine(out, "")
        sendLine(out, "--" + BOUNDARY + "--")
        
        out.flush
        out.close
        
        // For whatever reason, you have to read from the input stream before
        // the url connection will start sending
        in = conn.getInputStream
        
        println("Upload finished. Reading response.")
        
        println("HTTP Response Headers: " + conn.getHeaderFields)
        val responseBody = new StringBuilder
        count = in.read(buf)
        while (count >= 0 ) {
          responseBody.append(new String(buf, 0, count, "ascii"))
          count = in.read(buf)
        }
        println(responseBody.toString)
        in.close
        
        conn.disconnect
        None
      } catch {
        case e => Some(e.toString)
      }
    }
}
