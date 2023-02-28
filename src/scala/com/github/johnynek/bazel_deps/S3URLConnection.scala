package com.github.johnynek.bazel_deps

import com.amazonaws.AmazonServiceException
import com.amazonaws.services.s3.model.{ObjectMetadata, S3Object}
import com.github.johnynek.bazel_deps.S3URLHandler
import java.io.InputStream
import java.net.{HttpURLConnection, URL}
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter

object S3URLConnection {
  private val s3: S3URLHandler = new S3URLHandler()
}

/**
 * Implements an HttpURLConnection for compatibility with Coursier (https://github.com/coursier/coursier)
 */
final class S3URLConnection(url: URL) extends HttpURLConnection(url) {
  import S3URLConnection.s3

  private trait S3Response extends AutoCloseable {
    def meta: ObjectMetadata
    def inputStream: Option[InputStream]
  }

  private case class HEADResponse(meta: ObjectMetadata) extends S3Response {
    def close(): Unit = {}
    def inputStream: Option[InputStream] = None
  }

  private case class GETResponse(obj: S3Object) extends S3Response {
    def meta: ObjectMetadata = obj.getObjectMetadata
    def inputStream: Option[InputStream] = Option(obj.getObjectContent())
    def close(): Unit = obj.close()
  }

  private[this] var response: Option[S3Response] = None

  def connect(): Unit = {
    val (client, bucket, key) = s3.getClientBucketAndKey(url)

    try {
      response = getRequestMethod.toLowerCase match {
        case "head" => Option(HEADResponse(client.getObjectMetadata(bucket, key)))
        case "get" => Option(GETResponse(client.getObject(bucket, key)))
        case "post" => ???
        case "put" => ???
        case _ => throw new IllegalArgumentException("Invalid request method: "+getRequestMethod)
      }

      responseCode = if (response.isEmpty) 404 else 200
    } catch {
      case ex: AmazonServiceException => responseCode = ex.getStatusCode
    }

    // Also set the responseMessage (an HttpURLConnection field) for better compatibility
    responseMessage = statusMessageForCode(responseCode)
    connected = true
  }

  def usingProxy(): Boolean = Option(s3.getProxyConfiguration.getProxyHost).exists{ _ != "" }

  override def getInputStream: InputStream = {
    if (!connected) connect()
    response.flatMap{ _.inputStream }.orNull
  }

  override def getHeaderField(n: Int): String = {
    // n == 0 means you want the HTTP Status Line
    // This is called from HttpURLConnection.getResponseCode()
    if (n == 0 && responseCode != -1) {
      s"HTTP/1.0 $responseCode ${statusMessageForCode(responseCode)}"
    } else {
      super.getHeaderField(n)
    }
  }

  override def getHeaderField(field: String): String = {
    if (!connected) connect()

    field.toLowerCase match {
      case "content-type" => response.map{ _.meta.getContentType }.orNull
      case "content-encoding" => response.map{ _.meta.getContentEncoding }.orNull
      case "content-length" => response.map{ _.meta.getContentLength().toString }.orNull
      case "last-modified" => response.map{ _.meta.getLastModified }.map{ _.toInstant.atOffset(ZoneOffset.UTC) }.map{ DateTimeFormatter.RFC_1123_DATE_TIME.format }.orNull
      case _ => null // Should return null if no value for header
    }
  }

  override def disconnect(): Unit = {
    response.foreach{ _.close() }
  }

  private def statusMessageForCode(code: Int): String = {
    // I'm not sure if we care about any codes besides 200 and 404
    code match {
      case 200 => "OK"
      case 404 => "Not Found"
      case _   => "DUMMY"
    }
  }
}