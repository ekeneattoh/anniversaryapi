package models

import java.time.LocalDateTime

import play.api.Logger
import play.api.mvc.AnyContent

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Random
import play.api.libs.json._
import play.libs.ws.WSClient


object NchetaHelper {

  val SECURITY_SERVICE_URL: String = "https://securityserviceapi.herokuapp.com"
  val COLLECTION_NAME: String = "nchetaData"

  def sendDataToSecurityAPI(unencryptedData: JsValue, ws: WSClient, endpoint: String): Future[String] = {

    val encryptionEndpoint: String =  SECURITY_SERVICE_URL+"/"+endpoint

    val futureResponse: Future[String] = ws.asScala().url(encryptionEndpoint).post(unencryptedData)
      .map { response =>
        //        Logger.logger.info(response.toString())
        (response.json \ "data").as[String]
      }(ExecutionContext.global)

    futureResponse

  }

  def decryptDataWithSecurityAPI(encryptedData: JsValue, ws: WSClient): String = {

    val decrypted_string: String = Await.result(sendDataToSecurityAPI(encryptedData, ws, "decrypt"), Duration.Inf)

    decrypted_string

  }

  def encrypdataWithSecurityAPI(raw_data: String, ws: WSClient): String = {

    val encryptionData = Json.obj(

      "plain_data" -> raw_data
    )

    val encrypted_string: String = Await.result(sendDataToSecurityAPI(encryptionData, ws, "encrypt"), Duration.Inf)

    encrypted_string
  }

  def generateUniqueFileName(): String = {

    //remove "." and whitespace and colons from the name since Firebase does not accept it
    val uniqueJsonFileName: String = ( Random.alphanumeric.take(4).mkString + "-ncheta" + LocalDateTime.now())
      .filter(!".".contains(_))
      .filter(!" ".contains(_))
      .filter(!":".contains(_))

    uniqueJsonFileName

  }

  def prepareNchetaData(raw_data: JsValue, ws: WSClient): JsObject= {

    val unique_file_name = generateUniqueFileName()

    val recipient_email: String = (raw_data \ "recipientEmail").get.as[String]
    val image_file: String = (raw_data \ "imageFile").get.as[String]

    val encrypted_recipient_email: String = encrypdataWithSecurityAPI(recipient_email, ws)
    val encrypted_image_file: String = encrypdataWithSecurityAPI(image_file, ws)

    val clientName: String = (raw_data \ "clientName").get.as[String]
    val recipientName: String = (raw_data \ "recipientName").get.as[String]
    val anniversaryDate: String = (raw_data \ "anniversaryDate").get.as[String]
    val customMessage: String = (raw_data \ "customMessage").get.as[String]

    val ncheta_body = Json.obj(
      "clientName" -> clientName,
        "recipientName" -> recipientName,
        "recipientEmail" -> encrypted_recipient_email,
        "anniversaryDate" -> anniversaryDate,
        "customMessage" -> customMessage,
        "imageFile" -> encrypted_image_file,
        "uniqueJsonFileName" -> unique_file_name
    )

    ncheta_body

  }

  def sendDataToStorage(jsonBody: JsValue, ws: WSClient): Future[JsValue] = {

    val jsonData: JsValue = prepareNchetaData(jsonBody, ws)

    val uniqueJsonFileName: String = (jsonData \ "uniqueJsonFileName").get.as[String]

    val dbEndpoint: String = SECURITY_SERVICE_URL + "/" + COLLECTION_NAME + s"/$uniqueJsonFileName/add"

    val futureResponse: Future[JsValue] = ws.asScala().url(dbEndpoint).post(jsonData)
      .map { response =>
//        println(response.json)
        response.json
      }(ExecutionContext.global)

    futureResponse
  }

  def getDataFromStorage(fileName: String, ws: WSClient): Future[JsValue] = {

    val dbEndpoint: String = SECURITY_SERVICE_URL + "/" + COLLECTION_NAME + s"/$fileName/read"

    val futureResponse: Future[JsValue] = ws.asScala().url(dbEndpoint).get()
      .map { response =>
        response.json
      }(ExecutionContext.global)

    futureResponse
  }

  def validateJsonInput(requestBody: AnyContent): Boolean = {

    //check if the request body contains json data and return OK or NOT
    if(requestBody.asJson.isEmpty){

      false
    }

    else {

      true
    }
  }

  def validateRequiredParameters(parameterlist: List[String], jsonData: JsValue): (Boolean, String) = {

    var error: Boolean = true
    var missingElement: String = ""

    for (element <- parameterlist) {
      if ((jsonData \ element).asOpt[String].isEmpty) {
        error = false
        missingElement = element
      }
    }

    (error, missingElement)
  }

}

