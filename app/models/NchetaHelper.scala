package models

import java.time.LocalDateTime
import play.api.mvc.AnyContent

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Random
import play.api.libs.json._
import play.libs.ws.WSClient


object NchetaHelper {

  def sendDataToSecurityAPIEncryption(unencryptedData: JsValue, ws: WSClient): Future[String] = {

    val securityServiceUrl = "https://securityserviceapi.herokuapp.com/encrypt"
    //    val securityServiceUrl = "http://127.0.0.1:5000/encrypt"

    val futureResponse: Future[String] = ws.asScala().url(securityServiceUrl).post(unencryptedData)
      .map { response =>
        //        Logger.logger.info(response.toString())
        (response.json \ "data").as[String]
      }(ExecutionContext.global)

    futureResponse

  }

  def decryptDataWithSecurityAPI(encryptedData: JsValue, ws: WSClient): Future[String] = {

    val securityServiceUrl = "https://securityserviceapi.herokuapp.com/decrypt"
    //    val securityServiceUrl = "http://127.0.0.1:5000/decrypt"

    val futureResponse: Future[String] = ws.asScala().url(securityServiceUrl).post(encryptedData)
      .map { response =>
        //        Logger.logger.info(response.toString())
        (response.json \ "data").as[String]
      }(ExecutionContext.global)

    futureResponse

  }

  def prepareData(jsonBody: JsValue, ws: WSClient): JsObject = {

    /*

     In order to comply with GDPR, we are going to encrypt sensitive data and only decrypt it when we need
     to retrieve it again.
     We will use the microservice API https://securityserviceapi.herokuapp.com/ which I (ekeneattoh@gmail.com) created
    */

    val clientEmail: String = (jsonBody \ "clientEmail").get.as[String]
    val recipientEmail: String = (jsonBody \ "recipientEmail").get.as[String]
    val imageFile: String = (jsonBody \ "imageFile").get.as[String]

    val clientEmailEncryptionData = Json.obj(

      "plain_data" -> clientEmail
    )

    val recipientEmailEncryptionData = Json.obj(

      "plain_data" -> recipientEmail
    )

    val imageFileEncryptionData = Json.obj(

      "plain_data" -> imageFile
    )

    //encrypt sensitive data and get the values
    val encryptedClientEmail: String = Await.result(sendDataToSecurityAPIEncryption(clientEmailEncryptionData, ws), Duration.Inf)

    val encryptedRecipientEmail: String = Await.result(sendDataToSecurityAPIEncryption(recipientEmailEncryptionData, ws), Duration.Inf)

    val encryptedImageFile: String = Await.result(sendDataToSecurityAPIEncryption(imageFileEncryptionData, ws), Duration.Inf)

    //extract data and send to Firebase
    val clientName: String = (jsonBody \ "clientName").get.as[String]
    val recipientName: String = (jsonBody \ "recipientName").get.as[String]
    val anniversaryDate: String = (jsonBody \ "anniversaryDate").get.as[String]
    val customMessage: String = (jsonBody \ "customMessage").get.as[String]

    //remove "." and whitespace and colons from the name since Firebase does not accept it
    val uniqueJsonFileName: String = ( Random.alphanumeric.take(4).mkString + "-ncheta" + LocalDateTime.now()).filter(!".".contains(_))
      .filter(!" ".contains(_)).filter(!":".contains(_))
    //    Logger.logger.info(uniqueJsonFileName)


    val anniversaryJsonData = Json.obj(
      "clientName" -> clientName,
      "recipientName" -> recipientName,
      "recipientEmail" -> encryptedRecipientEmail,
      "anniversaryDate" -> anniversaryDate,
      "customMessage" -> customMessage,
      "imageFile" -> encryptedImageFile,
      "uniqueJsonFileName" -> uniqueJsonFileName
    )

    anniversaryJsonData
  }

  def sendDataToStorage(jsonBody: JsValue, ws: WSClient): Future[String] = {

    val jsonData: JsValue = prepareData(jsonBody, ws)


    val uniqueJsonFileName: String = (jsonData \ "uniqueJsonFileName").get.as[String]

    val dbEndpoint: String = s"https://anniversaryapi.firebaseio.com/anniversary/$uniqueJsonFileName.json?auth=GlCoXjk1gRIC8YxTuIY7ikUU02LXuNFYRS2scc7e"

    val futureResponse: Future[String] = ws.asScala().url(dbEndpoint).put(jsonData)
      .map { response =>
        //        Logger.logger.info(response.body)
        response.statusText
      }(ExecutionContext.global)

    futureResponse
  }

  def getDataFromStorage(fileName: String, ws: WSClient): Future[(String, Int)] = {

    //    Logger.logger.info(fileName)

    val dbEndpoint: String = s"https://anniversaryapi.firebaseio.com/anniversary/$fileName.json?auth=GlCoXjk1gRIC8YxTuIY7ikUU02LXuNFYRS2scc7e"

    val futureResponse: Future[(String, Int)] = ws.asScala().url(dbEndpoint).get()
      .map { response =>
        //        Logger.logger.info(response.body)
        (response.body, response.status)
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
