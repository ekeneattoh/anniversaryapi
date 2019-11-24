package controllers

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import javax.inject._
import play.api.mvc._
import play.libs.ws.WSClient
import play.api.libs.json.{JsValue, Json}
import models.NchetaHelper._
import play.api.Logger


class NchetaRequestController @Inject()(cc: ControllerComponents, ws: WSClient) extends AbstractController(cc) {

  def handleAnniversaryRequest(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>

    //get the body from the request as json
    val body: AnyContent = request.body

    val containsValidJson = validateJsonInput(body)

    var api_error_msg: String = "Something went wrong!"

    if (containsValidJson.equals(false)) {

      api_error_msg = "You must supply JSON data! Look at the API reference for more information"

      Status(400)(Json.parse(
        s"""
           |{
           |   "result": "$api_error_msg"
           |}
        """.stripMargin))

    }
    else {

      val jsonBody: JsValue = body.asJson.getOrElse(Json.parse(
        s"""
           |{
           |   "data": "$api_error_msg"
           |}
        """.stripMargin))

      val requiredElements: List[String] = List("clientName", "clientEmail", "recipientName",
        "recipientEmail", "customMessage", "anniversaryDate", "imageFile")


      /*
        retrieve the data from the request and make sure all required parameters are present
    */
      val isRequestBodyValid: (Boolean, String) = validateRequiredParameters(requiredElements, jsonBody)

      if (isRequestBodyValid._1.equals(false)) {

        api_error_msg = isRequestBodyValid._2 + " is required"

        Status(400)(Json.parse(
          s"""
             |{
             |   "result": "$api_error_msg"
             |}
        """.stripMargin))
      }
      else {

        //save the data to storage
        val storageResponse: String = Await.result(sendDataToStorage(jsonBody, ws), Duration.Inf)
        //      Logger.logger.debug(storageResponse)

        //error check and return the result
        if (storageResponse.equals("OK")) {

          Ok(Json.parse(
            s"""
               |{
               |   "result": "$storageResponse"
               |}
        """.stripMargin))

        }
        else {
          Status(400)(Json.parse(
            s"""
               |{
               |   "result": "error"
               |}
        """.stripMargin))
        }
      }

    }
  }

  def handleRetrieveAnniversaryMessage(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>

    //get the body from the request as json
    val body: AnyContent = request.body

    val containsValidJson = validateJsonInput(body)

    var api_error_msg: String = "Something went wrong!"

    if (containsValidJson.equals(false)) {

      api_error_msg = "You must supply JSON data! Look at the API reference for more information"

      Status(400)(Json.parse(
        s"""
           |{
           |   "result": "$api_error_msg"
           |}
        """.stripMargin))

    }
    else {

      val jsonBody: JsValue = body.asJson.getOrElse(Json.parse(
        s"""
           |{
           |   "data": "$api_error_msg"
           |}
        """.stripMargin))

      val requiredElements: List[String] = List("fileName")

      /*
        retrieve the data from the request and make sure all required parameters are present
    */
      val isRequestBodyValid: (Boolean, String) = validateRequiredParameters(requiredElements, jsonBody)

      Logger.logger.info(isRequestBodyValid._2)

      if (isRequestBodyValid._1.equals(false)) {

        api_error_msg = isRequestBodyValid._2 + " is required"

        Status(400)(Json.parse(
          s"""
             |{
             |   "result": "$api_error_msg"
             |}
        """.stripMargin))
      }
      else {
        //get data from storage

        //get the file name as a string and pass to getDataFromStrorage method
        val fileName: String = (jsonBody \ "fileName").get.as[String]

        val storageResponse: (String, Int) = Await.result(getDataFromStorage(fileName, ws), Duration.Inf)

        //error check and return the result
        if (storageResponse._2.equals(200)) {

          val firebaseResponse: JsValue = Json.parse(storageResponse._1)

          val clientName: String = (firebaseResponse \ "clientName").get.as[String]
          val anniversaryDate: String = (firebaseResponse \ "anniversaryDate").get.as[String]
          val customMessage: String = (firebaseResponse \ "customMessage").get.as[String]
          val recipientName: String = (firebaseResponse \ "recipientName").get.as[String]
          val uniqueJsonFileName: String = (firebaseResponse \ "uniqueJsonFileName").get.as[String]
          val recipientEmailEncrypted: String = (firebaseResponse \ "recipientEmail").get.as[String]
          val imageFileEncrypted: String = (firebaseResponse \ "imageFile").get.as[String]

          //prepare JSON bodies for decryption
          val recipientEmailEncryptedData = Json.obj(
            "encrypted_data" -> recipientEmailEncrypted
          )

          val imageFileEncryptedData = Json.obj(
            "encrypted_data" -> imageFileEncrypted
          )

          val recipientEmailDecrypted: String = Await.result(decryptDataWithSecurityAPI(recipientEmailEncryptedData, ws), Duration.Inf)

          val imageFileDecrypted: String = Await.result(decryptDataWithSecurityAPI(imageFileEncryptedData, ws), Duration.Inf)

          val jsonResult = Json.obj(
            "anniversaryDate" -> anniversaryDate,
            "clientName" -> clientName,
            "customMessage" -> customMessage,
            "recipientEmail" -> recipientEmailDecrypted,
            "recipientName" -> recipientName,
            "uniqueJsonFileName" -> uniqueJsonFileName,
            "imageFile" -> imageFileDecrypted
          )

          Ok(Json.parse(
            s"""
               |{
               |   "result": ${jsonResult}
               |}
        """.stripMargin))
        }
        else {
          Status(400)(Json.parse(
            s"""
               |{
               |   "result": "error"
               |}
        """.stripMargin))
        }

      }
    }

  }

}
