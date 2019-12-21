package controllers

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import javax.inject._
import play.api.mvc._
import play.libs.ws.WSClient
import play.api.libs.json.{JsError, JsPath, JsResult, JsSuccess, JsValue, Json, Reads}
import models.NchetaHelper._
import play.api.Logger
import models.NchetaMessage
import play.api.libs.functional.syntax._


class NchetaRequestController @Inject()(cc: ControllerComponents, ws: WSClient) extends AbstractController(cc) {

  def handleAnniversaryRequest(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>

    //get the body from the request as json
    val body: AnyContent = request.body

    val containsValidJson = validateJsonInput(body)

    var api_error_msg: String = "Please supply valid JSON Body with the required fields! Look through our API guide for more information."

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
           |   "result": "$api_error_msg"
           |}
        """.stripMargin))


      implicit val nchetaMsgReads: Reads[NchetaMessage] = (
        (JsPath \ "clientName").read[String] and
          (JsPath \ "clientEmail").read[String] and
          (JsPath \ "recipientName").read[String] and
          (JsPath \ "recipientEmail").read[String] and
          (JsPath \ "customMessage").read[String] and
          (JsPath \ "anniversaryDate").read[String] and
          (JsPath \ "imageFile").read[String]
        ) (NchetaMessage.apply _)

      val nchetaMsgReadsResult: JsResult[NchetaMessage] = jsonBody.validate[NchetaMessage]

      nchetaMsgReadsResult match {

        case JsSuccess(_, _) =>

          val storageResponse: JsValue = Await.result(sendDataToStorage(jsonBody, ws), Duration.Inf)

          val storageResult: JsResult[String] = (storageResponse \ "data").validate[String]

          println(storageResponse)

          storageResult match {

            case JsSuccess(_, _) => Ok(Json.parse(
              s"""
                 |{
                 |   "result": $storageResponse
                 |}
                  """.stripMargin))

            case _: JsError =>

              api_error_msg = (storageResponse \ "error").get.as[String]

              Status(400)(Json.parse(
                s"""
                   |{
                   |   "result": "$api_error_msg"
                   |}
                """.stripMargin))

          }

        case _: JsError =>

          Status(400)(Json.parse(
            s"""
               |{
               |   "result": "$api_error_msg"
               |}
                """.stripMargin))


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
           |   "result": "$api_error_msg"
           |}
        """.stripMargin))


      //validate the jsonBody to see if it contains event_name
      val jsonBodyInput: JsResult[String] = (jsonBody \ "fileName").validate[String]

      jsonBodyInput match {

        case JsSuccess(_, _) =>

          val fileName: String = (jsonBody \ "fileName").get.as[String]

          val storageResponse: JsValue = Await.result(getDataFromStorage(fileName, ws), Duration.Inf)

          val storageResponseResult: JsResult[JsValue] = (storageResponse \ "data").validate[JsValue]

          storageResponseResult match {

            case JsSuccess(_, _) =>

              val clientName: String = (storageResponse \ "data" \ "clientName").get.as[String]
              val anniversaryDate: String = (storageResponse \ "data" \ "anniversaryDate").get.as[String]
              val customMessage: String = (storageResponse \ "data" \ "customMessage").get.as[String]
              val recipientName: String = (storageResponse \ "data" \ "recipientName").get.as[String]
              val uniqueJsonFileName: String = (storageResponse \ "data" \ "uniqueJsonFileName").get.as[String]
              val recipientEmailEncrypted: String = (storageResponse \ "data" \ "recipientEmail").get.as[String]
              val imageFileEncrypted: String = (storageResponse \ "data" \ "imageFile").get.as[String]


              //prepare values for decryption
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

            case _: JsError =>

              api_error_msg = (storageResponse \ "error").get.as[String]
              Status(400)(Json.parse(
                s"""
                   |{
                   |   "result": ${api_error_msg}
                   |}
        """.stripMargin))
          }

        case _: JsError =>

          Status(400)(Json.parse(
            s"""
               |{
               |   "result": "$api_error_msg"
               |}
            """.stripMargin))


      }


    }

  }
}
