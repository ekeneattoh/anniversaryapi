package models

case class NchetaMessage(clientName: String, clientEmail: String, recipientName: String, recipientEmail: String,
                         customMessage: String, anniversaryDate: String, imageFile: String)
