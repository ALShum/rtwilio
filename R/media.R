base_url_media = function(account_sid, message_sid, media_sid = NULL, JSON = TRUE) {
  messages = paste("Messages", message_sid, sep = "/")
  media = "Media"
  if(!is.null(media_sid)) {
    media = paste(media, media_sid, sep = "/")
  }
  if(JSON) {
    media = paste0(media, ".json")
  }
  paste(base_url(account_sid), messages, media, sep = "/")
}

media_list = function(
  message_sid, 
  media_sid = NULL, 
  account_sid = Sys.getenv("twilio_account_sid"), 
  account_auth = Sys.getenv("twilio_account_auth")
) {

  resp = media_list_request(message_sid, media_sid, account_sid, account_auth)

  # check for errors
  content = media_list_parse(resp, media_sid)

  content
}

media_list_request = function(
  message_sid, 
  media_sid = NULL, 
  account_sid = Sys.getenv("twilio_account_sid"),
  account_auth = Sys.getenv("twilio_account_auth")
) {

  resp = httr::GET(
    url = base_url_media(account_sid, message_sid, media_sid),
    config = httr::authenticate(account_sid, account_auth)
  )

  resp
}

media_list_parse = function(resp, media_sid = NULL) {
  content = httr::content(resp, as = "text")
  json = jsonlite::fromJSON(content)

  if(resp$status_code > 399) {
    return(paste0(json$status, ": ", json$message))
  }

  if(!is.null(media_sid)) {
    return(data.frame(json))
  }

  return(json$media_list)
}

media_delete = function(
  message_sid, 
  media_sid, 
  account_sid = Sys.getenv("twilio_account_sid"),
  account_auth = Sys.getenv("twilio_account_auth")
) {

  resp = media_delete_request(message_sid, media_sid, account_sid, account_auth)

  # check for errors
  content = media_delete_parse(resp, message_sid, media_sid)

  content
}

media_delete_request = function(
  message_sid, 
  media_sid,
  account_sid = Sys.getenv("twilio_account_sid"),
  account_auth = Sys.getenv("twilio_account_auth")
) {

  resp = httr::DELETE(
    url = base_url_media(account_sid, message_sid, media_sid),
    config = httr::authenticate(account_sid, account_auth)
  )

  resp
}

media_delete_parse = function(resp, message_sid, media_sid) {
  if(resp$status_code < 400) {
    return(paste0(resp$status_code, ", message: ", message_sid, ", media: ", media_sid, " deleted."))
  }

  content = httr::content(resp, as = "text")
  json = jsonlite::fromJSON(content)

  paste0(json$status, ": ", json$message)
}