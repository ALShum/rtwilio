base_url_messages = function(account_sid, message_sid = NULL, JSON = TRUE) {
  messages = "Messages"
  if(!is.null(message_sid)) {
    messages = paste(messages, message_sid, sep = "/")
  }
  if(JSON) {
    messages = paste0(messages, ".json")
  }
  paste(base_url(account_sid), messages, sep = "/")
}

message_list = function(
  message_sid = NULL,
  account_sid = Sys.getenv("twilio_account_sid"),
  account_auth = Sys.getenv("twilio_account_auth")
) {

  resp = message_list_request(message_sid, account_sid, account_auth)  

  # check for errors
  content = message_list_parse(resp, message_sid)

  content
}

message_list_request = function(
  message_sid = NULL,
  account_sid = Sys.getenv("twilio_account_sid"),
  account_auth = Sys.getenv("twilio_account_auth")
) {
  resp = httr::GET(
    url = base_url_messages(account_sid, message_sid),
    config = httr::authenticate(account_sid, account_auth)
  )

  resp
}

message_list_parse = function(resp, message_sid = NULL) {
  content = httr::content(resp, as = "text")
  json = jsonlite::fromJSON(content)

  if(resp$status_code > 399) {
    return(paste0(json$status, ": ", json$message))
  }

  if(is.null(message_sid)) return(json$messages)
  else {
    col_names = paste(names(json), collapse = ";")

    L = lapply(json, function(x) {
      if(is.list(x)) x = x[[1]]
      if(is.null(x)) return(NA)
      else return(x)
    })

    L =  paste(
      col_names,
      paste(L, collapse = ";"),
        sep = "\n"
    )
    L = readr::read_delim(L, delim = ";")

    return(L)
  }
}

message_redact = function(
  message_sid,
  account_sid = Sys.getenv("twilio_account_sid"),
  account_auth = Sys.getenv("twilio_account_auth")
) {
  resp = message_redact_request(message_sid, account_sid, account_auth)

  #check for errors
  content = message_redact_parse(resp, message_sid)

  content
}

message_redact_request = function(
  message_sid,
  account_sid = Sys.getenv("twilio_account_sid"),
  account_auth = Sys.getenv("twilio_account_auth")
) {
  resp = httr::POST(
    url = base_url_messages(account_sid, message_sid),
    config = httr::authenticate(account_sid, account_auth),
    body = list(Body = "")
  )

  resp
}

message_redact_parse = function(resp, message_sid) {
  message_list_parse(resp, message_sid)
}

message_delete = function(
  message_sid,
  account_sid = Sys.getenv("twilio_account_sid"),
  account_auth = Sys.getenv("twilio_account_auth")
) {
  resp = message_delete_request(message_sid, account_sid, account_auth)

  #check for errors
  content = message_delete_parse(resp, message_sid)

  content
}

message_delete_request = function(
  message_sid,
  account_sid = Sys.getenv("twilio_account_sid"),
  account_auth = Sys.getenv("twilio_account_auth")
) {
  resp = httr::DELETE(
    url = base_url_messages(account_sid, message_sid),
    config = httr::authenticate(account_sid, account_auth)
  )

  resp
}

message_delete_parse = function(resp, message_sid = NULL) {
  if(resp$status_code < 400) return(paste0(resp$status_code, ", message: ", message_sid, " deleted.")) 

  content = httr::content(resp, as = "text")
  json = jsonlite::fromJSON(content)
  paste0(json$status, ": ", json$message)
}