base_url_sms = function(account_sid, JSON = TRUE) {
  messages = "Messages"
  if(JSON) {
    messages = paste0(messages, ".json")
  } 
  paste(base_url(account_sid), messages, sep = "/")
}

sms = function(
  message, 
  to, 
  from = Sys.getenv("twilio_account_number"), 
  media_url = NULL, 
  account_sid = Sys.getenv("twilio_account_sid"), 
  account_auth = Sys.getenv("twilio_account_auth"),
  status_callback = NULL,
  application_sid = NULL,
  max_price = NULL
  ) {

  resp = sms_request(
    message, 
    to, 
    from, 
    media_url, 
    account_sid, 
    account_auth, 
    status_callback, 
    application_sid, 
    max_price)

  # check for errors

  content = sms_parse(resp)

  content
}

sms_request = function(
  message, 
  to, 
  from, 
  media_url = NULL, 
  account_sid = Sys.getenv("twilio_account_sid"), 
  account_auth = Sys.getenv("twilio_account_auth"),
  status_callback = NULL,
  application_sid = NULL,
  max_price = NULL
  ) {
  
  resp = httr::POST(
    url = base_url_sms(account_sid = account_sid),
    body = sms_request_body(
      message, 
      to, 
      from, 
      media_url, 
      status_callback, 
      application_sid, 
      max_price),
    config = httr::authenticate(account_sid, account_auth),
    httr::add_headers(`Accept` = "application/json")
  )

  resp
}

sms_request_body = function(
  message,
  to,
  from,
  media_url,
  status_callback,
  application_sid,
  max_price
  ) {

  list(
    From = from,
    To = to,
    Body = message,
    MediaUrl = media_url,
    StatusCallback = status_callback,
    ApplicationSid = application_sid,
    MaxPrice = max_price
  )
}



sms_parse = function(resp) {
  content = httr::content(resp)
  resource_uri = content$uri
  message_sid = content$sid

  return(message_sid)
}
