base_url_calls = function(account_sid, call_sid = NULL, JSON = TRUE) {
  calls = "Calls"
  if(!is.null(call_sid)) {
    calls = paste(calls, call_sid, sep = "/")
  }
  if(JSON) {
    calls = paste0(calls, ".json")
  }

  paste(base_url(account_sid), calls, sep = "/")
}

call = function(
  to, 
  from = Sys.getenv("twilio_account_number"), 
  url = "http://demo.twilio.com/docs/voice.xml",
  application_sid = NULL,
  account_sid = Sys.getenv("twilio_account_sid"), 
  account_auth = Sys.getenv("twilio_account_auth"),
  method = NULL,
  fallback_url = NULL,
  fallback_method = NULL,
  status_callback = NULL,
  status_callback_method = NULL,
  status_callback_event = NULL,
  send_digits = NULL,
  if_machine = NULL,
  timeout = NULL,
  record = NULL
) {

  resp = call_request(
    to = to,
    from = from,
    url = url,
    application_sid = application_sid,
    account_sid = account_sid,
    account_auth = account_auth,
    method = method,
    fallback_url = fallback_url,
    fallback_method = fallback_method,
    status_callback = status_callback,
    status_callback_method = status_callback_method,
    status_callback_event = status_callback_event,
    send_digits = send_digits,
    if_machine = if_machine,
    timeout = timeout,
    record = record
  )

  #check for errors
  content = call_request_parse(resp)

  content
}

call_request = function(
  to, 
  from = Sys.getenv("twilio_account_number"),
  url = "http://demo.twilio.com/docs/voice.xml",
  application_sid = NULL,
  account_sid = Sys.getenv("twilio_account_sid"), 
  account_auth = Sys.getenv("twilio_account_auth"),
  method = NULL,
  fallback_url = NULL,
  fallback_method = NULL,
  status_callback = NULL,
  status_callback_method = NULL,
  status_callback_event = NULL,
  send_digits = NULL,
  if_machine = NULL,
  timeout = NULL,
  record = NULL
) {
  resp = httr::POST(
    url = base_url_calls(account_sid),
    body = call_request_body(
      to = to,
      from = from,
      url = url,
      application_sid = application_sid,
      method = method,
      fallback_url = fallback_url,
      fallback_method = fallback_method,
      status_callback = status_callback,
      status_callback_method = status_callback_method,
      status_callback_event = status_callback_event,
      send_digits = send_digits,
      if_machine = if_machine,
      timeout = timeout,
      record = record
    ),
    config = httr::authenticate(account_sid, account_auth)
  )

  resp
}

call_request_parse = function(resp) {

}


call_request_body = function(
  to, 
  from = Sys.getenv("twilio_account_number"),
  url = "http://demo.twilio.com/docs/voice.xml",
  application_sid = NULL,
  method = NULL,
  fallback_url = NULL,
  fallback_method = NULL,
  status_callback = NULL,
  status_callback_method = NULL,
  status_callback_event = NULL,
  send_digits = NULL,
  if_machine = NULL,
  timeout = NULL,
  record = NULL
  ) {

  list(
    From = from,
    To = to,
    Url = url,
    ApplicationSid = application_sid,
    Method = method,
    FallbackUrl = fallback_url,
    FallbackMethod = fallback_method,
    StatusCallback = status_callback,
    StatusCallbackMethod = status_callback_method,
    StatusCallbackEvent = status_callback_event,
    SendDigits = send_digits,
    IfMachine = if_machine,
    Timeout = timeout,
    Record = record
  )
}

call_list_request = function(
  call_sid = NULL,
  account_sid = Sys.getenv("twilio_account_sid"), 
  account_auth = Sys.getenv("twilio_account_auth")
) {

  resp = httr::GET(
    url = base_url_calls(account_sid, call_sid),
    config = httr::authenticate(account_sid, account_auth)
  )

  resp
}

call_list_parse = function(resp) {
  content = httr::content(resp, as = "text")
  json = jsonlite::fromJSON(content)

  json$calls ## need to check for errors
}