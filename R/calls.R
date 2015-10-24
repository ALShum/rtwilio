base_url_calls = function(base_url = base_url, account_sid) {
  paste(base_url(account_sid), "Calls", sep = "/")
}

call = function(
  to, 
  from, 
  url,
  application_sid,

  account_sid = Sys.getenv("twilio_account_sid"), 
  account_auth = Sys.getenv("twilio_account_auth"),
  StatusCallback = NULL,
  ApplicationSid = NULL,
  MaxPrice = NULL) {

}

call_request = function(
  to, 
  from,
  url,
  application_sid,
  
  account_sid = Sys.getenv("twilio_account_sid"), 
  account_auth = Sys.getenv("twilio_account_auth"),
  StatusCallback = NULL,
  ApplicationSid = NULL,
  MaxPrice = NULL) {

}

call_request_parse = function(resp) {

}
