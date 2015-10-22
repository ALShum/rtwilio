base_url_calls = function(base_url = base_url, account_sid) {
  paste(base_url(account_sid), "Calls", sep = "/")
}

call = function(
  to, 
  from, 
  media_url = NULL, 
  account_sid, 
  account_auth,
  StatusCallback = NULL,
  ApplicationSid = NULL,
  MaxPrice = NULL) {

}

call_request = function(
  to, 
  from, 
  media_url = NULL, 
  account_sid, 
  account_auth,
  StatusCallback = NULL,
  ApplicationSid = NULL,
  MaxPrice = NULL) {

}

