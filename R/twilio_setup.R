get_twilio_sid = function() {
  sid = Sys.getenv('twilio_account_sid')
  if(!identical(sid, "")) return(sid)

  if(!interactive()) {
    stop("Please set environ var twilio_account_sid to your account sid.", call. = FALSE)
  }
  
  message("Please enter your account sid and press enter:")
  sid = readline(": ")

  if(identical(sid, "")) {
    stop("Invalid account sid!", call. = FALSE)
  }

  Sys.setenv(twilio_account_sid = sid)
  
  Sys.getenv("twilio_account_sid")
}

get_twilio_auth = function() {
  auth = Sys.getenv('twilio_account_auth')
  if(!identical(auth, "")) return(auth)

  if(!interactive()) {
    stop("Please set environ var twilio_account_auth to your account auth.", call. = FALSE)
  }
  
  message("Please enter your account auth and press enter:")
  auth = readline(": ")

  if(identical(auth, "")) {
    stop("Invalid account auth!", call. = FALSE)
  }

  Sys.setenv(twilio_account_auth = auth)

  Sys.getenv("twilio_account_auth")
}

get_twilio_from_number = function() {
  num = Sys.getenv('twilio_account_number')
  if(!identical(num, "")) return(num)

  if(!interactive()) {
    stop("Please set environ var twilio_account_number to your twilio account phone number.", call. = FALSE)
  }
  
  message("Please enter your twilio assigned phone number and press enter:")
  num = readline(": ")

  if(identical(num, "")) {
    stop("Invalid account phone number!", call. = FALSE)
  }

  Sys.setenv(twilio_account_number = num)

  Sys.getenv("twilio_account_number")
}

#' Sets the Twilio login credentials
#'
#' @param account_sid Account SID for Twilio
#' @param account_auth Account auth for Twilio
#' @param account_number Assigned number for Twilio
#' @return Twilio account SID and auth token
#' @export 
#' @examples
#' \dontrun{
#'   set_twilio_credentials("account_sid", "account_auth")
#' }
set_twilio_credentials = function(
  account_sid = NULL, 
  account_auth = NULL, 
  account_number = NULL) {

  if(!is.null(account_sid)) {
    if(identical(account_sid, "")) {
      stop("Invalid account_sid!", call. = FALSE)
    }
    Sys.setenv(twilio_account_sid = account_sid)
  }

  if(!is.null(account_auth)) {
    if(identical(account_auth, "")) {
      stop("Invalid account_auth!", call. = FALSE)
    }
    Sys.setenv(twilio_account_auth = account_auth)
  }

  if(!is.null(account_number)) {
    if(identical(account_number, "")) {
      stop("Invalid account_number!", call. = FALSE)
    }
    Sys.setenv(twilio_account_number = account_number)
  }

  list(
    account_sid = Sys.getenv("twilio_account_sid"),
    account_auth = Sys.getenv("twilio_account_auth"),
    account_number = Sys.getenv("twilio_account_number")
  )
}

is_twilio_setup = function() {
  !identical(Sys.getenv("twilio_account_sid"), "") &
  !identical(Sys.getenv("twilio_account_auth"), "") &
  !identical(Sys.getenv("twilio_account_number"), "")
}