base_url = function(account_sid) {
  paste("https://api.twilio.com/2010-04-01/Accounts", account_sid, sep = "/")
}


number = function(number) {
  number = stringr::str_replace_all(number, "[^0-9+]", "")
  if(stringr::str_detect(number, "[+]")) return(number)

  paste0("+", number)
}

#' message_standard
#'
#' This is a recreation of the message used in the \code{httr} package.
#'
#' @param x    a request object
#'
#' @return character, single string concerning the request, suitable input to \code{cat()}
#' @examples
#'   x <- httr::GET("http://httpbin.org/status/200")
#'   message_standard(x)
#'
#' @export
#'
message_standard <- function(x){

  status <- httr::status_code(x)
  message <- httr::http_status(status)$message

  message
}

#' message_verbose
#'
#' A more-verbose message.
#'
#' This returns a more verbose message that includes:
#'
#' \itemize{
#'   \item standard message
#'   \item date
#'   \item url
#'   \item detailed message
#' }
#'
#' @param x    a request object
#'
#' @return single string concerning the request, suitable input to \code{cat()}
#' @examples
#'   x <- httr::GET("http://httpbin.org/status/200")
#'   message_verbose(x)
#'
#' @export
#'
message_verbose <- function(x){

  message <- paste(
    message_standard(x),
    paste("date", httr::headers(x)$date, sep = ": "),
    paste("url", x$url, sep = ": "),
    sep = "\n"
  )

  message
}



check_for_status <- function(
  x, type = "error", message_function = message_verbose, ...){

  # validate type
  type <- match.arg(type, c("error", "warning", "message"))

  # check to see if we can return without doing anything
  if (httr::status_code(x) < 300) 
    return(invisible(x))

  # define the possible actions
  action <- list(error = stop, warning = warning, message = message)

  # build the message
  msg <- message_function(x, ...)
  
  i_call <- -1

  # if the calling list is longer than one, and we are called by "do.call", 
  # refer to the call that called "do.call" (whew!)
  if (length(sys.calls()) > 1){
    if (as.character(sys.call(-1))[[1]] == "do.call"){
      i_call <- -2
    }
  }

  # act
  action[[type]](httr::http_condition(x, type, message = msg, call = sys.call(i_call)))

  invisible(x)
}
