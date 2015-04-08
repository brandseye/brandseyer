#' Read mentions from your account
mentions <- function(account, ...) {
    UseMethod("mentions", account)
}

#' @describeIn mentions
#' @param limit The maximum number of mentions to be returned
#' @param offset Mentions are returned in an order. Offset says how many of the 
#'   first mentions should be skipped.
#' @examples
#' # Read mentions using the default authentication
#' mentions("QUIR01BA", "published inthelast day")
mentions.character <- function(code, filter, 
                               limit = 30, offset = 0,
                               include = NULL,
                               authentication = pkg.env$defaultAuthentication) {
    url <- paste0("https://api.brandseye.com/rest/accounts/", code, "/mentions")
    data <- httr::GET(url, httr::authenticate(authentication$user, authentication$password), 
                      query = list(filter = filter, limit = limit, offset = offset, include=include))    
    results <- jsonlite::fromJSON(httr::content(data, "text"), flatten=TRUE)
    results$data
}

#' @describeIn mentions
mentions.brandseye.account <- function(account, filter) {
    mentions(account$code, filter, account$auth)
}