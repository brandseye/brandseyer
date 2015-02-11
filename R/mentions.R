#' Read mentions from your account
mentions <- function(account, ...) {
    UseMethod("mentions", account)
}

#' @describeIn mentions
#' @examples
#' # Read mentions using the default authentication
#' mentions("QUIR01BA", "published inthelast day")
mentions.character <- function(code, filter, authentication = defaultAuthentication) {
    url <- paste0("https://api.brandseye.com/rest/accounts/", code, "/mentions")
    data <- httr::GET(url, httr::authenticate(authentication$user, authentication$password), 
                      query = list(filter = filter))    
    results <- jsonlite::fromJSON(httr::content(data, "text"))
    results$data
}

#' @describeIn mentions
mentions.brandseye.account <- function(account, filter) {
    mentions(account$code, filter, account$auth)
}