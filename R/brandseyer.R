#' Provides access to a BrandsEye account
#' 
#' @details
#' Creates an object representing a BrandsEye account. It can be used to easily perform
#' various queries on the account. 
#' 
#' @examples
#' ac <- account("QUIR01BA", user = "rudy.neeser@@brandseye.com", 
#'               password = "This is not my real password")
#' account("QUIR01BA", key="<api key here>")              
account <- function (code, key = NULL, user = NULL, password = NULL) {    
    if (!is.null(key) & (!is.null(user) | !is.null(password)))
        stop("You must choose to authenticate with an API key or a username / password pair")
    
    if (is.null(user)) user = "API_KEY";
    if (is.null(password)) password = key;
    if (is.null(password)) stop("Please provide an API key");
    
    ac <- structure(list(
        code = code,
        username = user,
        password = password
    ), class = "brandseye.account")
    
    ac <- account.load(ac)
    ac
}

print.brandseye.account <- function(account, ...) {
    cat("BrandsEye Account:", account$code, "\n")
    if (!is.null(account.name(account))) cat("Account name: ", account.name(account), "\n")
    cat("login: ", account$username, "\n")
}

account.load <- function(account) {
    if (is.null(account$data)) {
        url = paste0("https://api.brandseye.com/rest/accounts/", account$code)
        data <- httr::GET(url, httr::authenticate(account$user, account$password))    
        account$data <- httr::content(data)        
        account.name(account) <- account$data$name
        
    }
    account
}

account.code <- function(account) {
    account$code
}

account.name <- function(account) {
    UseMethod("account.name", account)
}

account.name.brandseye.account <- function(account) {    
    account$name
}

'account.name<-' <- function(account, value) {
    UseMethod('account.name<-', account)
}

'account.name<-.brandseye.account' <- function(account, value) {
    account$name <- value
    account
}

#' Count mentions
#' 
#' \code{count} is used to count mentions in a BrandsEye account matching
#' matching a particular filter. It's possible to group mentions, order the
#' results, and to include various other bits of useful information.
#' 
#' @param account An account object to be queried.
count <- function(account, ...) {
    UseMethod("count", account)
}

#' @describeIn count
#' @param filter A filter string describing the mentions that should be counted by this query
#' Count aggregate mention information from your BrandsEye account
#' @param groupby A list of items that should be grouped by
#' 
#' @details
#' Filters are described in the api documentation \url{https://api.brandseye.com/docs}
#' 
#' @examples
#' ac <- account("QUIR01BA", key="<my key>")
#' 
#' # A single number counting the mentions published in the last week. 
#' count(ac, "published inthelast week")
#' 
#' # The number of relevant mentions published in the last month
#' count(ac, "published inthelast month and relevancy isnt irrelevant")
#' 
#' # As above, but grouped by publication day
#' count(ac, "published inthelast month and relevancy isnt irrelevant", groupby="published")
count.brandseye.account <- function(account, filter = NULL, groupby = NULL, 
                                    include = NULL) {
    url <- paste0("https://api.brandseye.com/rest/accounts/", account$code, "/mentions/count")
    query <- list()
    if (!is.null(filter)) query <- list(filter = filter, groupby=groupby, include=include)
    
    data <- httr::GET(url, httr::authenticate(account$user, account$password), query = query)    
    results <- data.frame(jsonlite::fromJSON(content(data, "text")))
    if ("published" %in% names(results)) results <- transform(results, published = as.POSIXct(published))
    results
}



mentions <- function(account, filter) {
    url <- paste0("https://api.brandseye.com/rest/accounts/", account$code, "/mentions")
    data <- httr::GET(url, httr::authenticate(account$user, account$password), 
                      query = list(filter = filter))    
    results <- jsonlite::fromJSON(httr::content(data, "text"))
    results
}