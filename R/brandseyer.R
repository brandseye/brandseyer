#' Provides access to a BrandsEye account
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
        data <- GET(url, authenticate(account$user, account$password))    
        account$data <- content(data)        
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

count <- function(account, ...) {
    UseMethod("count", account)
}

count.brandseye.account <- function(account, filter = NULL) {
    url <- paste0("https://api.brandseye.com/rest/accounts/", account$code, "/mentions/count")
    query <- list()
    if (!is.null(filter)) query <- list(filter = filter, groupby="published")
    
    data <- GET(url, authenticate(account$user, account$password), query = query)    
    fromJSON(content(data, "text"))
}