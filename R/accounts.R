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
account <- function (code, auth = defaultAuthentication, key = NULL, user = NULL, password = NULL) {        
    if (is.null(auth)) auth <- authentication(user = user, password = password, key = key)
    ac <- structure(list(
        code = code,
        auth = auth
    ), class = "brandseye.account")
    
    ac <- account.load(ac)
    ac
}

print.brandseye.account <- function(account, ...) {
    cat("BrandsEye Account:", account$code, "\n")
    if (!is.null(account.name(account))) cat("Account name: ", account.name(account), "\n")
    print(account$auth)
}

account.load <- function(account) {
    if (is.null(account$data)) {
        url = paste0("https://api.brandseye.com/rest/accounts/", account$code)
        data <- httr::GET(url, httr::authenticate(account$auth$user, account$auth$password))    
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


#' List accounts you have access to
#' 
#' This returns a data frame listing the accounts that you have access to,
#' along with their name and status.
#' 
#' @examples
#' listAccounts(key = "my api key")
#' 
#' listAccounts(user = "rudy.neeser@@brandseye.com", 
#'              password = "my brandseye password")
#' 
#' auth <- authentication(user = "rudy.neeser@@brandseye.com", 
#'                        password = "my brandseye password")
#' listAccounts(auth)
listAccounts <- function(auth = defaultAuthentication, key = NULL, user = NULL, password = NULL) {
    if (is.null(auth)) auth <- authentication(key = key, user = user, password = password)
    url <- paste0("https://api.brandseye.com/rest/accounts/")
    data <- httr::GET(url, httr::authenticate(auth$user, auth$password))
    results <- jsonlite::fromJSON(httr::content(data, "text"))
    results
}

#' @describeIn listAccounts
#' 
#' Returns a character vector of account codes that you have access to.
#'
#' @examples
#' # Get the number of mentions published in the last day across all 
#' # of your accounts.
#' count(listAccountCodes(), "published inthelast day")
listAccountCodes <- function(auth = defaultAuthentication, key = NULL, user = NULL, password = NULL) {
    listAccounts(auth, key, user, password)$code
}