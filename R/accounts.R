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
    output <- matrix(c(account$code, account.name(account), account$auth$user), ncol = 1)
    colnames(output) <- ""
    rownames(output) <- c("Code", "Name", "Login")    
    print(output, quote=F)
}

#' Summarising accounts
#' 
#' \code{summary} method for class "\code{brandseye.account}".
summary.brandseye.account <- function(account, ...) {
    lastMonthVolume <- count(ac, "published inthelast month", groupby="relevancy")
    total <- sum(lastMonthVolume$count)
    irrelevant <- lastMonthVolume[lastMonthVolume$relevancy == 'IRRELEVANT',]$count
    relevant <- total - irrelevant
    
    counts <- matrix(c(total, relevant, irrelevant), ncol = 1)
    rownames(counts) <- c("Total", "Relevant", "Irrelevant")
    colnames(counts) <- c("Mentions")  
    
    published <- count(ac, "published inthelast month", groupby="published", 
                       include="engagement")
    averages <- matrix(c(mean(published$count), 
                         sd(published$count),
                         mean(published$engagement),
                         sd(published$engagement)), 
                       ncol = 1)
    rownames(averages) <- c("Mean count", "SD count", "Mean engagement", "SD engagement")
    colnames(averages) <- ""
    
    structure(list(
        account = account,
        counts = counts,
        averages = averages
    ), class = "summary.brandseye.account")
}

print.summary.brandseye.account <- function(s.ac, ...) {
    cat("BrandsEye Account:\n")
    print(s.ac$account)
    cat("\n")
    cat("Summary of the last month:\n")
    print(s.ac$counts)
    cat("\nDaily averages:\n")
    print(round(s.ac$averages, 2))
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