# Copyright (c) 2015, Brandseye PTY (LTD) 
# 
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#     
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#' Provides access to a BrandsEye account
#' 
#' @details
#' Creates an object representing a BrandsEye account. It can be used to easily perform
#' various queries on the account. 
#' 
#' \code{summary} can be used to briefly summarise an account.
#' 
#' @examples
#' \dontrun{
#' ac <- account("QUIR01BA", user = "rudy.neeser@@brandseye.com", 
#'               password = "This is not my real password")
#' account("QUIR01BA", key="<api key here>")        
#' 
#' # Have a brief summary of an account
#' summary(ac)      
#' }
account <- function (code, key = NULL, user = NULL, password = NULL,
                     auth = pkg.env$defaultAuthentication) {        
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
    lastMonthVolume <- count(account, "published inthelast month", groupby="relevancy")
    total <- sum(lastMonthVolume$count)
    irrelevant <- dplyr::filter(lastMonthVolume, relevancy == 'IRRELEVANT')$count
    relevant <- total - irrelevant
    
    counts <- matrix(c(total, relevant, irrelevant), ncol = 1)    
    rownames(counts) <- c("Total", "Relevant", "Irrelevant")
    colnames(counts) <- c("Mentions")  
    
    published <- count(account, "published inthelast month", groupby="published", 
                       include="engagement")
    averages <- matrix(c(mean(published$count), 
                         sd(published$count),
                         mean(published$engagement),
                         sd(published$engagement)), 
                       ncol = 1)
    rownames(averages) <- c("Count mean", "Count SD", "Engagement mean", "Engagement SD")
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

account.name.character <- function(accounts) {
    sapply(accounts, function(ac) {
        account.name(account(ac))
    })
}

'account.name<-' <- function(account, value) {
    UseMethod('account.name<-', account)
}

'account.name<-.brandseye.account' <- function(account, value) {
    account$name <- value
    account
}

#' Listing brands in an account
#' 
#' This returns a \code{data.frame} containing brand IDs, their names,
#' whether they've been deleted or not, and the ID of the parent brand.
account.brands <- function(account) {
    UseMethod("account.brands", account)
}

#' @describeIn account.brands
account.brands.brandseye.account <- function(account) {
    id <- integer()
    name <- character()
    deleted <- logical()
    parents <- integer()
    
    recurse <- function(brand, parent = NA) {    
        id <<- c(id, brand$id)
        name <<- c(name, brand$name)
        deleted <<- c(deleted, ifelse(is.null(brand$deleted) || brand$deleted == FALSE, FALSE, TRUE))
        parents <<- c(parents, parent)
        
        if (length(brand$children)) {
            for (b in brand$children) {
                recurse(b, parent = brand$id)
            }
        }
    }
    
    for (b in account$data$brands) {
        recurse(b)
    }
    
    
    data.frame(id = id, name = name, deleted = deleted, parent = parents)
}

#' @describeIn account.brands
#' 
#' @examples
#' 
#' \dontrun{
#' # Fetching brands for a particular account
#' account.brands("QUIR01BA")
#' }
#' 
account.brands.character <- function(account) {
    account.brands(account(account))
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
listAccounts <- function(auth = pkg.env$defaultAuthentication, key = NULL, user = NULL, password = NULL) {
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
listAccountCodes <- function(auth = pkg.env$defaultAuthentication, key = NULL, user = NULL, password = NULL) {
    listAccounts(auth, key, user, password)$code
}