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
#' @section Querying an account:
#' 
#' \code{\link{account_count}} allows you to pull statistics from your account 
#' matching a filter. For example,
#' 
#' \verb{
#'  ac <- account("QUIR01BA")
#'  account_count(ac, "published inthelast week", groupby="country")
#' }
#' 
#' The above sums the number of mentions per country in the given account.
#' 
#' @seealso \code{\link{list_accounts}} to see the accounts that you have access to.
#' @seealso \code{\link{list_account_codes}} for a vector of account codes that you have
#' @seealso \code{\link{account_brands}} for listing the brands in an account.
#' @seealso \code{\link{account_phrases}} for listing the phrases used in an account.
#' @seealso \code{\link{account_tags}} for listing the tags used in an account.
#' @seealso \code{\link{client_service}} for the details of the client service
#'   person related to the account.
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
    
    if (length(code) > 1) {
        return(lapply(code, function(c) account(c, auth = auth)))
    }
    
    ac <- structure(list(
        code = code,
        auth = auth
    ), class = "brandseye.account")
    
    ac <- account.load(ac)
    ac
}

print.brandseye.account <- function(account, ...) {
    output <- matrix(c(account$code, account_name(account), account$auth$user), ncol = 1)
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
        account_name(account) <- account$data$name
        
    }
    account
}

account_code <- function(account) {
    UseMethod("account_code", account)
}

account_code.brandseye.account <- function(account) {
    account$code
}

account_code.list <- function(accounts) {
    sapply(accounts, function(ac) account_code(ac))
}

account_name <- function(account) {
    UseMethod("account_name", account)
}

account_name.brandseye.account <- function(account) {    
    account$name
}

account_name.character <- function(accounts) {
    sapply(accounts, function(ac) {
        account_name(account(ac))
    })
}

'account_name<-' <- function(account, value) {
    UseMethod('account_name<-', account)
}

'account_name<-.brandseye.account' <- function(account, value) {
    account$name <- value
    account
}

#' Client service details
#' 
#' Find out the details for the client service person to contact
#' for queries related to your account.
#' 
#' @return Returns a structure holding information about the 
#'  account's client service person, with \code{name} and
#'  \code{email} fields.
client_service <- function(account) {
    UseMethod("client_service", account)
}

#' @describeIn client_service
#' 
#' Returns client service details for an account object
#' 
#' @examples
#' \dontrun{
#' 
#' details <- client_service(account("QUIR01BA"))
#' details$name
#' details$email
#' 
#' }
client_service.brandseye.account <- function(account) {
    structure(
        list(name = account$data$clientService$name,
             email = account$data$clientService$email),
        class = "brandseye.clientService"        
    )    
}

#' @describeIn client_service
#' 
#' Returns client service information for an account code
#' 
#' @examples
#' \dontrun{
#' 
#' client_service("QUIR01BA")
#' 
#' }
client_service.character <- function(code) {
    client_service(account(code))
}

#' @describeIn client_service
#' 
#' @return As a special case, when given a list of accounts, 
#'   this function returns a data frame with name and email columns,
#'   and a row per account.
#'   
#' @examples 
#' \dontrun{
#' 
#' client_service(c("QUIR01BA", "BEAD33AA"))
#' client_service(list_account_codes())
#' 
#' }
client_service.list <- function(accounts) {
    cs <- lapply(accounts, function(ac) client_service(ac))
    
     data.frame(name = sapply(cs, function(cs) cs$name),
                email = sapply(cs, function(cs) cs$email))
}

#' Prints a client service S3 class.
print.brandseye.clientService <- function(client_service) {
    display <- matrix(c(client_service$name, client_service$email), 2, 1)
    rownames(display) <- c("name", "email")
    colnames(display) <- ""
    print(display, quote = FALSE)
}

#' Listing brands in an account
#' 
#' This returns a \code{data.frame} containing brand IDs, their names,
#' whether they've been deleted or not, and the ID of the parent brand.
account_brands <- function(account, ...) {
    UseMethod("account_brands", account)
}

#' @describeIn account_brands
#' 
#' Returns brand information for a particular \code{\link{account}} object.
#' 
#' @examples
#' \dontrun{
#' # Fetch brands for an \code{\link{account}} object.
#' ac <- account("QUIR01BA")
#' account_brands(ac)
#' }
account_brands.brandseye.account <- function(account, .process = TRUE) {
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
    
    
    dplyr::tbl_df(data.frame(id = if(.process) factor(id) else id, 
                             name = name, 
                             deleted = deleted, 
                             parent = if(.process) factor(parents) else parents,
                             stringsAsFactors = FALSE))
}

#' @describeIn account_brands
#' 
#' Returns brand information for a list of accounts
#' 
#' @examples
#' \dontrun{
#' # Returns a data.frame containing brand information for two accounts
#' account_brands(c("QUIR01BA", "BEAD33AA"))
#' 
#' # Return brand information for all accounts that you have access to
#' account_brands(list_account_codes())
#' }
account_brands.list <- function(accounts) {
    `%>%` <- dplyr::`%>%`    
    
    accounts %>% 
        lapply(function(ac) data.frame(code = account_code(ac),                                                      
                                       account_brands(ac, .process = FALSE), 
                                       stringsAsFactors = FALSE)) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(code = factor(code),
                      id = factor(id),
                      parent = factor(parent))
               
}

#' @describeIn account_brands
#' 
#' Returns brand information for an account identified by an account code.
#' 
#' @examples
#' 
#' \dontrun{
#' # Fetching brands for a particular account
#' account_brands("QUIR01BA")
#' }
#' 
account_brands.character <- function(account) {
    account_brands(account(account))
}


#' List phrases in an account
#' 
#' The returns a \code{data.frame} listing the phrases in an
#' account, their IDs, the IDs of the brands that the phrase is associated
#' with, and a flag indicating whether the phrase is inactive or deleted.
account_phrases <- function(account, ...) {
    UseMethod("account_phrases", account)
}

#' @describeIn account_phrases
#' 
#' Returns phrase information for a particular account object.
#' 
#' @examples
#' \dontrun{
#' # Returns account information for an account object
#' ac <- account("QUIR01BA")
#' account_phrases(ac)
#' }
account_phrases.brandseye.account <- function(account, .process = TRUE) {
    id <- integer()
    brand.id <- character()
    phrase <- character()
    inactive <- logical()
    deleted <- logical()
    
    recurse <- function(brand) {    
        
        for (p in brand$phrases) {
            id <<- c(id, p$id)
            brand.id <<- c(brand.id, brand$id)
            phrase <<- c(phrase, p$q)
            deleted <<- c(deleted, ifelse(is.null(p$deleted) || p$deleted == FALSE, FALSE, TRUE))
            inactive <<- c(inactive, ifelse(is.null(p$inactive) || p$inactive == FALSE, FALSE, TRUE))
        }
        
        if (length(brand$children)) {
            for (b in brand$children) {
                recurse(b)
            }
        }
    }
    
    for (b in account$data$brands) {
        recurse(b)
    }
    
    
    dplyr::tbl_df(data.frame(id = if(.process) factor(id) else id, 
                             brand.id = if(.process) factor(brand.id) else brand.id, 
                             phrase = phrase, 
                             inactive = inactive, 
                             deleted = deleted, 
                             stringsAsFactors = FALSE))
}

#' @describeIn account_phrases
#' 
#' Returns account information for an account identified using an account code
#' 
#' @examples
#' \dontrun{
#' account_phrases("QUIR01BA")
#' }
account_phrases.character <- function(account) {
    account_phrases(account(account))
}

#' @describeIn account_phrases
#' 
#' Given a list of \code{account} objects, this will return
#' a combined \code{data.frame} for the phrases in all of those accounts.
#' 
#' @examples
#' \dontrun{
#' account_phrases(c("QUIR01BA", "BEAD33AA"))
#' }
account_phrases.list <- function(accounts) {
    `%>%` <- dplyr::`%>%`    
        
    accounts %>% 
        lapply(function(ac) data.frame(code = account_code(ac),                                                      
                                       account_phrases(ac, .process = FALSE), 
                                       stringsAsFactors = FALSE)) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(code = factor(code),
                      id = factor(id),
                      brand.id = factor(brand.id))
}

#' List tags in an account
#' 
#' This returns a data frame of tags available in an account, along with the 
#' IDs of those tags.
account_tags <- function(account, ...) {
    UseMethod("account_tags", account)
}

#' @describeIn account_tags
#' 
#' Returns tag information for \code{\link{account}} objects.
#' 
#' @param .process Set to \code{FALSE} if you do not want IDs set as factors.
account_tags.brandseye.account <- function(account, .process = TRUE) {
    ids <- integer()
    names <- character()
        
    for (t in account$data$tags) {        
        ids <- c(ids, t$id)
        names <- c(names, t$name)
    }
        
    dplyr::tbl_df(data.frame(id = if(.process) factor(ids) else ids, 
                             name = names,
                             stringsAsFactors = FALSE))
}

#' @describeIn account_tags
#' 
#' Returns tag information an account identified using an account code
#' 
#' @examples
#' \dontrun{
#' account_tags("QUIR01BA")
#' account_tags(c("QUIR01BA", "BEAD33AA")
#' }
account_tags.character <- function(account) {
    account_tags(account(account))
}

#' @describeIn account_tags
#' 
#' Returns tag information for a list of \code{\link{account}} objects
account_tags.list <- function(accounts) {
    `%>%` <- dplyr::`%>%`    
    
    accounts %>% 
        lapply(function(ac) data.frame(code = account_code(ac),                                                      
                                       account_tags(ac, .process = FALSE), 
                                       stringsAsFactors = FALSE)) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(code = factor(code),
                      id = factor(id))
}


#' List accounts you have access to
#' 
#' This returns a data frame listing the accounts that you have access to,
#' along with their name and status.
#' 
#' @examples
#' list_accounts(key = "my api key")
#' 
#' list_accounts(user = "rudy.neeser@@brandseye.com", 
#'               password = "my brandseye password")
#' 
#' auth <- authentication(user = "rudy.neeser@@brandseye.com", 
#'                        password = "my brandseye password")
#' list_accounts(auth)
list_accounts <- function(auth = pkg.env$defaultAuthentication, key = NULL, user = NULL, password = NULL) {
    if (is.null(auth)) auth <- authentication(key = key, user = user, password = password)
    url <- paste0("https://api.brandseye.com/rest/accounts/")
    data <- httr::GET(url, httr::authenticate(auth$user, auth$password))
    results <- jsonlite::fromJSON(httr::content(data, "text"))
    results
}

#' @describeIn list_accounts
#' 
#' Returns a character vector of account codes that you have access to.
#'
#' @examples
#' # Get the number of mentions published in the last day across all 
#' # of your accounts.
#' count(list_account_codes(), "published inthelast day")
list_account_codes <- function(auth = pkg.env$defaultAuthentication, key = NULL, user = NULL, password = NULL) {
    list_accounts(auth, key, user, password)$code
}