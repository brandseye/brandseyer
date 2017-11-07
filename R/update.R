# Copyright (c) 2017, Brandseye PTY (LTD) 
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

#' Updates mentions for accounts
#' 
#' @return Nothing useful yet. This method is a placeholder for testing, and 
#' its return values will likely change. Its interface will probably remain stable,
#' although extra arguments may be added.
#'
#' @export
account_update <- function(account, ...) {
    UseMethod("account_update", account)
}


#' Updates mentions for accounts supplied as a vector of character codes.
#' 
#' @describeIn account_update
#' 
#' @details This function is solely for testing purposes, and its behaviour
#' may change in future versions. 
#' 
#' @param code A character string for the account to update
#' @param filter A filter selecting mentions to update
#' @param tag Either an id of a tag to update the selected mentions with, or
#'            a character string holding a tag name.
#' @param sentiment An integer representing sentiment. A value between -5 and 5. 
#'                  0 is not a valid number, and 1 represents neutral sentiment.   
#' @param auto.confirm Some actions, such as setting sentiment, will ask for interactive
#'                     confirmation of the action. If you're running from a script, this 
#'                     can be inconvenient. You can override that behaviour by setting
#'                     this parameter to \code{TRUE}. Buyer beware.                         
#' 
#' @author Rudy Neeser
#' @author Lara Basson
#'
#' @export
account_update.character <- function(code, filter, 
                                     tag,
                                     sentiment,
                                     media,
                                     auto.confirm = FALSE,
                                     authentication = pkg.env$defaultAuthentication) {
    update <- c()
    if (!missing(tag)) {
        update <- c(stringr::str_c("tag = ", ifelse(is.numeric(tag), 
                                                     as.character(tag),
                                                     stringr::str_c("'", tag, "'"))), 
                    update)
    }
    
    if (!missing(sentiment)) {
        if (!auto.confirm && !confirm("This could damage a lot of data in an account.")) {
            stop("Action cancelled by the user")
        }
        update <- c(stringr::str_c("sentiment = ", sentiment), update)
    }

    if (!missing(media)) {
        update <- c(stringr::str_c("media = ", media), update)
    }
    
    if (length(update) == 0) {
        stop("No update parameters have been supplied, such as tag or sentiment")
    }

    
    query <- list()
    if (!missing(filter)) query <- c(filter = filter, query)
    query <- c(update = stringr::str_c(update, collapse = ','), query)
    
    
    url <- paste0("https://api.brandseye.com/rest/accounts/", code, "/mentions")
    data <- httr::PUT(url, httr::authenticate(authentication$user, authentication$password),
                      query = query)
    check_errors(data)

    results <- jsonlite::fromJSON(httr::content(data, "text"), flatten=TRUE)
    results
}