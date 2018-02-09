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
#' The various storage formats are only relevant in terms of what parameters
#' they allow you to set on the mention. Calling \code{account} will return
#' an account object of the appropriate type, while the \code{account_storage}
#' method will indicate the type that it is. Most new accounts are stored in 
#' the V4 format, and so support the parameters of the V4 generic method below.
#' 
#' @return Nothing useful yet. This method is a placeholder for testing, and 
#' its return values will likely change. Its interface will probably remain stable,
#' although extra arguments may be added.
#'
#' @author Constance Neeser
#' @author Lara Basson
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
#'
#' @export
account_update.character <- function(code, 
                                     filter, 
                                     tag,
                                     sentiment,
                                     media,
                                     auto.confirm = FALSE,
                                     authentication = pkg.env$defaultAuthentication,
                                     ...) {
    account_update(account = account(code),
                   filter = filter,
                   tag = tag,
                   sentiment = sentiment,
                   media = media,
                   auto.confirm = auto.confirm,
                   authentication = authentication,
                   ...)
}

#' Updates mentions for accounts in our older format.
#' 
#' @describeIn account_update
#' 
#' @details This function is solely for testing purposes, and its behaviour
#' may change in future versions. 
#' 
#' @param account An account object.
#'
#' @export
account_update.brandseye.account.v3 <- function(account, filter, 
                                                tag,
                                                sentiment,
                                                media,
                                                auto.confirm = FALSE,
                                                authentication = account$auth) {
    ensureAuthenticated(authentication)
    
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
    
    url <- paste0("https://api.brandseye.com/rest/accounts/", account$code, "/mentions")
    data <- httr::PUT(url, httr::authenticate(authentication$user, authentication$password),
                      query = query)
    check_errors(data)

    results <- jsonlite::fromJSON(httr::content(data, "text"), flatten=TRUE)
    results
}


#' Updates mentions for accounts stored in our newer format.
#' 
#' @describeIn account_update
#' 
#' @param account The account object.
#' @param relevancy A string indicating whether mentions are relevant or not.
#' @param relevancyVerified A boolean indicating whether to consider the relevancy as verifed or not.
#' @param removeTag A list of tag IDs to remove
#' @param sentimentVerified A boolean indicating whether to consider sentiment as verified or not.
#' @param media A string indicating the category of the mentions, such as CONSUMER or PRESS
#' @param gender A string indicating the author's gender, such as MALE, FEMALE, OTHER, UNKNOWN.
#' @param language A two letter, lowercase string, giving the iso 639-1 language code for the mention.
#' @param race A string giving the race of the author
#' @param addPhrases A list of integer IDs for phrases to add to the mention
#' @param removePhrases A list of integer IDs for the phrases to remove from the mention
#' @param addCrowdJobs A list of integer IDs representing crowd jobs related to this mention, to be
#'                     added to the mention.
#' @param removeCrowdJobs A list of integer IDs representing crowd jobs to be removed from this mention. 
#' @param updateAuthor Whether the stored author information related to this mention should be 
#'                     updated with any of the information related to the author on this mention, such as race,
#'                     gender, language or media.
#'
#' @export
account_update.brandseye.account.v4 <- function(account, filter, 
                                                relevancy,
                                                relevancyVerified,
                                                tag,
                                                removeTag,
                                                sentiment,
                                                sentimentVerified,
                                                media,
                                                gender,
                                                language,
                                                race,
                                                addPhrases,
                                                removePhrases,
                                                addCrowdJobs,
                                                removeCrowdJobs,
                                                updateAuthor,
                                                auto.confirm = FALSE,
                                                authentication = account$auth) {
    
    ensureAuthenticated(authentication)
    
    if (missing(filter) || is.na(filter) || is.null(filter)) {
        stop("No filter provided")
    }
    
    json = list(filter = jsonlite::unbox(filter))
    
    if (!missing(tag)) {
        if (!is.numeric(tag)) {
            stop("account_update only supports tag IDs")
        }
        
        json <- c(json, list(addTags = tibble::tibble(id = tag)))
    }
    
    if (!missing(removeTag)) {
        if (!is.numeric(removeTag)) {
            stop("account_update only supports tag IDs")
        }
        
        json <- c(json, list(removeTags = tibble::tibble(id = removeTag)))
    }
    
    if (!missing(sentiment)) {
        if (!auto.confirm && !confirm("This could damage a lot of data in an account.")) {
            stop("Action cancelled by the user")
        }
        json <- c(json, list(sentiment = jsonlite::unbox(sentiment)))
    }
    
    if (!missing(sentimentVerified)) {
        json <- c(json, list(sentimentVerified = jsonlite::unbox(sentimentVerified)))
    }
    
    if (!missing(media)) {
        json <- c(json, list(category = jsonlite::unbox(media)))
    }
    
    if (!missing(gender)) {
        json <- c(json, list(gender = jsonlite::unbox(gender)))
    }
    
    if (!missing(language)) {
        json <- c(json, list(language = jsonlite::unbox(language)))
    }
    
    if (!missing(race)) {
        json <- c(json, list(race = jsonlite::unbox(race)))
    }
    
    if (!missing(relevancy)) {
        json <- c(json, list(relevancy = jsonlite::unbox(relevancy)))
    }
    
    if (!missing(relevancyVerified)) {
        json <- c(json, list(relevancyVerified = jsonlite::unbox(relevancyVerified)))
    }
    
    if (!missing(addPhrases)) {
        json <- c(json, list(addPhrases = addPhrases))
    }
    
    if (!missing(removePhrases)) {
        json <- c(json, list(removePhrases = removePhrases))
    }
    
    if (!missing(addCrowdJobs)) {
        json <- c(json, list(addCrowdJobs = addCrowdJobs))
    }
    
    if (!missing(removeCrowdJobs)) {
        json <- c(json, list(removeCrowdJobs = removeCrowdJobs))
    }
    
    if (!missing(updateAuthor)) {
        json <- c(json, list(updateAuthor = jsonlite::unbox(updateAuthor)))
    }
    
    
    if (length(json) == 1) {
        stop("No update parameters have been supplied, such as tag or sentiment")
    }
    
    url <- paste0("https://api.brandseye.com/v4/accounts/", account$code, "/mentions")
    data <- httr::PUT(url, # httr::content_type("application/json"),
                      httr::authenticate(authentication$user, authentication$password),
                      body = json, encode="json")
    check_errors(data)

    results <- jsonlite::fromJSON(httr::content(data, "text"), flatten=TRUE)
    invisible()
}