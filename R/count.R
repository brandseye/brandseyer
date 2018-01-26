# Copyright (c) 2015, 2017-2018, Brandseye PTY (LTD) 
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

#' Counting mentions, authors, etc
#' 
#' \code{account_count} is used to count mentions in a BrandsEye account matching
#' matching a particular filter, and to produce aggreagate data related to them.
#' It's possible to group mentions, order the
#' results, and to include various other bits of useful information. It's also
#' possible to perform count operations across multiple accounts, or to count
#' things other than the number of mentions received, such as the number of unique
#' authors, sites, and so on. 
#' 
#' @param account An account to be queried. 
#' 
#' @details
#' Filters are described in the api documentation \url{https://api.brandseye.com/docs}
#' 
#' @seealso \code{\link{account}} for information on querying account information, including
#'      seeing the brands and phrases associated with an account.
#' @seealso \code{\link{account_mentions}} for querying raw mention data.
#' @seealso \code{\link{sentiment}} for comparing sentiment values.
#' @export
#' @author Constance Neeser
account_count <- function(account, ...) {
    UseMethod("account_count", account)
}

#' @describeIn account_count
#' 
#' For querying accounts encoded as character strings, or as a vector
#' of character strings.
#' 
#' @param accounts A vector of account codes. If this is a single account code, this function
#'  will return a data frame of results just from that account. If it contains multiple accounts,
#'  this will return a data frame containing all the results across accounts, and a column indicating
#'  the account that the particular result is from. 
#' @param filter A filter string describing the mentions that should be counted by this query.
#' @param groupby A vector of items that should be grouped by. For example, 
#'        \code{c("published", "language")}. See below for more information.
#' @param count A vector items that should be counted instead of mentions themselves. By default,
#'        \code{account_count} will count mentions (equivalent to passing a value of "id" to \code{count}), 
#'        but various other items may be counted instead, such as unique authors. See below for more information.
#' @param include A vector of items naming values that should be included. 
#'        For example, \code{c("ots", "ave")}
#' @param .process Indicates whether the types should be cleaned. For instance, date values transformed
#'        from strings to POSIXct objects, NA values properly handled, etc. 
#' @param showProgress Set to true if you would like a progress bar to be shown when querying multiple
#'        accounts.
#' 
#' @details 
#' It's possible to parallelise this call. This is only useful if you're querying multiple accounts:
#' there will be no benefit when querying only a single account. Any parallel backend for 
#' the \code{\link{foreach}} package can be used to enable parallel functioning. For example, on
#' a Linux or OX X based system, the following will work well:
#'  
#' \verb{
#'  library(doMC)
#'  registerDoMC(8)
#'  account_count(list_account_codes(), "published inthelast day")
#' }
#' 
#' 
#' @section Grouping:
#' 
#' The \code{account_count} function will by default return only a count of the mentions
#' matching the given filter. If you would like more information, you should
#' group by particular values. The following (possibly incomplete) list
#' of fields can be grouped by:
#' 
#' action, alexaRank, assignee, author, authorName, brand, city, country, 
#' credibility, extract, feed, gender, language, link, linked, media, pageRank,
#' phrase, phraseMatches, pickedUp, process, published, region, relevancy, 
#' relevancyVerified, sentiment, sentimentVerified, tag, title, updated, uri,
#' replycount, resharecount, responsetime
#' 
#' @section Counting:
#' 
#' \code{account_count} will by default count the number of mentions matching 
#' the filter (or the group that the mentions are being grouped by). It is also
#' possible to count other items. These include:
#' 
#' id (the default), credibility, media, action, site, authorName, language, country, 
#' region, city, assignee, author, gender
#' 
#' @section Including extra data:
#' 
#' Grouping is the first step to include extra data. However, some data 
#' cannot be grouped by, and are instead extra information added on to 
#' each of the returned buckets. This might include information as simple
#' as a new format for the country code (\code{countryISO3} being an example),
#' or aggregate data for the group (\code{AVE} and \code{OTS} being examples).
#' 
#' An incomplete list of data that can be included are:
#' 
#' ave, ots, percentages, engagement, sentiment-reach, sentiment-count, countryISO3,
#' latlon, scale, yaw, pitch, roll
#' 
#' @section API documentation:
#' 
#' The canonical documentation for the filter language, and what fields
#' may be grouped and included, is the BrandsEye API documentation
#' \url{https://api.brandseye.com/docs}.
#'
#' @examples
#' \dontrun{
#' account_count("QUIR01BA", "published inthelast month") # Uses default authentication, 
#'                                                  # if that has been set up.
#' account_count("QUIR01BA", "published inthelast month", 
#'         authentication = authentication(key = "<my key>"))
#' # Return results for multiple accounts      
#' account_count(c("QUIR01BA", "BEAD33AA"), "published inthelast month")      
#' 
#' # Return results for all accounts
#' account_count(list_account_codes(), "published inthelast month") 
#' 
#' # Return results grouped by publication date
#' account_count("QUIR01BA", "published inthelast month", groupby = "published)
#' 
#' # Include Ad Value Equivalent (AVE) and Opportunity to See
#' account_count("QUIR01BA", "published inthelast month", groupby = "published, 
#'         include = c("ave", "ots"))
#'         
#' # Count the number of unique authors
#' account_count("QUIR01BA", "published inthelast month", count="author")
#' 
#' # Count the number of unique authors in each country that we have received 
#' # mentions from
#' account_count("QUIR01BA", "published inthelast month", count="author", groupby="country")
#' }  
#' @export
account_count.character <- function(accounts, 
                                    filter = NULL, 
                                    groupby = NULL, 
                                    include = NULL,
                                    count = NULL,
                                    authentication = pkg.env$defaultAuthentication,
                                    showProgress = length(accounts) > 10,
                                    .process = TRUE) {  
    
    ensureAuthenticated(authentication)
    
    if (length(groupby) > 1) groupby <- do.call(stringr::str_c, as.list(c(groupby, sep = ',')))
    if (length(include) > 1) include <- do.call(stringr::str_c, as.list(c(include, sep = ',')))
    if (length(count) > 1) count <- do.call(stringr::str_c, as.list(c(count, sep = ',')))
    
    # Transforms a data.frame to clean up the various data types
    # and so on returned as Strings from the API.
    process <- function(results) {        
        dates <- c("published", "pickedup", "updated")
        factorItems <- c("action", "authorname", "country", "feed", "gender",
                         "language", "media", "process", "region", "relevancy",
                         "tag", "countryiso3")
        
        # In many of the if branches to follow we need to cast to a data frame,
        # since we are likely to get something with class tbl_df (from dplyr), 
        # which, when subset, returns another data frame, causing the call 
        # to factor to break. See the documentation for tbl_df and the [ operator.
        for (n in names(results)) {
            if (tolower(n) %in% dates) {
                 results[, n] <- as.POSIXct(as.data.frame(replace(results[, n], results[, n] == "UNKNOWN", NA))[, n])
            }
            else if (n == "country") {
                results[, n] <- factor(as.data.frame(replace(results[, n], results[, n] == "UN", NA))[, n])
            }
            else if (tolower(n) %in% factorItems) {    
                replace(results[, n], results[, n] == "UNKNOWN", NA)
            }
            else if (tolower(n) == "sentiment") {
                results[, n] <- sentiment(as.data.frame(replace(results[, n], results[, n] == "UN", NA))[, n])
            }
            else results[, n] <- replace(results[, n], results[, n] == "UNKNOWN", NA)  
        }
        
        results
    }
    
    if (length(accounts) == 1) {
        url <- paste0("https://api.brandseye.com/rest/accounts/", accounts, "/mentions/count")
        query <- list()
        if (!is.null(filter)) query <- c(filter = filter, query)
        if (!is.null(groupby)) query <- c(groupby = groupby, query)
        if (!is.null(include)) query <- c(include = include, query)
        if (!is.null(count)) query <- c(count = count, query)
        
        data <- httr::GET(url, httr::authenticate(authentication$user, authentication$password), query = query)    
        check_errors(data)
        
        results <- dplyr::tbl_df(data.frame(jsonlite::fromJSON(httr::content(data, "text"))))
        # This is a sanity process for some bad data that might break merging data
        # frames. A big example are mentions with unknown sentiment forcing
        # sentiment to be a mixture of numeric and character between different accounts.
        if ("sentiment" %in% names(results) && (is.factor(results$sentiment) || is.character(results$sentiment))) {
            results$sentiment[results$sentiment == "UNKNOWN"] <- NA
            results$sentiment <- as.numeric(as.character(results$sentiment))
        }
        if (.process) results <- process(results)
        return(results)
    }    
    
    pb <- NULL
    if (showProgress) pb <- txtProgressBar(min = 0, max = length(accounts), style=3)
    i <- 0
    
    block <- function(code) {        
        data <- account_count(code, filter, groupby, include, count, authentication, 
                              .process = FALSE, showProgress = FALSE)        
        i <<- i + 1
        if (!is.null(pb)) setTxtProgressBar(pb, i)
        if (nrow(data) == 0) {
            # We don't want to add a code to an empty data frame: this
            # can cause errors
            return(data.frame(code = factor(code, levels = accounts)))
        }
        data.frame(code = factor(code, levels = accounts), data)
    }
    
    
    dopar <- foreach::`%dopar%`
    results <- dopar(foreach::foreach(code = accounts, .combine = dplyr::bind_rows, .multicombine = TRUE), 
                     block(code))
        
    if (!is.null(pb)) close(pb)
    
    if (.process) results <- process(results)    
    dplyr::tbl_df(results)
}

#' @describeIn account_count
#' 
#' For querying objects returned by \code{\link{account}}.
#' 
#' @examples
#' \dontrun{
#' # Not using global authentication, but authenticating directly in the call
#' # itself.
#' ac <- account("QUIR01BA", key="<my key>")
#' 
#' # A single number counting the mentions published in the last week. 
#' account_count(ac, "published inthelast week")
#' 
#' # The number of relevant mentions published in the last month
#' account_count(ac, "published inthelast month and relevancy isnt irrelevant")
#' 
#' # As above, but grouped by publication day
#' account_count(ac, "published inthelast month and relevancy isnt irrelevant", 
#'         groupby="published")
#' }
#' @export
account_count.brandseye.account <- function(account, filter = NULL, groupby = NULL, 
                                    include = NULL) {
    account_count(account$code, account$auth, filter = filter, groupby = groupby, include = include)
}

#' @describeIn account_count
#' 
#' Useful for querying accounts encoded as \code{factor}s, such as account
#' codes given in \code{data_frames}.  
#' @export
account_count.factor <- function(account, ...) {
    account_count(as.character(account), ...)
}