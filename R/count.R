#' Count mentions
#' 
#' \code{count} is used to count mentions in a BrandsEye account matching
#' matching a particular filter. It's possible to group mentions, order the
#' results, and to include various other bits of useful information. It's also
#' possible to perform count operations across multiple accounts. 
#' 
#' @param account An account to be queried. 
#' 
#' @details
#' Filters are described in the api documentation \url{https://api.brandseye.com/docs}
count <- function(account, ...) {
    UseMethod("count", account)
}

#' @describeIn count
#' @param accounts A vector of account codes. If this is a single account code, this function
#'  will return a data frame of results just from that account. If it contains multiple accounts,
#'  this will return a data frame containing all the results across accounts, and a column indicating
#'  the account that the particular result is from. 
#' @param filter A filter string describing the mentions that should be counted by this query
#' Count aggregate mention information from your BrandsEye account
#' @param groupby A vector of items that should be grouped by. For example, 
#'        \code{c("published", "language")}
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
#' the \code{foreach} package can be used to enable parallel functioning. For example, on
#' a Linux or OX X based system, the following will work well:
#'  
#' \verb{
#'  library(doMC)
#'  registerDoMC(8)
#'  count(listAccountCodes(), "published inthelast day")
#' }
#' 
#' 
#' @section Grouping:
#' 
#' The \code{count} function will by default return only a count of the mentions
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
#' count("QUIR01BA", "published inthelast month") # Uses default authentication, 
#'                                                # if that has been set up.
#' count("QUIR01BA", "published inthelast month", 
#'       authentication = authentication(key = "<my key>"))
#' # Return results for multiple accounts      
#' count(c("QUIR01BA", "BEAD33AA"), "published inthelast month")      
#' 
#' # Return results for all accounts
#' count(listAccountCodes(), "published inthelast month") 
#' 
#' # Return results grouped by publication date
#' count("QUIR01BA", "published inthelast month", groupby = "published)
#' 
#' # Include Ad Value Equivalent (AVE) and Opportunity to See
#' count("QUIR01BA", "published inthelast month", groupby = "published, 
#'       include = c("ave", "ots"))
#' } 
count.character <- function(accounts, 
                            filter = NULL, 
                            groupby = NULL, 
                            include = NULL,
                            authentication = pkg.env$defaultAuthentication,
                            showProgress = length(accounts) > 10,
                            .process = TRUE) {  
    
    if (length(groupby) > 1) groupby <- do.call(stringr::str_c, as.list(c(groupby, sep = ',')))
    if (length(include) > 1) include <- do.call(stringr::str_c, as.list(c(include, sep = ',')))
    
    # Transforms a data.frame to clean up the various data types
    # and so on returned as Strings from the API.
    process <- function(results) {        
        dates <- c("published", "pickedup", "updated")
        factorItems <- c("action", "authorname", "country", "feed", "gender",
                         "language", "media", "process", "region", "relevancy",
                         "sentiment", "tag", "countryiso3")
        
        for (n in names(results)) {
            if (tolower(n) %in% dates) results[, n] <- as.POSIXct(ifelse(results[, n] == "UNKNOWN", NA, results[, n]))
            else if (n == "country") results[, n] <- factor(replace(results[, n], results[, n] == "UN", NA))
            else if (tolower(n) %in% factorItems) results[, n] <- factor(replace(results[, n], results[, n] == "UNKNOWN", NA))
            else results[, n] <- replace(results[, n], results[, n] == "UNKNOWN", NA)  
        }
        
        results
    }
    
    if (length(accounts) == 1) {
        url <- paste0("https://api.brandseye.com/rest/accounts/", accounts, "/mentions/count")
        query <- list()
        if (!is.null(filter)) query <- list(filter = filter, groupby=groupby, include=include)
        
        data <- httr::GET(url, httr::authenticate(authentication$user, authentication$password), query = query)    
        if (httr::status_code(data) == 401) stop("You are not authorised to access this account")
        if (httr::status_code(data) != 200) {
            message = jsonlite::fromJSON(httr::content(data, "text"))$error
            stop("BrandsEye API error: ", message)
        }
        
        results <- data.frame(jsonlite::fromJSON(httr::content(data, "text")))
        if (.process) results <- process(results)
        return(results)
    }    
    
    pb <- NULL
    if (showProgress) pb <- txtProgressBar(min = 0, max = length(accounts), style=3)
    i <- 0
    
    block <- function(code) {        
        data <- count(code, filter, groupby, include, authentication, 
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
    results
}

#' @describeIn count
#' 
#' @examples
#' \dontrun{
#' # Not using global authentication, but authenticating directly in the call
#' # itself.
#' ac <- account("QUIR01BA", key="<my key>")
#' 
#' # A single number counting the mentions published in the last week. 
#' count(ac, "published inthelast week")
#' 
#' # The number of relevant mentions published in the last month
#' count(ac, "published inthelast month and relevancy isnt irrelevant")
#' 
#' # As above, but grouped by publication day
#' count(ac, "published inthelast month and relevancy isnt irrelevant", 
#'       groupby="published")
#' }
count.brandseye.account <- function(account, filter = NULL, groupby = NULL, 
                                    include = NULL) {
    count(account$code, account$auth, filter = filter, groupby = groupby, include = include)
}
