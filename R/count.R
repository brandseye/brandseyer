#' Count mentions
#' 
#' \code{count} is used to count mentions in a BrandsEye account matching
#' matching a particular filter. It's possible to group mentions, order the
#' results, and to include various other bits of useful information. It's also
#' possible to perform count operations across multiple accounts. 
#' 
#' @param account An account object to be queried.
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
#' @param groupby A list of items that should be grouped by
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
#' @examples
#' count("QUIR01BA", "published inthelast month") # Uses default authentication, 
#'                                                # if that has been set up.
#' count("QUIR01BA", "published inthelast month", 
#'       authentication = authentication(key = "<my key>"))
#' # Return results for multiple accounts      
#' count(c("QUIR01BA", "BEAD33AA"), "published inthelast month")      
#' 
#' # Return results for all accounts
#' count(listAccountCodes(), "published inthelast month")            
count.character <- function(accounts, 
                            filter = NULL, 
                            groupby = NULL, 
                            include = NULL,
                            authentication = defaultAuthentication) {
    if (length(accounts) == 1) {
        url <- paste0("https://api.brandseye.com/rest/accounts/", accounts, "/mentions/count")
        query <- list()
        if (!is.null(filter)) query <- list(filter = filter, groupby=groupby, include=include)
        
        data <- httr::GET(url, httr::authenticate(authentication$user, authentication$password), query = query)    
        if (httr::status_code(data) == 401) stop("You are not authorised to access this account")
        if (httr::status_code(data) != 200) {
            message = jsonlite::fromJSON(httr::content(data, "text"))$error
            stop(message)
        }
        
        results <- data.frame(jsonlite::fromJSON(httr::content(data, "text")))
        n <- names(results)
        if ("published" %in% n) results <- transform(results, published = ifelse(published == "UNKNOWN", NA, as.POSIXct(published)))
        if ("sentiment" %in% n) results <- transform(results,  sentiment = factor(sentiment))
        if ("media" %in% n) results <- transform(results, media = factor(media))
        if ("gender" %in% n) results <- transform(results, gender = factor(gender))
        if ("country" %in% n) results <- transform(results, country = factor(country))
        if ("language" %in% n) results <- transform(results, language = factor(language))
        return(results)
    }    
    
    foreach(code = accounts, .combine = rbind) %dopar% {
        data <- count(code, filter, groupby, include, authentication)
        data.frame(code = code, data)
    }    
}

#' @describeIn count

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
#' count(ac, "published inthelast month and relevancy isnt irrelevant", 
#'       groupby="published")
count.brandseye.account <- function(account, filter = NULL, groupby = NULL, 
                                    include = NULL) {
    count(account$code, account$auth, filter = filter, groupby = groupby, include = include)
}
