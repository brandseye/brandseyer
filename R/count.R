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
#' @examples
#' count("QUIR01BA", authentication(key = "my key"), "published inthelast month")
count.character <- function(accounts, authentication, filter = NULL, groupby = NULL, include = NULL) {
    if (length(accounts) == 1) {
        url <- paste0("https://api.brandseye.com/rest/accounts/", accounts, "/mentions/count")
        query <- list()
        if (!is.null(filter)) query <- list(filter = filter, groupby=groupby, include=include)
        
        data <- httr::GET(url, httr::authenticate(authentication$user, authentication$password), query = query)    
        results <- data.frame(jsonlite::fromJSON(httr::content(data, "text")))
        if ("published" %in% names(results)) results <- transform(results, published = as.POSIXct(published))
        return(results)
    }    
    
    
    
    Reduce(rbind, lapply(accounts, function(code) {
        data <- count(code, authentication, filter, groupby, include)
        data <- transform(data, code = code)        
    }))        
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
#' count(ac, "published inthelast month and relevancy isnt irrelevant", groupby="published")
count.brandseye.account <- function(account, filter = NULL, groupby = NULL, 
                                    include = NULL) {
    count(account$code, account$auth, filter = filter, groupby = groupby, include = include)
}
