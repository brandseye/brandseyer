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
#' @param .process Indicates whether the types should be cleaned. For instance, date values transformed
#'        from strings to POSIXct objects, NA values properly handled, etc. 
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
                            authentication = defaultAuthentication,
                            .process = TRUE) {        
    
    # Transforms a data.frame to clean up the various data types
    # and so on returned as Strings from the API.
    process <- function(results) {
        n <- names(results)
        if ("published" %in% n) results <- dplyr::mutate(results, published = ifelse(published == "UNKNOWN", NA, as.POSIXct(published)))
        if ("sentiment" %in% n) results <- dplyr::mutate(results,  sentiment = factor(replace(sentiment, sentiment == "UNKNOWN", NA)))
        if ("media" %in% n) results <- dplyr::mutate(results, media = factor(replace(media, media == "UNKNOWN", NA)))
        if ("gender" %in% n) results <- dplyr::mutate(results, gender = factor(replace(gender, gender == "UNKNOWN", NA)))
        if ("country" %in% n) results <- dplyr::mutate(results, country = factor(replace(country, country == "UN", NA)))
        if ("language" %in% n) results <- dplyr::mutate(results, language = factor(replace(language, language == "UNKNOWN", NA)))
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
            stop(message)
        }
        
        results <- data.frame(jsonlite::fromJSON(httr::content(data, "text")))
        if (.process) results <- process(results)
        return(results)
    }    
    
    block <- function(code) {        
        message(paste("Querying account:", code))
        data <- count(code, filter, groupby, include, authentication, .process = FALSE)        
        if (nrow(data) == 0) {
            # We don't want to add a code to an empty data frame: this
            # can cause errors
            return(data.frame(code = factor(code, levels = accounts)))
        }
        data.frame(code = factor(code, levels = accounts), data)
    }
    
    results <- NULL
    if (require("foreach")) {
        results <- foreach(code = accounts, .combine = dplyr::bind_rows, .multicombine = TRUE) %dopar% block(code)    
    }
    else {
        results <- Reduce(dplyr::bind_rows, lapply(accounts, block)) 
    }
    
    if (.process) results <- process(results)    
    results
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
