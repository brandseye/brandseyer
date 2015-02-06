mentions <- function(account, filter) {
    url <- paste0("https://api.brandseye.com/rest/accounts/", account$code, "/mentions")
    data <- httr::GET(url, httr::authenticate(account$auth$user, account$auth$password), 
                      query = list(filter = filter))    
    results <- jsonlite::fromJSON(httr::content(data, "text"))
    results
}