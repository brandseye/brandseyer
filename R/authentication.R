#' Authenticate yourself with BrandsEye
#' 
#' Provides a means to authenticate yourself with the BrandsEye API. Most of the time
#' you will want to do this using a BrandsEye API key.
#' 
#' @details
#' It's possible to set up default authentication information as well. In your home directory,
#' create the file \code{$HOME/.brandseyerd/authentication.json}. 
#'     
#' This file can contain either a key, or a user / password pair. For example, to
#' specify your key, the file should contain:
#' 
#' \verb{
#'  \{ 
#'      "key": "12342342342354345"
#'  \}
#' }
#' 
#' To specify a username and password pair, the file should contain:
#' 
#' \verb{
#'  \{ 
#'      "user": "rudy.neeser@@brandseye.com",
#'      "password": "This would be awkward if it were my real password"
#'  \}
#' }
#' 
#' @examples
#' # Authenticating with a key
#' authentication("adfd42345f534fgdfgd")
#' authentication(key = "adfd42345f534fgdfgd")
#' 
#' # Authenticating with a username and password
#' authentication(user = "jo.blogs@@brandseye.com", 
#'                password ="This is a safe password!!")
authentication <- function(key = NULL, user = NULL, password = NULL) {
    if (!is.null(key) & (!is.null(user) | !is.null(password)))
        stop("You must choose to authenticate with an API key or a username / password pair")
    
    if (is.null(user)) user = "API_KEY";
    if (is.null(password)) password = key;
    if (is.null(password)) stop("Please provide an API key");
    
    structure(list(
        username = user,
        password = password
    ), class = "brandseye.auth")    
}

print.brandseye.auth <- function(auth) {
    cat("login: ", auth$username, "\n")
}

#' @describeIn authentication
getAuthenticationFileName <- function() {
    file.path(Sys.getenv("HOME"), ".brandseyerd", "authentication.json")
}

#' @describeIn authentication
save.authentication <- function(key, user, password) {
    contents <- NULL
    if (!missing(key)) {
        contents <- paste0('{ "key": "', key, '" }')
    }
    if (!missing(user) && !missing(password)) {
        contents <- paste0('{ "user": "', user, '", "password": "', password, '" }')
    }
    
    if (!file.exists(getAuthenticationFileName())) {
        if (!is.null(contents)) {
            fileConn<-file(getAuthenticationFileName())        
            writeLines(contents, fileConn)                        
            close(fileConn)        
        }
        else {
            file.create(getAuthenticationFileName())
        }
                
        invisible()
    }
    else stop("Authentication file already exists")
}
