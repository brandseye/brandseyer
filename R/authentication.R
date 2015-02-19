#' Authenticate yourself with BrandsEye
#' 
#' Provides a means to authenticate yourself with the BrandsEye API. Most of the time
#' you will want to do this using a BrandsEye API key.
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
#' Returns the file name in which you can save your authentication information.
authentication.filename <- function() {
    file.path(Sys.getenv("HOME"), ".brandseyerd", "authentication.json")
}

#' @describeIn authentication
#' Save your authentication information to a file for automatic loading by BrandsEyeR.
#' 
#'  @details
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
#' You can use the \code{save.authentication} function to save a stub file 
#' for you to edit in your home directory. On windows, this might be in your 
#' \code{My Documents} directory. You can use the \code{authentication.filename}
#' function to find the directory and file name to save the authentication file in.
#' 
#' You can also use the \code{save.authentication} file to create the file for 
#' you in the appropriate place. Call it with no arguments to just make an empty file.
#' If you provide arguments, it will fill in the file for you, as needed.
#' 
#' Be wary of calling it with arguments, since your key or password will
#' land up in your R history file. The best practise is to call 
#' \code{save.authentication} with no arguments and edit the file manually.
#' On a shared computer, set permissions on that file so that only your user 
#' can read it.
save.authentication <- function(key, user, password) {
    contents <- NULL
    if (!missing(key)) {
        contents <- paste0('{ "key": "', key, '" }')
    }
    if (!missing(user) && !missing(password)) {
        contents <- paste0('{ "user": "', user, '", "password": "', password, '" }')
    }
    if (missing(key) & missing(user) & missing(password)) {
        contents <- '{ "key": "you API key goes here" }'
    }
    
    if (!file.exists(authentication.filename())) {
        if (!is.null(contents)) {
            fileConn<-file(authentication.filename())        
            writeLines(contents, fileConn)                        
            close(fileConn)        
        }
        else {
            file.create(authentication.filename())
        }
                
        invisible()
    }
    else stop("Authentication file already exists")
}
