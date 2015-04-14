# Copyright (c) 2015, Brandseye PTY (LTD) 
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

#' Authenticate yourself with BrandsEye
#' 
#' Provides a means to authenticate yourself with the BrandsEye API. Most of the time
#' you will want to do this using a BrandsEye API key.
#' 
#' @examples
#' \dontrun{
#' 
# Authenticating with a key
#' authentication("adfd42345f534fgdfgd")
#' authentication(key = "adfd42345f534fgdfgd")
#' 
#' # Authenticating with a username and password
#' authentication(user = "jo.blogs@@brandseye.com", 
#'                password ="This is a safe password!!")
#'                
#' }
#' 
authentication <- function(key = NULL, user = NULL, password = NULL) {
    if (is.null(key) & (is.null(user) | is.null(password)))
        stop("You must choose to authenticate with an API key or a username / password pair")
    
    if (is.null(user)) user <- "API_KEY";
    if (is.null(password)) password <- key;
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
authentication_filename <- function() {
    file.path(Sys.getenv("HOME"), ".brandseyerd", "authentication.json")
}

#' @describeIn authentication
#' Save your authentication for your current R session, or in a file for later R sessions. 
#' 
#' @details
#' This function allows you to set a global variable holding your authentication details.
#' The various BrandsEyeR functions will use these authentication details when communicating
#' with the BrandsEye API. 
#'  
#' It's possible to set up default authentication information as well which will be
#' loaded automatically every time you load the BrandsEyeR library.
#' 
#' In your home directory,
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
#' You can use the \code{authenticate} function to save a stub file 
#' for you to edit in your home directory. On windows, this might be in your 
#' \code{My Documents} directory. You can use the \code{authentication_filename}
#' function to find the directory and file name to save the authentication file in.
#' 
#' You can also use the \code{authenticate} file to create the file for 
#' you in the appropriate place. Call it with no arguments to just make an empty file.
#' If you provide arguments, it will fill in the file for you, as needed.
#' 
#' Be wary of calling it with arguments, since your key or password will
#' land up in your R history file. The best practise is to call 
#' \code{authenticate} with only \code{save = TRUE}  and 
#' no other arguments set and edit the file manually.
#' On a shared computer, set permissions on that file so that only your user 
#' can read it. Your authentication details are saved in plain text. 
#' 
#' @param key Your API key. Contact your client service representative if you do
#'   not already have one. You do not need to have a username or password if you are
#'   using an API key.
#' @param user Your username, if you know it. This should be used with your password.
#' @param password To be used in conjunction with a username
#' @param save Set this to true if your authentication details should be saved
#'   across sessions.
#'   
#' @examples
#' \dontrun{
#' 
#' # Create a json stub to fill in your authentication details:
#' authenticate(save = TRUE)
#' # And this will be created at:
#' authentication_filename()
#' 
#' # Log in for the session
#' authenticate(key = "this is my api key")
#' # Use the API with those credentials
#' account_count("QUIR01BA", "published inthelast month")
#' 
#' }
#' 
authenticate <- function(key = NULL, user = NULL, password = NULL, save = FALSE) {
    if (!is.null(key) | !is.null(user) | !is.null(password)) {
        pkg.env$defaultAuthentication <- authentication(key = key, user = user, password = password)        
    }    
    
    if (save) {
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
        
        if (!file.exists(authentication_filename())) {
            message(paste("Authentication file created at", authentication_filename()))
            if (!file.exists(dirname(authentication_filename()))) {
                dir.create(dirname(authentication_filename()))
            }
            if (!is.null(contents)) {
                fileConn<-file(authentication_filename())        
                writeLines(contents, fileConn)                        
                close(fileConn)        
            }
            else {
                file.create(authentication_filename())
            }
            
        }
        else stop("Authentication file already exists")
    }
    
    pkg.env$defaultAuthentication    
}
