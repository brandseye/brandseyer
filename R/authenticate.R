#' Authenticate yourself with BrandsEye
#' 
#' Provides a means to authenticate yourself with the BrandsEye API. Most of the time
#' you will want to do this using a BrandsEye API key.
#' 
#' @examples
#' # Authenticating with a key
#' authenticate("adfd42345f534fgdfgd")
#' authenticate(key = "adfd42345f534fgdfgd")
#' 
#' # Authenticating with a username and password
#' authenticate(user = "jo.blogs@@brandseye.com", 
#'              password ="This is a safe password!!")
authenticate <- function(key = NULL, user = NULL, password = NULL) {
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
