defaultAuthentication <- NULL

.onLoad <- function(libname, pkgname) {
    # Reads in default authentication information
    auth_file <- getAuthenticationFileName()
    if (file.exists(auth_file)) {        
        auth_data <- jsonlite::fromJSON(txt = auth_file)
        defaultAuthentication <<- authentication(key = auth_data$key, user = auth_data$user, password = auth_data$password)
    }
    invisible()
}