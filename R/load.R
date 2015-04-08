pkg.env <- new.env()

.onLoad <- function(libname, pkgname) {
    # Reads in default authentication information
    auth_file <- authentication.filename()
    if (file.exists(auth_file)) {        
        auth_data <- jsonlite::fromJSON(txt = auth_file)
        pkg.env$defaultAuthentication <- authentication(key = auth_data$key, user = auth_data$user, password = auth_data$password)
    }
    
    # Ensure that we have a backend for foreach registered
    if (!foreach::getDoParRegistered()) {
        foreach::registerDoSEQ()
    }
    
    invisible()
}