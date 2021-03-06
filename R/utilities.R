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

#' Checking for errors
#' 
#' Pass the results from httr calls to the API to examine errors 
#' and report on them in a standard way.
#' 
#' @param data Data returned from an \code{httr} call.
#' 
#' @author Constance Neeser
check_errors <- function(data) {
    if (httr::status_code(data) == 401) stop("You are not authorised to access this account")
    if (httr::status_code(data) != 200) {
        message = jsonlite::fromJSON(httr::content(data, "text"))$error
        stop("BrandsEye API error: ", message, call. = FALSE)
    }
}

#' Confirm an action.
#' 
#' Asks the user to answer Yes or No after prompting them with a message.
#' 
#' @param message A message to display to the user.
#' 
#' @author Constance Neeser
confirm <- function(message) {
    answer <- readline(paste(message, "Are you sure you want to proceed? [Y/N] "))
    if (toupper(answer) != 'Y') return(FALSE)
    TRUE
}