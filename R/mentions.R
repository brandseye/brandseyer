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

#' Read mentions from your account
mentions <- function(account, ...) {
    UseMethod("mentions", account)
}

#' @describeIn mentions
#' @param limit The maximum number of mentions to be returned
#' @param offset Mentions are returned in an order. Offset says how many of the 
#'   first mentions should be skipped.
#' @examples
#' # Read mentions using the default authentication
#' mentions("QUIR01BA", "published inthelast day")
mentions.character <- function(code, filter, 
                               limit = 30, offset = 0,
                               include = NULL,
                               authentication = pkg.env$defaultAuthentication) {
    url <- paste0("https://api.brandseye.com/rest/accounts/", code, "/mentions")
    data <- httr::GET(url, httr::authenticate(authentication$user, authentication$password), 
                      query = list(filter = filter, limit = limit, offset = offset, include=include))    
    results <- jsonlite::fromJSON(httr::content(data, "text"), flatten=TRUE)
    results$data
}

#' @describeIn mentions
mentions.brandseye.account <- function(account, filter) {
    mentions(account$code, filter, account$auth)
}