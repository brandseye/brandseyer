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

#' Create sentiment factors
#' 
#' Create representations of sentiment as ordered factors.
#' 
#' @param value The integer value of the sentiment you want to represent.
#' 
#' @details 
#' BrandsEye represents sentiment as ordered factors between -5 and 5. 
#' 0 is never used, but is represented here to allow for easy querying.
#' 
#' Sentiment is represents as ordered factors with the class 
#' \code{c("sentiment", "ordered", "factor")}.
#' 
#' @examples  
#' 
#' sentiment(1) < sentiment(2)
#' 
#' \dontrun{
#' 
#' library(dplyr)
#' account_count("QUIR01BA", "published inthelast month", groupby="sentiment") %>%
#'   filter(sentiment <= sentiment(-1))
#' }
#' 
#' 
#' @author Rudy Neeser
#' @export
sentiment <- function(value) {
    sent <- factor(value, levels = c(-5,-4,-3,-2,-1,0,1,2,3,4,5), ordered = TRUE)
    class(sent) <- c("sentiment", class(sent))
    sent
}