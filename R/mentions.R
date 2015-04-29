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
#' 
#' @return Returns a list containing the following items:
#' \itemize{
#' \item \code{mentions}, a \code{data.frame} of the mention data.
#' \item \code{media}, a \code{data.frame} listing mime types and urls for media
#'       associated with each mention.
#' \item \code{tags}, a \code{data.frame} containing the tags that match the mentions.
#' \item \code{phrases{}}, a \code{data.frame} listing the phrases that a mention matched.
#' \item \code{sentiment}, a \code{data.frame} listing the sentiment associated with a mention, possibly
#'       more than one per mention.
#' }
#' 
#' @seealso \code{\link{account_phrases}} to see a list of phrases in an account, with
#'          their associated brands.
#' @seealso \code{\link{account_brands}} for a list of brands associated with the account.
#' @author Rudy Neeser
account_mentions <- function(account, ...) {
    UseMethod("account_mentions", account)
}

#' @describeIn account_mentions
#' @param limit The maximum number of mentions to be returned
#' @param offset Mentions are returned in an order. Offset says how many of the 
#'   first mentions should be skipped.
#' @examples
#' \dontrun{
#' 
#' # Read mentions using the default authentication
#' account_mentions("QUIR01BA", "published inthelast day")
#' 
#' }
account_mentions.character <- function(code, filter, 
                               limit = 30, offset = 0,
                               include = NULL,
                               authentication = pkg.env$defaultAuthentication) {
    
    `%>%` <- dplyr::`%>%`
    embedded <- c("medialinks", "tags", "matchedphrases", "sentiments")
    
    url <- paste0("https://api.brandseye.com/rest/accounts/", code, "/mentions")
    data <- httr::GET(url, httr::authenticate(authentication$user, authentication$password), 
                      query = list(filter = filter, limit = limit, offset = offset, include=include))    
    results <- jsonlite::fromJSON(httr::content(data, "text"), flatten=TRUE)
    
    mentions <- dplyr::tbl_df(results$data %>%
                                  dplyr::select(-matches("mediaLinks"), -matches("tags"), 
                                                -matches("matchedPhrases"), -matches("sentiments")))
        
    # Media, tags, and so on, are stored as an embedded lists which we now need to extract.
    # A mention may have multiple media entities, tags, etc, attached.
    data_names <- names(results$data)
    media_present <- 'mediaLinks' %in% data_names
    tags_present <- 'tags' %in% data_names    
    
    media <- NULL
    tags <- NULL
    sentiment <- NULL 
    phrases <- NULL
        
    media_mention_ids <- c()
    mimetypes <- c()
    urls <- c()
    
    tag_mention_ids <- c()
    tag_ids <- c()
    tag_names <- c()
    
    raw_media <- if (media_present) results$data[, 'mediaLinks'] else NULL
    raw_tags <- if (tags_present) results$data[, 'tags'] else NULL
    raw_sentiment <- results$data[, 'sentiments']
    raw_phrases <- results$data[, 'matchedPhrases']
    
    # Sentiment data
    s_mention_ids <- c()
    s_brand_ids <- c()
    s_names <- c()
    s_sentiments <- c()
    s_sentiment_names <- c()
    
    # Phrase data
    p_mention_ids <- c()
    p_phrase_ids <- c()
    p_phrase <- c()
    
    for (i in 1:nrow(results$data)) {    
        sentiment_data <- raw_sentiment[[i]]
        s_mention_ids <- c(s_mention_ids, 
                           rep(results$data[i, 1], nrow(sentiment_data)))
        s_brand_ids <- c(s_brand_ids, sentiment_data[,1])
        s_names <- c(s_names, sentiment_data[, 2])
        s_sentiments <- c(s_sentiments, sentiment_data[, 3])
        s_sentiment_names <- c(s_sentiment_names, sentiment_data[, 4])
        
        phrase_data <- raw_phrases[[i]]
        p_mention_ids <- c(p_mention_ids, 
                           rep(results$data[i, 1], nrow(phrase_data)))
        p_phrase_ids <- c(p_phrase_ids, phrase_data[, 1])
        p_phrase <- c(p_phrase, phrase_data[, 2])        
        
        if (media_present) {
            if (!is.null(raw_media[[i]])) {
                media_data <- raw_media[[i]]
                for (j in 1:nrow(media_data)) {
                    media_mention_ids <- c(media_mention_ids, results$data[i, 1])
                    mimetypes <- c(mimetypes, media_data[j, 1])
                    urls <- c(urls, media_data[j, 2])
                }                
            }
        }            
        
        if (tags_present) {
            if (!is.null(raw_tags[[i]])) {
                tag_data <- raw_tags[[i]]
                for (j in 1:nrow(tag_data)) {
                    tag_mention_ids <- c(tag_mention_ids, results$data[i, 1])
                    tag_ids <- c(tag_ids, tag_data[j, 1])
                    tag_names <- c(tag_names, tag_data[j, 2])
                }
            }
        }
    }
    
    sentiment <- data.frame(mention.id = s_mention_ids,
                            brand.id = s_brand_ids,
                            brand = s_names,
                            sentiment = s_sentiments,
                            description = s_sentiment_names)
    
    phrases <- data.frame(mention.id = p_mention_ids,
                          phrase.id = p_phrase_ids,
                          phrase = p_phrase)
    
    if (media_present) {
        media <- data.frame(mention.id = media_mention_ids, 
                            mimetype = mimetypes,
                            url = urls)    
    }
    if (tags_present) {
        tags <- data.frame(mention.id = tag_mention_ids,
                           tag.id = tag_ids,
                           tag = tag_names)
    }    
        
    list(mentions = mentions, 
         media = media,
         tags = tags,
         sentiment = sentiment,
         phrases = phrases)        
}

#' @describeIn account_mentions
account_mentions.brandseye.account <- function(account, filter) {
    account_mentions(account$code, filter, account$auth)
}