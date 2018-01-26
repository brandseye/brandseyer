# Copyright (c) 2015-2017, Brandseye PTY (LTD) 
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
#' @return Returns an object of class \code{mention.results}, which is a list
#' containing at least the following items:
#' \itemize{
#' \item \code{mentions}, a \code{tibble} of the mention data.
#' \item \code{media}, a \code{tibble} listing mime types and urls for media
#'       associated with each mention.
#' \item \code{tags}, a \code{tibble} containing the tags that match the mentions.
#' \item \code{phrases}, a \code{tibble} listing the phrases that a mention matched.
#' \item \code{sentiment}, a \code{tibble} listing the sentiment associated with a mention, possibly
#'       more than one per mention.
#' }
#' 
#' @seealso \code{\link{account_phrases}} to see a list of phrases in an account, with
#'          their associated brands.
#' @seealso \code{\link{account_brands}} for a list of brands associated with the account.
#' @author Constance Neeser
#' @export
account_mentions <- function(account, ...) {
    UseMethod("account_mentions", account)
}

#' @describeIn account_mentions
#' 
#' Returns mentions for an account from a vector of character codes.
#' 
#' @param limit The maximum number of mentions to be returned
#' @param offset Mentions are returned in an order. Offset says how many of the 
#'   first mentions should be skipped.
#' @param include A character vector of extra information to include in the mentions
#'        You can see this list in the api documentation available at
#'        \url{https://api.brandseye.com/docs}
#' @param select A character vector of the mention fields to be returned.
#'        You can see this list in the api documentation available at
#'        \url{https://api.brandseye.com/docs}
#'        
#' @param all Set to true if you would like to return all mentions from the account.
#'            This overides the \code{limit} parameter.
#' @examples
#' \dontrun{
#' 
#' # Read mentions using the default authentication
#' account_mentions("QUIR01BA", "published inthelast day")
#' 
#' }
#' @export
account_mentions.character <- function(code, filter, 
                               limit = 30, offset = 0,
                               include,
                               select,
                               authentication = pkg.env$defaultAuthentication,
                               all = FALSE,
                               showProgress = length(code) != 0) {
    
    ensureAuthenticated(authentication)
    
    # Because we attempt to read all mentions from the account, 
    # and this can take some time to do, we want
    # to ensure that we ignore any mentions that might have come in after the call
    # was initially made. 
    pickedUpRestriction <- format(Sys.time(), "pickedUp before '%F %R'")
    
    if (!missing(include) && length(include) > 1) {
        include <- do.call(stringr::str_c, as.list(c(include, sep = ',')))
    }
    
    if (!missing(select) && length(select) > 1) {
        select <- do.call(stringr::str_c, as.list(c(select, sep = ',')))
    }
    
    # The sprintf is to avoid scientific notation for large numbers without
    # globally setting scipen in options.
    query <- list(limit = sprintf("%d", limit), offset = sprintf("%d", offset))
    if (!missing(filter)) query <- c(filter = filter, query)
    if (!missing(include)) query <- c(include = include, query)
    if (!missing(select)) query <- c(select = select, query)
    
    `%>%` <- dplyr::`%>%`
    tibble <- tibble::tibble
    embedded <- c("medialinks", "tags", "matchedphrases", "sentiments")
    
    process_mentions <- function(mentions) {
        mentions$sentiment$sentiment <- sentiment(mentions$sentiment$sentiment)
        mentions
    }
    
    
    if (length(code) == 1) {
        if (all) {
            # Begin setting up to read all mentions matching the filter,
            # and then recursively execute to fetch the data.
            if (missing(filter)) {
                filter <- pickedUpRestriction 
            }
            else {
                filter <- paste('(', filter, ') and', pickedUpRestriction)
            }
            
            results <- account_mentions(code, filter = filter,
                                        limit = 20000, offset = 0,
                                        include,
                                        select,
                                        authentication = authentication,
                                        showProgress = false,
                                        all = FALSE)
            
            total <- results$total
            numReturned <- nrow(results$mention)
            numSeen <- numReturned
            pb <- NULL
            i <- 0
            
            if (numSeen < total && showProgress) {
                pb <- txtProgressBar(min = 0, max = total, style=3, initial = numSeen)
                
            }
            
            while (numReturned > 0 && numSeen < total) {
                seconds <- account_mentions(code, filter = filter,
                                            limit = 20000, offset = numSeen,
                                            include,
                                            authentication = authentication,
                                            showProgress = false,
                                            all = FALSE)
                numReturned <- nrow(seconds$mentions)
                numSeen <- numSeen + numReturned
                setTxtProgressBar(pb, numSeen)
                results$mentions <- dplyr::bind_rows(results$mentions, seconds$mentions)
                results$media <- dplyr::bind_rows(results$media, seconds$media)
                results$tags <- dplyr::bind_rows(results$tags, seconds$tags)
                results$sentiment <- dplyr::bind_rows(results$sentiment, seconds$sentiment)
                results$phrases <- dplyr::bind_rows(results$phrases, seconds$phrases)
            }
            return(results)
        }
        
        # Here we fetch the actual data
        url <- paste0("https://api.brandseye.com/rest/accounts/", code, "/mentions")
        data <- httr::GET(url, httr::authenticate(authentication$user, authentication$password), 
                          query = query)    
        check_errors(data)
        
        results <- jsonlite::fromJSON(httr::content(data, "text"), flatten=TRUE)
        
        total <- results$total
        if (total == 0) {
            return(structure(
                list(mentions = tibble(), 
                     media = tibble(),
                     tags = tibble(),
                     sentiment = tibble(),
                     phrases = tibble(),
                     total = total,
                     call = match.call()),
                class = "mention.results"
            ))    
        }
        mentions <- results$data %>%
            dplyr::select(
                -dplyr::matches("mediaLinks"),
                -dplyr::matches("tags"),-dplyr::matches("matchedPhrases"),
                -dplyr::matches("sentiments")
            )
        # This is a complete hack to solve a problem where sometimes dplyr will select nothing, and just changing column order
        # sorts it out.
        if (nrow(mentions) == 0 && nrow(results$data) != 0) {
            mentions <- results$data %>%
                dplyr::select(
                    -dplyr::matches("sentiments"),
                    -dplyr::matches("tags"),
                    -dplyr::matches("mediaLinks"),
                    -dplyr::matches("matchedPhrases")
                )
        }
        
        mentions <- tibble::as_tibble(mentions)
        
        # Media, tags, and so on, are stored as an embedded lists which we now need to extract.
        # A mention may have multiple media entities, tags, etc, attached.
        data_names <- names(results$data)
        media_present <- 'mediaLinks' %in% data_names
        tags_present <- 'tags' %in% data_names    
        sentiment_present <- 'sentiments' %in% data_names
        phrases_present <- 'matchedPhrases' %in% data_names
        
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
        tag_namespaces <- c()
        tag_descriptions <- c()
        
        raw_media <- if (media_present) results$data[, 'mediaLinks'] else NULL
        raw_tags <- if (tags_present) results$data[, 'tags'] else NULL
        raw_sentiment <- if(sentiment_present) results$data[, 'sentiments'] else NULL
        raw_phrases <- if(phrases_present) results$data[, 'matchedPhrases'] else NULL
        
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
            if (sentiment_present) {
                sentiment_data <- raw_sentiment[[i]]
            
                sent_row <- ifelse(is.null(nrow(sentiment_data)), 0, nrow(sentiment_data))
                sent_col <- ifelse(is.null(ncol(sentiment_data)), 0, ncol(sentiment_data))
                s_mention_ids <- c(s_mention_ids, 
                                   rep(results$data[i, 1], sent_row))
                s_brand_ids <- c(s_brand_ids, sentiment_data[, 1])
                s_names <- c(s_names, sentiment_data[, 2])
                
                s_sentiments <- c(s_sentiments, if (sent_col >= 3) sentiment_data[, 3] else rep(NA, sent_row))
                s_sentiment_names <- c(s_sentiment_names, if (sent_col >= 4) sentiment_data[, 4] else rep(NA, sent_row))
            }
            
            if (phrases_present) {
                phrase_data <- raw_phrases[[i]]
                p_mention_ids <- c(p_mention_ids, 
                                   rep(results$data[i, 1], nrow(phrase_data)))
                p_phrase_ids <- c(p_phrase_ids, phrase_data[, 1])
                p_phrase <- c(p_phrase, phrase_data[, 2])        
            }
        
            
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
                        tag_namespaces <- c(tag_namespaces, tag_data[j, 3])
                        tag_descriptions <- c(tag_descriptions, ifelse(is.null(tag_data[j, 4]) || is.na(tag_data[j, 4]), '', tag_data[j, 4]))
                    }
                }
            }
        }
        
        if (sentiment_present) {
            sentiment <- tibble(
                mention.id = s_mention_ids,
                brand.id = s_brand_ids,
                brand = s_names,
                sentiment = s_sentiments,
                description = s_sentiment_names
            )
        }
        
        if (phrases_present) {
            phrases <- tibble(mention.id = p_mention_ids,
                              phrase.id = p_phrase_ids,
                              phrase = p_phrase)
        }
        
        
        if (media_present) {
            media <- tibble(mention.id = media_mention_ids,
                            mimetype = mimetypes,
                            url = urls)    
        }
        if (tags_present) {
            tags <- tibble(
                mention.id = tag_mention_ids,
                tag.id = tag_ids,
                tag = tag_names,
                namespace = tag_namespaces,
                description = tag_descriptions
            )
        }
        
        return(process_mentions(structure(
            list(mentions = mentions, 
                 media = media,
                 tags = tags,
                 sentiment = sentiment,
                 phrases = phrases,
                 total = total,
                 call = match.call()),
            class = "mention.results"
        )))    
    }
    
    # ---------------------------------
    # Multiple calls
    all_codes <- code
    filterMissing <- missing(filter)
    global_call <- match.call()
    pb <- NULL
    if (showProgress) pb <- txtProgressBar(min = 0, max = length(code), style=3)
    i <- 0
    
    combine <- function(lhs, rhs) {
        structure(
            list(mentions = dplyr::bind_rows(lhs$mentions, rhs$mentions), 
                 media = dplyr::bind_rows(lhs$media, rhs$media),
                 tags = dplyr::bind_rows(lhs$tags, rhs$tags),
                 sentiment = dplyr::bind_rows(lhs$sentiment, rhs$sentiment),
                 phrases = dplyr::bind_rows(lhs$phrases, rhs$phrases),
                 total = lhs$total + rhs$total,
                 call = global_call),
            class = "mention.results"
        )
    }
    
    block <- function(cd) {
        if (!is.null(pb)) setTxtProgressBar(pb, i)
        i <<- i + 1
        
        args <- list(cd, authentication = authentication)
        if (!filterMissing) args <- c(filter = filter, args, all = all)
        results <- do.call("account_mentions", args)
        if (length(results$mentions)) {
            results$mentions <- tibble::add_column(results$mentions,
                                                   code = factor(rep(cd, nrow(results$mentions)), levels = all_codes),
                                                   .before = TRUE)
        }
        if (length(results$media)) {
            results$media <-
                tibble::add_column(results$media,
                                   code = factor(rep(cd, nrow(results$media)), levels = all_codes),
                                   .before = TRUE)
        }
        if (length(results$tags)) {
            results$tags <-
                tibble::add_column(results$tags,
                                   code = factor(rep(cd, nrow(results$tags)), levels = all_codes),
                                   .before = TRUE)
        }
        if (length(results$sentiment)) {
            results$sentiment <-
                tibble::add_column(results$sentiment,
                                   code = factor(rep(cd, nrow(results$sentiment)), levels = all_codes),
                                   .before = TRUE)
        }
        if (length(results$phrases)) {
            results$phrases <-
                tibble::add_column(results$phrases,
                                   code = factor(rep(cd, nrow(results$phrases)), levels = all_codes),
                                   .before = TRUE)
        }
        
        results
    }
    
    dopar <- foreach::`%dopar%`
    results <- dopar(foreach::foreach(code = code, .combine = combine), #, .multicombine = TRUE), 
                     block(code))
    if (!is.null(pb)) setTxtProgressBar(pb, i)
    process_mentions(results)
}

#' @describeIn account_mentions
#' 
#' Returns mentions for an account represented by an \code{\link{account}} object.
#' 
#' @export
account_mentions.brandseye.account <- function(account, ...) {
    account_mentions(account$code, ..., authentication = account$auth)
}

#' @describeIn account_mentions
#' 
#' For querying mentions from accounts encoded as \code{factor}s. 
#' @export
account_mentions.factor <- function(account, ...) {
    account_mentions(as.character(account), ...)
}

#' Prints a summary of the results from a call to \code{\link{account_mentions}}
#' @export
#' @author Constance Neeser
print.mention.results <- function(mentions, ...) {
    cat("\nCall:\n")
    print(mentions$call)
    cat("\nNumber of mentions matching the filter:\n")
    cat(mentions$total, "\n")
    cat("\nNumber of mentions returned:\n")
    cat(nrow(mentions$mentions))
    cat("\n\n")
}
