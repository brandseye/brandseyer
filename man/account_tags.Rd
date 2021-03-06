% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accounts.R
\name{account_tags}
\alias{account_tags}
\alias{account_tags.brandseye.account}
\alias{account_tags.character}
\alias{account_tags.factor}
\alias{account_tags.list}
\title{List tags in an account}
\usage{
account_tags(account, ...)

\method{account_tags}{brandseye.account}(account, .process = FALSE)

\method{account_tags}{character}(account)

\method{account_tags}{factor}(account)

\method{account_tags}{list}(accounts)
}
\arguments{
\item{.process}{Set to \code{TRUE} if you do not want IDs set as factors.}
}
\description{
This returns a data frame of tags available in an account, along with the 
IDs of those tags.
}
\section{Methods (by class)}{
\itemize{
\item \code{brandseye.account}: Returns tag information for \code{\link{account}} objects.

\item \code{character}: Returns tag information an account identified using an account code

\item \code{factor}: Returns tag information for an account represented by a factor of its account code.

\item \code{list}: Returns tag information for a list of \code{\link{account}} objects
}}

\examples{
\dontrun{
account_tags("QUIR01BA")
account_tags(c("QUIR01BA", "BEAD33AA")
}
}
\author{
Constance Neeser
}
