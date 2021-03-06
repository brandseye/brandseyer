% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accounts.R
\name{account_api_version}
\alias{account_api_version}
\alias{account_api_version.brandseye.account}
\alias{account_api_version.character}
\alias{account_api_version.factor}
\title{Indicates the api version the account is available through.}
\usage{
account_api_version(account)

\method{account_api_version}{brandseye.account}(account)

\method{account_api_version}{character}(accounts)

\method{account_api_version}{factor}(accounts)
}
\description{
Indicates the api version used by the account, such as whether it's V4 or the older V3.
}
\section{Methods (by class)}{
\itemize{
\item \code{brandseye.account}: Returns the api version from an \code{\link{account}} object.

\item \code{character}: Returns the api version from a vector of account codes.

\item \code{factor}: Returns the api version of an account given by a factor of the account code.
}}

\author{
Constance Neeser
}
