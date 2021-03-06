% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accounts.R
\name{client_service}
\alias{client_service}
\alias{client_service.brandseye.account}
\alias{client_service.character}
\alias{client_service.factor}
\alias{client_service.list}
\title{Client service details}
\usage{
client_service(account)

\method{client_service}{brandseye.account}(account)

\method{client_service}{character}(code)

\method{client_service}{factor}(code)

\method{client_service}{list}(accounts)
}
\value{
Returns a structure holding information about the 
 account's client service person, with \code{name} and
 \code{email} fields.

As a special case, when given a vector of accounts, 
  this function returns a data frame with name and email columns,
  and a row per account.
}
\description{
Find out the details for the client service person to contact
for queries related to your account.
}
\section{Methods (by class)}{
\itemize{
\item \code{brandseye.account}: Returns client service details for an account object

\item \code{character}: Returns client service information for an account code.

\item \code{factor}: Returns the client service information for an account represented by a factor object.

\item \code{list}: Returns client service information for a vector of \code{\link{account}} objects.
}}

\examples{
\dontrun{

details <- client_service(account("QUIR01BA"))
details$name
details$email

}
\dontrun{

client_service("QUIR01BA")

}
\dontrun{

client_service(c("QUIR01BA", "BEAD33AA"))
client_service(list_account_codes())

}
}
\author{
Constance Neeser
}
