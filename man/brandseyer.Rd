% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brandseyer.R
\docType{package}
\name{brandseyer}
\alias{brandseyer}
\alias{brandseye}
\alias{BrandsEye}
\alias{BrandsEyeR}
\alias{brandseyer-package}
\title{brandseyer: access and perform analytics on your BrandsEye accounts}
\description{
BrandsEye is a company that provides online monitoring, analytics,
and insights for social media data. We provide an extensive JSON / RESTful API
to access your data. This library is a wrapper for that data, providing 
easy access to the data in your various BrandsEye accounts for use in 
any R analyses and data visualisations.
}
\section{Starting points}{


To ease your use of the library, you should begin by adding your API key using
the authenticate function: 

\verb{
authenticate(key = "<your api key>", save = TRUE)
}

If you do not know what your api key is, contact BrandsEye to find out.

After this, you can easily see a list of your available accounts using 
\code{\link{list_account_codes}}. \code{\link{account_count}} will
let you pull aggregate information matching a given filter from one or more
of your accounts.
}

\section{Online resources}{

The home page is \url{https://github.com/brandseye/brandseyer/}

If you have found a bug in the library, you can report it using the 
library's GitHub issue tracker: \url{https://github.com/brandseye/brandseyer/issues}
}

\seealso{
\code{\link{list_account_codes}} Find out what accounts you have access to.

\code{\link{account_count}} Query your account.

\code{\link{account}} Find out about general account queries.
}
