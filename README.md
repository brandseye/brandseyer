<!-- README.md is generated from README.Rmd. Please edit that file -->
BrandsEyeR
==========

[BrandsEye](http://www.brandseye.com) is a company that provides online monitoring, analytics, and insights for social media data. We provide an extensive JSON / RESTful API to access to the data that makes up your various BrandsEye accounts. This library is a wrapper for that API, and makes it easy to access the data outside of the BrandsEye application in any [R](http://www.r-project.org/) analyses that you might want to perform.

Installing
----------

Installation is currently done via the [devtools package](http://www.rstudio.com/products/rpackages/devtools/):

``` r
# Install the devtools package
install.packages("devtools")
library(devtools)

# Install the library
install_github("brandseye/brandseyer")

# Load the library
library(brandseyer)
```

Examples of use
---------------

You can create an account object using:

``` r
library(brandseyer)
ac <- account("QUIR01BA", user = "my mash username", password="my mash password")    
```

and then you can query it using *account\_count*:

``` r
library(dplyr)
# Break down mention counts per country for the month of January, 2015.
filter <- "published after '2015/01/01' and published before '2015/02/01'"
countries <- account_count(ac, filter, groupby = "country") %>%
             # And here we sort by the count field using the dplyr package
             arrange(desc(count))
```

And here are the first few entries of the returned data frame:

| countryName    |  count| country |
|:---------------|------:|:--------|
| Unknown        |  23523| NA      |
| United States  |  12781| US      |
| United Kingdom |   3238| GB      |
| Canada         |   1293| CA      |
| India          |    950| IN      |
| France         |    710| FR      |

The call to BrandsEye can be simplified to one line, and can also be done across all of the accounts that you have:

``` r

account_count(list_account_codes(), 
              "published after '2015/01/01' and published before '2015/02/01'",
              groupby = "country")
```

Authentication
--------------

You can automate this using the authenticate function:

    ?authenticate

Specifically, if you're happy having your credentials saved on your hard drive, you can do this:

``` r
authenticate(key = "this is my key", save = TRUE)
```

Please see the documentation for *authenticate* for some advice on securing that file.

Contact your client service representative to get your API key. If you are ever concerned that others may have acquired your key, contact client service and they'll supply you with a new one (and invalidate your old).

Documentation
-------------

All functions are documented within *R* itself. Interesting functions to read up on are:

-   *account*
-   *account\_count*
-   *list\_account\_codes*

The ultimate documentation for the filter language (the *filter* argument taken by many of the functions for selecting data sets) and the various grouping and inclusions options is the [BrandsEye api documentation](https://api.brandseye.com/docs).

License
-------

Copyright (c) 2015, Brandseye PTY (LTD)

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
