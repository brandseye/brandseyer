# BrandsEyeR

This is a small library for accessing data in your BrandsEye account.

# Installing

Installation is currently fairly simple

    # Install the devtools package
    install.packages("devtools")
    library(devtools)
   
    # Install the library
    install_bitbucket("brandseye/brandseyer", auth_user="my bitbucket user", password="my bitbucket password")
   
    # Load the library
    library(brandseyer)

## Examples of use

    ac <- account("QUIR01BA", user = "my mash username", password="my mash password")
    count(ac, "published inthelast week")
    count(ac, "published inthelast week", groupby="published")
    listAccounts(user = "my mash user", password = "my mash password")
    
## Authentication

You will likely find manually authenticating each query tedious. You can automate this
using the authenticate function:

    ?authenticate
    
Specifically, if you're happy having your credentials saved on your hard drive, 
you can do this:

    authenticate(key = "this is my key", save = TRUE)
    
Please see the documentation for *authenticate* for some advice on securing that file.

Contact your client service representative to get your API key.
    
## Documentation

Documentation is in the early stages, but somewhat present and growing. It can be accessed in *R*
like so:

    ?account
    ?authentication
    ?listAccounts
    ?mentions
    
The ultimate documentation for the filter language (the *filter* argument 
taken by many of the functions for selecting data sets) and the various
grouping and inclusions options is the [BrandsEye api documentation][api-docs]. 
    
## Authentication

See the documentation for the *authenticate* and *authentication* functions. 
*authenticate* has functionality to store your authentication details on your 
system so that you don't always have to enter it.

## License

Copyright (c) 2015, Brandseye PTY (LTD) 

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


[api-docs]: https://api.brandseye.com/docs