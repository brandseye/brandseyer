# BrandsEyeR

This is a small library for accessing data in your BrandsEye account.

# Installing

Installation is currently fairly simple

    # Install the devtools package
    install.packages(devtools)
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
    
## Documentation

Documentation is in the early stages, but somewhat present and growing. It can be accessed in *R*
like so:

    ?account
    ?authentication
    ?listAccounts
    ?mentions

## License