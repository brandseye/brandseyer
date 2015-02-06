# BrandsEyeR

This is a small library for accessing data in your BrandsEye account.

# Installing

Installation is currently fairly simple

   # Install the devtools package
   install.package(devtools)
   
   # Install the library
   install_bitbucket("brandseye/brandseyer", auth_user="my bitbucket user", password="my bitbucket password")
   
   # Load the library
   library(brandseyer)

## Examples of use

   ac <- account("QUIR01BA", user = "my mash username", password="my mash password")
   count(ac, "published inthelast week")
   count(ac, "published inthelast week", groupby="published")

## License