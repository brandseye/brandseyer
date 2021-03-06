# News

## 0.19.0 2018-05-08
* *update* Now supports setting the location in V4 accounts.

## 0.18.0 2018-09-11
* *account_tags* Now indicates if a tag is deleted
* *account_brands* Now indicates the tier and the associated topic tree for the brand.

## 0.17.0 2018-09-10
* *account_tags* Now supplies the list of child tags.
* Various account functions have been updated to make better use
  of tidy universe functions.

## 0.16.1 2018-04-09
* bug fix: brandseyer now works better in shiny
* *account_mentions* Now properly handles non SAST timezones when querying data.

## 0.16.0 2018-02-09
* *update* now checks that there are authentication credentials
* renamed *account_storage* to *account_api_version*
* bug fix: can now fetch all data from V4 accounts


## 0.15.0 2018-02-08
* Can now update V4 accounts
* Account information is now fetched directly from mash.
* Account objects now have an additional class indicating their storage type.
* *account_storage* New method added to determine if an account is V3 or V4.
* bug fix: *account_count* now correctly passes authentication info for account objects.
* bug fix: *summary* now works again.

## 0.14.0 2018-01-26
* *account_mentions* No longer returns title and extract for tweets.
* Better support for reporting to the user that they need to authenticate themselves.

## 0.13.0 2017-10-11
* *account_update* Added support for updating media type

## 0.12.1 2017-09-14
* *account_mentions* bug fix: we now support the all flag when pulling mentions
  from multiple accounts.
* *account_mentions* bug fix: better handling of empty sentiment values  

## 0.12.0 2017-07-11
* *update_mention* can now update sentiment. 
* bug fix: grouping by sentimentVerified now works.

## 0.11.0 2017-06-12
* Added a test form for a new update function, *account_update*. This is not
  a stable version of the function, and its behaviour may change in the near 
  future. 

## 0.10.1 2017-05-30
* bug fix: *account_mentions* now properly reads mentions that have multiple
  values but are missing sentiment text.

## 0.10.0 2017-04-18
* *account_mentions* now supports the _select_ parameter to select only certain
  fields from a mention.

## 0.9.2 2017-03-13
* bug fix: mentions that have multiple sentiment values are now properly read 
  from the api.

## 0.9.1 2017-02-22
* bug fix: mentions that have no sentiment values are now properly read from the
  api.

## 0.9.0 2016-12-09
* _account_mentions_ now uses tibbles rather than data frames.
* The documentation is slightly expanded.

## 0.8.1 2016-12-09
* bug fix: Tags that did not have descriptions could cause 
  *account_mentions* to return no data.

## 0.8.0 2016-11-29
* Tag information returned by *account_mentions* now returns the tag's namespace as well.
* bug fix: a bug was introduced that required dplyr to be always loaded. This is
  no longer the case

## 0.7.4 2016-05-06
* bug fix: Fixed a problem with various accounts return 0 data.

## 0.7.3 2015-09-17
* bug fix: Reverting a previous change which resulted in no data being returned.

## 0.7.2 2015-09-02
* bug fix: One account was return 0 mentions even though it had data. 

## 0.7.1 2015-09-02
* bug fix: can once again use *account_count* across multiple accounts.

## 0.7.0 2015-08-13
* *account_count* can now count more than just mentions, such as authors, sites,
  and so on.

## 0.6.0 2015-06-20
* Some experimental ability to automatically get all mentions matching 
  a filter in one call to *account_mentions*, rather than manually 
  calling it multiple times.

## 0.5.0 2015-06-15
* *account_count* and *account_mentions* both now support account codes
  given to them as factor variables. 

## 0.4.0 2015-06-10
* Provided a number of functions for generic methods dealing with accounts
  that are specialised to handle account codes as factors. This is useful
  when dealing with account codes returned by *account_count* and 
  *account_mentions*. 

## 0.3.0 2015-06-06
* There is now experimental support to query mentions from across
  multiple accounts by passing a vector of accounts to *account_mentions*.

## 0.2.0 2015-06-02
* sentiment values from the count endpoint are now represented as ordered
  factors with class c("sentiment", "ordered", "factor"). A function, 
  *sentiment*, exists to easily create sentiment values for comparison
  purposes.

## 0.1.2 2015-05-29
* bug fix: calling *account* on an unrecognised account code now results
  in a usable error message.

## 0.1.1 2015-05-28
* bug fix: using *account_count* across multiple accounts would break when
  one of those accounts had mentions with missing (unknown) sentiment.
* *account_count* now obeys the *groupby* and *include* arguments when no
  filter is given.
* *account_mentions* can be called without a default filter.
* Better error messages for bad filters when using *account_mentions*.
* Objects returned by *account_mentions* store their call parameters.
* Better support for the include parameter in *account_mentions*.