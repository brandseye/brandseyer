# News

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