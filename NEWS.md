# News

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