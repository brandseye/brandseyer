# News

## 0.1.1
* bug fix: using *account_count* across multiple accounts would break when
  one of those accounts had mentions with missing (unknown) sentiment.
* *account_count* now obeys the *groupby* and *include* arguments when no
  filter is given.
* *account_mentions* can be called without a default filter.  