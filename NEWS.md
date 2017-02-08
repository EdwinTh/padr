
# padr 0.2.0

## New feature

pad has gained a group parameter. This takes a character vector that indicates the column names within which group padding must be done. The returned data frame is complete for the grouping variable(s). Leaving no longer the doubt which record belongs to which group member, especially when start_val and / or end_val was specified.

## Bug fixes

* Issue #8: pad does no longer break when datetime variable contains one value only. Returns x and a warning, if start_val and end_val are NULL and will do proper padding when one or both are specified.

* Issue #9: when forgetting to specify at least one column, on which to apply the fill_ function, the fill_ function will now throw a meaningful error.

* Issue #10: pad was broken with an error the interval was quarter, month, or year. This was done by check_start_end, even when neither a start_val nor an end_val was specified. It appeared that when concatenating POSIX vectors, as happened in the check_start_end function, the result is enforced to the timezone of the locale (including daylight savings time). This breaks the interval if the original vectors were not of this timezone. Workaround is implemented. 

##################################################

# padr 0.1.0

* Initial release
