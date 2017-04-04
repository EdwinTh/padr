# padr 0.3.0

## Changes

#### Interval no long required to be of a single unit

The interval is no longer limited to be of a single unit, for each of the eight interval sizes. Every time span accepted by `seq.Date` or `seq.POSIXt` is now accepted. Since the original implementation was fully around single-unit-intervals, some default behavior had to change. Because of it, this version is not entirely backwards compatible with earlier versions of `padr`. The following functions are affected:

* `thicken`: the `interval` argument now has to be specified. In earlier versions it was optional. When it was not specified, the added variable was one interval level higher than that of the input datetime variable. With the widening of the interval definition, there is not longer a natural step up.

* `get_interval`: does no longer only retrieve the interval of a datetime variable, but also its unit (the step size). For instance, the following would have returned "day" in the past, but will now return "2 day":

date_var <- as.Date(c('2017-01-01', '2017-01-03', '2017-01-05'))
get_interval(date_var)

* `pad`: since the default behavior, when the interval is not specified, depends `get_interval`, its outcome might now be different. When `get_interval` returns a different interval than it used to, `pad` will do the padding at this different interval. Extending the above example, the have resulted in a data frame with two padded rows:

x <- data.frame(date_var, y = 1:3)

Since the interval of `date_var` used be "day", there were missing records for 2017-01-02 and 2017-01-04. These records were inserted, with missing values for y. However, now the interval of `date_var` is "2 day" and on this level there is no need for padding. To get ther original result the interval argument should be specified with "day".

#### fill_by functions default behavior is changed. They used to require specification of all the column names that had to filled. This is annoying when many columns had to filled. The functions no longer break when no variable names are specified, but they fill all columns in the data frame.

## New features

# pad_int

The new function pad_int does padding of an integer field. Its working is very similar to the general pad. The by argument must alway be specified, since a data.frame would almost alway contain multiple numeric columns. Instead of the interval, one can specify the step size by which the integer increases. 

## Bug fixes

# Issue #13 When the end_val is specified in `pad`, it would mistakenly update the start_val with its value. This resulted in the return of the only the last line of the padded data.frame, instead of the full padded data.frame.

# Issue #14 When dt_var has NULL as timezone, `to_posix` (helper of `round_thicken`, which itself is a helper of `thicken`) used to break, and thereby `thicken` itself broke.


##################################################

# padr 0.2.1

## Bug fixes

* Issue #11: in the pad function the by argument did not reach the lower function pad_single and pad_multiple. As a result pad broke when using this argument. This bug is fixed by using do.call in both pad and pad_multiple.

##################################################

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
