
# padr 0.1.9000

## Bug fixes

* Issue #8: pad does no longer break when datetime variable contains one value only. Returns x and a warning, if start_val and end_val are NULL and will do proper padding when one or both are specified.

* Issue #9: when forgetting to specify at least one column, on which to apply the fill_ function, the fill_ function will now throw a meaningful error.


##################################################

# padr 0.1.0

* Initial release
