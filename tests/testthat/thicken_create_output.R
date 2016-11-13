# This function creates the output to compare two the unit tests, since
# it is not feasible to write the full output by hand.

# output will always be the full year 2016, as specified in the tests.
test_thicken_output <- function(start_val,
                                end_val,
                                from_interval,
                                to_interval){

  all_ints <- c('year', 'quarter', 'month', 'week', 'day', 'hour','min', 'sec')
  all_combs <- t(combn(all_ints, 2)) %>% as.data.frame %>% select(2,1)

  colnames(all_combs) <- c('from', 'to')

}
