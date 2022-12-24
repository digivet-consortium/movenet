test_that(,{
  data <- reformat()
})
#coarsen_date works with expected data, existing level, agg = TRUE
#coarsen_date works with expected data, existing level, agg = FALSE
#coarsen_date raises error with expected data, existing level, agg = something else

#test config does not match data

#test prefix does not match with provided key

#test that functions give different responses for jitter or round being 0 vs FALSE (and for 1 vs TRUE if relevant)

#test diff combinations of jitter / round (FALSE, 0,)

#test output of anonymise does not have cols with names as attributes
#(previously the anonymised cols had their old ids as names still)
