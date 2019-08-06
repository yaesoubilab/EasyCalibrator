#########################################################
## Example data
#########################################################

valid_observations <- tibble::tibble(
  year=seq(2002,2008),
  value=10*seq(1002,1008) # Made-up values
)

# Example of a target with two time-series: 'tbLatent' is divided by
# 'populationSize' for each year in 'tbLatent' that is also a year
# in 'populationSize'
valid_target_1    <- list(type="TS",
                          model=c("tbLatent", "populationSize"),
                          observed=valid_observations)

# Example of a target with one time-series
valid_target_2    <- list(type="TS",
                          model=c("tbSusceptible"),
                          observed=valid_observations)

valid_targets     <- list(valid_target_1, valid_target_2)

# It's conceivable that two identical, or very-similar targets could be
# included in the list of targets, so this is there to illustrate that
# capability.
valid_targets_dup <- list(valid_target_1, valid_target_1, valid_target_2)

# Invalid because a 'PTS' (PyramidTimeSeries) can't be used with
# 'valid_observations', which is a TimeSeries data
invalid_target_1 <- list(type="PTS",
                         model=c("mydata2"),
                         observed=valid_observations)

# Invalid because of the '.csv': there shouldn't be file extensions
invalid_target_2 <- list(type="TS",
                         model=c("mydata1", "mydata2.csv"),
                         observed=valid_observations)

