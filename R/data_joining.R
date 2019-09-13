requireNamespace("assertthat")
requireNamespace("dplyr")
requireNamespace("magrittr")
requireNamespace("purrr")
requireNamespace("readr")
requireNamespace("readr")
requireNamespace("rlang")
requireNamespace("stringr")
requireNamespace("tibble")

#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that

#########################################################
## Data: Join
#########################################################

# mod, obs -> tbl[model, observed]
# Joins model data to observed data using the grouping vars
JoinModelObserved <- function(mod, obs)
  dplyr::inner_join(mod,
                    obs,
                    IDGroupingVars(mod, obs),
                    suffix=c(".mod", ".obs")) %>%
  dplyr::mutate(model     = value.mod,
                observed  = value.obs,
                value.mod = NULL,
                value.obs = NULL)

# obs [tbl] and [joined] -> bool
# Check to make sure that every observed value was joined to
# a model value
CheckJoin <- function(obs, joined) 
  all.equal(NRows(joined), NRows(obs))

# tar -> list[type=string, data[ tbl[model,observed] ]]
# Take a target which is ready for its join, do the join, reorganize
# the target
JoinTarget <- function(tar)
  purrr::list_modify(tar, data=JoinModelObserved(tar$model, tar$observed))

# list of targets -> list of joined targets
JoinAllTargets <- purrr::partial(purrr::map, ...=, JoinTarget)

