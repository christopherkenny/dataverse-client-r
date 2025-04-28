#' @param version A character specifying a version of the dataset.
#'  This can be of the form `"1.1"` or `"1"` (where in `"x.y"`, x is a major
#'  version and y is an optional minor version). As of v0.3.14, setting a version
#'  in this way will cache the dataset (See example in \code{\link{cache_dataset}})
#'  so that it will not re-download the file the second time and read from the cache.
#'  Finally, set `use_cache = "none"` to not read from the cache and re-download
#'  afresh even when `version` is provided.
#'  If the user specifies a `key` or `DATAVERSE_KEY` argument, they can access the
#'  draft version by `":draft"` (the current draft) or `":latest"` (which will
#'  prioritize the draft over the latest published version).
