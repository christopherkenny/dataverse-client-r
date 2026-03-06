#' @rdname create_dataset
#' @title Create or update a dataset
#' @description Create or update dataset within a Dataverse
#' @details \code{create_dataset} creates a Dataverse dataset. In Dataverse, a \dQuote{dataset} is the lowest-level structure in which to organize files. For example, a Dataverse dataset might contain the files used to reproduce a published article, including data, analysis code, and related materials. Datasets can be organized into \dQuote{Dataverse} objects, which can be further nested within other Dataverses. For someone creating an archive, this would be the first step to producing said archive (after creating a Dataverse, if one does not already exist). Once files and metadata have been added, the dataset can be published (i.e., made public) using \code{\link{publish_dataset}}.
#'
#' \code{update_dataset} updates a Dataverse dataset that has already been created using \code{\link{create_dataset}}. This creates a draft version of the dataset or modifies the current draft if one is already in-progress. It does not assign a new version number to the dataset nor does it make it publicly visible (which can be done with \code{\link{publish_dataset}}).
#'
#' @template dv
#' @template ds
#' @param body A list describing the dataset.
#' @template envvars
#' @template dots
#' @return An object of class \dQuote{dataverse_dataset}.
#' @seealso \code{\link{get_dataset}}, \code{\link{delete_dataset}}, \code{\link{publish_dataset}}
#' @examples
#' \dontrun{
#' meta <- list()
#' ds <- create_dataset("mydataverse", body = meta)
#'
#' meta2 <- list()
#' update_dataset(ds, body = meta2)
#'
#' # cleanup
#' delete_dataset(ds)
#' }
#' @export
create_dataset <- function(dataverse, body, key = Sys.getenv("DATAVERSE_KEY"), server = Sys.getenv("DATAVERSE_SERVER"), ...) {
    dataverse <- dataverse_id(dataverse, key = key, server = server, ...)
    u <- paste0(api_url(server), "dataverses/", dataverse, "/datasets/")
    req <- httr2::request(u) |>
        httr2::req_headers_redacted("X-Dataverse-key" = key) |>
        httr2::req_body_json(body) |>
        httr2::req_error(body = function(resp) {
            tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE)$message, error = function(e) NULL)
        })
    r <- httr2::req_perform(req)
    httr2::resp_body_json(r)
}

#' @rdname create_dataset
#' @export
update_dataset <- function(dataset, body, key = Sys.getenv("DATAVERSE_KEY"), server = Sys.getenv("DATAVERSE_SERVER"), ...) {
    dataset <- dataset_id(dataset, key = key, server = server, ...)
    u <- paste0(api_url(server), "datasets/", dataset, "/versions/:draft")
    req <- httr2::request(u) |>
        httr2::req_headers_redacted("X-Dataverse-key" = key) |>
        httr2::req_body_json(body) |>
        httr2::req_method("PUT") |>
        httr2::req_error(body = function(resp) {
            tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE)$message, error = function(e) NULL)
        })
    r <- httr2::req_perform(req)
    httr2::resp_body_string(r)
}
