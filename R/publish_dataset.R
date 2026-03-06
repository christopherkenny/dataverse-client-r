#' @title Publish dataset
#' @description Publish/release Dataverse dataset
#' @template ds
#' @param minor A logical specifying whether the new release of the dataset is a \dQuote{minor} release (\code{TRUE}, by default), resulting in a minor version increase (e.g., from 1.1 to 1.2). If \code{FALSE}, the dataset is given a \dQuote{major} release (e.g., from 1.1 to 2.0).
#' @template envvars
#' @template dots
#' @details Use this function to \dQuote{publish} (i.e., publicly release) a draft Dataverse dataset. This creates a publicly visible listing of the dataset, accessible by its DOI, with a numbered version. This action cannot be undone.
#' There are no requirements for what constitutes a major or minor release, but a minor release might be used to update metadata (e.g., a new linked publication) or the addition of supplemental files. A major release is best used to reflect a substantial change to the dataset, such as would require a published erratum or a substantial change to data or code.
#' @return A list.
#' @seealso \code{\link{get_dataset}}, \code{\link{publish_dataverse}}
#' @examples
#' \dontrun{
#' meta <- list()
#' ds <- create_dataset("mydataverse", body = meta)
#' publish_dataset(ds)
#' }
#' @export
publish_dataset <- function(dataset, minor = TRUE, key = Sys.getenv("DATAVERSE_KEY"), server = Sys.getenv("DATAVERSE_SERVER"), ...) {
    dataset <- dataset_id(dataset, key = key, server = server, ...)
    u <- paste0(api_url(server), "datasets/", dataset, "/actions/:publish?type=", if (minor) "minor" else "major")
    req <- httr2::request(u) |>
        httr2::req_headers_redacted("X-Dataverse-key" = key) |>
        httr2::req_method("POST") |>
        httr2::req_error(body = function(resp) {
            tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE)$message, error = function(e) NULL)
        })
    r <- httr2::req_perform(req)
    httr2::resp_body_string(r)
}
