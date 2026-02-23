#' @title Delete draft dataset
#' @description Delete a dataset draft
#' @details This function can be used to delete a draft (unpublished) Dataverse dataset. Once published, a dataset cannot be deleted. An existing draft can instead be modified using \code{\link{update_dataset}}.
#' @template ds
#' @template envvars
#' @template dots
#' @return A logical.
#' @examples
#' \dontrun{
#' meta <- list()
#' ds <- create_dataset("mydataverse", body = meta)
#' delete_dataset(ds)
#' }
#' @seealso \code{\link{get_dataset}}, \code{\link{create_dataset}}, \code{\link{update_dataset}}, \code{\link{delete_dataset}}, \code{\link{publish_dataset}}
#' @export
delete_dataset <- function(dataset, key = Sys.getenv("DATAVERSE_KEY"), server = Sys.getenv("DATAVERSE_SERVER"), ...) {
    # can only delete a "draft" dataset
    dataset <- dataset_id(dataset, key = key, server = server, ...)
    u <- paste0(api_url(server), "datasets/", dataset, "/versions/:draft")
    req <- httr2::request(u) |>
        httr2::req_headers_redacted("X-Dataverse-key" = key) |>
        httr2::req_method("DELETE") |>
        httr2::req_error(body = function(resp) {
            tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE)$message, error = function(e) NULL)
        })
    r <- httr2::req_perform(req)
    httr2::resp_body_string(r)
}
