#' @title Delete Dataverse
#' @description Delete a dataverse
#' @details This function deletes a Dataverse.
#' @template dv
#' @template envvars
#' @template dots
#' @return A logical.
#' @examples
#' \dontrun{
#' dv <- create_dataverse("mydataverse")
#' delete_dataverse(dv)
#' }
#' @seealso To manage Dataverses: \code{\link{create_dataverse}}, \code{\link{publish_dataverse}}, \code{\link{dataverse_contents}}; to get datasets: \code{\link{get_dataset}}; to search for Dataverses, datasets, or files: \code{\link{dataverse_search}}
#' @export
delete_dataverse <- function(dataverse, key = Sys.getenv("DATAVERSE_KEY"), server = Sys.getenv("DATAVERSE_SERVER"), ...) {
    dataverse <- dataverse_id(dataverse, key = key, server = server, ...)
    u <- paste0(api_url(server), "dataverses/", dataverse)
    req <- httr2::request(u) |>
        httr2::req_headers_redacted("X-Dataverse-key" = key) |>
        httr2::req_method("DELETE") |>
        httr2::req_error(body = function(resp) {
            tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE)$message, error = function(e) NULL)
        })
    r <- httr2::req_perform(req)
    httr2::resp_body_string(r)
}
