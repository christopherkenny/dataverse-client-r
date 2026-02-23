#' @title Dataverse metadata
#' @description Get metadata for a named Dataverse.
#' @details This function returns a list of metadata for a named Dataverse. Use \code{\link{dataverse_contents}} to list Dataverses and/or datasets contained within a Dataverse or use \code{\link{dataset_metadata}} to get metadata for a specific dataset.
#' @template dv
#' @template envvars
#' @template dots
#' @return A list
#' @seealso \code{\link{set_dataverse_metadata}}
#' @examples
#' \dontrun{
#' # download file from:
#' # https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/ARKOTI
#' monogan <- get_dataverse("monogan")
#' monogan_data <- dataverse_contents(monogan)
#' dataverse_metadata(monogan)
#' }
#' @export
dataverse_metadata <- function(dataverse, key = Sys.getenv("DATAVERSE_KEY"), server = Sys.getenv("DATAVERSE_SERVER"), ...) {
    dataverse <- dataverse_id(dataverse, key = key, server = server, ...)
    u <- paste0(api_url(server), "dataverses/", dataverse, "/metadatablocks")
    r <- api_get(u, ..., key = key)
    jsonlite::fromJSON(r, simplifyDataFrame = FALSE)$data
}

#' @title Set Dataverse metadata
#' @description Set Dataverse metadata
#' @details This function sets the value of metadata fields for a Dataverse. Use \code{\link{update_dataset}} to set the metadata fields for a dataset instead.
#' @template dv
#' @param body A list.
#' @param root A logical.
#' @template envvars
#' @template dots
#' @return A list
#' @seealso \code{\link{dataverse_metadata}}
#' @export
set_dataverse_metadata <- function(dataverse, body, root = TRUE, key = Sys.getenv("DATAVERSE_KEY"), server = Sys.getenv("DATAVERSE_SERVER"), ...) {
    dataverse <- dataverse_id(dataverse, key = key, server = server, ...)
    u <- paste0(api_url(server), "dataverses/", dataverse, "/metadatablocks/", tolower(as.character(root)))
    req <- httr2::request(u) |>
        httr2::req_headers_redacted("X-Dataverse-key" = key) |>
        httr2::req_method("POST") |>
        httr2::req_error(body = function(resp) {
            tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE)$message, error = function(e) NULL)
        })
    r <- httr2::req_perform(req)
    httr2::resp_body_json(r)$data
}
