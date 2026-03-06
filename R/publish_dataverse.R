#' @title Publish Dataverse (SWORD)
#' @description Publish/re-publish a Dataverse via SWORD
#' @details This function is used to publish a (possibly already published) Dataverse. It is part of the SWORD API, which is used to upload data to a Dataverse server.
#' @param dataverse An object of class \dQuote{sword_collection}, as returned by \code{\link{service_document}}.
#' @template envvars
#' @template dots
#' @return A list.
#' @seealso Managing a Dataverse: \code{\link{publish_dataverse}}; Managing a dataset: \code{\link{dataset_atom}}, \code{\link{list_datasets}}, \code{\link{create_dataset}}, \code{\link{delete_dataset}}, \code{\link{publish_dataset}}; Managing files within a dataset: \code{\link{add_file}}, \code{\link{delete_file}}
#' @export
publish_dataverse <- function(dataverse, key = Sys.getenv("DATAVERSE_KEY"), server = Sys.getenv("DATAVERSE_SERVER"), ...) {
    if (inherits(dataverse, "sword_collection")) {
        u <- sub("/collection/", "/edit/", dataverse$url, fixed = TRUE)
    } else if (inherits(dataverse, "dataverse")) {
        dataverse <- dataverse$alias
        u <- paste0(api_url(server, prefix="dvn/api/"), "data-deposit/v1.1/swordv2/edit/dataverse/", dataverse)
    } else {
        # publish via native API
        dataverse <- dataverse_id(dataverse, key = key, server = server, ...)
        u <- paste0(api_url(server), "dataverses/", dataverse, "/actions/:publish")
        req <- httr2::request(u) |>
            httr2::req_headers_redacted("X-Dataverse-key" = key) |>
            httr2::req_method("POST") |>
            httr2::req_error(body = function(resp) {
                tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE)$message, error = function(e) NULL)
            })
        r <- httr2::req_perform(req)
        return(httr2::resp_body_json(r)$data)
    }
    # publish via sword API
    req <- httr2::request(u) |>
        httr2::req_auth_basic(key, "") |>
        httr2::req_headers("In-Progress" = "false") |>
        httr2::req_method("POST") |>
        httr2::req_error(body = function(resp) {
            tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE)$message, error = function(e) NULL)
        })
    r <- httr2::req_perform(req)
    out <- xml2::as_list(xml2::read_xml(httr2::resp_body_string(r)))
    # clean up response structure
    out
}
