# @rdname role_assignments
# @title Role assignments
# @description Get, set, and delete role assignments for a Dataverse
# @details \code{assign_role} assigns a Dataverse role to a user.
# @template dv
# @param assignee \dots
# @param role \dots
# @param assignment \dots
# @template envvars
# @template dots
# @return A list of objects of class \dQuote{dataverse_role_assignment}.
# @examples
# \dontrun{
# # get role assignments
# get_assignments("my_dataverse")
# }
# @seealso \code{\link{create_role}}
# @export
get_assignments <- function(dataverse, key = Sys.getenv("DATAVERSE_KEY"), server = Sys.getenv("DATAVERSE_SERVER"), ...) {
    dataverse <- dataverse_id(dataverse, key = key, server = server, ...)
    u <- paste0(api_url(server), "dataverses/", dataverse, "/assignments")
    r <- api_get(u, ..., key = key)
    out <- jsonlite::fromJSON(r, simplifyDataFrame = FALSE)$data
    lapply(out, function(x) {
        x$dataverse <- dataverse
        class(x) <- "dataverse_role_assignment"
        x
    })
}

# @rdname role_assignments
# @export
assign_role <- function(dataverse, assignee, role, key = Sys.getenv("DATAVERSE_KEY"), server = Sys.getenv("DATAVERSE_SERVER"), ...) {
    dataverse <- dataverse_id(dataverse, key = key, server = server, ...)
    u <- paste0(api_url(server), "dataverses/", dataverse, "/assignments")
    b <- list(assignee = assignee, role = role)
    req <- httr2::request(u) |>
        httr2::req_headers_redacted("X-Dataverse-key" = key) |>
        httr2::req_body_json(b) |>
        httr2::req_error(body = function(resp) {
            tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE)$message, error = function(e) NULL)
        })
    r <- httr2::req_perform(req)
    j <- jsonlite::fromJSON(httr2::resp_body_string(r))$data
    j
}

# @rdname role_assignments
# @export
delete_assignment <- function(dataverse, assignment, key = Sys.getenv("DATAVERSE_KEY"), server = Sys.getenv("DATAVERSE_SERVER"), ...) {
    dataverse <- dataverse_id(dataverse, key = key, server = server, ...)
    u <- paste0(api_url(server), "dataverses/", dataverse, "/assignments/", assignment)
    req <- httr2::request(u) |>
        httr2::req_headers_redacted("X-Dataverse-key" = key) |>
        httr2::req_method("DELETE") |>
        httr2::req_error(body = function(resp) {
            tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE)$message, error = function(e) NULL)
        })
    r <- httr2::req_perform(req)
    httr2::resp_body_string(r)
}
