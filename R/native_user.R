# @title Create user
# @description Create a Dataverse user
# @details Create a new Dataverse user.
# @param password A character vector specifying the password for the new user.
# @template envvars
# @template dots
# @return A list.
# @seealso \code{\link{get_user_key}}
# @examples
# \dontrun{
# create_user("password")
# }
# @export
create_user <- function(password, key = Sys.getenv("DATAVERSE_KEY"), server = Sys.getenv("DATAVERSE_SERVER"), ...) {
    u <- paste0(api_url(server), "builtin-users?password=", password)
    req <- httr2::request(u) |>
        httr2::req_headers_redacted("X-Dataverse-key" = key) |>
        httr2::req_method("POST") |>
        httr2::req_error(body = function(resp) {
            tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE)$message, error = function(e) NULL)
        })
    r <- httr2::req_perform(req)
    httr2::resp_body_string(r)
}

#' @title Get API Key
#' @description Get a user's API key
#' @details Use a Dataverse server's username and password login to obtain an
#'  API key for the user. This can be used if one does not yet have an API key,
#'  or desires to reset the key. This function does not require an API \code{key}
#'  argument to authenticate, but \code{server} must still be specified.
#' @param user A character vector specifying a Dataverse server username.
#' @param password A character vector specifying the password for this user.
#' @param server The Dataverse instance. See `get_file`.
#'
#' @template dots
#'
#' @return A list.
#' @examples
#' \dontrun{
#'  # Replace Username and password with personal login
#'  get_user_key("username", "password", server = "dataverse.harvard.edu")
#' }
#' @export
get_user_key <- function(user, password, server = Sys.getenv("DATAVERSE_SERVER"), ...) {
    u <- paste0(api_url(server), "builtin-users/", user, "/api-token?password=", password)
    r <- api_get(u, ...)
    j <- jsonlite::fromJSON(r)
    j$data$message
}
