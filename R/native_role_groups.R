# @rdname role_groups
# @title Role groups
# @description Create, update, and delete role groups
# @details Role groups can be used to organize roles for a Dataverse.
# @template dv
# @template group
# @param alias A character string specifying an alias for the role group.
# @param name A character string specifying the name of the role group.
# @param description A character string specifying a description of the role group.
# @template envvars
# @template dots
# @return A list of class \dQuote{dataverse_group}.
# @seealso \code{\link{add_roles_to_group}}
# @examples
# \dontrun{
# # create a group
# g <- create_group("my_dataverse", "testgroup", "aGroupName", "this is a test group")
#
# # update group details
# update_group(g, description = "this needs a new description")
#
# # get group details
# get_group(g)
#
# # list all groups
# list_groups("my_dataverse")
#
# # delete group
# delete_group(g)
# }
# @export
create_group <- function(dataverse, alias, name, description, key = Sys.getenv("DATAVERSE_KEY"), server = Sys.getenv("DATAVERSE_SERVER"), ...) {
    b <- list(description = description,
              displayName = name,
              aliasInOwner = alias)
    dataverse <- dataverse_id(dataverse, key = key, server = server, ...)
    u <- paste0(api_url(server), "dataverses/", dataverse, "/groups")
    req <- httr2::request(u) |>
        httr2::req_headers_redacted("X-Dataverse-key" = key) |>
        httr2::req_body_json(b) |>
        httr2::req_error(body = function(resp) {
            tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE)$message, error = function(e) NULL)
        })
    r <- httr2::req_perform(req)
    j <- jsonlite::fromJSON(httr2::resp_body_string(r))$data
    j$dataverse <- dataverse
    structure(j, class = "dataverse_group")
}

# @rdname role_groups
# @export
update_group <- function(group, name, description, dataverse, key = Sys.getenv("DATAVERSE_KEY"), server = Sys.getenv("DATAVERSE_SERVER"), ...) {
    b <- list()
    if (inherits(group, "dataverse_group")) {
        b$groupAliasInOwner <- group$groupAliasInOwner
        if (missing(name)) {
            b$displayName <- group$displayName
        }
        if (missing(description)) {
            b$description <- group$description
        }
        dataverse <- dataverse_id(group$dataverse, key = key, server = server, ...)
        u <- paste0(api_url(server), "dataverses/", dataverse, "/groups/", group$groupAliasInOwner)
    } else {
        b$groupAliasInOwner <- group
        if (!missing(name)) {
            b$displayName <- name
        }
        if (!missing(description)) {
            b$description <- description
        }
        dataverse <- dataverse_id(dataverse, key = key, server = server, ...)
        u <- paste0(api_url(server), "dataverses/", dataverse, "/groups/", group)
    }
    req <- httr2::request(u) |>
        httr2::req_headers_redacted("X-Dataverse-key" = key) |>
        httr2::req_body_json(b) |>
        httr2::req_method("PUT") |>
        httr2::req_error(body = function(resp) {
            tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE)$message, error = function(e) NULL)
        })
    r <- httr2::req_perform(req)
    j <- jsonlite::fromJSON(httr2::resp_body_string(r))$data
    j$dataverse <- dataverse
    structure(j, class = "dataverse_group")
}

# @rdname role_groups
# @export
list_groups <- function(dataverse, key = Sys.getenv("DATAVERSE_KEY"), server = Sys.getenv("DATAVERSE_SERVER"), ...) {
    dataverse <- dataverse_id(dataverse, key = key, server = server, ...)
    u <- paste0(api_url(server), "dataverses/", dataverse, "/groups")
    r <- api_get(u, ..., key = key)
    j <- jsonlite::fromJSON(r, simplifyDataFrame = FALSE)$data
    lapply(j, function(x) {
        x$dataverse <- dataverse
        class(x) <- "dataverse_group"
        x
    })
}

# @rdname role_groups
# @export
get_group <- function(group, dataverse, key = Sys.getenv("DATAVERSE_KEY"), server = Sys.getenv("DATAVERSE_SERVER"), ...) {
    if (inherits(group, "dataverse_group")) {
        dataverse <- dataverse_id(group$dataverse, key = key, server = server, ...)
        u <- paste0(api_url(server), "dataverses/", dataverse, "/groups/", group$groupAliasInOwner)
    } else {
        dataverse <- dataverse_id(dataverse, key = key, server = server, ...)
        u <- paste0(api_url(server), "dataverses/", dataverse, "/groups/", group)
    }
    r <- api_get(u, ..., key = key)
    j <- jsonlite::fromJSON(r)$data
    j$dataverse <- dataverse
    structure(j, class = "dataverse_group")
}

# @rdname role_groups
# @export
delete_group <- function(group, dataverse, key = Sys.getenv("DATAVERSE_KEY"), server = Sys.getenv("DATAVERSE_SERVER"), ...) {
    if (inherits(group, "dataverse_group")) {
        dataverse <- dataverse_id(group$dataverse, key = key, server = server, ...)
        group <- group$groupAliasInOwner
    } else {
        dataverse <- dataverse_id(dataverse, key = key, server = server, ...)
    }
    u <- paste0(api_url(server), "dataverses/", dataverse, "/groups/", group)
    req <- httr2::request(u) |>
        httr2::req_headers_redacted("X-Dataverse-key" = key) |>
        httr2::req_method("DELETE") |>
        httr2::req_error(body = function(resp) {
            tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE)$message, error = function(e) NULL)
        })
    r <- httr2::req_perform(req)
    out <- jsonlite::fromJSON(httr2::resp_body_string(r))
    if (out$status == "OK") {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

# @rdname add_roles
# @title Add/Remove role to/from group
# @description Add/remove role from group
# @details Role groups can be used to organize roles for a Dataverse. These functions add or remove roles from a group
# @template group
# @template role
# @param dataverse A character string specifying a Dataverse name or an object of class \dQuote{dataverse}. Can be missing if \code{role} is an object of class \dQuote{dataverse_group}.
# @template envvars
# @template dots
# @return A list.
# @export
add_roles_to_group <- function(group, role, dataverse, key = Sys.getenv("DATAVERSE_KEY"), server = Sys.getenv("DATAVERSE_SERVER"), ...) {

    if (inherits(group, "dataverse_group")) {
        dataverse <- dataverse_id(group$dataverse, key = key, server = server, ...)
        group <- group$groupAliasInOwner
    } else {
        dataverse <- dataverse_id(dataverse, key = key, server = server, ...)
    }

    # need support for bulk add

    if(is.null(server) || server == "") {
        stop("'server' is missing, but required")
    }
    u <- paste0(api_url(server), "dataverses/", dataverse, "/groups/", group, "/roleAssignees/", role)
    req <- httr2::request(u) |>
        httr2::req_headers_redacted("X-Dataverse-key" = key) |>
        httr2::req_method("PUT") |>
        httr2::req_error(body = function(resp) {
            tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE)$message, error = function(e) NULL)
        })
    r <- httr2::req_perform(req)
    j <- jsonlite::fromJSON(httr2::resp_body_string(r))$data
    j
}

# @rdname add_roles
# @export
remove_role_from_group <- function(group, role, dataverse, key = Sys.getenv("DATAVERSE_KEY"), server = Sys.getenv("DATAVERSE_SERVER"), ...) {
    if (inherits(group, "dataverse_group")) {
        dataverse <- dataverse_id(group$dataverse, key = key, server = server, ...)
        group <- group$groupAliasInOwner
    } else {
        dataverse <- dataverse_id(dataverse, key = key, server = server, ...)
    }
    u <- paste0(api_url(server), "dataverses/", dataverse, "/groups/", group, "/roleAssignees/", role)
    req <- httr2::request(u) |>
        httr2::req_headers_redacted("X-Dataverse-key" = key) |>
        httr2::req_method("DELETE") |>
        httr2::req_error(body = function(resp) {
            tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE)$message, error = function(e) NULL)
        })
    r <- httr2::req_perform(req)
    httr2::resp_body_string(r)
}
