as.data.frame.registry <-
function(x, ...)
{
    .one_line <- function(entry) {
        entry <- lapply(.functions_to_characters(entry),
                        function(i) i[[1]])
        data.frame(unclass(entry), ...)
    }
    
    ret <- do.call(rbind, lapply(x$get_entries(), .one_line))
    row.names(ret) <- NULL
    ret
}

.functions_to_characters <-
function(x)
{
    ## transform function entries into character strings
    funs <- sapply(x, inherits, "function")
    for (field in names(x)[funs])
        x[[field]] <- paste(format(x[[field]]), collapse = "")
    x
}
