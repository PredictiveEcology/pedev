globalVariables(c(":=", "fileSize", "fileSizeWOLinks", "size"))

#' Calculate file sizes omitting hard links
#'
#' If two files are hardlinks, they don't actually take up extra space
#' on disk: there is only one copy of the data and two pointers to
#' the data.
#'
#' @param path The path to evaluate file sizes in. Must be a directory.
#' @param recursive Logical indicating whether to search recursively.
#'
#' @export
#' @rdname fileSize
#' @inheritParams utils::object.size
file.sizeWOLinks <- function(path = ".", units = "auto", recursive = FALSE) {
  basePath <- path
  if (recursive) {
    path <- unique(dirname(dir(recursive = TRUE, path = path, full.names = TRUE)))
  }
  names(path) <- path
  d <- lapply(path, function(p) {
    old <- setwd(p)
    a <- try(system("ls -i", intern = TRUE))
    if (is(a, "try-error")) {
      stop("Can't run this function because can't access 'ls' function (a unix function)")
    }
    a <- grep("/", a, invert = TRUE, value = TRUE)
    a <- a[nzchar(a)]
    a <- gsub("^ *", "", a)
    b <- gsub(" .*", "", a)
    e <- gsub(".* ", "", a);
    d <- sum(file.size(e[!duplicated(b)]));
    class(d) <- "object_size";
    setwd(old)
    d
  })
  e <- sum(unlist(d))
  class(e) <- "object_size";

  largest <- format(d[[which.max(unlist(d))]], units = units)
  units <- gsub(".* ", "", largest)
  dPaths <- gsub(reproducible::normPath(basePath), "", reproducible::normPath(names(d)))
  byDir <- paste0(dPaths,
                  "  ", lapply(d, format, units = units))

  message(paste("File size (link duplicates omitted): ", format(e, units = units)))
  message(paste("File size by dir (link dups omit'd): \n ",
                 paste(byDir, collapse = "\n  ")))
  invisible(d)
}

#' \code{file.sizeCompare} will run \code{file.sizeWOLinks} and
#' \code{file.size} to compare the two outputs. Any difference will be
#' due to hard links.
#' @export
#' @rdname fileSize
#' @importFrom data.table data.table
#' @importFrom testthat capture_messages
#' @importFrom utils capture.output
file.sizeCompare <- function(path = ".", units = "auto", recursive = TRUE) {
  basePath <- path
  mess <- capture_messages({
    a <- try(file.sizeWOLinks(path = path, units = units, recursive = recursive))
  })
  if (is(a, "try-error")) {
    stop("Can't run this function because can't access 'ls' function (a unix function)")
  }
  old <- setwd(path)
  files <- dir(path, recursive = recursive, full.names = TRUE)
  d <- file.size(files)
  dPaths <- gsub(reproducible::normPath(basePath), "", reproducible::normPath(files))
  dPaths <- dirname(dPaths)
  dt <- data.table(path = dPaths, files, size = d)
  dt1 <- dt[, list(fileSize = sum(size)), by = "dPaths"]
  dt1[, fileSizeWOLinks := unlist(a)]
  dt1[, diff := fileSize - fileSizeWOLinks]
  message(paste0(capture.output(dt1), collapse = "\n"))
  e <- sum(unlist(d))
  setwd(old)
  class(e) <- "object_size";
  f <- sum(unlist(a))
  class(f) <- "object_size"
  message(paste("File size (normal):                  ", format(e, units = units)))
  message(paste("File size (link duplicates omitted): ", format(f, units = units)))
  message(paste("Difference (disk space saved):       ", format(e - f, units = units)))

  invisible(list(file.size = e, file.sizeWOLinks = a))
}
