#' Pull from git and install
#'
#' Fetches all branches, then pulls the identified branch from git,
#' then runs a digest on the local folders. If that digest is different
#' as a previous one, then the function will run \code{devtools::install}.
#'
#' @param pkgs A character vector of package names, which is actually
#'   the path names of the packages. i.e., must be absolute or relative
#'   path. Defaults to current directory. It will also check in "..",
#'   i.e., one folder up from the current active folder if it doesn't find
#'   \code{pkgs} in the current folder.
#' @export
#' @param install Logical. If TRUE, then it will run devtools::install if
#'   there is new content
#' @param branch The branch to pull from. Default is \code{"development"}
#' @importFrom reproducible CacheDigest Cache
#' @importFrom crayon yellow bgBlack
#' @importFrom digest digest
updateGit <- function(pkgs = NULL,
                      install = TRUE,
                      branch = "development") {
  oldWd <- getwd()
  on.exit(setwd(oldWd))
  if (missing(pkgs))
    pkgs <- basename(getwd())

  for (i in pkgs) {
    pkgDir <- paste0(i)
    insidePkg <- file.path("..", pkgDir)
    dirExistsA <- dir.exists(pkgDir)
    dirExistsB <- dir.exists(insidePkg)
    if (dirExistsA || dirExistsB) {
      message("#########################################################")
      message(crayon::bgBlack(crayon::yellow("updating ", i)))
      if (dirExistsA) {
        setwd(pkgDir)
      } else {
        setwd(insidePkg)
      }

      cmd1 <- "git fetch"
      message("  ", cmd1)
      system(cmd1, intern = TRUE)

      cmd1 <- paste("git checkout", branch)
      message("  ", cmd1)
      test1 <- system(cmd1, intern = TRUE)
      message("    ", paste(test1, collapse = "\n"))

      cmd1 <- "git pull"
      message("  ", cmd1)
      test2 <- system(cmd1, intern = TRUE)
      message("    ", paste(test2, collapse = "\n"))
      if (any(grepl("error", c(test1, test2)))) next

      isAPackage <- length(dir(pattern = "DESCRIPTION")) > 0

      if (!isAPackage) message("Not a package; no install")
      if (isTRUE(install)) {
        if (isAPackage) {
          files <- dir(recursive = TRUE)
          d2 <- lapply(files, function(x) try(digest::digest(file = x, algo = "xxhash64")))
          opts <- options("reproducible.useCache" = "devMode",
                          "reproducible.cachePath" =
                            reproducible::checkPath(file.path(system.file(package = "pedev"), ".Cache"), create = TRUE))
          #suppressPackageStartupMessages(require(reproducible))
          suppressMessages(dig <- reproducible::Cache(reproducible::CacheDigest, d2))
          #try(detach("package:reproducible", unload = TRUE, character.only = TRUE), silent = TRUE)
          options(opts)

          if (attr(dig, ".Cache")$newCache) {
            message("  installing ... ")
            devtools::install(dependencies = FALSE, reload = FALSE)
          } else {
            message("  not reinstalling; already installed this version")
          }
        }
      }
    } else {
      message("Package ", i, " does not exist locally; skipping")
    }
  }
}

.pkgDepsGraph <- function(pkgs) {
  dt <- lapply(pkgs, function(pkg) {
    deps <- pkgs[pkgs %in% reproducible::pkgDep(pkg)[[1]]]
    if (NROW(deps))
      data.table::data.table(pkg = pkg,
                             depends = deps)
  })
  dt <- data.table::rbindlist(dt);
  dtGraph <- igraph::graph_from_data_frame(dt);
  dtGraph
}


#' A verion of devtools::load_all that detaches dependencies
#'
#' This is very idiosyncratic for the Predictive Ecology group
#' @export
#' @param pkgs A character vector of the package(s) to run "devtools::load_all"
#' @param load_all Logical. If \code{FALSE}, then this function will only
#'   detach the packages necessary
reload_all <- function(pkgs, load_all = TRUE) {
  allPkgs <- c("LandR", "SpaDES.core", "SpaDES.tools", "map", "pemisc", "reproducible",
               "quickPlot")
  if (length(pkgs) > 1) {
    # ordGeneral1 <- .pkgDepsGraph(pkgs = allPkgs)
    # ordGeneral2 <- igraph::topo_sort(ordGeneral1)
    # allPkgs <- names(ordGeneral2)
    if (!all(pkgs %in% allPkgs)) {
      ord1 <- .pkgDepsGraph(pkgs = pkgs)
      ord2 <- igraph::topo_sort(ord1)
      pkgs <- names(ord2)
    }
  }

  pkgsToUnload <- allPkgs[seq(max(which(allPkgs %in% pkgs)))]

  browser()
  for (i in pkgsToUnload) {
    #for (i in pkgs) {
    try(detach(paste0("package:", i), unload = TRUE, character.only = TRUE))
  }
  if (isTRUE(load_all))
    for (i in rev(pkgsToUnload)) {
      devtools::load_all(file.path(gitPath, i))
    }
}
