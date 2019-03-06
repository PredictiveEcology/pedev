#' Pull from git and install
#'
#' Fetches all branches, then pulls the identified branch from git,
#' then runs a digest on the local folders. If that digest is different
#' as a previous one, then the function will run \code{devtools::install}.
#' This should be safe even in cases where local files have changed. If
#' they were uncommitted, Git will error, and nothing will be pulled,
#' and if they were committed, then it will try a merge. If the automoated
#' merge works, then it will proceed. If automated merge fails, then nothing
#' will be pulled.
#'
#' @param pkgs A character vector of package names, which is actually
#'   the path names of the packages. i.e., must be absolute or relative
#'   path. Defaults to current directory. It will also check in "..",
#'   i.e., one folder up from the current active folder if it doesn't find
#'   \code{pkgs} in the current folder.
#' @export
#' @param install Logical. If TRUE, then it will run \code{devtools::install} if
#'   there is new content. If \code{branch} was length > 1, only the active,
#'   i.e., first branch, will be installed.
#' @param cacheRepo The location where subsequent calls will store their history.
#'   To be most effective, this should be "persistent", and not part of any
#'   other cacheRepo.
#' @param branches A vector of branch names to pull from, \emph{in reverse order}
#'    so that the first one is the active branch after this function call finishes.
#'    Default is \code{c("development", "master")}, so it will pull from master,
#'    then development. If one of them does not exist, it will try, deteremine
#'    it doesn't exist, skip it and go to next branch.
#' @param fetch Logical. Should it fetch before pulling.
#' @param submodule Logical. VERY EXPERIMENTAL. \code{TRUE} would mean pull all
#'   submodules... the problem is that branch gets complicated.
#' @param ... Passed to \code{devtools::install}
#' @importFrom reproducible CacheDigest Cache
#' @importFrom crayon yellow bgBlack white bgBlue
#' @importFrom digest digest
#' @examples
#' \dontrun{
#' # This will pull development branch of all these packages, and install them
#' #    all, if there are any file changes in each respective directory
#' allPkgs <- c("quickPlot", "reproducible", "SpaDES.core", "SpaDES.tools",
#'              "pemisc", "map", "LandR", "pedev")
#' updateGit(allPkgs)
#'
#' # Will update and install all development branches of all repositories
#' #   in ~/GitHub folder
#' pedev::updateGit(dir("~/GitHub"))
#' }
updateGit <- function(pkgs = NULL,
                      install = TRUE,
                      branch = c("development", "master"),
                      cacheRepo = getOption("pedev.cacheRepo", "~/.pedevCache"),
                      fetch = TRUE, submodule = FALSE,
                      ...) {
  oldWd <- getwd()
  on.exit(setwd(oldWd))
  if (missing(pkgs))
    pkgs <- basename(getwd())

  branches <- branch
  aborted <- list()
  unfinished <- list()
  on.exit({
    if (length(aborted)) {
      message(crayon::magenta(
        "                                                        \n",
        "########### Summary of aborted cases  #######################\n",
        "  -", paste(lapply(names(aborted),
                             function(nam)
                               paste(c(nam, aborted[[nam]]), collapse = "\n       ")),
                      collapse = "\n   - ")))
    }
    if (length(unfinished)) {
      message(crayon::blue(
        "                                                        \n",
        "########### Summary of unfinished cases  #######################\n",
        "  -", paste(lapply(names(unfinished),
                            function(nam)
                              paste(c(nam, unfinished[[nam]]), collapse = "\n       ")),
                     collapse = "\n   - ")))
    }

  }, add = FALSE)

  for (i in pkgs) {
    pkgDir <- paste0(i)
    insidePkg <- file.path(oldWd, "..", pkgDir)
    dirExistsA <- dir.exists(file.path(oldWd, pkgDir))
    dirExistsB <- dir.exists(insidePkg)
    if (dirExistsA || dirExistsB) {
      message("#########################################################")
      message(crayon::bgBlack(crayon::yellow("updating ", i)))
      if (dirExistsA) {
        setwd(pkgDir)
        isGitRepo <- file.exists(file.path(pkgDir, ".git"))
      } else {
        setwd(insidePkg)
        isGitRepo <- file.exists(file.path(insidePkg, ".git"))
      }

      if (isTRUE(isGitRepo)) {
        if (isTRUE(fetch)) {
          cmd1 <- "git fetch"
          message("  ", cmd1)
          system(cmd1, intern = TRUE)
        }
        anyBranchExists <- FALSE
        for (branch in rev(branches)) {
          cmd1 <- paste("checkout", branch)
          message("  ", cmd1)
          test1 <- suppressWarnings(system2("git", args = cmd1, stdout = TRUE, stderr = TRUE))
          message("    ", paste(test1, collapse = "\n"))
          if (any(grepl("error", c(test1)))) {
            aborted <- errorHadAbort(test1, i, branch, aborted)
            next
          }

          lenUnfinished <- length(unfinished)
          unfinished <- unfinished(test1, i, branch, unfinished,
                                   expectedMsg = paste0("(up.to.date)|(",branch,")"))

          cmd1 <- "pull"
          message("  ", cmd1)
          test2 <- suppressWarnings(system2("git", args = cmd1, stdout = TRUE, stderr = TRUE))
          message("    ", paste(test2, collapse = "\n"))
          anyBranchExists <- TRUE
          if (any(grepl("error", c(test2)))) {
            aborted <- errorHadAbort(test2, i, branch, aborted)
            next
          }

          if (file.exists(".gitmodules")) {
            if (isTRUE(submodule)) {
              message("running submodule updates -- VERY EXPERIMENTAL")
              message("- checking out & pulling the branches indicated in .gitmodules")
              gitCheckoutEachBranchCmd <- paste("submodule foreach -q --recursive 'branch=\"$(git config -f",
                                                "$toplevel/.gitmodules submodule.$name.branch)\";",
                                                "echo $name && git checkout $branch && git pull'")
              if (.Platform$OS.type != "windows") {
                test1e <- system2("git", gitCheckoutEachBranchCmd, stdout = TRUE, stderr = TRUE)
                message("   ", lapply(test1e, paste, "\n   "))
              } else {
                # Have to make a temporary .bat and .sh file so command can work
                updateGitTxt <- "updateGit"
                tmpSh <- paste0(updateGitTxt, i, "_", branch, ".sh")
                tmpBat <- paste0(updateGitTxt, i, "_", branch, ".bat")
                on.exit({unlink(tmpSh); unlink(tmpBat)}, add = TRUE)
                cat(file = tmpSh, fill = FALSE,
                    paste("#!/bin/bash",
                          paste("git", gitCheckoutEachBranchCmd), sep = "\n"))
                gitBashExePath <- list()
                gitBashExePath[[1]] <- "C:\\Program Files (x86)\\Git\\git-bash.exe"
                gitBashExePath[[2]] <- "C:\\Program Files\\Git\\git-bash.exe"
                gitBashExists <- sapply(gitBashExePath, file.exists)
                gitBashExePath <- if (!any(gitBashExists)) {
                  gitBashExePathTry <- suppressWarnings(shell("where git-bash.exe", intern = TRUE))
                  if (grepl("INFO: Could not", gitBashExePathTry)) {
                    warning("git-bash.exe is not available in your PATH. Please add it. ",
                            "This means that submodules didn't get updated. ",
                            "Already tried to find it in:\n  ", paste(unlist(gitBashExePath), collapse = "\n  "))
                  }
                  gitBashExePathTry
                } else {
                  gitBashExePath[gitBashExists][[1]]
                }
                if (file.exists(gitBashExePath)) {
                  cat(file = tmpBat, paste0('cmd /c "',gitBashExePath,'" -c ', #--cd-to-home
                                           paste0("./",tmpSh)
                  ))
                  shell(tmpBat, intern = TRUE)
                }
              }
            } else {
              message(crayon::white(crayon::bgBlue("This is a git repository with submodules, but submodule is FALSE;",
                      "not updating submodules")))
            }
          }


          unfinished <- unfinished(test2, i, branch, unfinished,
                                   expectedMsg = paste0("(up.to.date)|(can be fast)|(Already on)"))

        }

        if (!anyBranchExists) {
          next
        }
      } else {
        message("Folder ", i, " is not a git repo; skipping")
      }
      isAPackage <- length(dir(pattern = "DESCRIPTION")) > 0

      if (!isAPackage) message("Not a package; no install")
      if (isTRUE(install)) {
        if (isAPackage) {
          files <- dir(recursive = TRUE)
          d2 <- lapply(files, function(x) try(digest::digest(file = x, algo = "xxhash64")))
          opts <- options("reproducible.useCache" = "devMode",
                          "reproducible.cachePath" =
                            reproducible::checkPath(cacheRepo, create = TRUE))
          #suppressPackageStartupMessages(require(reproducible))
          suppressMessages(dig <- reproducible::Cache(reproducible::CacheDigest, d2,
                                                      userTags = i))
          #try(detach("package:reproducible", unload = TRUE, character.only = TRUE), silent = TRUE)
          options(opts)

          if (attr(dig, ".Cache")$newCache) {
            message("  installing ... ")
            devtools::install(dependencies = FALSE, reload = FALSE, ...)
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
reload_all <- function(pkgs, load_all = TRUE, gitPath = "~/GitHub") {
  allPkgs <- c("LandR", "SpaDES.core", "SpaDES.tools", "map", "pemisc",
               "pedev", "reproducible",
               "quickPlot", "amc")
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

  wh <- which(allPkgs %in% pkgs)
  pkgsToUnload <- if (length(wh) > 0)
    allPkgs[seq(max(wh))]
  else
    character(0)

  pkgsToUnload2 <- character()
  for (i in pkgsToUnload) {
    #for (i in pkgs) {
    if (isNamespaceLoaded(i)) {
      pkgsToUnload2 <- c(i, pkgsToUnload2)
      try(detach(paste0("package:", i), unload = TRUE, character.only = TRUE))
    }
  }
  if (isTRUE(load_all))
    for (i in pkgsToUnload2) {
      devtools::load_all(file.path(gitPath, i))
    }
}

errorHadAbort <- function(errorMsg, pkg, branch, aborted) {
    if (any(grepl("Aborting", errorMsg))) {
      abortedCur <- list(errorMsg)
      names(abortedCur) <- paste0(pkg, "@", branch)
      aborted <- append(aborted, abortedCur)
    }
  aborted
}

unfinished <- function(msg, pkg, branch, unfinished, expectedMsg) {
  if (!all(grepl(expectedMsg, msg))) {
    unfinishedCur <- list(msg)
    names(unfinishedCur) <- paste0(pkg, "@", branch)
    unfinished <- append(unfinished, unfinishedCur)
  }
  unfinished
}
