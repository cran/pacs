last_version_raw <- function(pac, repos) {
  which_p <- rownames(available_packages(repos = repos)) == pac
  if (any(which_p)) {
    available_packages(repos = repos)[which_p, "Version"]
  } else {
    NA
  }
}

last_version_fun <- memoise::memoise(last_version_raw, cache = cachem::cache_mem(max_age = 60 * 60))

#' Getting the most recent package version
#' @description using `utils::available.packages` to get the newest package version.
#' @param pac character a package name.
#' @param repos character vector base URLs of the repositories to use. By default checking CRAN and newest Bioconductor per R Version. Default `pacs::biocran_repos()`
#' @return character most recent package version.
#' @note Results are cached for 1 hour with `memoise` package.
#' For Bioconductor the newest one per R version.
#' @export
#' @examples
#' \dontrun{
#' pac_last("dplyr")
#' pac_last("S4Vectors")
#' }
pac_last <- function(pac, repos = biocran_repos()) {
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(is.character(repos))

  if (!pac_isin(pac, repos = repos)) {
    return(NA)
  }

  last_version_fun(pac, repos = repos)
}

#' Checking if a package is in repositories
#' @description using `utils::available.packages` to check if package is in repoitories.
#' @param pac character a package name.
#' @param repos character vector base URLs of the repositories to use. By default checking CRAN and newest Bioconductor per R version. Default `pacs::biocran_repos()`
#' @return logical if a package is inside repositories.
#' @note Results are cached for 1 hour with `memoise` package.
#' @export
#' @examples
#' \dontrun{
#' pac_isin("dplyr")
#' pac_isin("dplyr", repos = "https://cran.rstudio.com/")
#' pac_isin("dplyr", repos = biocran_repos()[grep("Bio", names(biocran_repos()))])
#' }
pac_isin <- function(pac, repos = biocran_repos()) {
  stopifnot((length(pac) == 1) && is.character(pac))
  stopifnot(is.character(repos))
  is_isin(pac, repos = repos)
}

is_isin_raw <- function(pac, repos = biocran_repos()) {
  if (isTRUE(pac %in% rownames(available_packages(repos = repos)))) {
    TRUE
  } else {
    FALSE
  }
}

is_isin <- memoise::memoise(is_isin_raw)

#' Checking if a packge version is the most recent one
#' @description checking if a package version is the most recent one, by default the installed version is compared.
#' @param pac character a package name.
#' @param version character package version, by default the installed version is taken. Default: NULL
#' @param lib.loc character vector. Is omitted for non NULL version. Default: NULL
#' @param repos character vector base URLs of the repositories to use. By default checking CRAN and newest Bioconductor per R Version. Default `pacs::biocran_repos()`
#' @return logical if a package is inside ropositories.
#' @note Results are cached for 1 hour with `memoise` package.
#' For Bioconductor if package is the newest one per R version.
#' @export
#' @examples
#' \dontrun{
#' pac_islast("memoise")
#' pac_islast("dplyr", version = "1.0.0")
#' pac_islast("S4Vectors")
#' pac_islast("S4Vectors", version = pac_last("S4Vectors"))
#' }
pac_islast <- function(pac, version = NULL, lib.loc = NULL, repos = biocran_repos()) {
  if (!pac_isin(pac, repos)) {
    return(NA)
  }

  last_version <- pac_last(pac, repos = repos)

  is_installed <- isTRUE(pac %in% rownames(installed_packages(lib.loc = lib.loc)))

  if (isTRUE(is.null(version))) {
    if (is_installed) {
      version <- pac_description(pac, local = TRUE)$Version
    } else {
      return(FALSE)
    }
  }

  isTRUE(utils::compareVersion(last_version, version) == 0)
}