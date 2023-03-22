# A `use_test` function for the tinytest framework
use_test = function(name, open = TRUE) {
  test_file = fs::path("inst", "tinytest", paste0("test_", name), ext = "R")
  fs::file_create(test_file)
  if (open) fs::file_show(test_file)
}

# Document and test the package.
doc_detach_test = function(pkgdir = getwd()) {
  rm(
    list = setdiff(ls(envir = .GlobalEnv), c("use_test", "doc_detach_test")),
    envir = .GlobalEnv
  )


  roxygen2::roxygenize(pkgdir)

  loaded_pkgs = names(utils::sessionInfo()$otherPkgs)
  if (!is.null(loaded_pkgs)) {
    invisible(lapply(
      paste0("package:", loaded_pkgs), detach, character.only = TRUE
    ))
  }

  invisible(lapply(
    c("testthat", "data.table.extras"),
    unloadNamespace
  ))


  tinytest::build_install_test(pkgdir)
}
