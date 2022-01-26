#' @importFrom rprojroot is.root_criterion is_rstudio_project
is_rproj_folder <- function() {
    rprojroot::is.root_criterion(rprojroot::is_rstudio_project)
}

#' @importFrom withr with_dir
#' @importFrom fs path_dir dir_tree
viz_project_tree <- function(path) {
    withr::with_dir(fs::path_dir(path),
                    {fs::dir_tree(basename(path), all = TRUE)})
}
