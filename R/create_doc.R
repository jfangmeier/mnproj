
#' Create a basic R Markdown document from a template.
#'
#' Creates manuscript/report or slide R Markdown file and saves it into
#' the `doc/` folder.
#'
#' @param type The file type (e.g. report, slides).
#'
#' @return A created `.Rmd` file in the `doc/` folder.
#'
#' @importFrom rlang abort
#' @importFrom fs path_package
#' @importFrom rmarkdown draft
#' @importFrom cli cli_alert_success
#'
#' @examples
#' \dontrun{
#' create_manuscript()
#' create_report()
#' create_slides()
#' }
create_doc <- function(type = c("report_docx", "report_html", "slides_pptx", "slides_html")) {
    if (!is_rproj_folder())
        rlang::abort("The folder does not contain an `.Rproj` file. Please use this function while in the project created from `setup_project().`")

    if (!dir.exists("doc"))
        rlang::abort("What happened to your `doc/` folder?")

    type <- match.arg(type)
    file_name <- normalizePath(file.path("doc", paste0(type, ".Rmd")), mustWork = FALSE)
    template_file <- fs::path_package("mnproject", "rmarkdown", "templates", type)
    if (file.exists(file_name)) {
        rlang::abort(paste0("The file '", type, ".Rmd' already exists in the doc folder."))
    } else {
        rmarkdown::draft(
            file = file_name,
            template = template_file,
            package = NULL,
            create_dir = FALSE,
            edit = FALSE
        )
        cli::cli_alert_success("Creating a {.val {type}} file in the {.val {'doc/'}} folder.")
    }
    invisible()
}

#' @describeIn create_doc Creates a R Markdown document for a html report in the `doc/` folder.
#' @export
create_report_html <- function() {
    create_doc(type = "report_html")
    return(invisible())
}

#' @describeIn create_doc Creates a R Markdown document for making html slides in the `doc/` folder.
#' @export
create_slides_html <- function() {
    create_doc(type = "slides_html")
}
#' @describeIn create_doc Creates a R Markdown document for a Word report in the `doc/` folder.
#' @export
create_report_html <- function() {
    create_doc(type = "report_docx")
    return(invisible())
}

#' @describeIn create_doc Creates a R Markdown document for making PowerPoint slides in the `doc/` folder.
#' @export
create_slides_html <- function() {
    create_doc(type = "slides_pptx")
}

#' List project templates within \pkg{mnproject}.
#'
#' Get a list of available templates in a package.
#'
#' @return Vector of templates available
#' @export
#' @examples
#' template_list
#'
template_list <- c("report_html", "slides_html", "report_docx", "slides_pptx")
