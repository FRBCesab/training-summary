#' training-summary: A Research Compendium
#'
#' @description
#' A paragraph providing a full description of the project and describing each
#' step of the workflow.
#'
#' @author Nicolas Casajus \email{rdev.nc@gmail.com}
#'
#' @date 2024/12/15

## Install Dependencies (listed in DESCRIPTION) ----

pak::local_install_deps()


## Load Project Addins (R Functions and Packages) ----

pkgload::load_all()


## Update figures ----

courses_timeline()
students_evolution()
students_evolution(FALSE)
students_evolution_per_course()
registrations_evolution()
speaker_timeline()
course_costs()
prices_vs_registrations()

quarto::quarto_render("index.qmd")
