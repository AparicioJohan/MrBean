
#' User interface for MrBean
#'
#' @return shiny interface
#' @export
#'
#' @examples
#' # app_MrBean()
#' @author Johan Aparicio, \email{j.aparicio@cgiar.org}
#'
app_MrBean <- function()
{
  if (!requireNamespace(package = "shiny"))
    message("Package 'shiny' is required to run this function")
  suppressWarnings(
    pacman::p_load("shiny", "shinydashboard","plotly", "datasets", "SpATS", "lme4", "tools","readxl","shinyBS","purrr",
                 "GGally","ggpubr","corrplot","missMDA","FactoMineR","factoextra","psych","ggpmisc","lmerTest","car",
                 "data.table","shinytoastr","broom.mixed","shinyjs","DT"," ggvis","shinyalert","shinycssloaders","shinyWidgets",
                 "rintrojs","dplyr","shinydashboardPlus","robustbase")
  )
  shiny::shinyAppDir(system.file("examples/",
                                 package = "MrBean", mustWork = TRUE), options = list(launch.browser=T))
}
