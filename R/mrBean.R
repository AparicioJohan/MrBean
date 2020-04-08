
#' User interface for MrBean
#'
#' @return shiny interface
#' @export
#'
#' @examples
#' # mrBean()
#' @author Johan Aparicio, \email{j.aparicio@cgiar.org}
#'
mrBean <- function()
{
  if (!requireNamespace(package = "shiny"))
    message("Package 'shiny' is required to run this function")
  if (!requireNamespace(package = "plotly"))
    message("Package 'plotly' is required to run this function")
  shiny::shinyAppDir(system.file("examples/",
                                 package = "MrBean", mustWork = TRUE), options = list(launch.browser=T))
}
