pcaAnalysisUI <- function(id) {
  tagList(
    checkboxInput(NS(id, "center"), "Center data", value = TRUE),
    checkboxInput(NS(id, "scale"), "Scale variables", value = TRUE)
  )
}

pcaAnalysisServer <- function(id, data_set) {
  stopifnot(is.reactive(data_set))
  
  moduleServer(id, function(input, output, session) {
    # PCA analysis through prcomp()
    reactive(
      prcomp(data_set(), center = input$center, scale. = input$scale)
    )
    
  })
}