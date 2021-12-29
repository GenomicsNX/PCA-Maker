pcaSummaryUI <- function(id) {
  tagList(
    textOutput(NS(id, "text")),
    verbatimTextOutput(NS(id, "pca_summary"))
  )
}

pcaSummaryServer <- function(id, pca_analysis) {
  stopifnot(is.reactive(pca_analysis))
  
  moduleServer(id, function(input, output, session) {
    # PCA summary
    output$text <- renderText("PCA Summary:")
    
    output$pca_summary <- renderPrint({
      summary(pca_analysis())
    })
  })
}