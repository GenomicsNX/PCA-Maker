dataSummaryUI <- function(id) {
  tagList(
    textOutput(NS(id, "text")),
    verbatimTextOutput(NS(id, "data_size")),
    verbatimTextOutput(NS(id, "data_summary")),
    textOutput(NS(id, "exp_text")),
    verbatimTextOutput(NS(id, "exp_summary"))
  )
}

dataSummaryServer <- function(id, data_set, exp_names) {
  stopifnot(is.reactive(data_set))
  stopifnot(is.reactive(exp_names))
  
  moduleServer(id, function(input, output, session) {
    # Display data summary
    output$text <- renderText("Data Summary:")
    
    output$data_size <- renderPrint({
      cat(
        "Data size (within R environment):", 
        object.size(data_set())/1000, "Kb", "\n"
      )
      cat("Number of rows:", nrow(data_set()), "\n") 
      cat("Number of columns:", ncol(data_set()))
    })
    
    output$data_summary <- renderPrint({
      summary(data_set())
    })
    
    # Display summary for explanatory variable
    output$exp_text <- renderText("Summary for explanatory variable:")
    
    exp_table <- reactive(table(exp_names()))
    
    output$exp_summary <- renderPrint({
      cat("Total count:", sum(as.data.frame(exp_table())$Freq), "\n")
      cat("Frequency table:")
      exp_table()
    })
  })
}