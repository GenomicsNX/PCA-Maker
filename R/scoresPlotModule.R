pcaScoresUI <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(NS(id, "x_axis"), "Select PC on X axis", choices = NULL),
        selectInput(NS(id, "y_axis"), "Select PC on Y axis", choices = NULL), 
        textInput(NS(id, "plot_name"), "Plot name:", value = "ScoresPlot"),
        radioButtons(NS(id, "format"), NULL, c("png", "bmp", "pdf"), inline = TRUE),
        downloadButton(NS(id, "download_image"), "Download plot"),
        textInput(NS(id, "file_name"), "File name:", value = "PCAScores"),
        downloadButton(NS(id, "download_scores"), "Download scores as a CSV file")
      ),
      mainPanel(
        plotOutput(NS(id, "scores_plot"), height = 550),
      )
    )
  )
}

pcaScoresServer <- function(id, pca_analysis, exp_names) {
  stopifnot(is.reactive(pca_analysis))
  stopifnot(is.reactive(exp_names))
  
  moduleServer(id, function(input, output, session) {
    
    # Data
    pca_scores <- reactive(as.data.frame(pca_analysis()$x))
    
    # Update choices
    observeEvent(pca_scores(), {
      updateSelectInput(session, "x_axis", choices = colnames(pca_scores()))
      updateSelectInput(session, "y_axis", choices = colnames(pca_scores()))
    })
    
    # Variation percentage for PCs to plot
    per_var_pcs <- reactive(var_pcs(pca_analysis(), input$x_axis, input$y_axis))
    x_lab <- reactive(per_var_pcs()$var_pcx)
    y_lab <- reactive(per_var_pcs()$var_pcy)
    
    # Scores plot
    scores_plot <- reactive({
      ggplot(
        data = pca_scores(), 
        aes(
          .data[[input$x_axis]], 
          .data[[input$y_axis]],
          color = as.factor(unlist(exp_names()))
        )
      ) +
        geom_point(size = 2) +
        xlab(x_lab()) +
        ylab(y_lab()) +
        theme_minimal() +
        theme(legend.title = element_blank()) 
    })
    
    # Render plot
    output$scores_plot <- renderPlot({scores_plot()}, res = 150)
    
    # Download scores plot
    output$download_image <- downloadHandler(
      filename = function() {
        paste0(input$plot_name, ".", input$format)
      },
      content = function(file) {
        ggsave(filename = file, plot = scores_plot())
      }
    )
    
    # Download pca scores as a CSV file
    output$download_scores <- downloadHandler(
      filename = function() {
        paste0(input$file_name, ".csv")
      },
      content = function(file) {
        readr::write_csv(pca_scores(), file)
      }
    )
  })
}