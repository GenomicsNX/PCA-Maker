loadingsPlotUI <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(NS(id, "x_axis"), "Select PC on X axis", choices = NULL),
        selectInput(NS(id, "y_axis"), "Select PC on Y axis", choices = NULL), 
        textInput(NS(id, "plot_name"), "Plot name:", value = "LoadingsPlot"),
        radioButtons(NS(id, "format"), NULL, c("png", "bmp", "pdf"), inline = TRUE),
        downloadButton(NS(id, "download_image"), "Download plot"),
        textInput(NS(id, "file_name"), "File name:", value = "LoadingsData"),
        downloadButton(NS(id, "download_data"), "Download loadings as a CSV file")
      ),
      mainPanel(
        plotOutput(NS(id, "scatter_plot"), height = 550),
      )
    )
  )
}

loadingsPlotServer <- function(id, pca_analysis) {
  stopifnot(is.reactive(pca_analysis))
  
  moduleServer(id, function(input, output, session) {
    # Data
    pca_loadings <- reactive(
      as.data.frame(pca_analysis()$rotation)
    )
    
    # Update choices
    observeEvent(pca_loadings(), {
      updateSelectInput(session, "x_axis", choices = colnames(pca_loadings()))
      updateSelectInput(session, "y_axis", choices = colnames(pca_loadings()))
    })
    
    # Variation percentage for PCs to plot
    per_var_pcs <- reactive(var_pcs(pca_analysis(), input$x_axis, input$y_axis))
    x_lab <- reactive(per_var_pcs()$var_pcx)
    y_lab <- reactive(per_var_pcs()$var_pcy)
    
    # Scatter loadings plot
    scatter_load_plot <- reactive(
      ggplot(
        pca_loadings(), 
        aes(.data[[input$x_axis]], .data[[input$y_axis]])
      ) +
        geom_point(color = "blue", size = 2) +
        geom_vline(xintercept = 0, alpha = 0.3) +
        geom_hline(yintercept = 0, alpha = 0.3) +
        geom_text(aes(label = rownames(pca_loadings())), vjust = -0.7, size = 3) +
        xlab(x_lab()) +
        ylab(y_lab()) +
        theme_minimal()
    )
    
    # Render plot
    output$scatter_plot <- renderPlot({scatter_load_plot()}, res = 150)
    
    # Download scatter loadigns plot
    output$download_image <- downloadHandler(
      filename = function() {
        paste0(input$plot_name, ".", input$format)
      },
      content = function(file) {
        ggsave(filename = file, plot = scatter_load_plot())
      }
    )
    
    # Download loadings as a CSV file
    output$download_data <- downloadHandler(
      filename = function() {
        paste0(input$file_name, ".csv")
      },
      content = function(file) {
        readr::write_csv(pca_loadings(), file)
      }
    )
  })
}
