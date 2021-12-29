screePlotUI <- function(id) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        numericInput(
          NS(id, "columns"), "Select number of PCs to plot", 
          value = 3, min = 1, max = 1,
        ),
        textInput(NS(id, "plot_name"), "Plot name:", value = "ScreePlot"),
        radioButtons(NS(id, "format"), NULL, c("png", "bmp", "pdf"), inline = TRUE),
        downloadButton(NS(id, "download_image"), "Download plot"),
        textInput(NS(id, "file_name"), "File name:", value = "VariationData"),
        downloadButton(NS(id, "download_data"), "Download variation data as a CSV file")
      ),
      mainPanel(
        plotOutput(NS(id, "scree_plot"), height = 550)
      )
    )
  )
}

screePlotServer <- function(id, pca_analysis) {
  stopifnot(is.reactive(pca_analysis))
  
  moduleServer(id, function(input, output, session) {
    # Data
    var_prcomp <- reactive(per_var(pca_analysis()))
    
    # Update maximum value for number of PCs to plot
    observeEvent(var_prcomp(), {
      updateSliderInput(session, "columns", max = nrow(var_prcomp()))
    })
    
    # Scree plot
    scree_plot <- reactive(
      ggplot(
        data = select(var_prcomp(), PC_i, VarPer)[1:input$columns,], 
        aes(reorder(PC_i, -VarPer), VarPer) 
      ) +
        geom_col(width = 0.6, color = "black", fill = "skyblue3") +
        xlab("Principal component") +
        ylab("Percentage of variation (%)") +
        theme_minimal()
    )
    
    # Render scree plot 
    output$scree_plot <- renderPlot({scree_plot()}, res = 150)
    
    # Download scores plot
    output$download_image <- downloadHandler(
      filename = function() {
        paste0(input$plot_name, ".", input$format)
      },
      content = function(file) {
        ggsave(filename = file, plot = scree_plot())
      }
    )
    
    # Download variation data as a CSV file
    output$download_data <- downloadHandler(
      filename = function() {
        paste0(input$file_name, ".csv")
      },
      content = function(file) {
        readr::write_csv(select(var_prcomp(), PC, VarPer), file)
      }
    )
    
  })
}