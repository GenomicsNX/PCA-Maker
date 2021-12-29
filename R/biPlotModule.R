biplotUI <- function(id) {
  tagList(
    tagList(
      sidebarLayout(
        sidebarPanel(
          selectInput(NS(id, "x_axis"), "Select PC on X axis", choices = NULL),
          selectInput(NS(id, "y_axis"), "Select PC on Y axis", choices = NULL), 
          textInput(NS(id, "plot_name"), "Plot name:", value = "BiPlot"),
          radioButtons(NS(id, "format"), NULL, c("png", "bmp", "pdf"), inline = TRUE),
          downloadButton(NS(id, "download_image"), "Download plot")
        ),
        mainPanel(
          plotOutput(NS(id, "biPlot"), height = 550),
        )
      )
    )
  )
}

biplotServer <- function(id, pca_analysis, exp_names) {
  stopifnot(is.reactive(pca_analysis))
  stopifnot(is.reactive(exp_names))
  
  moduleServer(id, function(input, output, session) {
    # Scores
    pca_scores <- reactive(
      as.data.frame(pca_analysis()$x) 
    )
    
    # Loadings 
    pca_loadings <- reactive(
      as.data.frame(pca_analysis()$rotation)
    )
    
    # Update choices
    observeEvent(pca_scores(), {
      updateSelectInput(session, "x_axis", choices = colnames(pca_scores()))
      updateSelectInput(session, "y_axis", choices = colnames(pca_scores()))
    })
    
    # Variation percentage for PCs to plot
    per_var_pcs <- reactive(var_pcs(pca_analysis(), input$x_axis, input$y_axis))
    x_lab <- reactive(per_var_pcs()$var_pcx)
    y_lab <- reactive(per_var_pcs()$var_pcy)
    
    # Biplot
    biPlot <- reactive(
      bi_plot(
        pca_scores = pca_scores(), 
        pca_loadings = pca_loadings(), 
        exp_names = exp_names(), 
        x_axis = input$x_axis, 
        y_axis = input$y_axis,
        x_lab = x_lab(),
        y_lab = y_lab()
      )
    )
    
    # Render biplot
    output$biPlot <- renderPlot({biPlot()}, res = 150)
    
    # Download biplot
    output$download_image <- downloadHandler(
      filename = function() {
        paste0(input$plot_name, ".", input$format)
      },
      content = function(file) {
        ggsave(filename = file, plot = biPlot())
      }
    )
  })
}