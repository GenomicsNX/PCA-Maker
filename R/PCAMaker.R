library(shiny)
library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)
library(ggplot2)

PCAMaker <- function() {
  ui <- fluidPage(
    
    # Application title
    titlePanel("PCA Maker"),
    
    tabsetPanel(
      # Upload data and display summary
      tabPanel(
        "Upload your data", 
        sidebarLayout(
          sidebarPanel(uploadDataUI("upload_data")),
          mainPanel(dataSummaryUI("data_summary"))
        )
      ),
      # Make PCA analysis and display PCA summary
      tabPanel(
        "Customize PCA analysis",
        sidebarLayout(
          sidebarPanel(pcaAnalysisUI("pca_analysis")),
          mainPanel(pcaSummaryUI("pca_summary"))
        )
      ),
      # Display bar scree plot, download data and plot
      tabPanel("Scree Plot", screePlotUI("scree_plot")),
      # Display scores plot, download scores and plot
      tabPanel("PCA Scores", pcaScoresUI("pca_scores")),
      # Display loadings scatter plot, download data and plot
      tabPanel("Loadings", loadingsPlotUI("loadings_plot")),
      # Display a biplot
      tabPanel("Bi-Plot", biplotUI("bi_plot"))
    )
  )
  
  server <- function(input, output) {
    # Upload Data
    x <- uploadDataServer("upload_data")
    data_set <- x$data_set
    exp_names <- x$exp_names
    
    # Display data summary
    dataSummaryServer("data_summary", data_set, exp_names)
    
    # Principal component analysis
    pca_analysis <- pcaAnalysisServer("pca_analysis", data_set)
    
    # Display pca summary
    pcaSummaryServer("pca_summary", pca_analysis)
    
    # Display Scree plot
    screePlotServer("scree_plot", pca_analysis)
    
    # Display PCA scores plot
    pcaScoresServer("pca_scores", pca_analysis, exp_names)
    
    # Display Loadings plot
    loadingsPlotServer("loadings_plot", pca_analysis)
    
    # Display Biplot
    biplotServer("bi_plot", pca_analysis, exp_names)
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
  
}
