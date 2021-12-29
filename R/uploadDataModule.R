uploadDataUI <- function(id) {
  tagList(
    fileInput(
      NS(id, "data_file"), 
      "Upload a CSV or TSV with your data", 
      accept = c(".csv", ".tsv")
    ),
    fileInput(
      NS(id, "exp_file"),
      "Upload a CSV or TSV file with your explanatory variable"
    )
   
  )
}

uploadDataServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Upload data
    data_set <- reactive(
      upload_data(
        name = input$data_file$name, 
        path = input$data_file$datapath, 
        file = input$data_file,
        default_data = wine_data
      )
    )
    
    # Upload data for explanatory variable
    exp_names <- reactive(
      upload_data(
        name = input$exp_file$name,
        path = input$exp_file$datapath,
        file = input$exp_file,
        default_data = wine_region
      ) 
    )
    
    # Return data
    list(
      data_set = reactive(data_set()),
      exp_names = reactive(exp_names())
    )
  })
}