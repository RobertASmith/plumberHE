# Define server logic required to draw a histogram
server <- function(input, output) {
  
  library(shiny)
  library(ggplot2)
  library(scales)
  library(reshape2)
  library(shinybusy)
  library(waiter)
  # 1. Load the shinyvalidate library:
  library(shinyvalidate)
  
  # 2. Create the parent and children input validators:
  ## Parent input validator:
  model_inputs_iv <- InputValidator$new()
  ## Children input validator:
  probs_iv <- InputValidator$new()
  utils_iv <- InputValidator$new()
  
  # 3. Add children input validators to the Parent input validator:
  model_inputs_iv$add_validator(probs_iv)
  model_inputs_iv$add_validator(utils_iv)
  
  # 4. Define dataframes to be used in the user function below:
  ## Probabilities distributions:
  dists_bounds_probs <- data.frame(
    dist =       c(  "beta", "gamma",  "rlnorm", "fixed"),
    param_1_lb = c(       0,       0,      -Inf,       0),
    param_1_ub = c(     Inf,     Inf,       Inf,       1),
    param_2_lb = c(       0,       0,         0,      NA),
    param_2_ub = c(     Inf,     Inf,     1e300,      NA),
    param_1_nm = c("shape1", "shape", "meanlog", "fixed"),
    param_2_nm = c("shape2", "scale",   "sdlog",      "")
  )
  ## Utilities distributions:
  dists_bounds_utils <- data.frame(
    dist =       c(  "beta", "gamma",  "rlnorm", "fixed"),
    param_1_lb = c(       0,       0,      -Inf,      -1),
    param_1_ub = c(     Inf,     Inf,       Inf,       1),
    param_2_lb = c(       0,       0,         0,      NA),
    param_2_ub = c(     Inf,     Inf,     1e300,      NA),
    param_1_nm = c("shape1", "shape", "meanlog", "fixed"),
    param_2_nm = c("shape2", "scale",   "sdlog",      "")
  )
  
  # 5. Define a function for the input validator: 
  dist_input <- function(value, dist, param, data) {
    dist_bounds <- data[data$dist == dist, -1]
    
    if(param == 1) {
      if(value < dist_bounds[1, param]) {
        if(dist == "fixed") {
          return(
            paste0("The acceptable fixed value should be between ",
                   dist_bounds[1, param], 
                   " and ", 
                   dist_bounds[1, param + 1]
            )
          )}
        return(
          paste0("In a ", dist, " distribution, an acceptable value for the ",
                 dist_bounds[1, param + 4],
                 " parameter should be between ", 
                 dist_bounds[1, param], 
                 " and ", 
                 dist_bounds[1, param + 1]
          )
        )}
      if(value > dist_bounds[1, param + 1]) {
        if(dist == "fixed") {
          return(
            paste0("The acceptable fixed value should be between ",
                   dist_bounds[1, param], 
                   " and ", 
                   dist_bounds[1, param + 1]
            )
          )}
        return(
          paste0("In a ", dist, " distribution, an acceptable value for the ",
                 dist_bounds[1, param + 4],
                 " parameter should be between ", 
                 dist_bounds[1, param], 
                 " and ", 
                 dist_bounds[1, param + 1]
          )
        )}
    } else if (param == 2) {
      if(dist != "fixed"){
        if(value < dist_bounds[1, param + 1]) {
          return(
            paste0("In a ", dist, " distribution, an acceptable value for the ",
                   dist_bounds[1, param + 4],
                   " parameter should be between ", 
                   dist_bounds[1, param + 1], 
                   " and ", 
                   dist_bounds[1, param + 2]
            )
          )
        }}
      if(dist != "fixed"){
        if(value > dist_bounds[1, param + 2]) {
          return(
            paste0("In a ", dist, " distribution, an acceptable value for the ",
                   dist_bounds[1, param + 4],
                   " parameter should be between ", 
                   dist_bounds[1, param + 1], 
                   " and ", 
                   dist_bounds[1, param + 2]
            )
          )
        }}}}
  
  # 6. Attach rules to the child input validators:
  ## Probability input validator:
  probs_iv$condition(~ shiny::isTruthy(input$p_HS1_dist))
  probs_iv$add_rule(inputId = "p_HS1_v1", sv_required())
  probs_iv$add_rule(inputId = "p_HS1_v1", rule = ~ {
    dist_input(value = ., dist = input[["p_HS1_dist"]], param = 1,
               data = dists_bounds_probs)
  })
  probs_iv$add_rule(inputId = "p_HS1_v2", sv_required())
  probs_iv$add_rule(inputId = "p_HS1_v2", rule = ~ {
    dist_input(value = ., dist = input[["p_HS1_dist"]], param = 2, 
               data = dists_bounds_probs)
  })
  ## Utilities input validator:
  utils_iv$condition(~ shiny::isTruthy(input$u_S1_dist))
  probs_iv$add_rule(inputId = "u_S1_v1", sv_required())
  probs_iv$add_rule(inputId = "u_S1_v1", rule = ~ {
    dist_input(value = ., dist = input[["u_S1_dist"]], param = 1,
               data = dists_bounds_utils)
  })
  probs_iv$add_rule(inputId = "u_S1_v2", sv_required())
  probs_iv$add_rule(inputId = "u_S1_v2", rule = ~ {
    dist_input(value = ., dist = input[["u_S1_dist"]], param = 2, 
               data = dists_bounds_utils)
  })
  
  # 7. Start displaying errors in the UI:
  model_inputs_iv$enable()
  
  source("../report/makeCEAC.R")
  source("../report/makeCEPlane.R")
  source("../app_files/landing_div.R")
  
  list_results <- eventReactive(input$runModel, {
    if(model_inputs_iv$is_valid()) {
      # PS: error message when api key not provided? 
      # Is the API/key supposed accessible to everyone?
      if(Sys.getenv("CONNECT_KEY") == ""){
        shiny::showNotification(type = "error","Error: No API Key provided")
        return(NULL)
      }
      
      # convert inputs into a single data-frame to be passed to the API call
      df_input <- data.frame(
        parameter = c("p_HS1", "u_S1"),
        distribution = c(input$p_HS1_dist, input$u_S1_dist),
        v1 = c(input$p_HS1_v1, input$u_S1_v1),
        v2 = c(input$p_HS1_v2, input$u_S1_v2)
      )
      
      # show modal saying sending to API
      shinybusy::show_modal_gif(text = "Interacting with client API",
                                modal_size = "l",
                                width = "200px", 
                                height = "300px",
                                src = "bean.gif"
      )
      
      # run the model using the connect server API
      results <- httr::content(
        httr::POST(
          # the Server URL can also be kept confidential, but will leave here for now 
          url = "https://connect.bresmed.com",
          # path for the API within the server URL
          path = "rhta2022/runDARTHmodel",
          # code is passed to the client API from GitHub.
          query = list(model_functions = "https://raw.githubusercontent.com/BresMed/plumberHE/main/R/darth_funcs.R"),
          # set of parameters to be changed ... we are allowed to change these but not some others...
          body = list(
            param_updates = jsonlite::toJSON(df_input)),
          # we include a key here to access the API ... like a password protection
          
          config = httr::add_headers(Authorization = paste0("Key ", 
                                                            Sys.getenv("CONNECT_KEY")))
        )
      )
      # insert debugging message
      message("API returned results")
      
      # show modal saying finished getting data from API
      shinybusy::remove_modal_gif()
      
      # rename the costs columns
      results_C <- results[,1:3]
      # same for qalys
      results_Q <- results[,4:6]
      # name all the columns the same
      colnames(results_C) <- colnames(results_Q) <- c("A", "B", "C")
      
      # create ceac based on brandtools package from lumanity...
      temp_cols <- c("#D8D2BF", "#001B2B", "#007B84")
      names(temp_cols) <- c("A", "B", "C")
      
      list("results_C" = results_C, 
           "results_Q" = results_Q,
           "temp_cols" = temp_cols)
      
    } else if (!model_inputs_iv$is_valid()) {
      showNotification(
        "Please fix the errors in the parameters' input form before continuing",
        type = "warning"
      )
      if(Sys.getenv("CONNECT_KEY") == "") {
        showNotification(
          "Error: No API Key provided",
          type = "error")
        return(NULL)
      }
    }
  })  
  output$CEPlane <- renderPlot({
    
    
    # create the CEP
    makeCEPlane(total_costs = list_results()$results_C,
                total_qalys = list_results()$results_Q,
                treatment = "B",
                comparitor = "A",
                thresh = 30000,
                show_ellipse = T,
                colors = list_results()$temp_cols)
    
  })
  
  
  output$CEAC <- renderPlot({
    
    makeCEAC(total_costs = list_results()$results_C,
             total_qalys = list_results()$results_Q,
             treatment = c("A", "B", "C"),
             lambda_min = 0,
             lambda_max = 100000,
             col = list_results()$temp_cols)
    
  })
  
  output$ResultsTab_C <- renderTable({
    head(list_results()$results_C)
  })
  
  output$ResultsTab_Q <- renderTable({
    head(list_results()$results_Q)
  })
}