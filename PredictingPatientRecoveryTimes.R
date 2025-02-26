# Load required libraries
library(shiny)
library(shinymanager)
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)
library(openxlsx)
library(rmarkdown)
library(rsconnect)
library(randomForest)

#Connecting to shinyapps.io
rsconnect::deployApp(appDir = "C:\\Users\\CHEELACHONE\\Documents\\PredictingPatientRecoveryTime_Final", 
                     account = "maichouaxiong")



#Creating user credentials for logging in
credentials <- data.frame(
  user = c("admin", "mxiong"),  # Usernames
  password = c("adminpass1", "Bakyuazz12*"),
  role = c("admin", "user"),
  stringsAsFactors = FALSE
)

#Creating functions for logic
data <- read.csv("Hospital Length of Stay Dataset Microsoft .csv")

# Identify general and disease columns
general_columns <- c("los", "age", "gender", "prior_appts_attended")
disease_columns <- setdiff(colnames(data), general_columns)

# Convert gender to factor
data$gender <- factor(data$gender, levels = c(1, 2)) 

# Convert prior appointments to numeric
data$prior_appts_attended <- as.numeric(data$prior_appts_attended)

# Train a Linear Regression model
lm_formula <- as.formula(paste("los ~ age + gender + prior_appts_attended +", paste(disease_columns, collapse = " + ")))
lm_model <- lm(lm_formula, data = data)

# Train a Random Forest model
rf_model <- randomForest(lm_formula, data = data, ntree = 100)

#Creating the user interface
# Define UI
ui <- secure_app(
  fluidPage(
    titlePanel("Predicting Patient Length of Stay"),
    
    sidebarLayout(
      sidebarPanel(
        div(class = "sidebar-heading", "Upload Data"),
        fileInput("file_upload", "Upload File", accept = c(".csv", ".xlsx")),
        textInput("url_input", "Enter Dataset URL", ""),
        actionButton("load_data", "Load Data"),
        actionButton("reset_data", "Reset Data"),
        
        hr(),
        
        div(class = "sidebar-heading", "Data Cleaning"),
        selectInput("cleaning_method", "Data Cleaning Method", choices = c("Remove Missing Rows", "Impute Missing Values with Mean")),
        actionButton("clean_data", "Clean Data"),
        
        hr(),
        
        div(class = "sidebar-heading", "Analysis"),
        selectInput("analysis_method", "Choose Analysis Method", choices = c("Summary Statistics", "Correlation Matrix")),
        actionButton("run_analysis", "Run Analysis"),
        
        hr(),
        
        div(class = "sidebar-heading", "Visualization"),
        selectInput("visualize_column", "Select Variable for Visualization", choices = NULL),
        selectInput("plot_type", "Select Plot Type", choices = c("Bar Graph", "Line Graph")),
        actionButton("visualize_data", "Generate Visualization"),
        
        hr(),
        
        div(class = "sidebar-heading", "Patient Information"),
        numericInput("age", "Enter Age", value = 30, min = 1, max = 100),
        selectInput("gender", "Select Gender", choices = c("Male" = 1, "Female" = 2)),
        numericInput("prior_appts_attended", "Number of Prior Appointments", value = 2, min = 0),
        selectInput("disease", "Select Disease Admission", choices = disease_columns),
        selectInput("model_choice", "Select Regression Model", choices = c("Linear Regression", "Random Forest")),
        actionButton("predict", "Predict the Patients Length of Stay"),
        
        hr(),
        
        actionButton("help", "Help")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Data Overview", verbatimTextOutput("data_summary")),
          tabPanel("Cleaned Data", DT::dataTableOutput("cleaned_data")),
          tabPanel("Analysis Results", DT::dataTableOutput("analysis_results")),
          tabPanel("Visualization", plotlyOutput("data_plot")),
          tabPanel("Patient Prediction", verbatimTextOutput("prediction"))
        )
      )
    )
  )
)

#Creating the server function 
server <- function(input, output, session) {
  # Activate authentication using pre-defined credentials
  res_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  options(shiny.maxRequestSize = 12 * 1024^2)  # Set max upload size to 12 MB
  
  # Reactive variables to store data
  raw_data <- reactiveVal(NULL)
  cleaned_data <- reactiveVal(NULL)
  
  
  # Load data from file or URL
  observeEvent(input$load_data, {
    if (!is.null(input$file_upload)) {
      ext <- tools::file_ext(input$file_upload$name)
      if (ext == "csv") {
        raw_data(read_csv(input$file_upload$datapath))
      } else if (ext == "xlsx") {
        raw_data(read_excel(input$file_upload$datapath))
      } else {
        showNotification("Unsupported file type. Please upload a CSV or Excel file.", type = "error")
        return()
      }
    } else if (nzchar(input$url_input)) {
      raw_data(read_csv(input$url_input))
    } else {
      showNotification("Please upload a file or provide a valid URL", type = "error")
    }
    
    updateSelectInput(session, "visualize_column", choices = names(raw_data()))
    updateSelectInput(session, "target_column", choices = setdiff(names(raw_data()), c("id", "death", "los", "age", "gender", "quintile", "ethnicgroup", "fu_time")))
    
    output$data_summary <- renderPrint({
      req(raw_data())
      summary(raw_data())
    })
  })
  
  # Reset data
  observeEvent(input$reset_data, {
    raw_data(NULL)
    cleaned_data(NULL)
    updateTextInput(session, "url_input", value = "")
    updateSelectInput(session, "visualize_column", choices = NULL)
    updateSelectInput(session, "target_column", choices = NULL)
    
    output$data_summary <- renderPrint(NULL)
    output$data_plot <- renderPlotly(NULL)
    output$cleaned_data <- renderTable(NULL)
    output$analysis_results <- renderTable(NULL)
    output$los <- renderPrint(NULL)
  })
  
  # Clean data
  observeEvent(input$clean_data, {
    req(raw_data())
    
    # Apply selected cleaning method
    if (input$cleaning_method == "Remove Missing Rows") {
      cleaned_data(raw_data() %>% drop_na())
    } else if (input$cleaning_method == "Impute Missing Values with Mean") {
      cleaned_data(raw_data() %>% 
                     mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))))
    }
    
    # Render cleaned data in a DataTable format
    output$cleaned_data <- DT::renderDataTable({
      req(cleaned_data())
      cleaned_data()
    }, options = list(
      pageLength = 10,   # Show 10 rows per page
      scrollX = TRUE,    # Enable horizontal scrolling
      searching = TRUE   # Enable search functionality
      
    ))
  })
  
  # Visualization
  observeEvent(input$visualize_data, {
    req(raw_data(), input$visualize_column, input$plot_type)
    
    output$data_plot <- renderPlotly({
      plot <- NULL
      column_data <- raw_data()[[input$visualize_column]]
      
      if (input$plot_type == "Bar Graph") {
        # Convert to factor if categorical
        if (!is.numeric(column_data)) {
          raw_data()[[input$visualize_column]] <- factor(column_data)
        }
        
        plot <- ggplot(raw_data(), aes_string(x = input$visualize_column)) +
          geom_bar(fill = "#3498db", color = "black") +  # Use a better color palette
          labs(title = "Bar Graph", x = input$visualize_column, y = "Count") +
          theme_minimal() +  # Apply cleaner theme
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x labels if needed
          coord_flip()  # Flip bars for readability
        
      } else if (input$plot_type == "Line Graph") {
        plot <- ggplot(raw_data(), aes_string(x = input$visualize_column)) +
          geom_line(stat = "count", color = "#2ecc71", size = 1) +
          geom_point(stat = "count", color = "#e74c3c", size = 2) +  # Highlight points
          labs(title = "Line Graph", x = input$visualize_column, y = "Count") +
          theme_minimal() + 
          theme(panel.grid.major = element_line(color = "grey90"))  # Improve readability
      }
      
      ggplotly(plot)
    })
  })
  
  #Analyzing the data
  observeEvent(input$run_analysis, {
    req(cleaned_data())
    results <- NULL
    if (input$analysis_method == "Summary Statistics") {
      results <- cleaned_data() %>%
        summarise(across(where(is.numeric), list(
          Mean = ~mean(.),
          Median = ~median(.),
          SD = ~sd(.),
          Min = ~min(.),
          Max = ~max(.)
        ), .names = "{col}_{fn}")) %>%
        pivot_longer(everything(), names_to = c("Variable", "Statistic"), names_sep = "_")
      
      output$analysis_results <- DT::renderDataTable({
        req(results)
        results
      }, options = list(pageLength = 5))
      
    } else if (input$analysis_method == "Correlation Matrix") {
      numeric_data <- cleaned_data() %>% select(where(is.numeric))
      
      if (ncol(numeric_data) < 2) {
        output$analysis_results <- renderPrint({
          "Correlation Matrix cannot be computed with fewer than two numeric columns."
        })
      } else {
        results <- cor(numeric_data, use = "complete.obs") %>% as.data.frame()
        results <- tibble::rownames_to_column(results, "Variable")
        
        output$analysis_results <- DT::renderDataTable({
          req(results)
          results
        }, options = list(pageLength = 5, scrollX = TRUE))
      }
    }
  })
  
  #Predicting patient length of stay
  predicted_value <- reactiveVal(NULL)
  
  # Perform prediction
  observeEvent(input$predict, {
    if (is.null(input$disease)) {
      output$prediction <- renderText("Select a disease to predict length of stay.")
      return()
    }
    
    # Prepare new patient data
    new_patient <- data.frame(
      age = as.numeric(input$age),
      gender = factor(input$gender, levels = c(1, 2)),
      prior_appts_attended = as.numeric(input$prior_appts_attended)
    )
    
    # Initialize all diseases as 0
    disease_data <- setNames(as.list(rep(0, length(disease_columns))), disease_columns)
    
    # Set selected disease to 1
    disease_data[[input$disease]] <- 1
    
    # Combine patient details and disease info
    new_patient <- cbind(new_patient, as.data.frame(disease_data))
    
    # Predict hospital stay using selected model
    if (input$model_choice == "Linear Regression") {
      prediction <- predict(lm_model, newdata = new_patient)
      y_true <- data$los
      y_pred <- predict(lm_model, newdata = data)
      
    } else {
      prediction <- predict(rf_model, newdata = new_patient)
      y_true <- data$los
      y_pred <- predict(rf_model, newdata = data)
    }
    
    # Compute Mean Squared Error (MSE)
    mse <- mean((y_true - y_pred)^2)
    
    # Compute R² Score
    ss_res <- sum((y_true - y_pred)^2)
    ss_tot <- sum((y_true - mean(y_true))^2)
    r2_score <- 1 - (ss_res / ss_tot)
    
    # Display results
    output$prediction <- renderText({
      paste0(
        "Predicted Length of Stay: ", round(prediction, 2), " days\n",
        "Mean Squared Error (MSE): ", round(mse, 4), "\n",
        "R² Score: ", round(r2_score, 4)
      )
    })
  })
  
  # Help feature
  observeEvent(input$help, {
    showModal(modalDialog(
      title = "Help",
      HTML("
Predicting Patient Length of Stay

<br><br>Introduction
This program is designed to assist healthcare workers in determining the length of patient admission based on the institutions prior patient admission data. By leveraging this tool, hospitals can improve resource allocation and staffing, while promoting transparency between hospital staff and patients. 

<br><br>Instructions
This guide helps users navigate and utilize the features available. Below are detail descriptions of each functon in the app:

<br><br>1. Data Upload
<br>- Click Upload File to load in a CSV or Excel file, or enter a dataset in through a URL in the text box below. 
<br>- Click Load Data to import the selected dataset.
<br>- If the dataset entered or imported is incorrect click Reset Data to clear the existing dataset.

<br><br>2. Data Cleaning
<br>- From the dropdown menu select a data cleaning method:
	Remove Missing Rows - delete rows with missing values.
	Impute Missing Values with Mean - substitues any missing values with the mean of each numeric column.
<br>- Click Clean Data to apply the selected cleaning method.

<br><br>3. Data Analysis 
<br>- Choose an analytical method from the dropdown menu:
	Summary Statistics - Displays the mean, median, standard deviation, min, and max for each numeric column.
	Correlation Matrix - COmputes the correlation values between numeric variables.
<br>- After choosing the desired analytical method click Run Analysis to generate a result.

<br><br>4. Data Visualization
<br>- Select a variable to display into a graph, first begin by choosing which variable from the dropdown menu n the Select Variable for Visualization section.
<br>- Then choose between a line graph or bar graph to display the variable.
<br>- Click Generate Visualization to display the graph.

<br><br>5. Patient Prediction 
<br>- To predict the length of stay of a patient provide the necessary information of the patient. 
<br>= Enter the patient's Age.
<br>- From the dropdown menu select the gender.
<br>- Specify the number of prior appointments attended by the patient.
<br>- Select the reaseon for admission.
<br>- Click Predict the Patient's Length of Stay to determine the predicted results.

<br><br>6. Generating Reports
<br>- To generate a final report of each function choose between a PDF or Word format.
<br>- Click Download Report to save the generated report. 

<br><br>Model Performance Metrics
<br>The model performance metrics can be found in the Patient Prediction column under the predicted amount of days. The model performance metrics helps determine how well the predictive model performs in regards to the dataset. The two key metrics used in this application are Mean Squared Error (MSE) and R2 Score. The following provides a clear definition of each metric and how to distinguish the results.

<br><br>Mean Squared Error (MSE)
<br>MSE measures the average squared difference between the actual and predicted values. A lower MSE score indicates better model accuracy, meaning that the prediction is closer to the actual value. The higher the MSE score is the larger errors are in the prediction. 

<br><br>R2 Score
<br>R2 measures how well the independent variables explain the variability of the dependent variable. The R2 score ranges from 0 to 1, where 0 means the model does not explain any variance and 1 meaning there is a perfect prediction. 

<br><br>What to Look For:
<br>To determine which model is more accurate choose the model with the lowest MSE and highest R2 score for better accuracy on the predicted length of stay for a patient. If the MSE is high and R2 is low, consider using a different model for a better prediction. 

    "),
      easyClose = TRUE
    ))
  })
}


# Run the app
shinyApp(ui = ui, server = server)