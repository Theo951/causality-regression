library(shiny)
library(ggplot2)
library(car)

# Define UI for the app
ui <- fluidPage(
  titlePanel("Causality and Multiple Regression Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dep_var", "Dependent Variable:",
                  choices = colnames(mtcars), selected = "mpg"),
      
      checkboxGroupInput("indep_vars", "Independent Variables:",
                         choices = colnames(mtcars),
                         selected = c("wt", "hp")),
      
      actionButton("run_model", "Run Regression")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Model Summary", verbatimTextOutput("model_summary")),
        tabPanel("Residuals Plot", plotOutput("residual_plot")),
        tabPanel("Diagnostics", plotOutput("diagnostics_plot")),
        tabPanel("Variable Importance", plotOutput("importance_plot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive expression to run regression when action button is clicked
  model <- eventReactive(input$run_model, {
    req(input$indep_vars)  # Require at least one independent variable
    formula <- as.formula(paste(input$dep_var, "~", paste(input$indep_vars, collapse = " + ")))
    lm(formula, data = mtcars)
  })
  
  # Display the model summary
  output$model_summary <- renderPrint({
    summary(model())
  })
  
  # Residuals plot
  output$residual_plot <- renderPlot({
    mod <- model()
    ggplot(data.frame(residuals = residuals(mod), fitted = fitted(mod)), aes(x = fitted, y = residuals)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals")
  })
  
  # Diagnostics plots (QQ Plot)
  output$diagnostics_plot <- renderPlot({
    mod <- model()
    par(mfrow = c(2, 2))
    plot(mod)
  })
  
  # Variable importance (coefficients)
  output$importance_plot <- renderPlot({
    mod <- model()
    coefs <- summary(mod)$coefficients[-1, "Estimate"]
    barplot(coefs, main = "Variable Importance", col = "steelblue", horiz = TRUE, las = 1)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
