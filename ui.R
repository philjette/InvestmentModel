library(shiny)
library(DT)

risk_levels <- c("LOW", "MINOR", "MODERATE", "SIGNIFICANT", "MAJOR", "CATASTROPHIC", "EXTREME")
likelihood <- c("MINIMAL", "SOME", "MODERATE", "MAJOR", "SIGNIFICANT")

renderInputs <- function() {
  wellPanel(
    fluidRow(
      column(12, textInput("description", "Description")),     
      column(6,
             selectInput("asset_type", "Asset Type", c("General", "Electronic", "Batteries", "Generators"), 
                         selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
             sliderInput("equip_age", "Current Equipment Age (in Years):", min = 1, max = 50, value = 20),
             sliderInput("econ_life", "Expected Economic Life:", min = 1, max = 50, value = 25),
             numericInput("cond_override", "Equipment Condition (AHI):", min = 0.0, max = 10.0, value=5.0),
             sliderInput("rep_cost", "Asset Replacement Cost (in $):", min = 1000, max = 5000000, value = 500000.0, step = 1000, pre = "$", sep = ","),
             sliderInput("emerg_factor", "Emergency Replacement Cost Factor:", min = 0.0, max = 5.0, value = 2.0, step = 0.5),
             numericInput("energy_savings", "Energy Savings (annual)", value=10000, width = NULL),
             numericInput("cost_avoid", "Cost Avoidance", value=0, width = NULL),
             sliderInput("cost_avoid_period", "Period of Cost Avoided:", min = 1, max = 5, value = 3)
             ,       
             downloadButton("downloadData","Download")
      ),
      column(6,
             sliderInput("rest_time", "Time to Restore (Hrs):", min = 1, max = 336, value = 5),
	     numericInput("biz_cont", "Business Continuity Risk ($/min)", value=84, width = NULL),
             numericInput("rev_supported", "Supported Revenue (Annual)", value=50000, width = NULL),
             selectInput("redundancy", "Level of Redundancy", c("Direct Impact", "N+1", "Beyond N+1"), 
                         selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
             selectInput("regulatory", "Regulatory Risk", risk_levels, 
                         selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
             selectInput("regulatory_p", "Regulatory Impact Likelihood", likelihood, 
                         selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
             selectInput("community", "Community Relations Risk", risk_levels, 
                         selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
             selectInput("community_p", "Community Impact Likelihood", likelihood, 
                         selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
             selectInput("health", "Health & Safety Risk", risk_levels, 
                         selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
             selectInput("health_p", "Health & Safety Impact Likelihood", likelihood, 
                         selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)
	     
      )
    )
  )
}

# Define UI for application that plots results
fluidPage(theme="simplex.min.css",
          tags$style(type="text/css",
                     "label {font-size: 12px;}",
                     ".recalculating {opacity: 1.0;}"
          ),
          
          # Application title
          tags$h2("Risk-Based Investment: Evaluating Returns to Reliability Capital"),
          hr(),
          
          fluidRow(
            column(6, tags$h3("Investment Scenario"))
          ),
          fluidRow(
            column(12, DTOutput('resultsTable'))	
          ),
          fluidRow(
            column(12, renderInputs())
          ),
          fluidRow(
            column(12,plotOutput("a_distPlot", height = "600px"))
          )
)
