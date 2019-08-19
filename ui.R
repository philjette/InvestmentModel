library(shiny)
risk_levels <- c("LOW", "MINOR", "MODERATE", "SIGNIFICANT", "MAJOR", "CATASTROPHIC", "EXTREME")
likelihood <- c("MINIMAL", "SOME", "MODERATE", "MAJOR", "SIGNIFICANT")

renderInputs <- function(prefix) {
  wellPanel(
    fluidRow(
      column(12, textInput(paste0(prefix, "_", "desc"), "Description")),     
      column(6,
             selectInput(paste0(prefix, "_", "asset_type"), "Asset Type", c("General", "Electronic", "Batteries", "Generators"), 
                         selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
             sliderInput(paste0(prefix, "_", "equip_age"), "Current Equipment Age (in Years):", min = 1, max = 50, value = 20),
             sliderInput(paste0(prefix, "_", "econ_life"), "Expected Economic Life:", min = 1, max = 50, value = 25),
             numericInput(paste0(prefix, "_", "cond_override"), "Equipment Condition (AHI):", min = 0.0, max = 10.0, value=5.0),
             sliderInput(paste0(prefix, "_", "rep_cost"), "Asset Replacement Cost (in $):", min = 1000, max = 5000000, value = 500000.0, step = 1000, pre = "$", sep = ","),
             sliderInput(paste0(prefix, "_", "emerg_factor"), "Emergency Factor:", min = 0.0, max = 5.0, value = 2.0, step = 0.5),
             numericInput(paste0(prefix, "_", "energy_savings"), "Energy Savings (annual)", value=10000, width = NULL),
             numericInput(paste0(prefix, "_", "cost_avoid"), "Cost Avoidance", value=0, width = NULL),
             sliderInput(paste0(prefix, "_", "cost_avoid_period"), "Period of Cost Avoided:", min = 1, max = 5, value = 3)
      ),
      column(6,
             sliderInput(paste0(prefix, "_", "rest_time"), "Time to Restore (Hrs):", min = 1, max = 336, value = 5),
	     numericInput(paste0(prefix, "_", "biz_cont"), "Business Continuity Risk ($/min)", value=84, width = NULL),
             numericInput(paste0(prefix, "_", "rev_supported"), "Supported Revenue (Annual)", value=50000, width = NULL),
             selectInput(paste0(prefix, "_", "redundancy"), "Level of Redundancy", c("Direct Impact", "N+1", "Beyond N+1"), 
                         selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
             selectInput(paste0(prefix, "_", "regulatory"), "Regulatory Risk", risk_levels, 
                         selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
             selectInput(paste0(prefix, "_", "regulatory_p"), "Impact Likelihood", likelihood, 
                         selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
             selectInput(paste0(prefix, "_", "community"), "Community Relations Risk", risk_levels, 
                         selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
             selectInput(paste0(prefix, "_", "community_p"), "Impact Likelihood", likelihood, 
                         selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
             selectInput(paste0(prefix, "_", "health"), "Health & Safety Risk", risk_levels, 
                         selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL),
             selectInput(paste0(prefix, "_", "health_p"), "Impact Likelihood", likelihood, 
                         selected = NULL, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)
      )
    ),
    p(downloadButton(paste0(prefix, "_", "downloadData"),
                   "Download")
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
            column(6, tags$h3("Scenario A")),
            column(6, tags$h3("Scenario B"))
          ),
	  fluidRow(
		column(6,dataTableOutput('table_a')),
	        column(6,dataTableOutput('table_b'))	
          ),
          fluidRow(
            column(6, renderInputs("a")),
            column(6, renderInputs("b"))
          ),
          fluidRow(
            column(6,plotOutput("a_distPlot", height = "600px")),
            column(6,plotOutput("b_distPlot", height = "600px"))
          )
)
