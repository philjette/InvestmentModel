library(dplyr)
library(ggplot2)
library(scales)
library(DT)
library(gridExtra)

paramNames <- c("asset_type", "equip_age", "econ_life",
                "cond_override", "rep_cost", "emerg_factor", "energy_savings",
                "rest_time", "biz_cont", "rev_supported", "redundancy", "regulatory", "regulatory_p",
                "community", "community_p", "health", "health_p", "cost_avoid", "cost_avoid_period", "description")

## NOTE: Left prime_power out of the paramNames for now. Difficulty getting it to work due to type issues 

failure_curves <- read.csv("FailureCurves.csv", header = TRUE, sep=",", stringsAsFactors=FALSE)
cons_likelihood <- read.csv("ConsLikelihood.csv", header = TRUE, sep=",", stringsAsFactors=FALSE)
risk_matrix <- read.csv("RiskMatrix.csv", header = TRUE, sep=",", stringsAsFactors=FALSE)

calculate_return <- function(asset_type = "General", equip_age = 20,
                         econ_life = 15, cond_override = NULL,
                         rep_cost = 500000, emerg_factor = 2, energy_savings = 15000,
                         rest_time = 200, biz_cont = 84, rev_supported=10000000, redundancy="N+1", regulatory="MINOR", 
                         regulatory_p = "MINIMAL", community = "MODERATE", community_p = "MINIMAL", health = "MINOR", 
                         health_p = "MODERATE", cost_avoid = 0, cost_avoid_period = 3, prime_power=1, description="") {
  #-------------------------------------
  # Inputs
  #-------------------------------------
  
  #constants
  redundancy_factor <- .06
  current_year <- 2019
  discount_rate <- .0825
  npv_period <- 5
  churn_factor <- 1/1000000000
  
  #initialize vars
  a_type <- asset_type
  e_age <- equip_age
  e_life <- econ_life
  current_condition <- 0
  r_level <- 0
  c_avoid <- cost_avoid
  c_avoid_period <- cost_avoid_period
  npv<-0
  desc <- description
  
  # if user overides condition, use that value, otherwise use calculated (linear) value
  if (is.null(cond_override)) {
    current_condition <- max(c(0,10-(7*e_age/e_life)))
  } else {
    current_condition <- cond_override
  }
  
  # set the appropriate factor based on redundancy level. 
  # we don't discount direct impact at all, but scale accordingly for N+1 and beyond
  if (redundancy=="Direct Impact") {
    r_level <- 0
  } else if (redundancy=="N+1"){
    r_level <- 1
  } else {
    r_level <- 2
  }
  
  #calculate churn rates by hour of time to restore. Keep in a dataframe
  Hrs <- seq(1,336,by=1)
  Churn <- sapply(Hrs, function(x){round(churn_factor^(1/x),2)})
  churn_rates<-data.frame(cbind(Hrs, Churn))
  
  
  #calculate cost of outage based on Time to Restore and business systems impacted. 
  #mult by 60 as we're concerned with hourly cost
  r_time <- rest_time
  cost_of_outage <- r_time*biz_cont*60
  r_supported <-rev_supported
  rev_risk <- churn_rates[which(churn_rates$Hrs == r_time), 2]*rev_supported
  failure_curve <- failure_curves[which(failure_curves$Type == a_type), ]
  
  # For certain consequences we capture the likelihood seperately from the probability of failure
  # eg: not all battery failures result in thermal runaway and pose a major H&S risk
  # Hence we scale the possible impacts based on the likelihood
  reg_risk <- risk_matrix[which(risk_matrix$Level == regulatory), 2]*cons_likelihood[which(cons_likelihood$Risk_Level == regulatory_p), 2]
  com_risk <- risk_matrix[which(risk_matrix$Level == community), 2]*cons_likelihood[which(cons_likelihood$Risk_Level == community_p), 2]
  health_risk <- risk_matrix[which(risk_matrix$Level == health), 2]*cons_likelihood[which(cons_likelihood$Risk_Level == health_p), 2]
  
  r_cost <- rep_cost
  e_factor <- emerg_factor
  e_savings <- energy_savings
  total_risk <- (r_cost*e_factor)+cost_of_outage+rev_risk+reg_risk+com_risk+health_risk
  
  #function to compute the PV of a given period cashflow
  pv <- function(cashflow, rate, row) {
    return(round(cashflow/((1+rate)^(row[2])),2))
  }
  
  #function to return expected condition for given period
  future_cond <- function(exp_life, curr_cond, row){
    return(round(max(c(0,curr_cond-(7*row[2]/exp_life))),2))
  }
  
  #function to return probability of failure based on asset condition in the given period. Uses the defined failure curve for a given
  #asset type
  pof <- function(x, red_fact, red_level) {
    round((exp(1)^failure_curve$Mid)/(1+exp(1)^(failure_curve$Mid+failure_curve$bias+failure_curve$convexity*x))*red_fact^red_level*prime_power,2)
  }
  
  
  #initialize a vector of years starting at the current year of analysis
  years <- seq(from = current_year, to = current_year+50, by = 1)
  #compute the period of analysis from the current year
  periods <- years - current_year
  #compute the equipment age starting from the current age
  age <- seq(from = e_age, to = e_age+50, by = 1)
  
  #initiatlize a vector for cost avoidance. calculate the PV of avoided cost and replace this value
  #in the given period
  cost_avoid_pv <- rep(0, length(years))
  cost_avoid_pv[c_avoid_period+1] <- round(c_avoid/((1+discount_rate)^(c_avoid_period)),2)
  
  results <- data.frame(cbind(years, periods, age, cost_avoid_pv))
  
  #present value of cost in each period
  cost_pv <- apply(results, 1, pv, cashflow = r_cost, rate = discount_rate)
  #present value of energy savings in each period
  energy_pv <- apply(results, 1, pv, cashflow = e_savings, rate = discount_rate)
  #condition in each period
  condition <- apply(results, 1, future_cond, exp_life = e_life, curr_cond = current_condition)
  #pof in each period based on condition
  p_failure <- sapply(condition, pof, red_fact = redundancy_factor, red_level = r_level)
  #scale total risk by PoF to get the aggregate risk in each period
  agg_risk <- total_risk*p_failure
  
  #include where we would expect the condition to be in the baseline case
  expected_condition <- apply(results, 1, future_cond, exp_life = e_life, curr_cond = max(c(0,10-(7*e_age/e_life))))
  
  #bind the new fields into our DF
  results <- data.frame(cbind(results, cost_pv, energy_pv, condition, p_failure, agg_risk))
  
  #create more calculated fields 
  agg_risk_pv <- apply(results, 1, function(row){row[9]/((1+.0825)^(row[2]))})
  total_value_pv <- energy_pv+sum(cost_avoid_pv)+agg_risk_pv
  roi <- round((total_value_pv - cost_pv)/cost_pv,2)
  
  #again bind these into our DF
  results <- data.frame(cbind(results, agg_risk_pv, total_value_pv, roi, expected_condition))
  
  #calculate a n year NPV for each year. We only calculate an NPV if we're not going to run into the EoF
  #NPV is: cost in period 1, value in next 4 periods
  for (row in 1:dim(results)[1]) {
    if (row+npv_period > dim(results)[1]) {
      npv[row] <- 0
    } else {
      npv[row] <- round(sum(results$total_value_pv[(row+1):(row+npv_period-1)],-results$cost_pv[row],2))
      }
    }
  
  #finally, bind the NPV into our DF
  results <- data.frame(cbind(results, npv))
  
  results$desc <- desc

  return(results)
}

plot_roi <- function(results) {
  
  results <- results[ which(results$condition > 0), ]
  
  layout(matrix(c(1,2,1,3),2,2))
  
  palette(c("black", "grey50", "grey30", "grey70", "#d9230f"))
  
  #plot(results$years, (results$npv)/1000, main="5-Year Net Present Value of Investment", 
  #     ylab="Risk ($1000s)", xlab="Period")
  
  # plot scenarios
  
  p2 <- ggplot(data=results, aes(x=years, y=npv/1000000, group=1)) +
    geom_line()+
    geom_point() + 
    scale_y_continuous(name="5-Year NPV ($M)") +
    scale_x_continuous(name="Year of Investment") +
    geom_hline(yintercept=0, linetype="dashed", 
               color = "red", size=1)
  
  #plot(results$years, results$agg_risk/1000, main="Financial Risk by Period", 
  #     ylab="Risk ($1000s)", xlab="Period")
  #grid()
  
  p3 <- ggplot(data=results, aes(x=years, y=roi, group=1)) +
    geom_line()+
    geom_point() + 
    scale_y_continuous(labels = scales::percent, name="1-Year ROI") +
    scale_x_continuous(name="Year of Investment") +
    geom_hline(yintercept=0, linetype="dashed", 
               color = "red", size=1)
  
  grid.arrange(p2,p3, nrow=2)
  #plot(results$years, results$roi, main="ROI of Capital Investment", 
  #     ylab="ROI", xlab="Period")
  #grid()
}

# Define server logic required to generate and plot a random distribution
#
# Idea and original code by Pierre Chretien
# Small updates by Michael Kapler
#
function(input, output, session) {
  
  getParams <- function(prefix) {
    input[[paste0(prefix, "_recalc")]]
    
    params <- lapply(paramNames, function(p) {
      input[[paste0(prefix, "_", p)]]
    })
    names(params) <- paramNames
    params
  }
  
  # Function that generates scenarios and computes NAV. The expression
  # is wrapped in a call to reactive to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #
  roiA <- reactive(do.call(calculate_return, getParams("a")))
  roiB <- reactive(do.call(calculate_return, getParams("b")))
  
  # Expression that plot NAV paths. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
  #
  
  keeps<-c("condition", "expected_condition", "p_failure", "roi", "npv")
  
  output$a_distPlot <- renderPlot({
    plot_roi(roiA())
  })
  output$b_distPlot <- renderPlot({
    plot_roi(roiB())
  })
  
  output$a_expected_cond <- renderText({ 
    paste("Expected condition: ",roiA()[1,13])
  })
  output$b_expected_cond <- renderText({ 
    paste("Expected condition: ",roiB()[1,13])
  })
  
  output$table_a <- DT::renderDataTable({
    DT::datatable(
      select(roiA(), keeps)[1,], options = list(
      paging = FALSE, searching = FALSE, dom = 't'
      ),
      colnames = c('AHI', 'Expected AHI', 'PoF', 'ROI (1-year)', 'NPV (5-year)'),
      class = 'cell-border stripe',
      rownames= FALSE
    ) %>% formatCurrency(5,'$') %>% formatPercentage(3:4,2)
  })
  output$table_b <- DT::renderDataTable({
    DT::datatable(
      select(roiB(), keeps)[1,], options = list(
        paging = FALSE, searching = FALSE, dom = 't'
      ),
      colnames = c('AHI', 'Expected AHI', 'PoF', 'ROI (1-year)', 'NPV (5-year)'),
      class = 'cell-border stripe',
      rownames= FALSE
    ) %>% formatCurrency(5,'$') %>% formatPercentage(3:4,2)
  })
  
  output$a_downloadData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
         write.csv(roiA(), con)
       }
     )
  output$b_downloadData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
         write.csv(roiB(), con)
       }
     )

  
}
