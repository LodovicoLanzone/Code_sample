library (plm)
library(DescTools)
library(stargazer)
library(lfe)
library(dplyr)
library(survival)
library(lmtest)
library(sandwich)
library (fixest)
library(pscl)
library (TMB)
library (glmmTMB)
library (ggplot2)
library (rlang)
library (margins)
rm(list = ls())
setwd("...")
#### Loading and data preparing ####
path = 'data_processed/pc1_thousand.csv'
data_ger = read.csv(path)
path = 'data_processed/france_grid_cells_amenities_income.csv'
data_fra = read.csv(path)

#DATA PREPARATION FOR FRANCE
#change name of population parameter and municipality parameter:
data_fra <- data_fra %>%
  rename (pop = ind,
          AGS = ID)
#create dummies for extensive analysis
data_fra <- data_fra %>%
  mutate (tpm_ps = if_else (ps_count > 0, 1, 0),
          tpm_edu = if_else (edu_count > 0, 1, 0),
          tpm_pt = if_else (pt_count > 0, 1, 0),
          tpm_park = if_else (park_common_count > 0, 1, 0),
          tpm_pec = if_else (pec_count > 0, 1, 0),
          tpm_sports = if_else (sports_count > 0, 1, 0),
          tpm_cyc = if_else (cyc_count > 0, 1, 0),
          tpm_town = if_else (town_count > 0, 1, 0))
#create share of under 18:
data_fra <- data_fra %>%
  mutate (share_018 = (ind_0_3 + ind_4_5 + ind_6_10 + ind_11_17)/pop)
#create our X of interest: disposable income per capita
data_fra <- data_fra %>%
  mutate (income_per_capita = ind_snv / pop)

#DEFINITION OF FUNCTIONS
#DEALING WITH OUTLIERS
winsorization <- function (data){ data$ps_count <- Winsorize(data$ps_count, val = quantile (data$ps_count, probs = c(0.001, 0.999), na.rm = FALSE))
data$edu_count <- Winsorize(data$edu_count, val = quantile (data$edu_count, probs = c(0.001, 0.999), na.rm = FALSE))
data$pt_count <- Winsorize(data$pt_count, val = quantile(data$pt_count, probs = c(0.001, 0.999), na.rm = FALSE))
data$park_common_count <- Winsorize(data$park_common_count, val = quantile(data$park_common_count, probs = c(0.001, 0.999), na.rm = FALSE))
data$pec_count <- Winsorize(data$pec_count, val = quantile(data$pec_count, probs = c(0.001, 0.999), na.rm = FALSE))
data$sports_count <- Winsorize(data$sports_count, val = quantile(data$sports_count, probs = c(0.001, 0.999), na.rm = FALSE))
data$cyc_count <- Winsorize(data$cyc_count, val = quantile(data$cyc_count, probs = c(0.001, 0.999), na.rm = FALSE))
data$town_count <- Winsorize(data$town_count, val = quantile(data$town_count, probs = c(0.001, 0.999), na.rm = FALSE))
return (data)
}


extract_margins <- function(model, dep_var_name, var) {
  margins_result <- margins(model, type = "risk")
  margins_summary <- summary(margins_result)
  ame_row <- margins_summary[margins_summary$factor == var, ]
  ame <- ame_row$AME
  se <- ame_row$SE
  coefficient_col <- paste0("Coefficient_", var)
  se_col <- paste0("SE_", var)
  results <- data.frame(
    Dependent_Variable = dep_var_name,
    ame = ame,  
    se = se     
  )
  colnames(results)[2] <- coefficient_col
  colnames(results)[3] <- se_col
  return(results)
}
extract_coefs <- function(model, dep_var_name, se_type = "Std. Error", coef = "Estimate", vars) {
  # Extract the coefficient summary
  coefs <- summary(model)$coefficients
  
  # Print out the coefficient names for debugging (optional)
  print(rownames(coefs))
  
  # Initialize a list to store the coefficients and standard errors
  results <- list(Dependent_Variable = dep_var_name)
  
  # Loop over the specified variables to extract their coefficients and SE
  for (var in vars) {
    # Check if the variable exists in the coefficient table
    if (var %in% rownames(coefs)) {
      results[[paste("Coefficient_", var, sep = "")]] <- coefs[var, coef]
      results[[paste("SE_", var, sep = "")]] <- coefs[var, se_type]
    } else {
      # If variable not found in the coefficients table, assign NA
      results[[paste("Coefficient_", var, sep = "")]] <- NA
      results[[paste("SE_", var, sep = "")]] <- NA
    }
  }
  # Convert the results list into a data frame
  return(as.data.frame(results))
}
extract_coefs3 <- function(model, dep_var_name, coef = "Estimate", se_type = "Std. Error", vars) {
  # Extract coefficient names and values
  coefs <- coef(model)  # Extracts coefficients
  se <- sqrt(diag(vcov(model)))  # Extracts standard errors
  # Initialize a list to store the coefficients and standard errors
  results <- list(Dependent_Variable = dep_var_name)
  
  # Loop over the specified variables to extract their coefficients and SE
  for (var in vars) {
    # Check if the variable exists in the coefficient table
    if (var %in% names(coefs)) {
      results[[paste("Coefficient_", var, sep = "")]] <- coefs[var]
      results[[paste("SE_", var, sep = "")]] <- se[var]
    } else {
      # If variable not found in the coefficients table, assign NA
      results[[paste("Coefficient_", var, sep = "")]] <- NA
      results[[paste("SE_", var, sep = "")]] <- NA
    }
  }
  return(as.data.frame(results))
}
#function to create graph
create_plot <- function(results, variable, title, output_file) {
  # Dynamically generate the coefficient and standard error column names
  coefficient_col <- paste0("Coefficient_", variable)
  se_col <- paste0("SE_", variable)
  # Calculate lower and upper bounds explicitly
  lower_bound <- min(results[[coefficient_col]] - 1.96 * results[[se_col]], na.rm = TRUE)
  upper_bound <- max(results[[coefficient_col]] + 1.96 * results[[se_col]], na.rm = TRUE)
  max_bound <- max (abs (lower_bound), abs (upper_bound))
  # Create the plot
  plot <- ggplot(results, aes(x = !!sym(coefficient_col), y = Dependent_Variable)) +
    geom_point() +  # Dots representing coefficient estimates
    geom_errorbarh(
      aes(
        xmin = !!sym(coefficient_col) - 1.96 * !!sym(se_col),  # Calculate the lower bound of the error bars
        xmax = !!sym(coefficient_col) + 1.96 * !!sym(se_col)   # Calculate the upper bound of the error bars
      )
    ) +  # Error bars (95% CI)
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Line at 0 (null hypothesis)
    theme_minimal() +
    labs(
      title = title,
      x = "Coefficient Estimate",
      y = "Dependent Variable"
    ) +
    scale_x_continuous(
      limits = c(-max_bound, max_bound),
      expand = c(0, 0)  # Ensures the plot is tightly fit around the data
    )
  # Print the plot
  print (plot)
  ggsave(output_file, plot = plot, width = 10, height = 6)
}
#filter_column, filter_value, add them to the function if filtering is needed
run_regression <- function (data, dependent_var, output, control_vars) {
  #data <- data %>%
  #filter(!!sym(filter_column) == filter_value)
  #Extensive
  #without fixed effects:
  output_name <- paste0 ("Regression_Results/", output,"_extensive.csv")
  graph_name <- paste0("graphs/",output,"_extensive.pdf")
  full_formula1 <- as.formula (paste ("tpm_ps ~ log(", dependent_var, ") ", control_vars ))
  full_formula2 <- as.formula (paste ("tpm_edu ~ log(", dependent_var, ") ", control_vars))
  full_formula3 <- as.formula (paste ("tpm_pt ~ log(", dependent_var, ") ", control_vars))
  full_formula4 <- as.formula (paste ("tpm_park ~ log(", dependent_var, ") ", control_vars ))
  full_formula5 <- as.formula (paste ("tpm_pec ~ log(", dependent_var, ") ", control_vars ))
  full_formula6 <- as.formula (paste ("tpm_sports ~ log(", dependent_var, ") ", control_vars ))
  full_formula7 <- as.formula (paste ("tpm_cyc ~ log(", dependent_var, ") ", control_vars ))
  full_formula8 <- as.formula (paste ("tpm_town ~ log(", dependent_var, ") ", control_vars ))
  m1 = lm (full_formula1, data = data)
  m2 = lm (full_formula2, data = data)
  m3 = lm (full_formula3, data = data)
  m4 = lm (full_formula4, data = data)
  m5 = lm (full_formula5, data = data)
  m6 = lm (full_formula6, data = data)
  m7 = lm (full_formula7, data = data)
  m8 = lm (full_formula8, data = data)
  results <- rbind(
    extract_coefs(m1, "public security",  vars = paste0("log(", dependent_var, ")") ),
    extract_coefs(m2, "education",  vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m3, "public transport",  vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m4, "park",  vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m5, "culture",  vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m6, "sports",  vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m7, "child and youth care",  vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m8, "town hall", vars = paste0("log(", dependent_var, ")"))
  )
  write.csv(results, output_name, row.names = FALSE)
  create_plot(
    results = results,
    variable = paste0("log.", dependent_var, "."),
    title = "extensive margins - OLS", 
    output_file = graph_name
  )
  #with fixed effects:  
  data_FE <- pdata.frame(data, index = c("AGS"))
  output_name <- paste0 ("Regression_Results/", output,"_extensive_FE.csv")
  graph_name <- paste0("graphs/",output,"_extensive_FE.pdf")
  full_formula1 <- as.formula (paste ("tpm_ps ~ log(", dependent_var, ") ", control_vars ))
  full_formula2 <- as.formula (paste ("tpm_edu ~ log(", dependent_var, ") ", control_vars))
  full_formula3 <- as.formula (paste ("tpm_pt ~ log(", dependent_var, ") ", control_vars))
  full_formula4 <- as.formula (paste ("tpm_park ~ log(", dependent_var, ") ", control_vars ))
  full_formula5 <- as.formula (paste ("tpm_pec ~ log(", dependent_var, ") ", control_vars ))
  full_formula6 <- as.formula (paste ("tpm_sports ~ log(", dependent_var, ") ", control_vars ))
  full_formula7 <- as.formula (paste ("tpm_cyc ~ log(", dependent_var, ") ", control_vars ))
  full_formula8 <- as.formula (paste ("tpm_town ~ log(", dependent_var, ") ", control_vars ))
  m1 = plm (full_formula1, data = data_FE, model = "within")
  m2 = plm (full_formula2, data = data_FE, model = "within")
  m3 = plm (full_formula3, data = data_FE, model = "within")
  m4 = plm (full_formula4, data = data_FE, model = "within")
  m5 = plm (full_formula5, data = data_FE, model = "within")
  m6 = plm (full_formula6, data = data_FE, model = "within")
  m7 = plm (full_formula7, data = data_FE, model = "within")
  m8 = plm (full_formula8, data = data_FE, model = "within")
  results <- rbind(
    extract_coefs(m1, "public security",  vars = paste0("log(", dependent_var, ")") ),
    extract_coefs(m2, "education",  vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m3, "public transport",  vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m4, "park",  vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m5, "culture",  vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m6, "sports",  vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m7, "child and youth care",  vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m8, "town hall", vars = paste0("log(", dependent_var, ")"))
  )
  write.csv(results, output_name, row.names = FALSE)
  create_plot(
    results = results,
    variable = paste0("log.", dependent_var, "."),
    title = "extensive margins - OLS - FE", 
    output_file = graph_name
  )
  #intensive:
  df1 = data %>%
    filter(tpm_ps == 1)
  df2 = data %>%
    filter(tpm_edu == 1)
  df3 = data %>%
    filter(tpm_pt == 1)
  df4 = data %>%
    filter(tpm_park == 1)
  df5 = data %>%
    filter(tpm_pec == 1)
  df6 = data %>%
    filter(tpm_sports == 1)
  df7 = data %>%
    filter(tpm_cyc == 1)
  df8 = data %>%
    filter(tpm_town == 1)
  #without fixed effects:
  output_name <- paste0 ("Regression_Results/", output,"_intensive.csv")
  graph_name <- paste0("graphs/",output,"_intensive.pdf")
  full_formula1 <- as.formula (paste ("log (ps_count) ~ log(", dependent_var, ")", control_vars, "|0|0|AGS" ))
  full_formula2 <- as.formula (paste ("log (edu_count) ~ log(", dependent_var, ") ", control_vars, "|0|0|AGS" ))
  full_formula3 <- as.formula (paste ("log (pt_count) ~ log(", dependent_var, ") ", control_vars, "|0|0|AGS" ))
  full_formula4 <- as.formula (paste ("log (park_common_count) ~ log(", dependent_var, ") ", control_vars, "|0|0|AGS" ))
  full_formula5 <- as.formula (paste ("log (pec_count) ~ log(", dependent_var, ") ", control_vars, "|0|0|AGS" ))
  full_formula6 <- as.formula (paste ("log (sports_count) ~ log(", dependent_var, ") ", control_vars, "|0|0|AGS" ))
  full_formula7 <- as.formula (paste ("log (cyc_count) ~ log(", dependent_var, ") ", control_vars, "|0|0|AGS" ))
  full_formula8 <- as.formula (paste ("log (town_count) ~ log(", dependent_var, ") ", control_vars, "|0|0|AGS" ))
  m1 = felm (full_formula1, data = df1)
  m2 = felm (full_formula2, data = df2)
  m3 = felm (full_formula3, data = df3)
  m4 = felm (full_formula4, data = df4)
  m5 = felm (full_formula5, data = df5)
  m6 = felm (full_formula6, data = df6)
  m7 = felm (full_formula7, data = df7)
  m8 = felm (full_formula8, data = df8)
  results <- rbind(
    extract_coefs(m1, "public security" , 'Cluster s.e.', vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m2, "education", 'Cluster s.e.', vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m3, "public transport", 'Cluster s.e.', vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m4, "park", 'Cluster s.e.', vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m5, "culture", 'Cluster s.e.', vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m6, "sports", 'Cluster s.e.', vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m7, "child and youth care", 'Cluster s.e.', vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m8, "town hall", 'Cluster s.e.', vars = paste0("log(", dependent_var, ")"))
  )
  write.csv(results, output_name, row.names = FALSE)
  create_plot(
    results = results,
    variable = paste0("log.", dependent_var, "."),
    title = "intensive margins", 
    output_file = graph_name
  )
  #with fixed effects:
  output_name <- paste0 ("Regression_Results/", output,"_intensive_FE.csv")
  graph_name <- paste0("graphs/",output,"_intensive_FE.pdf")
  full_formula1 <- as.formula (paste ("log (ps_count) ~ log(", dependent_var, ")", control_vars, "|AGS|0|0" ))
  full_formula2 <- as.formula (paste ("log (edu_count) ~ log(", dependent_var, ")", control_vars, "|AGS|0|0" ))
  full_formula3 <- as.formula (paste ("log (pt_count) ~ log(", dependent_var, ")", control_vars, "|AGS|0|0" ))
  full_formula4 <- as.formula (paste ("log (park_common_count) ~ log(", dependent_var, ")", control_vars, "|AGS|0|0" ))
  full_formula5 <- as.formula (paste ("log (pec_count) ~ log(", dependent_var, ")", control_vars, "|AGS|0|0" ))
  full_formula6 <- as.formula (paste ("log (sports_count) ~ log(", dependent_var, ")", control_vars, "|AGS|0|0" ))
  full_formula7 <- as.formula (paste ("log (cyc_count) ~ log(", dependent_var, ")", control_vars, "|AGS|0|0" ))
  full_formula8 <- as.formula (paste ("log (town_count) ~ log(", dependent_var, ")", control_vars, "|AGS|0|0" ))
  m1 = felm (full_formula1, data = df1)
  m2 = felm (full_formula2, data = df2)
  m3 = felm (full_formula3, data = df3)
  m4 = felm (full_formula4, data = df4)
  m5 = felm (full_formula5, data = df5)
  m6 = felm (full_formula6, data = df6)
  m7 = felm (full_formula7, data = df7)
  m8 = felm (full_formula8, data = df8)
  results <- rbind(
    extract_coefs(m1, "public security" ,  vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m2, "education",  vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m3, "public transport",  vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m4, "park",  vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m5, "culture",  vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m6, "sports",  vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m7, "child and youth care",  vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m8, "town hall", vars = paste0("log(", dependent_var, ")"))
  )
  write.csv(results, output_name, row.names = FALSE)
  create_plot(
    results = results,
    variable = paste0("log.", dependent_var, "."),
    title = "intensive margins - FE", 
    output_file = graph_name
  )
  #Y scaled
  data = data %>%
    mutate (ps_count_scaled = ps_count / mean (ps_count),
            edu_count_scaled = edu_count / mean (edu_count),
            pt_count_scaled = pt_count / mean (pt_count),
            park_common_count_scaled = park_common_count / mean (park_common_count),
            pec_count_scaled = pec_count / mean (pec_count),
            sports_count_scaled = sports_count / mean (sports_count),
            cyc_count_scaled = cyc_count / mean (cyc_count),
            town_count_scaled = town_count / mean (town_count))
  #without fixed effects:
  output_name <- paste0 ("Regression_Results/", output,"_Ypercapita.csv")
  graph_name <- paste0("graphs/",output,"_Ypercapita.pdf")
  full_formula1 <- as.formula (paste ("ps_count_scaled ~ log(", dependent_var, ")", control_vars, "|0|0|AGS" ))
  full_formula2 <- as.formula (paste ("edu_count_scaled ~ log(", dependent_var, ")", control_vars, "|0|0|AGS" ))
  full_formula3 <- as.formula (paste ("pt_count_scaled ~ log(", dependent_var, ")", control_vars, "|0|0|AGS" ))
  full_formula4 <- as.formula (paste ("park_common_count_scaled ~ log(", dependent_var, ")", control_vars, "|0|0|AGS" ))
  full_formula5 <- as.formula (paste ("pec_count_scaled ~ log(", dependent_var, ")", control_vars, "|0|0|AGS" ))
  full_formula6 <- as.formula (paste ("sports_count_scaled ~ log(", dependent_var, ")", control_vars, "|0|0|AGS" ))
  full_formula7 <- as.formula (paste ("cyc_count_scaled ~ log(", dependent_var, ")", control_vars, "|0|0|AGS" ))
  full_formula8 <- as.formula (paste ("town_count_scaled ~ log(", dependent_var, ")", control_vars, "|0|0|AGS" ))
  m1 = felm (full_formula1, data = data)
  m2 = felm (full_formula2, data = data)
  m3 = felm (full_formula3, data = data)
  m4 = felm (full_formula4, data = data)
  m5 = felm (full_formula5, data = data)
  m6 = felm (full_formula6, data = data)
  m7 = felm (full_formula7, data = data)
  m8 = felm (full_formula8, data = data)
  #extract coefficients
  results <- rbind(
    extract_coefs(m1, "public security" , 'Cluster s.e.', vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m2, "education", 'Cluster s.e.', vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m3, "public transport", 'Cluster s.e.', vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m4, "park", 'Cluster s.e.', vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m5, "culture", 'Cluster s.e.', vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m6, "sports", 'Cluster s.e.', vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m7, "child and youth care", 'Cluster s.e.', vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m8, "town hall", 'Cluster s.e.', vars = paste0("log(", dependent_var, ")"))
  )
  write.csv(results, output_name, row.names = FALSE)
  create_plot(
    results = results,
    variable = paste0("log.", dependent_var, "."),
    title = "Y scaled", 
    output_file = graph_name
  )
  #with fixed effects:
  output_name <- paste0 ("Regression_Results/", output,"_Ypercapita_FE.csv")
  graph_name <- paste0("graphs/",output,"_Ypercapita_FE.pdf")
  full_formula1 <- as.formula (paste ("ps_count_scaled ~ log(", dependent_var, ")", control_vars, "|AGS|0|AGS" ))
  full_formula2 <- as.formula (paste ("edu_count_scaled ~ log(", dependent_var, ")", control_vars, "|AGS|0|AGS" ))
  full_formula3 <- as.formula (paste ("pt_count_scaled ~ log(", dependent_var, ")", control_vars, "|AGS|0|AGS" ))
  full_formula4 <- as.formula (paste ("park_common_count_scaled ~ log(", dependent_var, ")", control_vars, "|AGS|0|AGS" ))
  full_formula5 <- as.formula (paste ("pec_count_scaled ~ log(", dependent_var, ")", control_vars, "|AGS|0|AGS" ))
  full_formula6 <- as.formula (paste ("sports_count_scaled ~ log(", dependent_var, ")", control_vars, "|AGS|0|AGS" ))
  full_formula7 <- as.formula (paste ("cyc_count_scaled ~ log(", dependent_var, ")", control_vars, "|AGS|0|AGS" ))
  full_formula8 <- as.formula (paste ("town_count_scaled ~ log(", dependent_var, ")", control_vars, "|AGS|0|AGS" ))
  m1 = felm (full_formula1, data = data)
  m2 = felm (full_formula2, data = data)
  m3 = felm (full_formula3, data = data)
  m4 = felm (full_formula4, data = data)
  m5 = felm (full_formula5, data = data)
  m6 = felm (full_formula6, data = data)
  m7 = felm (full_formula7, data = data)
  m8 = felm (full_formula8, data = data)
  #extract coefficients
  results <- rbind(
    extract_coefs(m1, "public security" , 'Cluster s.e.', vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m2, "education", 'Cluster s.e.', vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m3, "public transport", 'Cluster s.e.', vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m4, "park", 'Cluster s.e.', vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m5, "culture", 'Cluster s.e.', vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m6, "sports", 'Cluster s.e.', vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m7, "child and youth care", 'Cluster s.e.', vars = paste0("log(", dependent_var, ")")),
    extract_coefs(m8, "town hall", 'Cluster s.e.', vars = paste0("log(", dependent_var, ")"))
  )
  write.csv(results, output_name, row.names = FALSE)
  create_plot(
    results = results,
    variable = paste0("log.", dependent_var, "."),
    title = "Y scaled - FE", 
    output_file = graph_name
  )
}

#USE OF THE FUNCTIONS
winsorization (data = data_ger)
control_vars <- ("+ log (pop) + share_018")
control_vars <- ("")
dependent_var <- ("income_per_capita")
run_regression (filter_column = "ur_binary", filter_value = 2, output = "rural", control_vars = control_vars)
run_regression (data = data_ger, dependent_var = dependent_var, output = "germany_control", control_vars = control_vars) #if you don't need filtering, adjust also the function







