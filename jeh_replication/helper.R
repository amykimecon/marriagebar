### helper.R: HELPER FUNCTIONS FOR REPLICATION OF FIGURES AND TABLES FROM ``The Effects of Prohibiting Marriage Bars: The Case of U.S. Teachers'' 
### UPDATED: DECEMBER 2025
### AUTHORS: AMY KIM (kimamy@princeton.edu) AND CAROLYN TSAO (carolyntsao@microsoft.com)

#________________________________________________________________________________________________________
# MAIN DIFFERENCE IN DIFFERENCES REGRESSIONS ----
#________________________________________________________________________________________________________
## UTIL FOR CREATING DID DUMMIES
#  Takes in dataset with YEAR and TREAT, and creates indicators for YEAR x TREAT for all years
add_did_dummies <- function(dataset){
  for (yr in unique(dataset$YEAR)){
    dataset[[glue("TREATx{yr}")]] <- ifelse(dataset$YEAR == yr, 1, 0)*ifelse(dataset$TREAT == 1, 1, 0)
    dataset[[glue("Year{yr}")]]   <- ifelse(dataset$YEAR == yr, 1, 0)
  }
  return(dataset)
}

## COUNTY x YEAR REGRESSION
#  Regression using dataframe <dataset> (grouped to county x year) of <depvar> on:
#    dummies for years (omitting 1930 as reference year),
#    interactions between TREAT dummy and year dummies, and
#    county fixed effects. 
#    Std errors clustered at county level.
#    Optional <forgraph> parameter if helper is being used to plot DiD coefficients on graph.

did_data_county <- function(dataset, depvar, forgraph = FALSE){
  # adding interaction terms and filtering to relevant years
  years = c(1910, 1920, 1940, 1950)
  yearomit = 1930
  regdata <- dataset %>% 
    add_did_dummies() %>% 
    filter(YEAR %in% c(years, yearomit))
  
  # vectors of dummies
  interact_vars <- glue("TREATx{years}")
  yearvars <- glue("Year{years}")
  
  # run reg: include year and county FE + interaction terms
  did_reg <- lm(glue("{depvar} ~ {glue_collapse(yearvars, sep = '+')} + factor(FIPS) + 
                       {glue_collapse(interact_vars, sep = '+')}"), 
                data = regdata)

  # clustered standard errors (hc1 equiv to ,robust in stata)
  vcov = vcovCL(did_reg, cluster = regdata[["FIPS"]], type = "HC1")

  # if forgraph, make clean dataframe for graphing
  if (forgraph){
    # make dataframe for output -- y is coef, var is estimated variance using clustered standard errors
    effects <- data.frame(y      = c(sapply(interact_vars, function (.x) did_reg$coefficients[[.x]]), 0),
                          depvar = depvar,
                          year   = c(years, yearomit),
                          var    = c(sapply(interact_vars, function(.x) as.numeric(diag(vcov)[[.x]])), 0)) %>%
      mutate(y_ub = y + 1.96*sqrt(var),
             y_lb = y - 1.96*sqrt(var))
    return(effects)
  }
  
  # otherwise return table of estimates
  return(list(did_reg, vcov))
}

## GRAPHING DID COEFFICIENTS
did_graph_county <- function(dataset, depvarlist, depvarnames, colors, yvar, ymax = NA, ymin = NA){
  # check that varlist and namelist passed to function are same length
  nvars = length(depvarlist)
  if (nvars != length(depvarnames)){
    print("Error: depvarlist length diff from depvarnames length")
    return(NA)
  }
  
  # create separate reg tables for each depvar, then bind together
  did_datasets <- list()
  for (i in seq(1,nvars)){
    did_data_temp <- did_data_county(dataset, depvarlist[[i]], forgraph = TRUE) %>%
      mutate(group      = depvarnames[[i]], 
             year_graph = year - 1 + (i-1)*(2/(nvars - 1))) # shifting over so dots don't overlap
    did_datasets[[i]] <- did_data_temp
  }
  did_data <- bind_rows(did_datasets)
  
  # graphing data
  graph_out <- ggplot(did_data, aes(x = year_graph, 
                                    y = y, 
                                    color = factor(group, levels = depvarnames), 
                                    shape = factor(group, levels = depvarnames))) + 
    geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
    geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.5, alpha = 0.05)) +
    scale_color_manual(values=colors) +
    annotate("rect", xmin = 1933, xmax = 1938, ymin = -Inf, ymax = Inf, alpha = 0.2) +
    geom_point(size = 4) + labs(x = "Year", y = yvar, color = "", shape = "") + theme_minimal() + 
    theme(legend.position = "bottom") + guides(linewidth = "none", alpha = "none") 
  
  # adjust ymin/ymax 
  if (!is.na(ymax) | !is.na(ymin)){ # if ymin/ymax bounds are specified, and ...
    if (ymax > max(did_data$y_ub) & ymin < min(did_data$y_lb)){ # the observed ymin/ymax are within said bounds
      graph_out <- graph_out + 
        ylim(ymin,ymax) + 
        geom_text(aes(x = 1935.5, y = ymin + (ymax-ymin)/10, 
                      label = "Marriage Bars \n Removed"), color = "#656565")   
    }
    else{ # the observed ymin/ymax are outside of said bounds
      print("Warning: ymin/ymax out of bounds") ##! changed from "Error" just so that it doesn't seem like a calc was wrong!
      graph_out <- graph_out + geom_text(aes(x = 1935.5, 
                                             y = min(y_lb) + (max(y_ub) - min(y_lb))/10, 
                                             label = "Marriage Bars \n Removed"), color = "#656565") 
    }
  }
  else{ # no ymin/ymax bound was specified
    graph_out <- graph_out + geom_text(aes(x = 1935.5, 
                                           y = min(y_lb) + (max(y_ub) - min(y_lb))/10, 
                                           label = "Marriage Bars \n Removed"), color = "#656565") 
  }
  
  return(graph_out)
}

## INDIVIDUAL X YEAR REGRESSION
#  Regression using linked dataframe <dataset> (at individual x year level) of <depvar> on:
#    dummies for years (omitting 1930 as reference year),
#    interactions between TREAT dummy and year dummies, 
#    county fixed effects,
#    control for age.
#    Std errors clustered at county level.

did_data_indiv_linked <- function(dataset, depvar){
  # adding interaction terms and filtering to relevant years
  years = c(1920, 1940)
  yearomit = 1930
  regdata <- dataset %>% 
    add_did_dummies() %>% 
    filter(YEAR %in% c(years, yearomit))
  
  # vectors of dummies
  interact_vars <- glue("TREATx{years}")
  yearvars <- glue("Year{years}")
  
  # run reg: include year and county FE + interaction terms
  did_reg <- feols(as.formula(glue("{depvar} ~ {glue_collapse(yearvars, sep = '+')} + 
                       {glue_collapse(interact_vars, sep = '+')} | STATEICP^COUNTYICP + AGE")),
                   data = regdata)
  
  # return model
  return(did_reg)
}
