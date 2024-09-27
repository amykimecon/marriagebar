## SYNTHDID
data('california_prop99')
setup = panel.matrices(california_prop99)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
plot(tau.hat)


## TESTING CLUSTERED STANDARD ERRORS ----
years <- c(1910, 1920, 1940, 1950)
yearomit = 1930
controls = ""
depvar = "pct_mw_Teacher"
cl = "STATEICP"
dataset <- neighbor

regdata <- dataset %>% 
  add_did_dummies() %>% 
  filter(YEAR %in% c(years, yearomit))

if(!("weight" %in% names(dataset))){
  regdata$weight <- 1 
}

interact_vars <- glue("TREATx{years}")
yearvars <- glue("Year{years}")
# run reg: include year and county FE + interaction terms + any controls
did_reg <- lm(glue("{depvar} ~ {glue_collapse(yearvars, sep = '+')} + 
                       factor(FIPS) + {glue_collapse(interact_vars, sep = '+')} {controls}"), 
              data = regdata, weights = weight)
vcov = vcovCL(did_reg, cluster = regdata[[cl]], type = "HC1")

effects2 <- data.frame(y      = c(sapply(interact_vars, function (.x) did_reg$coefficients[[.x]]), 0),
                      depvar = depvar,
                      year   = c(years, yearomit),
                      var    = c(sapply(interact_vars, function(.x) as.numeric(diag(vcov)[[.x]])), 0),
                      native_se = c(sapply(interact_vars, function(.x) summary(did_reg)$coefficients[.x,2]), 0)) %>%
  mutate(y_ub = y + 1.96*sqrt(var),
         y_lb = y - 1.96*sqrt(var),
         se = sqrt(var))








did_graph_data <- function(dataset, depvar, controls = "", 
                           years = c(1910, 1920, 1940, 1950), yearomit = 1930, 
                           verbose = FALSE, table = FALSE, septreat = FALSE){
  # modifying dataset (adding interaction terms, FIPS vars for clustering, 
  #   filter to only include relevant years, setting weights to 1 if they don't exist)
  regdata <- dataset %>% 
    add_did_dummies() %>% 
    filter(YEAR %in% c(years, yearomit)) %>% 
    mutate(cluster = as.character(STATEICP))
  
  # if weight not already a variable, just weight by vector of 1s
  if(!("weight" %in% names(dataset))){
    regdata$weight <- 1 
  }
  
  if (septreat){ # if septreat==TRUE, run regs separately by treatment group
    yearvars     <- glue("Year{years}")
    # for control group
    did_reg_ctrl <- lm(glue("{depvar} ~ {glue_collapse(yearvars, sep = '+')} + factor(FIPS) + cluster {controls}"),  
                       data = regdata %>% filter(TREAT == 0), weights = weight)
    vcov_ctrl    = vcovCL(did_reg_ctrl, type = "HC1")
    # for treated group
    did_reg_treat <- lm(glue("{depvar} ~ {glue_collapse(yearvars, sep = '+')} + factor(FIPS) + cluster {controls}"), 
                        data = regdata %>% filter(TREAT == 1), weights = weight)
    vcov_treat    = vcovCL(did_reg_treat, type = "HC1")
    # make dataframe for output to return 
    if (table){ # for stargazer 
      return(list(did_reg_ctrl, vcov_ctrl, did_reg_treat, vcov_treat))
    }
    else { # standalone table of point estimates/CIs
      # include omitted year (1930) for plotting, with point estimate/CIs = 0
      effects <- data.frame(y      = c(sapply(yearvars, function (.x) did_reg_ctrl$coefficients[[.x]]), 0), 
                            depvar = depvar,
                            year   = c(years, yearomit),
                            var    = c(sapply(yearvars, function(.x) as.numeric(diag(vcov_ctrl)[[.x]])), 0),
                            treat  = "Control") %>%
        rbind(data.frame(y      = c(sapply(yearvars, function (.x) did_reg_treat$coefficients[[.x]]), 0),
                         depvar = depvar,
                         year   = c(years, yearomit),
                         var    = c(sapply(yearvars, function(.x) as.numeric(diag(vcov_treat)[[.x]])), 0),
                         treat  = "Treated")) %>%
        mutate(y_ub = y + 1.96*sqrt(var),
               y_lb = y - 1.96*sqrt(var))
      return(effects)
    }
  } 
  else { # otherwise, if septreat==FALSE, run standard DiD regression
    # vector of interaction terms
    interact_vars <- glue("TREATx{years}")
    yearvars <- glue("Year{years}")
    # run reg: include year and county FE + interaction terms + any controls
    did_reg <- lm(glue("{depvar} ~ {glue_collapse(yearvars, sep = '+')} + factor(FIPS) + 
                       cluster + {glue_collapse(interact_vars, sep = '+')} {controls}"), 
                  data = regdata, weights = weight)
    if (verbose){
      print(summary(did_reg))
    }
    # clustered standard errors (hc1 equiv to ,robust in stata)
    vcov = vcovCL(did_reg, type = "HC1")
    # return table of estimates as output
    if (table){
      return(list(did_reg, vcov))
    }
    else {
      # make dataframe for output -- y is coef, var is estimated variance using clustered standard errors
      effects <- data.frame(y      = c(sapply(interact_vars, function (.x) did_reg$coefficients[[.x]]), 0),
                            depvar = depvar,
                            year   = c(years, yearomit),
                            var    = c(sapply(interact_vars, function(.x) as.numeric(diag(vcov)[[.x]])), 0)) %>%
        mutate(y_ub = y + 1.96*sqrt(var),
               y_lb = y - 1.96*sqrt(var))
      return(effects)
    }
  } # end ifelse
} #!#! CHECKED

# Creating dynamic DiD graph
# takes in all parameters of did_graph_data (with list of dep vars), 
# as well as vector of labels for dep vars and labels for graph
#   and toggles for slides (default is for paper) 
#   and steps (i.e. saving versions of the graph with points gradually revealed -- default is no)
#   and pointspan, i.e. total width of all dots for a given year, default is 2
did_graph <- function(dataset, depvarlist, depvarnames, colors, controls = "", pointtypes = NA,
                      years = c(1910, 1920, 1940, 1950), yearomit = 1930, 
                      verbose = FALSE, yvar = "Coef on Treat X Year",
                      ymax = NA, ymin = NA, 
                      slides = FALSE, steps = FALSE, pointspan = 2, 
                      septreat = FALSE, filename = NA){
  # check that varlist and namelist passed to function are same length
  nvars = length(depvarlist)
  if (nvars != length(depvarnames)){
    print("Error: depvarlist length diff from depvarnames length")
    return(NA)
  }
  
  # make tables ----
  # make table of DiD regression outputs (in format for graphing) from all dependent variables
  if (nvars == 1){ # if only one depvar is specified
    did_data_temp <- did_graph_data(dataset, depvarlist[[1]], controls, 
                                    years, yearomit, 
                                    verbose, septreat, table = FALSE) 
    did_data      <- did_data_temp %>%
      mutate(group      = depvarnames[[1]], 
             year_graph = did_data_temp$year)
  }
  else{ # if more than one depvar is specified
    if (septreat){
      print("Error: can only use septreat for single variable")
      return(NA)
    }
    # create separate reg tables for each depvar, then bind together
    did_datasets <- list()
    for (i in seq(1,nvars)){
      did_data_temp <- did_graph_data(dataset, depvarlist[[i]], controls, 
                                      years, yearomit, verbose) %>%
        mutate(group      = depvarnames[[i]], 
               year_graph = year - pointspan/2 + (i-1)*(pointspan/(nvars - 1))) # shifting over so dots don't overlap
      did_datasets[[i]] <- did_data_temp
    }
    did_data <- bind_rows(did_datasets)
  }
  if(verbose){
    print(did_data)
  } # end make tables
  
  # make graphs ----
  if (septreat){ # if only one dep var
    graph_out <- ggplot(did_data, aes(x = year_graph, 
                                      y = y, 
                                      color = treat, ##! Where is treat generated?
                                      shape = treat)) + 
      geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
      geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.5, alpha = 0.05)) +
      annotate("rect", xmin = 1933, xmax = 1938, ymin = -Inf, ymax = Inf, alpha = 0.2) +
      geom_point(size = 4) + labs(x = "Year", y = yvar, color = "", shape = "") + theme_minimal() + 
      theme(legend.position = "bottom") + guides(linewidth = "none", alpha = "none")
    
    return(graph_out)
  } else { # if more than one dep var
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
    
    
    if(!is.na(pointtypes)){
      graph_out <- graph_out + scale_shape_manual(values = pointtypes)
    }
    
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
    
    # save graphs
    if (!is.na(filename) & !slides){ #saving graph in folder for paper figs
      ggsave(glue("{outfigs}/paper/{filename}.png"), graph_out, width = 8, height = 5)
    }
    if (!is.na(filename) & slides){ #changing text size for slides and saving in folder for slide figs
      ggsave(glue("{outfigs}/slides/{filename}.png"), graph_out + 
               theme(text = element_text(size = 18), 
                     axis.text = element_text(size = 14)), width = 8, height = 5)
    }
    return(graph_out)
  } # end make graphs
  
} #!#! CHECKED
