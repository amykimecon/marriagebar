### tables.R: REPLICATION OF MAIN TABLES 1-5 FROM ``The Effects of Prohibiting Marriage Bars: The Case of U.S. Teachers''
### UPDATED: DECEMBER 2025
### AUTHORS: AMY KIM (kimamy@princeton.edu) AND CAROLYN TSAO (carolyntsao@microsoft.com)

cat("\n================================================================\n")
cat(" TABLES\n")
cat("================================================================\n")

#________________________________________________________________________________________________________
# TABLE 1: SUMMARY OF KEY COUNTY-LEVEL STATISTICS BY COUNTY GROUP IN 1930 ----
#________________________________________________________________________________________________________
cat("\nTable 1: Summary statistics by county group (1930)...\n")
# Column 1 Data: All Counties
tab1_grp1 <- countysumm %>% mutate(summgroup = "All")

# Column 2 Data: Southern Counties
tab1_grp2 <- countysumm %>% filter(SOUTH == 1) %>% mutate(summgroup = "South")

# Columns 3 and 4 Data: Treated and Control (Neighboring Southern) Counties
tab1_grp3and4 <- countysumm %>% filter(neighbor_samp == 1) %>% mutate(summgroup = ifelse(TREAT == 1, "Treated", "Neighb. Sth."))

# Combining data and normalizing/creating relevant variables
tab1_allgrps <- bind_rows(tab1_grp1, tab1_grp2, tab1_grp3and4) %>%
  mutate(POP_THOUS            = POP/1000, 
         SCHOOLPOP_THOUS      = SCHOOLPOP/1000, 
         STUDENT_PER_TEACH    = ifelse(SCHOOLPOP != 0, SCHOOLPOP/num_Teacher, NA),
         summgroup            = factor(summgroup, levels = c("All", "South", "Treated", "Neighb. Sth.")))

# mapping variable names to labels
tab1_varnames = c("POP_THOUS","SCHOOLPOP_THOUS", "URBAN", 
                  "LFP_MW", "LFP_WMW", "NCHILD", "UNEMP_RATE",
                  "STUDENT_PER_TEACH","pct_m_Teacher", 
                  "pct_sw_Teacher", "pct_mw_Teacher")
tab1_varlabs = c("Population (Thous.)", "School-Age Pop. (Thous.)", "Share Urban", 
                 "LFP of Married Women", "LFP of White Married Women", "Num. Children of Marr. Wom.", 
                 "Unemployment Rate",
                 "Students/Teachers", "Share Men", 
                 "Share Single Women", "Share Married Women")

# summary statistics by group
tab1_data_raw <- tab1_allgrps %>%
  filter(YEAR == 1930) %>%
  group_by(summgroup) %>% 
  summarize(OBS = n(),
            across(all_of(tab1_varnames), .fns = c(~mean(.x, na.rm=TRUE),
                                                   ~sd(.x, na.rm=TRUE)/sqrt(OBS))))

tab1_data <- as.data.frame(t(tab1_data_raw))

# computing p-values (treated and control)
tab1_pvals = list()
tab1_grpa <- tab1_allgrps %>% filter(summgroup == "Treated" & YEAR == 1930)
tab1_grpb <- tab1_allgrps %>% filter(summgroup == "Neighb. Sth." & YEAR == 1930)
for (i in 1:length(tab1_varnames)){
  var = tab1_varnames[i]
  tab1_pvals[[i]] <- t.test(tab1_grpa[[var]], tab1_grpb[[var]])$p.value
}

# build latex lines into character vector
names <- tab1_data[1,]
tab1_lines <- c(
  "\\begin{tabular}{lC{2cm}C{2cm}C{2.3cm}C{2cm}C{2cm}}",
  "\\toprule",
  "&",
  glue("{names[1]} & {names[2]} & {names[3]} & {names[4]} & p-value \\\\"),
  "& (1) & (2) & (3) & (4) & (5) \\\\",
  "\\midrule"
)

for (i in 1:length(tab1_varlabs)){
  means <- round(as.numeric(tab1_data[2*i+1,]), 3)
  sds   <- paste0("(", round(as.numeric(tab1_data[2*i+2,]), 3), ")")
  if (tab1_varlabs[i] == "Population (Thous.)"){
    tab1_lines <- c(tab1_lines, "\\multicolumn{6}{l}{\\underline{\\textbf{Panel A: General County Statistics}}}\\\\ [1em]")
  }
  if (tab1_varlabs[i] == "Students/Teachers"){
    tab1_lines <- c(tab1_lines, "[1em] \\multicolumn{6}{l}{\\underline{\\textbf{Panel B: County Statistics on White Teachers}}}\\\\ [1em]")
  }
  tab1_lines <- c(tab1_lines,
                  paste(tab1_varlabs[i], "&", glue("{means[1]} & {means[2]} & {means[3]} & {means[4]} & {round(tab1_pvals[[i]],3)} \\\\")),
                  "&",
                  glue("{sds[1]} & {sds[2]} & {sds[3]} & {sds[4]} & \\\\")
  )
}

obs <- round(as.numeric(tab1_data[2,]), 0)
tab1_lines <- c(tab1_lines,
                "\\midrule",
                paste("$N$ (Counties)", "&", glue("{obs[1]} & {obs[2]} & {obs[3]} & {obs[4]} & \\\\")),
                "\\bottomrule",
                "\\end{tabular}"
)

if (save)    writeLines(tab1_lines, "output/tab1_summstats.tex")
if (verbose) cat(paste(tab1_lines, collapse = "\n"), "\n")
cat("  -> tab1_summstats.tex\n")

#________________________________________________________________________________________________________
# TABLE 2: ESTIMATED EFFECTS OF THE PROHIBITION OF MARRIAGE BARS ON MARRIED WOMEN TEACHERS ----
#________________________________________________________________________________________________________
cat("\nTable 2: DiD effects on married women teacher share (all, white, Black samples)...\n")
# prepare table inputs
tab2_models         <- list()
tab2_ses            <- list()
tab2_sharereg_means <- c() #dep var mean for treated in 1930

i = 1
for (df in list(neighbor, neighbor_wht, neighbor_blk)){
  for (coefname in c("pct_mw_Teacher", "pct_Teacher_mw_100")){
    tab2_out_did_temp        <- did_data_county(df, coefname)
    tab2_models[[i]]    <- tab2_out_did_temp[[1]]
    tab2_ses[[i]]       <- sqrt(diag(tab2_out_did_temp[[2]]))
    tab2_sharereg_means <- c(tab2_sharereg_means, mean(filter(df, YEAR == 1930 & TREAT == 1)[[coefname]]))
    i = i + 1
  }
}

# generate table
tab2_latex <- capture.output(
  stargazer(tab2_models, se = tab2_ses, keep = c("TREATx1940", "TREATx1950"),
            out = NULL,
            float = FALSE,
            keep.stat = c('n', 'adj.rsq'),
            dep.var.caption = "Dependent Variable:",
            dep.var.labels.include = FALSE,
            column.labels = c("All teachers", "White teachers only", "Black teachers only"),
            column.separate = c(2, 2, 2),
            covariate.labels = c("Treated $\\times$ 1940 ($\\gamma_{1940}^{DD}$)",
                                 "Treated $\\times$ 1950 ($\\gamma_{1950}^{DD}$)"),
            add.lines = list(c("Dep. Var. 1930 Treated Mean", formatC(tab2_sharereg_means))),
            table.layout = "=lc#-t-as=")
)

dep_var_row <- " & Share Teach Mar. Wom. & MW Teach per 100 MW & Share Teach Mar. Wom. & MW Teach per 100 MW & Share Teach Mar. Wom. & MW Teach per 100 MW \\\\"
col_label_idx <- grep("All teachers", tab2_latex)
tab2_latex <- c(
  tab2_latex[1:col_label_idx],
  dep_var_row,
  tab2_latex[(col_label_idx + 1):length(tab2_latex)]
)

if (save) {
  writeLines(tab2_latex, "output/tab2_mwregs.tex")
}
if (verbose){
  print(tab2_latex)
}
cat("  -> tab2_mwregs.tex\n")

#________________________________________________________________________________________________________
# TABLE 3: ESTIMATED EFFECTS OF THE PROHIBITIONS ON WOMEN'S PROPENSITY TO GET MARRIED AND WORK ----
#________________________________________________________________________________________________________
cat("\nTable 3: DiD effects on marriage and work outcomes (3 panels)...\n")
tab3_datalist = list(link1_swt, link2_mwnilf, link3_swnilf)
tab3_datalabs = list("panel1_swt", "panel2_mwnilf", "panel3_swnilf")
tab3_datanames = c("Sample 1: Women who were unmarried and teaching in t-10", 
                   "Sample 2: Women who were married and not in the labor force in t-10", 
                   "Sample 3: Women who were unmarried and not in the labor force in t-10")

tab3_coefs = c("married_link", "mwt_link", "mwnt_link", "mwnilf_link")
tab3_coefnames_dict = c(married_link = "P[Married in t]",
                        mwt_link = "P[Married Teacher in t]", 
                        mwnt_link = "P[Married Non-Teacher in LF in t]", 
                        mwnilf_link = "P[Married Not in LF in t]",
                        TREATx1940 = "Treated $\\times$ 1940 ($\\gamma_{1940}^{DD}$)")

for (dataind in 1:3){
  cat(glue("  Panel {dataind}: {tab3_datanames[[dataind]]}...\n"))
  tab3_data_temp <- tab3_datalist[[dataind]] %>% filter(neighbor_samp == 1 & RACE == 1)

  tab3_models = list()
  tab3_means = list()
  i = 1
  for (coefind in 1:4){
    tab3_models[[i]] <- did_data_indiv_linked(tab3_data_temp, tab3_coefs[coefind])
    tab3_means[[i]] <- mean(filter(tab3_data_temp, YEAR == 1930 & TREAT == 1)[[tab3_coefs[coefind]]], na.rm=TRUE)
    i = i + 1
  }

  if (save){
    tab3_filename = glue("output/tab3_{tab3_datalabs[[dataind]]}.tex")
  }else{
    tab3_filename = NULL
  }

  tab3_out_temp <- esttex(tab3_models, keep = c("%TREATx1940"),
                          fitstat = c("n","ar2"),
                          extralines = list("Dep. Var. 1930 Treated Mean" = unlist(tab3_means)),
                          file = tab3_filename,
                          title = tab3_datanames[[dataind]],
                          dict=tab3_coefnames_dict)

  if (verbose){
    print(tab3_out_temp)
  }
  cat(glue("  -> tab3_{tab3_datalabs[[dataind]]}.tex\n"))
}


#________________________________________________________________________________________________________
# TABLE 4: ESTIMATED EFFECTS OF THE PROHIBITIONS ON THE GENDER COMPOSITION OF TEACHERS ----
#________________________________________________________________________________________________________
cat("\nTable 4: DiD effects on gender composition of white teachers...\n")
tab4_models         <- list()
tab4_ses            <- list()
tab4_sharereg_means <- c()

i = 1
for (coefname in c("pct_mw_Teacher","pct_m_Teacher","pct_sw_Teacher", "num_Teacher")){
  tab4_out_did_temp   <- did_data_county(neighbor_wht, coefname)
  tab4_models[[i]]    <- tab4_out_did_temp[[1]]
  tab4_ses[[i]]       <- sqrt(diag(tab4_out_did_temp[[2]]))
  tab4_sharereg_means <- c(tab4_sharereg_means, mean(filter(neighbor_wht, YEAR == 1930 & TREAT == 1)[[coefname]]))
  i = i + 1
}

if (save){
  tab4_outpath = "output/tab4_teachregs.tex"
}else{
  tab4_outpath = NULL
}

stargazer(tab4_models, se=tab4_ses, keep = c("TREATx1940", "TREATx1950"),
          out = tab4_outpath,
          float = FALSE,
          keep.stat = c('n','adj.rsq'),
          dep.var.caption = "Dependent Variable:",
          dep.var.labels.include = FALSE,
          column.labels = c("\\% Teach Mar. Wom.", "\\% Teach Men",
                            "\\% Teach Unmar. Wom.","\\# Teachers"),
          column.separate = c(1,1,1,1,1),
          covariate.labels = c("Treated $\\times$ 1940 ($\\gamma_{1940}^{DD}$)",
                               "Treated $\\times$ 1950 ($\\gamma_{1950}^{DD}$)"),
          add.lines = list(c("Dep. Var. 1930 Treated Mean", formatC(tab4_sharereg_means))),
          table.layout = "=lc#-t-as=")
cat("  -> tab4_teachregs.tex\n")

#__________________________________________________________________________________________________________________________
# TABLE 5: ESTIMATED EFFECTS OF THE PROHIBITIONS ON UNMARRIED WOMEN TEACHERS' PROPENSITY TO REMAIN UNMARRIED AND WORK ----
#__________________________________________________________________________________________________________________________
cat("\nTable 5: DiD effects on outcomes for unmarried women teachers...\n")
tab5_coefs = c("unmarried_link", "swt_link", "swnt_link", "swnilf_link")
tab5_coefnames_dict = c(unmarried_link = "P[Unmarried in t]",
                        swt_link = "P[Unmarried Teacher in t]", 
                        swnt_link = "P[Unmarried Non-Teacher in LF in t]", 
                        swnilf_link = "P[Unmarried Not in LF in t]",
                        TREATx1940 = "Treated $\\times$ 1940 ($\\gamma_{1940}^{DD}$)")

tab5_data <- link1_swt %>% filter(neighbor_samp == 1 & RACE == 1) 

tab5_models = list()
tab5_means = list()
i = 1
for (coefind in 1:4){
  tab5_models[[i]] <- did_data_indiv_linked(tab5_data, tab5_coefs[coefind])
  tab5_means[[i]] <- mean(filter(tab5_data, YEAR == 1930 & TREAT == 1)[[tab5_coefs[coefind]]], na.rm=TRUE)
  i = i + 1 
}

if (save){
  tab5_filename = glue("output/tab5_swt.tex")
}else{
  tab5_filename = NULL
}

tab5_out <- esttex(tab5_models, keep = c("%TREATx1940"), 
                   fitstat = c("n","ar2"), 
                   extralines = list("Dep. Var. 1930 Treated Mean" = unlist(tab5_means)),
                   file = tab5_filename,
                   title = "Sample 1: Women who were unmarried and teaching in t-10",
                   dict = tab5_coefnames_dict)

if (verbose){
  print(tab5_out)
}
cat("  -> tab5_swt.tex\n")

cat("\n----------------------------------------------------------------\n")
cat(" Tables complete.\n")
cat("----------------------------------------------------------------\n")