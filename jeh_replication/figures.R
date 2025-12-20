### figures.R: REPLICATION OF MAIN FIGURES 1-3 FROM ``The Effects of Prohibiting Marriage Bars: The Case of U.S. Teachers'' 
### UPDATED: DECEMBER 2025
### AUTHORS: AMY KIM (kimamy@princeton.edu) AND CAROLYN TSAO (carolyntsao@microsoft.com)

#________________________________________________________________________________________________________
# FIGURE 1: DENSITY PLOTS OF THE COUNTY-LEVEL FRACTION OF WHITE TEACHERS WHO ARE MARRIED WOMEN, 1910-1950 ----
#________________________________________________________________________________________________________
# filtering county-level dataset to main sample (balanced panel with at least 10 white teachers in 1930 and 1940)
fig1_samp <- countysumm_wht %>% filter(mainsampwht == 1)

# computing year and treatment group means
fig1_means <- countysumm_wht %>% 
  group_by(YEAR, TREAT) %>%
  summarise(across(c(pct_mw_Teacher, pct_mw_Secretary), function(.x) mean(.x, na.rm=TRUE))) %>%
  mutate(fig_label = ifelse(TREAT == 1, glue("Treated Mean: {round(pct_mw_Teacher,3)}"),
                            glue("Untreated Mean: {round(pct_mw_Teacher,3)}")),
         xpos = ifelse(YEAR == 1950, 0.17, 0.5),
         ypos = ifelse(TREAT == 1, 8, 11),
         TREAT = ifelse(TREAT == 1, "Marriage Bar Removed (Treated)", "Marriage Bar Not Removed (Untreated)"),
  )

# plotting histograms
fig1_plot <- ggplot(fig1_samp %>% mutate(TREAT = ifelse(TREAT == 1, "Marriage Bar Removed (Treated)", "Marriage Bar Not Removed (Untreated)")),
                    aes(x = pct_mw_Teacher, color = factor(TREAT), fill = factor(TREAT))) + 
  geom_histogram(aes(y=after_stat(density)), position = "identity", alpha = 0.3, binwidth = 0.01, linewidth = 0.2) + 
  geom_vline(data = fig1_means, 
             aes(xintercept = pct_mw_Teacher, color = factor(TREAT)), 
             linewidth = 0.6,linetype = "dashed") + 
  geom_text(data = fig1_means, aes(x = xpos, y = ypos, label = fig_label), size = 3) +
  scale_color_manual(values=c(control_col, treat_col)) +
  scale_fill_manual(values=c(control_col, treat_col), guide = "none") +
  facet_wrap(~YEAR) + labs(y = "Density", x = "Married Women Teachers as Fraction of White Teachers in County", color = "") + 
  theme_minimal() + 
  theme(legend.position = "bottom", axis.text = element_text(size = 12), text = element_text(size = 14))

if (verbose){
  print(fig1_plot)
}

if (save){
  ggsave(fig1_plot, filename = "output/fig1_pctmwteacher_dist.png", width = 8, height = 5)
}

#________________________________________________________________________________________________________
# FIGURE 2: EFFECTS OF PROHIBITIONS ON GENDER COMPOSITION OF ALL TEACHERS ----
#________________________________________________________________________________________________________
fig2_plot = did_graph_county(dataset     = neighbor, 
          depvarlist  = c("pct_m_Teacher", "pct_mw_Teacher", "pct_sw_Teacher"), 
          depvarnames = c("Men", "Married Women", "Single Women"),
          colors      = c(men_col, mw_col, sw_col),
          yvar        = "DiD Estimate: Share of All Teachers")
if (verbose){
  print(fig2_plot)
}

if (save){
  ggsave(fig2_plot, filename = "output/fig2_shareteach.png", width = 8, height = 5)
}
#________________________________________________________________________________________________________
# FIGURE 3: EFFECTS OF PROHIBITIONS ON GENDER COMPOSITION OF WHITE/BLACK TEACHERS ----
#________________________________________________________________________________________________________
# WHITE TEACHERS
fig3a_plot_wht = did_graph_county(dataset     = neighbor_wht, 
                                  depvarlist  = c("pct_m_Teacher", "pct_mw_Teacher", "pct_sw_Teacher"), 
                                  depvarnames = c("Men", "Married Women", "Single Women"),
                                  colors      = c(men_col, mw_col, sw_col),
                                  yvar        = "DiD Estimate: Share of White Teachers", 
                                  ymin = -0.07, ymax = 0.07)

if (verbose){
  print(fig3a_plot_wht)
}

if (save){
  ggsave(fig3a_plot_wht, filename = "output/fig3a_shareteach_wht.png", width = 8, height = 5)
}

# BLACK TEACHERS
fig3b_plot_blk = did_graph_county(dataset     = neighbor_blk, 
                                  depvarlist  = c("pct_m_Teacher", "pct_mw_Teacher", "pct_sw_Teacher"), 
                                  depvarnames = c("Men", "Married Women", "Single Women"),
                                  colors      = c(men_col, mw_col, sw_col),
                                  yvar        = "DiD Estimate: Share of Black Teachers", ymin = -0.07, ymax = 0.07)

if (verbose){
  print(fig3b_plot_blk)
}

if (save){
  ggsave(fig3b_plot_blk, filename = "output/fig3b_shareteach_blk.png", width = 8, height = 5)
}

