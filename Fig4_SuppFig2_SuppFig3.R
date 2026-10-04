library(dplyr)
library(tidyr)
library(ggplot2)
library(emmeans)
library(purrr)

abcd_data_filtered_no_NA <- read.csv(
  "abcd_data_filtered_no_NA.csv"
)

# -----------------------------
# Helper function
# -----------------------------
make_adjusted_violin_plot <- function(data, dvs, y_label, title_text, tract_labels) {
  
  # keep only complete rows for all DVs + covariates used in the adjusted models
  vars_needed <- c(
    dvs,
    "scrn_asd",
    "interview_age.x",
    "sex.x",
    "nihtbx_totalcomp_uncorrected",
    "dmdtifp1_1183",
    "acs_raked_propensity_score"
  )
  
  df <- data %>%
    dplyr::select(all_of(vars_needed)) %>%
    tidyr::drop_na()
  
  # -----------------------------
  # Raw data for violins
  # -----------------------------
  raw_long <- df %>%
    pivot_longer(
      cols = all_of(dvs),
      names_to = "Tract",
      values_to = "value"
    ) %>%
    mutate(
      Tract = factor(Tract, levels = dvs),
      scrn_asd = factor(scrn_asd,
                        levels = c(0, 1),
                        labels = c("without ASD Dx", "with ASD Dx"))
    ) %>%
    group_by(Tract) %>%
    mutate(
      lower = quantile(value, 0.05, na.rm = TRUE),
      upper = quantile(value, 0.95, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(value >= lower & value <= upper)
  
  # -----------------------------
  # Adjusted means + 95% CI from weighted linear models
  # -----------------------------
  emm_df <- map_dfr(dvs, function(dv) {
    formula_txt <- paste0(
      dv,
      " ~ scrn_asd + interview_age.x + sex.x + nihtbx_totalcomp_uncorrected + dmdtifp1_1183"
    )
    
    fit <- lm(
      as.formula(formula_txt),
      data = df,
      weights = acs_raked_propensity_score
    )
    
    emmeans(fit, ~ scrn_asd) %>%
      as.data.frame() %>%
      mutate(
        Tract = dv,
        scrn_asd = factor(scrn_asd,
                          levels = c(0, 1),
                          labels = c("without ASD Dx", "with ASD Dx"))
      )
  }) %>%
    mutate(
      Tract = factor(Tract, levels = dvs)
    )
  
  # -----------------------------
  # Plot
  # -----------------------------
  ggplot(raw_long, aes(x = scrn_asd, y = value, fill = scrn_asd, color = scrn_asd)) +
    geom_violin(alpha = 0.45, trim = FALSE, color = "darkgrey") +

    # adjusted means and 95% CI overlaid
    geom_linerange(
      data = emm_df,
      aes(x = scrn_asd, ymin = lower.CL, ymax = upper.CL),
      inherit.aes = FALSE,
      width = 0.10,
      linewidth = 0.9,
      color = "black"
    ) +
    geom_point(
      data = emm_df,
      aes(x = scrn_asd, y = emmean),
      inherit.aes = FALSE,
      shape = 23,
      size = 2,
      fill = "white",
      color = "black"
    ) +
    
    facet_wrap(
      ~ Tract,
      scales = "free_y",
      labeller = labeller(Tract = as_labeller(tract_labels))
    ) +
    labs(
      x = "ASD Group",
      y = y_label,
      title = title_text,
      subtitle = "Violin plots show observed distributions; diamonds and error bars show covariate-adjusted estimated means and 95% CI"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "right",
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 20, hjust = 1)
    ) +
    scale_fill_manual(
      values = c("without ASD Dx" = "lightblue",
                 "with ASD Dx" = "lightpink"),
      name = "ASD Group"
    ) +
    scale_color_manual(
      values = c("without ASD Dx" = "blue",
                 "with ASD Dx" = "red"),
      name = "ASD Group"
    )
}

# -----------------------------
# NoS
# -----------------------------
nos_dvs <- c(
  "ICP_Num_Fibers",
  "MCP_Num_Fibers",
  "SCP_Num_Fibers",
  "IP_Num_Fibers",
  "PaT_Num_Fibers"
)

nos_labels <- c(
  "ICP_Num_Fibers" = "ICP",
  "MCP_Num_Fibers" = "MCP",
  "SCP_Num_Fibers" = "SCP",
  "IP_Num_Fibers"  = "IP",
  "PaT_Num_Fibers" = "PF"
)

p_nos <- make_adjusted_violin_plot(
  data = abcd_data_filtered_no_NA,
  dvs = nos_dvs,
  y_label = "NoS",
  title_text = "NoS by Cerebellar Pathway and ASD Group",
  tract_labels = nos_labels
)

p_nos


# -----------------------------
# FA
# -----------------------------
fa_dvs <- c(
  "ICP_FA_tensor1",
  "MCP_FA_tensor1",
  "SCP_FA_tensor1",
  "IP_FA_tensor1",
  "PaT_FA_tensor1"
)

fa_labels <- c(
  "ICP_FA_tensor1" = "ICP",
  "MCP_FA_tensor1" = "MCP",
  "SCP_FA_tensor1" = "SCP",
  "IP_FA_tensor1"  = "IP",
  "PaT_FA_tensor1" = "PF"
)

p_fa <- make_adjusted_violin_plot(
  data = abcd_data_filtered_no_NA,
  dvs = fa_dvs,
  y_label = "FA",
  title_text = "FA by Cerebellar Pathway and ASD Group",
  tract_labels = fa_labels
)

p_fa


# -----------------------------
# MD
# -----------------------------
md_dvs <- c(
  "ICP_MD_tensor1",
  "MCP_MD_tensor1",
  "SCP_MD_tensor1",
  "IP_MD_tensor1",
  "PaT_MD_tensor1"
)

md_labels <- c(
  "ICP_MD_tensor1" = "ICP",
  "MCP_MD_tensor1" = "MCP",
  "SCP_MD_tensor1" = "SCP",
  "IP_MD_tensor1"  = "IP",
  "PaT_MD_tensor1" = "PF"
)

p_md <- make_adjusted_violin_plot(
  data = abcd_data_filtered_no_NA,
  dvs = md_dvs,
  y_label = "MD",
  title_text = "MD by Cerebellar Pathway and ASD Group",
  tract_labels = md_labels
)

p_md