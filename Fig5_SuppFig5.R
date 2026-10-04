
library(lm.beta)


abcd_data_filtered_no_NA <- read.csv("abcd_data_filtered_no_NA.csv")

abcd_data_filtered_no_NA$scrn_asd <- as.factor(abcd_data_filtered_no_NA$scrn_asd)

#' Plot interaction with standardized betas (lm.beta) and optional points
#'
#' @param model an lm object (weights ok)
#' @param data the data.frame used to fit the model (or compatible newdata)
#' @param pred character, name of the continuous predictor (e.g., "SCP_Num_Fibers")
#' @param modx character, name of the binary moderator (0/1 or 2-level factor)
#' @param modx_labels length-2 character vector of labels for levels 0 and 1
#' @param point_mode "fitted", "residuals", or "none"
#' @param colors named vector for line colors (level0, level1)
#' @param fills  named vector for ribbon fills (level0, level1)
#' @param point_alpha numeric alpha for points
#' @param point_size numeric size for points
#' @param jitter_height numeric vertical jitter for points
#' @param title main plot title
#' @param xlab,ylab axis labels
#' @param y_limits optional numeric vector of length 2 to fix y-axis limits
#' @return a ggplot object
#' @examples
#' plot_interaction_basic_manual(model, data, "SCP_Num_Fibers", "scrn_asd", y_limits = c(0, 200))
plot_interaction_basic_manual <- function(
    model, data, pred, modx,
    modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
    title = "Interaction Plot",
    xlab = NULL, ylab = NULL,
    n_points = 100,
    point_alpha = 0.25, point_size = 1.3, jitter_height = 0.25,
    y_limits = NULL
){
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  library(ggplot2)
  
  stopifnot(inherits(model, "lm"))
  if (!all(c(pred, modx) %in% names(data))) {
    stop("`pred` and/or `modx` not found in `data` (use raw column names).")
  }
  
  # Labels
  outcome <- all.vars(formula(model))[1]
  if (is.null(xlab)) xlab <- pred
  if (is.null(ylab)) ylab <- paste("Predicted", outcome)
  
  # How modx appears in the model frame (factor vs numeric)
  mf <- model.frame(model)
  modx_in_model <- if (modx %in% names(mf)) mf[[modx]] else data[[modx]]
  
  # Helper to create moderator column matching the model’s type/levels
  mk_modx_for_model <- function(z01){
    if (is.factor(modx_in_model)) {
      levs <- levels(modx_in_model)
      if (length(levs) != 2) stop("`modx` must have exactly two levels in the model.")
      factor(ifelse(z01 == 0, levs[1], levs[2]), levels = levs)
    } else {
      as.numeric(z01)
    }
  }
  
  # Typical covariate values from rows actually used in the fit
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  mf_used <- model.frame(model)
  w <- model$weights %||% rep(1, nrow(mf_used))
  w <- as.numeric(w)
  
  typical <- lapply(mf_used, function(col){
    if (is.numeric(col)) stats::weighted.mean(col, w, na.rm = TRUE)
    else if (is.factor(col)) levels(col)[1]
    else col[1]
  })
  typical <- as.data.frame(typical, stringsAsFactors = FALSE)
  if (outcome %in% names(typical)) typical[[outcome]] <- NULL
  
  # Prediction grid over pred × modx (0/1)
  x_seq <- seq(min(data[[pred]], na.rm = TRUE),
               max(data[[pred]], na.rm = TRUE),
               length.out = n_points)
  
  grid0 <- typical[rep(1, n_points), , drop = FALSE]
  grid1 <- typical[rep(1, n_points), , drop = FALSE]
  if (!pred %in% names(grid0)) { grid0[[pred]] <- NA_real_; grid1[[pred]] <- NA_real_ }
  if (!modx %in% names(grid0)) { grid0[[modx]] <- NA;       grid1[[modx]] <- NA }
  grid0[[pred]] <- x_seq; grid1[[pred]] <- x_seq
  grid0[[modx]] <- mk_modx_for_model(0)
  grid1[[modx]] <- mk_modx_for_model(1)
  
  pr0 <- predict(model, newdata = grid0, se.fit = TRUE)
  pr1 <- predict(model, newdata = grid1, se.fit = TRUE)
  tcrit <- qt(0.975, df.residual(model))
  
  ef0 <- data.frame(
    x = x_seq, fit = as.numeric(pr0$fit), se = as.numeric(pr0$se.fit),
    lower = as.numeric(pr0$fit) - tcrit * as.numeric(pr0$se.fit),
    upper = as.numeric(pr0$fit) + tcrit * as.numeric(pr0$se.fit),
    group_lab = modx_labels[1]
  )
  ef1 <- data.frame(
    x = x_seq, fit = as.numeric(pr1$fit), se = as.numeric(pr1$se.fit),
    lower = as.numeric(pr1$fit) - tcrit * as.numeric(pr1$se.fit),
    upper = as.numeric(pr1$fit) + tcrit * as.numeric(pr1$se.fit),
    group_lab = modx_labels[2]
  )
  eff_df <- rbind(ef0, ef1)
  eff_df$group_lab <- factor(eff_df$group_lab, levels = modx_labels)
  
  # Fitted values for points (aligned to full data; NA where missing)
  fitted_full <- predict(model, newdata = data, type = "response", na.action = na.pass)
  plot_df <- transform(
    data,
    .fitted = fitted_full,
    .group_lab = factor(
      ifelse(as.character(data[[modx]]) %in% c("1", 1), modx_labels[2], modx_labels[1]),
      levels = modx_labels
    )
  )
  
  # Color scheme
  col_map  <- setNames(c("blue",      "red"),       modx_labels)
  fill_map <- setNames(c("lightblue", "lightpink"), modx_labels)
  
  # Plot
  p <- ggplot(eff_df, aes(x = x, y = fit, color = group_lab, fill = group_lab)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25, linewidth = 0, show.legend = FALSE) +
    geom_line(linewidth = 1) +
    geom_point(
      data = plot_df,
      inherit.aes = FALSE,
      aes(x = .data[[pred]], y = .fitted, color = .group_lab),
      alpha = point_alpha, size = point_size,
      position = position_jitter(width = 0, height = jitter_height),
      na.rm = TRUE
    ) +
    scale_color_manual(values = col_map, name = "Group") +
    scale_fill_manual(values  = fill_map, name = "Group") +
    labs(
      title = title,
      x = xlab,
      y = ylab
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      plot.title = element_text(face = "bold")
    )
  
  # Apply y-axis limits if provided
  if (!is.null(y_limits)) {
    p <- p + coord_cartesian(ylim = y_limits)
  }
  
  return(p)
}


# Compute 95% CI for the STANDARDIZED interaction effect (Δβ*) in a two-way interaction
# - Works with weighted or unweighted lm() models
# - method = "delta" (fast, analytical) or "bootstrap" (robust, recommended)
# - For bootstrap, pass the original data used to fit the model
get_standardized_interaction_CI <- function(
    model,
    pred,                   # continuous predictor name, e.g. "SCP_Num_Fibers"
    modx,                   # binary moderator name, e.g. "scrn_asd"
    data = NULL,            # required for method = "bootstrap"
    method = c("bootstrap", "delta"),
    boot_iter = 1000,
    seed = 123
){
  stopifnot(inherits(model, "lm"))
  method <- match.arg(method)
  
  # --- Identify the exact interaction coefficient name (either order) ---
  b_raw <- coef(model)
  inter_candidates <- grep(
    paste0("^", pred, ":", modx, "$|^", modx, ":", pred, "$"),
    names(b_raw), value = TRUE
  )
  if (length(inter_candidates) == 0) {
    # fallback: any coef name containing both terms
    inter_candidates <- names(b_raw)[grepl(paste0(pred, ":"), names(b_raw)) & grepl(modx, names(b_raw))]
  }
  if (length(inter_candidates) != 1) {
    stop("Could not uniquely identify the interaction coefficient between `", pred, "` and `", modx, "`.\n",
         "Found: ", paste(inter_candidates, collapse = ", "))
  }
  inter_name <- inter_candidates[[1]]
  
  # --- Standardized coefficient for the interaction from lm.beta ---
  if (!requireNamespace("lm.beta", quietly = TRUE)) install.packages("lm.beta")
  beta_std_all <- coef(lm.beta::lm.beta(model))
  beta_int_std <- as.numeric(beta_std_all[inter_name])
  
  # ------------------------------------------------------------------
  # Option A: Delta-method CI on the lm.beta scale (fast, analytical)
  # Uses per-term scaling:  k = beta_std / b_raw  -> SE_std = |k| * SE_raw
  # ------------------------------------------------------------------
  if (method == "delta") {
    sm <- summary(model)$coefficients
    if (!inter_name %in% rownames(sm)) {
      stop("Interaction term not found in model summary: ", inter_name)
    }
    se_raw <- sm[inter_name, "Std. Error"]
    b_raw_inter <- b_raw[inter_name]
    if (is.na(beta_int_std) || is.na(b_raw_inter) || b_raw_inter == 0) {
      stop("Cannot compute delta-method SE: standardized or raw interaction coefficient is NA/0.")
    }
    k <- beta_int_std / b_raw_inter
    se_std <- abs(k) * se_raw
    
    tcrit <- qt(0.975, df = df.residual(model))
    ci <- beta_int_std + c(-1, 1) * tcrit * se_std
    
    return(list(
      method      = "delta",
      term        = inter_name,
      beta_std    = beta_int_std,
      se_std      = se_std,
      ci_lower    = ci[1],
      ci_upper    = ci[2]
    ))
  }
  
  # ------------------------------------------------------------------
  # Option B: Bootstrap CI (robust; recommended)
  # Refit the model on bootstrap resamples; extract standardized interaction each time
  # ------------------------------------------------------------------
  if (is.null(data)) {
    stop("`data` must be provided for method = 'bootstrap'.")
  }
  if (!all(c(pred, modx) %in% names(data))) {
    stop("`pred` and/or `modx` not found in `data`.")
  }
  
  set.seed(seed)
  
  # Capture weights expression if present, so we can evaluate it in boot data
  weights_expr <- NULL
  if (!is.null(model$call$weights)) {
    weights_expr <- model$call$weights
  }
  
  boot_vals <- numeric(boot_iter)
  n <- nrow(data)
  fml <- formula(model)
  
  for (i in seq_len(boot_iter)) {
    idx <- sample.int(n, replace = TRUE)
    boot_data <- data[idx, , drop = FALSE]
    
    # Evaluate weights in boot_data if the original model used weights
    w_vec <- NULL
    if (!is.null(weights_expr)) {
      w_vec <- try(eval(weights_expr, boot_data), silent = TRUE)
      if (inherits(w_vec, "try-error")) w_vec <- NULL
    }
    
    # Refit; guard against occasional failures (e.g., dropped factor level)
    boot_fit <- try(
      if (is.null(w_vec)) {
        lm(fml, data = boot_data)
      } else {
        lm(fml, data = boot_data, weights = w_vec)
      },
      silent = TRUE
    )
    
    if (inherits(boot_fit, "try-error")) {
      boot_vals[i] <- NA_real_
      next
    }
    
    # Get standardized interaction from lm.beta
    b_std_boot <- try(coef(lm.beta::lm.beta(boot_fit))[inter_name], silent = TRUE)
    if (inherits(b_std_boot, "try-error") || is.na(b_std_boot)) {
      boot_vals[i] <- NA_real_
    } else {
      boot_vals[i] <- as.numeric(b_std_boot)
    }
  }
  
  # Drop NAs from failed iterations
  boot_vals <- boot_vals[is.finite(boot_vals)]
  if (length(boot_vals) < max(50, boot_iter * 0.5)) {
    warning("Many bootstrap iterations failed; CI may be unstable.")
  }
  
  ci_boot <- stats::quantile(boot_vals, probs = c(0.025, 0.975), na.rm = TRUE)
  beta_boot_mean <- mean(boot_vals, na.rm = TRUE)
  
  return(list(
    method      = "bootstrap",
    term        = inter_name,
    beta_std    = unname(beta_int_std),  # from original model
    boot_mean   = unname(beta_boot_mean),
    ci_lower    = unname(ci_boot[1]),
    ci_upper    = unname(ci_boot[2]),
    iterations  = boot_iter,
    used        = length(boot_vals)
  ))
}


results_tab <- data.frame(
  ASD_Symptom_Domain      = character(),
  Diffusion_MRI_Measure   = character(),
  Pathway                 = character(),
  Std_Delta_Beta          = numeric(),   # interaction (standardized)
  SE                      = numeric(),
  CI                      = character(), # "[lower, upper]"
  raw_p_value             = numeric(),
  corrected_p_value       = numeric(),
  ASD_Beta                = numeric(),
  Non_ASD_Beta            = numeric(),
  stringsAsFactors = FALSE
)

add_row_from_model <- function(model,
                               pred,              # e.g. "SCP_Num_Fibers"
                               pathway_label,     # e.g. "SCP Number of Streamlines"
                               domain,            # "SCI" or "RRB"
                               dMRI_measure       # "NoS", "FA", "MD"
){
  # standardized interaction CI (delta method)
  ci <- get_standardized_interaction_CI(
    model = model,
    pred  = pred,
    modx  = "scrn_asd",
    method = "delta"
  )
  
  # get p-value for the same interaction term
  sm <- summary(model)$coefficients
  inter_name <- ci$term
  raw_p <- sm[inter_name, "Pr(>|t|)"]
  
  # standardized betas for slopes in each group
  bstd <- coef(lm.beta(model))
  non_asd_beta <- as.numeric(bstd[pred])
  asd_beta     <- as.numeric(bstd[pred] + bstd[inter_name])
  
  # nicely formatted CI string
  ci_str <- sprintf("[%.2f, %.2f]", ci$ci_lower, ci$ci_upper)
  
  data.frame(
    ASD_Symptom_Domain    = domain,
    Diffusion_MRI_Measure = dMRI_measure,
    Pathway               = pathway_label,
    Std_Delta_Beta        = ci$beta_std,
    SE                    = ci$se_std,
    CI                    = ci_str,
    raw_p_value           = raw_p,
    corrected_p_value     = NA_real_,     # fill later with p.adjust
    ASD_Beta              = asd_beta,
    Non_ASD_Beta          = non_asd_beta,
    stringsAsFactors      = FALSE
  )
}

sci_lm_NoS_scp <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + SCP_Num_Fibers*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
summary(sci_lm_NoS_scp)
lm.beta(sci_lm_NoS_scp)
overallbeta <- coef(lm.beta(sci_lm_NoS_scp))["SCP_Num_Fibers:scrn_asd1"]
overallbeta
bluebeta <- coef(lm.beta(sci_lm_NoS_scp))["SCP_Num_Fibers"]
bluebeta
redbeta <- coef(lm.beta(sci_lm_NoS_scp))["SCP_Num_Fibers"] + coef(lm.beta(sci_lm_NoS_scp))["SCP_Num_Fibers:scrn_asd1"]
redbeta
# Delta method (fast)
res_delta <- get_standardized_interaction_CI(
  model = sci_lm_NoS_scp,
  pred  = "SCP_Num_Fibers",
  modx  = "scrn_asd",
  method = "delta"
)
res_delta
p <- plot_interaction_basic_manual(
  model = sci_lm_NoS_scp,
  data  = abcd_data_filtered_no_NA,
  pred  = "SCP_Num_Fibers",
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = "SCP Number of Streamlines",
  ylab  = "Model-Adjusted SCI Score"
)
print(p)


sci_lm_FA_PF <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + PaT_FA_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
overallbeta <- coef(lm.beta(sci_lm_FA_PF))["PaT_FA_tensor1:scrn_asd1"]
overallbeta
bluebeta <- coef(lm.beta(sci_lm_FA_PF))["PaT_FA_tensor1"]
bluebeta
redbeta <- coef(lm.beta(sci_lm_FA_PF))["PaT_FA_tensor1"] + coef(lm.beta(sci_lm_FA_PF))["PaT_FA_tensor1:scrn_asd1"]
redbeta
p <- plot_interaction_basic_manual(
  model = sci_lm_FA_PF,
  data  = abcd_data_filtered_no_NA,
  pred  = "PaT_FA_tensor1",
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = "PF Fractional Anisotropy",
  ylab  = "Model-Adjusted SCI Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = sci_lm_FA_PF,
  pred  = "PaT_FA_tensor1",
  modx  = "scrn_asd",
  method = "delta"
)
res_delta

results_tab <- rbind(
  results_tab,
  add_row_from_model(
    model         = sci_lm_FA_PF,
    pred          = "PaT_FA_tensor1",
    pathway_label = "PF",
    domain        = "SCI",
    dMRI_measure  = "FA"
  )
)




sci_lm_NoS_ICP <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + ICP_Num_Fibers*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
overallbeta <- coef(lm.beta(sci_lm_NoS_ICP))["ICP_Num_Fibers:scrn_asd1"]
overallbeta
bluebeta <- coef(lm.beta(sci_lm_NoS_ICP))["ICP_Num_Fibers"]
bluebeta
redbeta <- coef(lm.beta(sci_lm_NoS_ICP))["ICP_Num_Fibers"] + coef(lm.beta(sci_lm_NoS_ICP))["ICP_Num_Fibers:scrn_asd1"]
redbeta
p <- plot_interaction_basic_manual(
  model = sci_lm_NoS_ICP,
  data  = abcd_data_filtered_no_NA,
  pred  = "ICP_Num_Fibers",
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = "ICP Number of Streamlines",
  ylab  = "Model-Adjusted SCI Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = sci_lm_NoS_ICP,
  pred  = "ICP_Num_Fibers",
  modx  = "scrn_asd",
  method = "delta"
)
res_delta



sci_lm_NoS_IP <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + IP_Num_Fibers*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
overallbeta <- coef(lm.beta(sci_lm_NoS_IP))["IP_Num_Fibers:scrn_asd1"]
overallbeta
bluebeta <- coef(lm.beta(sci_lm_NoS_IP))["IP_Num_Fibers"]
bluebeta
redbeta <- coef(lm.beta(sci_lm_NoS_IP))["IP_Num_Fibers"] + coef(lm.beta(sci_lm_NoS_IP))["IP_Num_Fibers:scrn_asd1"]
redbeta
p <- plot_interaction_basic_manual(
  model = sci_lm_NoS_IP,
  data  = abcd_data_filtered_no_NA,
  pred  = "IP_Num_Fibers",
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
#  y_limits = c(0, 100),  # <— new parameter
  title = "Interaction Plot",
  xlab  = "IP Number of Streamlines",
  ylab  = "Model-Adjusted SCI Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = sci_lm_NoS_IP,
  pred  = "IP_Num_Fibers",
  modx  = "scrn_asd",
  method = "delta"
)
res_delta






rrb_lm_NoS_scp <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + SCP_Num_Fibers*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
overallbeta <- coef(lm.beta(rrb_lm_NoS_scp))["SCP_Num_Fibers:scrn_asd1"]
overallbeta
bluebeta <- coef(lm.beta(rrb_lm_NoS_scp))["SCP_Num_Fibers"]
bluebeta
redbeta <- coef(lm.beta(rrb_lm_NoS_scp))["SCP_Num_Fibers"] + coef(lm.beta(rrb_lm_NoS_scp))["SCP_Num_Fibers:scrn_asd1"]
redbeta
p <- plot_interaction_basic_manual(
  model = rrb_lm_NoS_scp,
  data  = abcd_data_filtered_no_NA,
  pred  = "SCP_Num_Fibers",
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = "SCP Number of Streamlines",
  ylab  = "Model-Adjusted RRB Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = rrb_lm_NoS_scp,
  pred  = "SCP_Num_Fibers",
  modx  = "scrn_asd",
  method = "delta"
)
res_delta


rrb_lm_MD_ICP <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + ICP_MD_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
overallbeta <- coef(lm.beta(rrb_lm_MD_ICP))["ICP_MD_tensor1:scrn_asd1"]
overallbeta
bluebeta <- coef(lm.beta(rrb_lm_MD_ICP))["ICP_MD_tensor1"]
bluebeta
redbeta <- coef(lm.beta(rrb_lm_MD_ICP))["ICP_MD_tensor1"] + coef(lm.beta(rrb_lm_MD_ICP))["ICP_MD_tensor1:scrn_asd1"]
redbeta
p <- plot_interaction_basic_manual(
  model = rrb_lm_MD_ICP,
  data  = abcd_data_filtered_no_NA,
  pred  = "ICP_MD_tensor1",
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = "ICP Mean Diffusivity",
  ylab  = "Model-Adjusted RRB Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = rrb_lm_MD_ICP,
  pred  = "ICP_MD_tensor1",
  modx  = "scrn_asd",
  method = "delta"
)
res_delta






# NON_SIGNIFICANT PLOTS

###### RRB ####
#RRB & MD
pathway_measure <- "IP_MD_tensor1"
pathway_name <- "IP Mean Diffusivity"
model <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + IP_MD_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
coef_name <- paste0(pathway_measure, ":scrn_asd1")
overallbeta <- coef(lm.beta(model))[coef_name]
overallbeta
bluebeta <- coef(lm.beta(model))[pathway_measure]
bluebeta
redbeta <- coef(lm.beta(model))[pathway_measure] + coef(lm.beta(model))[coef_name]
redbeta
p <- plot_interaction_basic_manual(
  model = model,
  data  = abcd_data_filtered_no_NA,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = pathway_name,
  ylab  = "Model-Adjusted RRB Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = model,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  method = "delta"
)
res_delta

pathway_measure <- "SCP_MD_tensor1"
pathway_name <- "SCP Mean Diffusivity"
model <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + SCP_MD_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
coef_name <- paste0(pathway_measure, ":scrn_asd1")
overallbeta <- coef(lm.beta(model))[coef_name]
overallbeta
bluebeta <- coef(lm.beta(model))[pathway_measure]
bluebeta
redbeta <- coef(lm.beta(model))[pathway_measure] + coef(lm.beta(model))[coef_name]
redbeta
p <- plot_interaction_basic_manual(
  model = model,
  data  = abcd_data_filtered_no_NA,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = pathway_name,
  ylab  = "Model-Adjusted RRB Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = model,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  method = "delta"
)
res_delta

pathway_measure <- "MCP_MD_tensor1"
pathway_name <- "MCP Mean Diffusivity"
model <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + MCP_MD_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
coef_name <- paste0(pathway_measure, ":scrn_asd1")
overallbeta <- coef(lm.beta(model))[coef_name]
overallbeta
bluebeta <- coef(lm.beta(model))[pathway_measure]
bluebeta
redbeta <- coef(lm.beta(model))[pathway_measure] + coef(lm.beta(model))[coef_name]
redbeta
p <- plot_interaction_basic_manual(
  model = model,
  data  = abcd_data_filtered_no_NA,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = pathway_name,
  ylab  = "Model-Adjusted RRB Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = model,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  method = "delta"
)
res_delta


pathway_measure <- "PaT_MD_tensor1"
pathway_name <- "PF Mean Diffusivity"
model <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + PaT_MD_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
coef_name <- paste0(pathway_measure, ":scrn_asd1")
overallbeta <- coef(lm.beta(model))[coef_name]
overallbeta
bluebeta <- coef(lm.beta(model))[pathway_measure]
bluebeta
redbeta <- coef(lm.beta(model))[pathway_measure] + coef(lm.beta(model))[coef_name]
redbeta
p <- plot_interaction_basic_manual(
  model = model,
  data  = abcd_data_filtered_no_NA,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = pathway_name,
  ylab  = "Model-Adjusted RRB Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = model,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  method = "delta"
)
res_delta



#RRB & NoS
pathway_measure <- "PaT_Num_Fibers"
pathway_name <- "PF Number of Streamlines"
model <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + PaT_Num_Fibers*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
coef_name <- paste0(pathway_measure, ":scrn_asd1")
overallbeta <- coef(lm.beta(model))[coef_name]
overallbeta
bluebeta <- coef(lm.beta(model))[pathway_measure]
bluebeta
redbeta <- coef(lm.beta(model))[pathway_measure] + coef(lm.beta(model))[coef_name]
redbeta
p <- plot_interaction_basic_manual(
  model = model,
  data  = abcd_data_filtered_no_NA,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = pathway_name,
  ylab  = "Model-Adjusted RRB Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = model,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  method = "delta"
)
res_delta

pathway_measure <- "IP_Num_Fibers"
pathway_name <- "IP Number of Streamlines"
model <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + IP_Num_Fibers*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
coef_name <- paste0(pathway_measure, ":scrn_asd1")
overallbeta <- coef(lm.beta(model))[coef_name]
overallbeta
bluebeta <- coef(lm.beta(model))[pathway_measure]
bluebeta
redbeta <- coef(lm.beta(model))[pathway_measure] + coef(lm.beta(model))[coef_name]
redbeta
p <- plot_interaction_basic_manual(
  model = model,
  data  = abcd_data_filtered_no_NA,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = pathway_name,
  ylab  = "Model-Adjusted RRB Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = model,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  method = "delta"
)
res_delta

pathway_measure <- "ICP_Num_Fibers"
pathway_name <- "ICP Number of Streamlines"
model <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + ICP_Num_Fibers*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
coef_name <- paste0(pathway_measure, ":scrn_asd1")
overallbeta <- coef(lm.beta(model))[coef_name]
overallbeta
bluebeta <- coef(lm.beta(model))[pathway_measure]
bluebeta
redbeta <- coef(lm.beta(model))[pathway_measure] + coef(lm.beta(model))[coef_name]
redbeta
p <- plot_interaction_basic_manual(
  model = model,
  data  = abcd_data_filtered_no_NA,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = pathway_name,
  ylab  = "Model-Adjusted RRB Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = model,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  method = "delta"
)
res_delta

pathway_measure <- "MCP_Num_Fibers"
pathway_name <- "MCP Number of Streamlines"
model <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + MCP_Num_Fibers*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
coef_name <- paste0(pathway_measure, ":scrn_asd1")
overallbeta <- coef(lm.beta(model))[coef_name]
overallbeta
bluebeta <- coef(lm.beta(model))[pathway_measure]
bluebeta
redbeta <- coef(lm.beta(model))[pathway_measure] + coef(lm.beta(model))[coef_name]
redbeta
p <- plot_interaction_basic_manual(
  model = model,
  data  = abcd_data_filtered_no_NA,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = pathway_name,
  ylab  = "Model-Adjusted RRB Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = model,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  method = "delta"
)
res_delta


#RRB & FA
pathway_measure <- "PaT_FA_tensor1"
pathway_name <- "PF Fractional Anisotropy"
model <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + PaT_FA_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
coef_name <- paste0(pathway_measure, ":scrn_asd1")
overallbeta <- coef(lm.beta(model))[coef_name]
overallbeta
bluebeta <- coef(lm.beta(model))[pathway_measure]
bluebeta
redbeta <- coef(lm.beta(model))[pathway_measure] + coef(lm.beta(model))[coef_name]
redbeta
p <- plot_interaction_basic_manual(
  model = model,
  data  = abcd_data_filtered_no_NA,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = pathway_name,
  ylab  = "Model-Adjusted RRB Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = model,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  method = "delta"
)
res_delta

pathway_measure <- "IP_FA_tensor1"
pathway_name <- "IP Fractional Anisotropy"
model <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + IP_FA_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
coef_name <- paste0(pathway_measure, ":scrn_asd1")
overallbeta <- coef(lm.beta(model))[coef_name]
overallbeta
bluebeta <- coef(lm.beta(model))[pathway_measure]
bluebeta
redbeta <- coef(lm.beta(model))[pathway_measure] + coef(lm.beta(model))[coef_name]
redbeta
p <- plot_interaction_basic_manual(
  model = model,
  data  = abcd_data_filtered_no_NA,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = pathway_name,
  ylab  = "Model-Adjusted RRB Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = model,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  method = "delta"
)
res_delta


pathway_measure <- "ICP_FA_tensor1"
pathway_name <- "ICP Fractional Anisotropy"
model <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + ICP_FA_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
coef_name <- paste0(pathway_measure, ":scrn_asd1")
overallbeta <- coef(lm.beta(model))[coef_name]
overallbeta
bluebeta <- coef(lm.beta(model))[pathway_measure]
bluebeta
redbeta <- coef(lm.beta(model))[pathway_measure] + coef(lm.beta(model))[coef_name]
redbeta
p <- plot_interaction_basic_manual(
  model = model,
  data  = abcd_data_filtered_no_NA,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = pathway_name,
  ylab  = "Model-Adjusted RRB Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = model,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  method = "delta"
)
res_delta

pathway_measure <- "MCP_FA_tensor1"
pathway_name <- "MCP Fractional Anisotropy"
model <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + MCP_FA_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
coef_name <- paste0(pathway_measure, ":scrn_asd1")
overallbeta <- coef(lm.beta(model))[coef_name]
overallbeta
bluebeta <- coef(lm.beta(model))[pathway_measure]
bluebeta
redbeta <- coef(lm.beta(model))[pathway_measure] + coef(lm.beta(model))[coef_name]
redbeta
p <- plot_interaction_basic_manual(
  model = model,
  data  = abcd_data_filtered_no_NA,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = pathway_name,
  ylab  = "Model-Adjusted RRB Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = model,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  method = "delta"
)
res_delta

pathway_measure <- "SCP_FA_tensor1"
pathway_name <- "SCP Fractional Anisotropy"
model <- lm(rrb_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + SCP_FA_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
coef_name <- paste0(pathway_measure, ":scrn_asd1")
overallbeta <- coef(lm.beta(model))[coef_name]
overallbeta
bluebeta <- coef(lm.beta(model))[pathway_measure]
bluebeta
redbeta <- coef(lm.beta(model))[pathway_measure] + coef(lm.beta(model))[coef_name]
redbeta
p <- plot_interaction_basic_manual(
  model = model,
  data  = abcd_data_filtered_no_NA,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = pathway_name,
  ylab  = "Model-Adjusted RRB Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = model,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  method = "delta"
)
res_delta



###### SCI ####
## NoS
pathway_measure <- "MCP_Num_Fibers"
pathway_name <- "MCP Number of Streamlines"
model <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + MCP_Num_Fibers*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
coef_name <- paste0(pathway_measure, ":scrn_asd1")
overallbeta <- coef(lm.beta(model))[coef_name]
overallbeta
bluebeta <- coef(lm.beta(model))[pathway_measure]
bluebeta
redbeta <- coef(lm.beta(model))[pathway_measure] + coef(lm.beta(model))[coef_name]
redbeta
p <- plot_interaction_basic_manual(
  model = model,
  data  = abcd_data_filtered_no_NA,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = pathway_name,
  ylab  = "Model-Adjusted SCI Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = model,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  method = "delta"
)
res_delta

pathway_measure <- "PaT_Num_Fibers"
pathway_name <- "PF Number of Streamlines"
model <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + PaT_Num_Fibers*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
coef_name <- paste0(pathway_measure, ":scrn_asd1")
overallbeta <- coef(lm.beta(model))[coef_name]
overallbeta
bluebeta <- coef(lm.beta(model))[pathway_measure]
bluebeta
redbeta <- coef(lm.beta(model))[pathway_measure] + coef(lm.beta(model))[coef_name]
redbeta
p <- plot_interaction_basic_manual(
  model = model,
  data  = abcd_data_filtered_no_NA,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = pathway_name,
  ylab  = "Model-Adjusted SCI Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = model,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  method = "delta"
)
res_delta


## FA
pathway_measure <- "IP_FA_tensor1"
pathway_name <- "IP Fractional Anisotropy"
model <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + IP_FA_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
coef_name <- paste0(pathway_measure, ":scrn_asd1")
overallbeta <- coef(lm.beta(model))[coef_name]
overallbeta
bluebeta <- coef(lm.beta(model))[pathway_measure]
bluebeta
redbeta <- coef(lm.beta(model))[pathway_measure] + coef(lm.beta(model))[coef_name]
redbeta
p <- plot_interaction_basic_manual(
  model = model,
  data  = abcd_data_filtered_no_NA,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = pathway_name,
  ylab  = "Model-Adjusted SCI Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = model,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  method = "delta"
)
res_delta


pathway_measure <- "ICP_FA_tensor1"
pathway_name <- "ICP Fractional Anisotropy"
model <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + ICP_FA_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
coef_name <- paste0(pathway_measure, ":scrn_asd1")
overallbeta <- coef(lm.beta(model))[coef_name]
overallbeta
bluebeta <- coef(lm.beta(model))[pathway_measure]
bluebeta
redbeta <- coef(lm.beta(model))[pathway_measure] + coef(lm.beta(model))[coef_name]
redbeta
p <- plot_interaction_basic_manual(
  model = model,
  data  = abcd_data_filtered_no_NA,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = pathway_name,
  ylab  = "Model-Adjusted SCI Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = model,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  method = "delta"
)
res_delta

pathway_measure <- "MCP_FA_tensor1"
pathway_name <- "MCP Fractional Anisotropy"
model <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + MCP_FA_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
coef_name <- paste0(pathway_measure, ":scrn_asd1")
overallbeta <- coef(lm.beta(model))[coef_name]
overallbeta
bluebeta <- coef(lm.beta(model))[pathway_measure]
bluebeta
redbeta <- coef(lm.beta(model))[pathway_measure] + coef(lm.beta(model))[coef_name]
redbeta
p <- plot_interaction_basic_manual(
  model = model,
  data  = abcd_data_filtered_no_NA,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = pathway_name,
  ylab  = "Model-Adjusted SCI Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = model,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  method = "delta"
)
res_delta

pathway_measure <- "SCP_FA_tensor1"
pathway_name <- "SCP Fractional Anisotropy"
model <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + SCP_FA_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
coef_name <- paste0(pathway_measure, ":scrn_asd1")
overallbeta <- coef(lm.beta(model))[coef_name]
overallbeta
bluebeta <- coef(lm.beta(model))[pathway_measure]
bluebeta
redbeta <- coef(lm.beta(model))[pathway_measure] + coef(lm.beta(model))[coef_name]
redbeta
p <- plot_interaction_basic_manual(
  model = model,
  data  = abcd_data_filtered_no_NA,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = pathway_name,
  ylab  = "Model-Adjusted SCI Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = model,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  method = "delta"
)
res_delta

## MD
pathway_measure <- "IP_MD_tensor1"
pathway_name <- "IP Mean Diffusivity"
model <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + IP_MD_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
coef_name <- paste0(pathway_measure, ":scrn_asd1")
overallbeta <- coef(lm.beta(model))[coef_name]
overallbeta
bluebeta <- coef(lm.beta(model))[pathway_measure]
bluebeta
redbeta <- coef(lm.beta(model))[pathway_measure] + coef(lm.beta(model))[coef_name]
redbeta
p <- plot_interaction_basic_manual(
  model = model,
  data  = abcd_data_filtered_no_NA,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = pathway_name,
  ylab  = "Model-Adjusted SCI Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = model,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  method = "delta"
)
res_delta

pathway_measure <- "SCP_MD_tensor1"
pathway_name <- "SCP Mean Diffusivity"
model <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + SCP_MD_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
coef_name <- paste0(pathway_measure, ":scrn_asd1")
overallbeta <- coef(lm.beta(model))[coef_name]
overallbeta
bluebeta <- coef(lm.beta(model))[pathway_measure]
bluebeta
redbeta <- coef(lm.beta(model))[pathway_measure] + coef(lm.beta(model))[coef_name]
redbeta
p <- plot_interaction_basic_manual(
  model = model,
  data  = abcd_data_filtered_no_NA,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = pathway_name,
  ylab  = "Model-Adjusted SCI Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = model,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  method = "delta"
)
res_delta

pathway_measure <- "MCP_MD_tensor1"
pathway_name <- "MCP Mean Diffusivity"
model <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + MCP_MD_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
coef_name <- paste0(pathway_measure, ":scrn_asd1")
overallbeta <- coef(lm.beta(model))[coef_name]
overallbeta
bluebeta <- coef(lm.beta(model))[pathway_measure]
bluebeta
redbeta <- coef(lm.beta(model))[pathway_measure] + coef(lm.beta(model))[coef_name]
redbeta
p <- plot_interaction_basic_manual(
  model = model,
  data  = abcd_data_filtered_no_NA,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = pathway_name,
  ylab  = "Model-Adjusted SCI Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = model,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  method = "delta"
)
res_delta

pathway_measure <- "ICP_MD_tensor1"
pathway_name <- "ICP Mean Diffusivity"
model <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + ICP_MD_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
coef_name <- paste0(pathway_measure, ":scrn_asd1")
overallbeta <- coef(lm.beta(model))[coef_name]
overallbeta
bluebeta <- coef(lm.beta(model))[pathway_measure]
bluebeta
redbeta <- coef(lm.beta(model))[pathway_measure] + coef(lm.beta(model))[coef_name]
redbeta
p <- plot_interaction_basic_manual(
  model = model,
  data  = abcd_data_filtered_no_NA,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = pathway_name,
  ylab  = "Model-Adjusted SCI Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = model,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  method = "delta"
)
res_delta


pathway_measure <- "PaT_MD_tensor1"
pathway_name <- "PF Mean Diffusivity"
model <- lm(sci_score ~ interview_age.x + sex.x + dmdtifp1_1183 + nihtbx_totalcomp_uncorrected + PaT_MD_tensor1*scrn_asd, data = abcd_data_filtered_no_NA, weights = acs_raked_propensity_score)
coef_name <- paste0(pathway_measure, ":scrn_asd1")
overallbeta <- coef(lm.beta(model))[coef_name]
overallbeta
bluebeta <- coef(lm.beta(model))[pathway_measure]
bluebeta
redbeta <- coef(lm.beta(model))[pathway_measure] + coef(lm.beta(model))[coef_name]
redbeta
p <- plot_interaction_basic_manual(
  model = model,
  data  = abcd_data_filtered_no_NA,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
  y_limits = c(5, 20),  # <— new parameter
  title = "Interaction Plot",
  xlab  = pathway_name,
  ylab  = "Model-Adjusted SCI Score"
)
print(p)
res_delta <- get_standardized_interaction_CI(
  model = model,
  pred  = pathway_measure,
  modx  = "scrn_asd",
  method = "delta"
)
res_delta


results_tab <- rbind(
  results_tab,
  add_row_from_model(
    model         = sci_lm_NoS_scp,
    pred          = "SCP_Num_Fibers",
    pathway_label = "SCP",
    domain        = "SCI",
    dMRI_measure  = "NoS"
  )
)

sci_lm_NoS_mcp <- 
results_tab <- rbind(
  results_tab,
  add_row_from_model(
    model         = sci_lm_NoS_mcp,
    pred          = "MCP_Num_Fibers",
    pathway_label = "MCP",
    domain        = "SCI",
    dMRI_measure  = "NoS"
  )
)




results_tab <- rbind(
  results_tab,
  add_row_from_model(
    model         = sci_lm_FA_PF,
    pred          = "PaT_FA_tensor1",
    pathway_label = "PF Fractional Anisotropy",
    domain        = "PF",
    dMRI_measure  = "FA"
  )
)




