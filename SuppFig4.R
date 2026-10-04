


abcd_data_filtered_no_NA <- read.csv("abcd_data_filtered_no_NA.csv")

abcd_data_filtered_no_NA$scrn_asd <- as.factor(abcd_data_filtered_no_NA$scrn_asd)


plot_raw_points_only <- function(
    data,
    outcome,
    pred,
    modx,
    modx_labels = c("Control (ASD=0)", "ASD (ASD=1)"),
    title = "Raw Data",
    xlab = NULL,
    ylab = NULL,
    point_size = 1.5,
    jitter_height = 0.25,
    y_limits = NULL
){
  
  library(ggplot2)
  
  if(is.null(xlab)) xlab <- pred
  if(is.null(ylab)) ylab <- outcome
  
  plot_df <- data.frame(
    x = data[[pred]],
    y = data[[outcome]],
    group_lab = factor(
      ifelse(
        as.character(data[[modx]]) %in% c("1",1),
        modx_labels[2],
        modx_labels[1]
      ),
      levels=modx_labels
    )
  )
  
  plot_df <- na.omit(plot_df)
  
  gg <- ggplot() +
    
    # controls (blue transparent)
    geom_point(
      data=subset(plot_df,
                  group_lab==modx_labels[1]),
      aes(x=x,y=y,color=group_lab),
      alpha=.25,
      size=point_size,
      position=position_jitter(
        width=0,
        height=jitter_height
      )
    ) +
    
    # ASD (red non-transparent)
    geom_point(
      data=subset(plot_df,
                  group_lab==modx_labels[2]),
      aes(x=x,y=y,color=group_lab),
      alpha=1,
      size=point_size,
      position=position_jitter(
        width=0,
        height=jitter_height
      )
    ) +
    
    scale_color_manual(
      values=c(
        "Control (ASD=0)"="blue",
        "ASD (ASD=1)"="red"
      )
    ) +
    
    labs(
      title=title,
      x=xlab,
      y=ylab
    ) +
    
    theme_minimal(base_size=13) +
    theme(
      legend.position="top",
      legend.title=element_blank(),
      plot.title=element_text(face="bold")
    )
  
  if(!is.null(y_limits)){
    gg <- gg + coord_cartesian(
      ylim=y_limits
    )
  }
  
  return(gg)
}


p <- plot_raw_points_only(
  data = abcd_data_filtered_no_NA,
  outcome = "sci_score",
  pred = "SCP_Num_Fibers",
  modx = "scrn_asd",
  xlab = "SCP Number of Streamlines",
  ylab = "Raw SCI Score",
  y_limits = c(5,20)
)

print(p)


p <- plot_raw_points_only(
  data = abcd_data_filtered_no_NA,
  outcome = "sci_score",
  pred = "PaT_FA_tensor1",
  modx = "scrn_asd",
  xlab = "PF Fractional Anisotropy",
  ylab = "Raw SCI Score",
  y_limits = c(5,20)
)

print(p)


p <- plot_raw_points_only(
  data = abcd_data_filtered_no_NA,
  outcome = "sci_score",
  pred = "IP_Num_Fibers",
  modx = "scrn_asd",
  xlab = "IP Number of Streamlines",
  ylab = "Raw SCI Score",
  y_limits = c(5,20)
)

print(p)


p <- plot_raw_points_only(
  data = abcd_data_filtered_no_NA,
  outcome = "sci_score",
  pred = "ICP_Num_Fibers",
  modx = "scrn_asd",
  xlab = "ICP Number of Streamlines",
  ylab = "Raw SCI Score",
  y_limits = c(5,20)
)

print(p)


p <- plot_raw_points_only(
  data = abcd_data_filtered_no_NA,
  outcome = "rrb_score",
  pred = "SCP_Num_Fibers",
  modx = "scrn_asd",
  xlab = "SCP Number of Streamlines",
  ylab = "Raw RRB Score",
  y_limits = c(5,20)
)

print(p)



p <- plot_raw_points_only(
  data = abcd_data_filtered_no_NA,
  outcome = "rrb_score",
  pred = "ICP_MD_tensor1",
  modx = "scrn_asd",
  xlab = "ICP Mean Diffusivity",
  ylab = "Raw RRB Score",
  y_limits = c(5,20)
)

print(p)




