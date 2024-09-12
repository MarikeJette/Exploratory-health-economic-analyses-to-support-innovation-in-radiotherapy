# Calculation function for shiny application Flexible and generic tool to facilitate early exploration of MR-Linac's value for money (Marike Ulehake)

rm(list = ls())

## Load functions 
source("Input_MRL.R")

Calc.shiny <- function(f_L,
                       tf_L, 
                       a_t_L, 
                       occ_t_L, 
                       f_MRL, 
                       tf_MRL,
                       #a_t_MRL, 
                       occ_t_MRL, 
                       wtp,  
                       c_aanschaf_L,
                       c_bunk_L,
                       c_onderhoud_L,
                       c_qaequip_L, 
                       c_aanschaf_MRL,
                       c_bunk_MRL,
                       c_onderhoud_MRL,
                       c_qaequip_MRL, 
                       c_arts,
                       c_fys,
                       c_labo,
                       n_arts_MRL,
                       n_fys_MRL,
                       n_labo_MRL,
                       n_arts_L,
                       n_fys_L,
                       n_labo_L,
                       tv_labo_MRL, 
                       tv_fys_MRL,
                       tv_arts_MRL, 
                       tv_labo_L, 
                       tv_fys_L, 
                       tv_arts_L, 
                       perc_arts_MRL,
                       perc_fys_MRL,
                       perc_labo_MRL,
                       perc_arts_L,
                       perc_fys_L,
                       perc_labo_L,
                       c_IMD_MRL, 
                       c_IMD_L, 
                       p_rente,
                       t_afschr_bunk,
                       t_afschr_app
                       
                       ){

  
  #check class
  print(sapply(list(f_L, tf_L, a_t_L, occ_t_L, tf_MRL, #a_t_MRL, occ_t_MRL, wtp, 
                    c_aanschaf_L, c_bunk_L, c_onderhoud_L, c_qaequip_L, c_aanschaf_MRL, 
                    c_bunk_MRL, c_onderhoud_MRL, c_qaequip_MRL, c_arts, c_fys, c_labo, 
                    n_arts_MRL, n_fys_MRL, n_labo_MRL, n_arts_L, n_fys_L, n_labo_L, 
                    tv_labo_MRL, tv_fys_MRL, tv_arts_MRL, tv_labo_L, tv_fys_L, tv_arts_L), class))
  
  
  # Annuity factor 
  p_rente <- p_rente/100
  t_ann_app       <-   (1/p_rente)*(1-1/((1+p_rente)^t_afschr_app))  
  t_ann_bunk      <-   (1/p_rente)*(1-1/((1+p_rente)^t_afschr_bunk))
  
  
  # Convert available hours to minutes 
  max_t_L <- a_t_L  * 60
  max_t_MRL <- max_t_L  
  
  # total execution time per fraction (in minutes) 
  tu_arts_L     <- (tf_L*n_arts_L)*(perc_arts_L/100)                    
  tu_fys_L      <- tf_L*n_fys_L * (perc_fys_L/100)
  tu_labo_L     <- tf_L*n_labo_L * (perc_labo_L/100)   
  tu_arts_MRL     <- tf_MRL * n_arts_MRL * (perc_arts_MRL/100)                   
  tu_fys_MRL      <- tf_MRL * n_fys_MRL *(perc_fys_MRL/100)
  tu_labo_MRL     <- tf_MRL * n_labo_MRL *(perc_labo_MRL/100)    
  
  # NUMBER OF FRACTIONS
  # Total personnel costs 
  # Conventional treatment
  # Total treatment costs
  tk_arts_L <- (tv_arts_L + tu_arts_L * f_L) * (c_arts/60) 
  tk_fys_L  <- (tv_fys_L +  tu_fys_L * f_L) * (c_fys/60) 
  tk_labo_L <- (tv_labo_L + tu_labo_L * f_L)  * (c_labo/60)
  
  tk_pers_L <- tk_arts_L + tk_fys_L + tk_labo_L  + c_IMD_L 
  
  ## MR-Linac 
  # Total treatment costs
  tk_arts_MRL <- (tv_arts_MRL + tu_arts_MRL* f_MRL_range) * (c_arts/60)
  tk_fys_MRL  <- (tv_fys_MRL +  tu_fys_MRL * f_MRL_range) * (c_fys/60) 
  tk_labo_MRL <- (tv_labo_MRL + tu_labo_MRL *f_MRL_range)  * (c_labo/60)
  
  m_c_IMD_MRL <- rep(c_IMD_MRL, times = length(tf_MRL))
  
  tk_pers_MRL <- tk_arts_MRL + tk_fys_MRL + tk_labo_MRL + c_IMD_MRL
  
  # Summarizing table personnel costs
  l_tk_pers_L <- rep(tk_pers_L, 15)
  tk_pers <- cbind(l_tk_pers_L, tk_pers_MRL)
  
  #  Total equipment costs
  ### Conventional treatment 
  tc_L <- ((c_aanschaf_L + c_qaequip_L)/t_ann_app + c_onderhoud_L) + (c_bunk_L/t_ann_bunk)       # costs per year
  tc_L_pm <- tc_L / (max_t_L*occ_t_L)          # cost per minute Linac 
  tc_app_L <- tc_L_pm * (f_L*tf_L)             # cost radiation treatment scheme (f_L*tf_L is total treatment time)
  
  ### MR-Linac 
  tc_MRL <- ((c_aanschaf_MRL + c_qaequip_MRL)/t_ann_app) + (c_bunk_MRL/t_ann_bunk) + c_onderhoud_MRL # costs per year 
  c_MRL_pm <- tc_MRL / (max_t_MRL*occ_t_MRL)                   # costs per minute MR-Linac 
  l_tc_app_MRL <- c_MRL_pm * (f_MRL_range*tf_MRL)                    #kosten voor 1 tot en met 15 fracties 
  
  ## equipment cost table
  l_tc_app_L <- rep(tc_app_L, 15)
  tk_app <- cbind(l_tc_app_L, l_tc_app_MRL)
  
  # Total cost (equipment + personnel) 
  # Table
  tk <- tk_app + tk_pers
  tk_df <- as.data.frame(tk)
  colnames(tk_df) <- c("Total costs Linac", "Total costs MR-Linac" )
  tk_df$Fractions <- f_MRL_range
  tk_df$Diff <- tk_df$"Total costs MR-Linac" - tk_df$"Total costs Linac"
  
  tk_df <- tk_df[,c("Fractions", "Total costs Linac", "Total costs MR-Linac", "Diff")]
  colnames (tk_df) <- c("Number of fractions MRgRT", "Total costs Linac (€)", "Total costs MR-Linac (€)", "Difference compared to conventional treatment (€)")
  
  tk_df_QALY <- data.frame(tk_df$Diff * (1 / wtp))
  colnames(tk_df_QALY) <- "QALYs needed to outweigh extra costs"
  
  #determine number of fractions cost difference is 0 
  snijwaarde_benaderd <- approx(tk_df$Diff, tk_df$`Number of fractions MRgRT`, xout = 0)$y
  snijwaarde_fractions <- ifelse(
    is.na(snijwaarde_benaderd), "The value for which the costs are equal falls outside the tested range",
    paste("MRgRT becomes more expensive after", round(snijwaarde_benaderd, 1), "fractions")
  )
  
  #Plot including cost difference and QALYs
  scale_ratio <- max(tk_df$`Difference compared to conventional treatment (€)`) / max(tk_df_QALY)
  breaks_QALYs <- seq(from = floor(min(tk_df_QALY)), 
                      to = ceiling(max(tk_df_QALY)), by = 0.05)
  
  tk_df_total <- cbind(tk_df, tk_df_QALY)
  
  plot_fractions <- ggplot(tk_df_total, aes(x = `Number of fractions MRgRT`)) +
    geom_line(aes(y = `Difference compared to conventional treatment (€)`), color = "blue", linewidth = 1) +
    geom_line(aes(y = `QALYs needed to outweigh extra costs` * scale_ratio), color = "blue", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
    labs(x = "Number of fractions MRgRT", y = "Cost difference [euro]", title = "Figure 1. Number of fractions MR-Linac treatment") +
    scale_y_continuous(
      name = "Cost difference [euro]",
      sec.axis = sec_axis(
        trans = ~./scale_ratio, 
        name = "QALYs needed to outweigh extra costs",
        breaks = breaks_QALYs
      )
    ) +
    scale_x_continuous(breaks = seq(1, 15, 1)) + 
    theme(
      axis.title.y.right = element_text(margin = margin(l = 10)) 
    )
  
  tk_df_total$`QALYs needed to outweigh extra costs` <- data.frame(ifelse(tk_df$Diff * (1 / wtp) < 0, "n/a", format(round(tk_df$Diff * (1 / wtp), 2), nsmall = 2)))
  
  table_fractions <- tk_df_total
  table_fractions$"Total costs Linac (€)" <- NULL
  table_fractions$"fracties" <- NULL
  
  print(snijwaarde_fractions)
  print(table_fractions)
  print(plot_fractions)
  
  #TIME PER FRACTION 
  tf_MRL_th <- as.integer(seq(0, 90, by = 1))                        #tijd range in minuten 
  
  #  Totale kosten personele inzet 
  tu_arts_MRL_tf     <- tf_MRL_th * n_arts_MRL * (perc_arts_MRL/100)                   
  tu_fys_MRL_tf      <- tf_MRL_th * n_fys_MRL *(perc_fys_MRL/100)
  tu_labo_MRL_tf     <- tf_MRL_th * n_labo_MRL *(perc_labo_MRL/100) 
  
  ## Conventional treatment 
  # Total costs
  tk_arts_L <- (tv_arts_L + tu_arts_L * f_L) * (c_arts/60) 
  tk_fys_L  <- (tv_fys_L + tu_fys_L * f_L) * (c_fys/60) 
  tk_labo_L <- (tv_labo_L + tu_labo_L * f_L)  * (c_labo/60)
  
  tk_pers_L <- tk_arts_L + tk_fys_L + tk_labo_L  + c_IMD_L  
  tk_pers_L <- rep(tk_pers_L, times = length(tf_MRL_th))
  
  ##MR-Linac 
  # Total treatment costs
  tk_arts_MRL <- (tv_arts_MRL + tu_arts_MRL_tf* f_MRL) * (c_arts/60)
  tk_fys_MRL  <- (tv_fys_MRL +  tu_fys_MRL_tf * f_MRL) * (c_fys/60) 
  tk_labo_MRL <- (tv_labo_MRL + tu_labo_MRL_tf *f_MRL)  * (c_labo/60)
  
  m_c_IMD_MRL <- rep(c_IMD_MRL, times = length(tf_MRL_th))
  
  ## Sum of total treatment costs
  tk_pers_MRL <- tk_arts_MRL + tk_fys_MRL + tk_labo_MRL + c_IMD_MRL
  #tk_pers_MRL <- rep(tk_pers_MRL, times = length(tf_MRL_th))
  
  # Summarizing table personnel costs
  tk_pers <- cbind(tk_pers_L, tk_pers_MRL)
  tk_pers_df <- tk_pers 
  colnames(tk_pers_df) <- c("Linac", "MRL")
  rownames(tk_pers_df) <- tf_MRL_th
  
  #  Total equipment costs
  ### Conventional treatment 
  tc_L <- ((c_aanschaf_L + c_qaequip_L)/t_ann_app + c_onderhoud_L) + (c_bunk_L/t_ann_bunk)       # costs per year
  tc_L_pm <- tc_L / (max_t_L*occ_t_L)                 # costs per fraction
  tc_app_L <- tc_L_pm * (f_L*tf_L)                    # costs per treatment  
  l_tc_app_L <- rep(round(tc_app_L, 2), length(tf_MRL_th))  
  
  ### MR-Linac 
  tc_MRL <- ((c_aanschaf_MRL + c_qaequip_MRL)/t_ann_app + c_onderhoud_MRL) + (c_bunk_MRL/t_ann_bunk) # costs per year 
  c_MRL_pm <- tc_MRL / (max_t_MRL*occ_t_MRL)                                                         # costs per minute MR-Linac 
  m_tc_app_MRL <- c_MRL_pm * (tf_MRL_th * f_MRL)            # costs for treatment scheme 
  
  ## Equipment costs table
  tk_app_df <- cbind(l_tc_app_L, m_tc_app_MRL)
  colnames(tk_app_df) <- c("Linac", "MRL" )
  rownames(tk_app_df) <- tf_MRL_th                 
  
  # Total costs (equipment + personnel) 
  # Table
  tk <- tk_app_df + tk_pers_df
  tk_df <- as.data.frame(tk)
  colnames(tk_df) <- c("Linac", "MRL" )
  rownames(tk_df) <- tf_MRL_th
  tk_df$Diff <- tk_df$"MRL" - tk_df$"Linac"
  
  tk_df$QALYs <- tk_df$Diff *(1/wtp)
  
  tk_df$Diff <- as.integer(tk_df$"MRL" - tk_df$"Linac")
  tk_df$time <- tf_MRL_th
  colnames (tk_df) <- c("Total costs Linac (€)", "Total costs MR-Linac (€)", "Difference compared to conventional treatment (€)", "QALYs needed to outweigh extra costs", "Fraction duration")
  
  table_time <- tk_df 
  table_time$"Total costs Linac ()" <- NULL
  table_time <- table_time[, c("Fraction duration", "Total costs MR-Linac (€)", "Difference compared to conventional treatment (€)", "QALYs needed to outweigh extra costs")]
  
  scale_ratio <- max(tk_df$`Difference compared to conventional treatment (€)`) / max(tk_df$`QALYs needed to outweigh extra costs`)
  breaks_QALYs <- seq(from = floor(min(tk_df$`QALYs needed to outweigh extra costs`)), 
                      to = ceiling(max(tk_df$`QALYs needed to outweigh extra costs`)), by = 0.02)
  
  plot_time <- ggplot(tk_df, aes(x = `Fraction duration`)) +
    geom_line(aes(y = `Difference compared to conventional treatment (€)`), color = "blue", linewidth = 1) +
    geom_line(aes(y = `QALYs needed to outweigh extra costs` * scale_ratio), color = "blue", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
    labs(x = "Time per fraction [min]", y = "Cost differences [euro] per atient", title = "Figure 2. Time per fraction MR-Linac treatment") +
    scale_y_continuous(
      name = "Cost difference [euro]",
      sec.axis = sec_axis(
        trans = ~./scale_ratio, 
        name = "QALYs needed to outweigh extra costs",
        breaks = breaks_QALYs
      )
    ) +
    scale_x_continuous(breaks = seq(0, max(tf_MRL_th), 10))
  
  # Results
  # Number of minutes cost difference is 0 
  max_tf <- approx(tk_df$`Difference compared to conventional treatment (€)`, tf_MRL_th, xout = 0)$y
  snijwaarde_time <- ifelse(is.na(max_tf),
                            "The value for which the costs are equal falls outside the tested range",
                            paste("MRgRT becomes more expensive after", round(max_tf, 1), "minutes per fraction")
  )
  
  table_time$`QALYs needed to outweigh extra costs`<- data.frame(ifelse(tk_df$Diff * (1 / wtp) < 0, "n/a", format(round(tk_df$Diff * (1 / wtp), 2), nsmall = 2)))
  
  print(snijwaarde_time)
  print(table_time)
  print(plot_time)
  
  
  #INVESTMENT (acquistion costs MR-Linac + QA)
  c_invest_MRL  <-  seq(0, 100000000, by = 1000000)
  
  # Total costs personnel 
  # Conventional treatment
  # Total costs
  tk_arts_L <- (tv_arts_L + tu_arts_L * f_L) * (c_arts/60) 
  tk_fys_L  <- (tv_fys_L +  tu_fys_L * f_L) * (c_fys/60) 
  tk_labo_L <- (tv_labo_L + tu_labo_L * f_L)  * (c_labo/60)
  
  tk_pers_L <- tk_arts_L + tk_fys_L + tk_labo_L + c_IMD_L 
  
  ##MR-Linac 
  # Total treatment costs
  tk_arts_MRL <- (tv_arts_MRL + tu_arts_MRL* f_MRL) * (c_arts/60)
  tk_fys_MRL  <- (tv_fys_MRL +  tu_fys_MRL * f_MRL) * (c_fys/60) 
  tk_labo_MRL <- (tv_labo_MRL + tu_labo_MRL *f_MRL)  * (c_labo/60)
  
  tk_pers_MRL <- tk_arts_MRL + tk_fys_MRL + tk_labo_MRL + c_IMD_MRL
  
  # Summarizing table personnel costs
  tk_pers_L <- rep(tk_pers_L, length(c_invest_MRL))
  tk_pers_MRL <- rep(tk_pers_MRL, length(c_invest_MRL))
  tk_pers <- data.frame(tk_pers_L, tk_pers_MRL)
  colnames(tk_pers) <- c("Linac", "MR-Linac")
  
  # Total equipment costs
  # Conventional treatment 
  tc_L <- ((c_aanschaf_L + c_qaequip_L)/t_ann_app) + (c_bunk_L/t_ann_bunk) + c_onderhoud_L       # costs per year
  tc_L_pm <- tc_L / (max_t_L*occ_t_L)                   # costs per minute Linac 
  tc_app_L <- tc_L_pm * (f_L*tf_L)                      # costs per treatment
  l_tc_app_L <- rep(round(tc_app_L, 2), length(c_invest_MRL)) 
  
  ### MR-Linac 
  tc_MRL <- ((c_invest_MRL)/t_ann_app + c_onderhoud_MRL) + (c_bunk_MRL/t_ann_bunk) # costs per year 
  c_MRL_pm <- tc_MRL / (max_t_MRL*occ_t_MRL)             # costs per minute MR-Linac 
  m_tc_app_MRL <- c_MRL_pm * (tf_MRL * f_MRL)            # costs for treatment scheme  
  
  ## Equipment cost table
  tk_app_df <- cbind(l_tc_app_L, m_tc_app_MRL)
  colnames(tk_app_df) <- c("Linac", "MRL" )
  rownames(tk_app_df) <- c_invest_MRL               
  
  # Total costs (equipment + personnel) 
  # Table
  tk_df <- tk_app_df + tk_pers 
  colnames(tk_df) <- c("TC L", "TC MRL")
  tk_df$invest <- c_invest_MRL
  tk_df$invest_M <- as.integer(c_invest_MRL * 1e-6)
  tk_df$Diff <- tk_df$"TC MRL" - tk_df$"TC L"
  
  tk_df$QALYs <- tk_df$Diff *(1/wtp)
  
  colnames (tk_df) <- c("Total costs Linac (€)", "Total costs MR-Linac (€)", "investment costs", "Investment costs in millions", "Difference compared to conventional treatment (€)", "QALYs needed to outweigh extra costs")
  
  scale_ratio <- max(tk_df$`Difference compared to conventional treatment (€)`) / max(tk_df$`QALYs needed to outweigh extra costs`)
  breaks_QALYs <- seq(from = floor(min(tk_df$`QALYs needed to outweigh extra costs`)), 
                      to = ceiling(max(tk_df$`QALYs needed to outweigh extra costs`)), by = 0.05)
  
  plot_invest <- ggplot(tk_df, aes(x = `investment costs`)) +
    geom_line(aes(y = `Difference compared to conventional treatment (€)`), color = "blue", linewidth = 1) +
    geom_line(aes(y = `QALYs needed to outweigh extra costs` * scale_ratio), color = "blue", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
    labs(x = 'Investment costs [in million euros]', y = "Cost difference [euro]", title = "Figure 3. Investment costs MR-Linac treatment") +
    scale_y_continuous(
      name = "Cost difference [euro]",
      sec.axis = sec_axis(
        trans = ~./scale_ratio, 
        name = "QALYs needed to outweigh extra costs",
        breaks = breaks_QALYs
      )
    ) +
    scale_x_continuous(
      breaks = seq(0, max(c_invest_MRL), 10000000), 
      labels = scales::label_number(scale = 1e-6, suffix = "M"))
  
  table_invest <- tk_df 
  table_invest$"Total costs Linac (€)" <- NULL
  table_invest <- table_invest[, c("Investment costs in millions", "Total costs MR-Linac (€)", 
                                   "Difference compared to conventional treatment (€)", "QALYs needed to outweigh extra costs")]
  
  
  
  # Number of fractions cost difference is 0 
  snijwaarde_benaderd <- approx(tk_df$Diff, tk_df$`Investment costs in millions`, xout = 0)$y
  snijwaarde_invest <- ifelse(is.na(snijwaarde_benaderd),
                              "The value for which the costs are equal falls outside the tested range",
                              paste("MRgRT becomes more expensive after", round(snijwaarde_benaderd, 1), "million euros investment costs for MR-Linac")
  )
  
  table_invest$`QALYs needed to outweigh extra costs`<- data.frame(ifelse(tk_df$Diff * (1 / wtp) < 0, "n/a", format(round(tk_df$Diff * (1 / wtp), 2), nsmall = 2)))
  
  print(snijwaarde_invest)
  print(table_invest)
  print(plot_invest)
  
  
  ##AVAILABLE TIME 
  max_t_MRL_th <- seq(from = 48000, to = 240000, by = 6000) #time in minutes
  
  # Total personnel costs 
  # Conventional treatment
  # Total costs
  tk_arts_L <- (tv_arts_L + tu_arts_L * f_L) * (c_arts/60) 
  tk_fys_L  <- (tv_fys_L +  tu_fys_L * f_L) * (c_fys/60) 
  tk_labo_L <- (tv_labo_L + tu_labo_L * f_L)  * (c_labo/60)
  
  tk_pers_L <- tk_arts_L + tk_fys_L + tk_labo_L  + c_IMD_L  
  tk_pers_L <- rep(tk_pers_L, length(max_t_MRL_th))
  
  ## MR-Linac 
  # Total treatment costs
  tk_arts_MRL <- (tv_arts_MRL + tu_arts_MRL* f_MRL) * (c_arts/60)
  tk_fys_MRL  <- (tv_fys_MRL +  tu_fys_MRL * f_MRL) * (c_fys/60) 
  tk_labo_MRL <- (tv_labo_MRL + tu_labo_MRL *f_MRL)  * (c_labo/60)
  
  tk_pers_MRL <- tk_arts_MRL + tk_fys_MRL + tk_labo_MRL + c_IMD_MRL
  
  # Summarizing table personnel costs
  tk_pers <- cbind(tk_pers_L, tk_pers_MRL)
  tk_pers_df <- tk_pers 
  colnames(tk_pers_df) <- c("Linac", "MRL" )
  rownames(tk_pers_df) <- max_t_MRL_th
  
  # Total equipment costs
  ### Conventional treatment
  tc_L <- ((c_aanschaf_L + c_qaequip_L)/t_ann_app + c_onderhoud_L) + (c_bunk_L/t_ann_bunk)       # costs per year
  tc_L_pm <- tc_L / (max_t_L*occ_t_L)                  # costs per minute Linac 
  tc_app_L <- tc_L_pm * (f_L*tf_L)                     # costs per treatment 
  l_tc_app_L <- rep(round(tc_app_L, 2), length(max_t_MRL_th))  
  
  ### MR-Linac 
  tc_MRL <- ((c_aanschaf_MRL + c_qaequip_MRL)/t_ann_app + c_onderhoud_MRL) + (c_bunk_MRL/t_ann_bunk) # costs per year 
  c_MRL_pm <- tc_MRL / (max_t_MRL_th*occ_t_MRL)           # costs per minute MR-Linac
  tc_app_MRL <- c_MRL_pm * (tf_MRL * f_MRL)
  
  ## Equipment costs table
  tk_app_df <- cbind(l_tc_app_L, tc_app_MRL)
  colnames(tk_app_df) <- c("Linac", "MRL" )
  rownames(tk_app_df) <- max_t_MRL_th
  
  # Table
  tk <- tk_app_df + tk_pers_df
  tk_df <- as.data.frame(tk)
  colnames(tk_df) <- c("Linac", "MRL" )
  rownames(tk_df) <- max_t_MRL_th
  tk_df$Diff <- tk_df$"MRL" - tk_df$"Linac"
  
  tk_df$QALYs <- tk_df$Diff *(1/wtp)
  tk_df$maxtime <- max_t_MRL_th 
  tk_df$maxtime_h <- as.integer(max_t_MRL_th *  1/60)
  colnames (tk_df) <- c("Total costs Linac (€)", "Total costs MR-Linac (€)", "Difference compared to conventional treatment (€)", "QALYs needed to outweigh extra costs", "Available time MR-Linac in minutes", "Available time MR-Linac in hours")
  
  
  scale_ratio <- max(tk_df$`Difference compared to conventional treatment (€)`) / max(tk_df$`QALYs needed to outweigh extra costs`)
  breaks_QALYs <- seq(from = floor(min(tk_df$`QALYs needed to outweigh extra costs`)), 
                      to = ceiling(max(tk_df$`QALYs needed to outweigh extra costs`)), by = 0.05)
  
  plot_maxtime <- ggplot(tk_df, aes(x = `Available time MR-Linac in minutes`)) +
    geom_line(aes(y = `Difference compared to conventional treatment (€)`), color = "blue", linewidth = 1) +
    geom_line(aes(y = `QALYs needed to outweigh extra costs` * scale_ratio), color = "blue", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
    labs(x = "Available time MRL [hour]", y = "Cost difference [euro]", title = "Figure 5. Available time MR-Linac") +
    scale_y_continuous(
      name = "Cost difference [euro]",
      sec.axis = sec_axis(
        trans = ~./scale_ratio, 
        name = "QALYs needed to outweigh extra costs",
        breaks = breaks_QALYs
      )
    ) +
    scale_x_continuous(breaks = seq(48000, max(max_t_MRL_th), 18000), 
                       labels = scales::label_number(scale = 1/60, suffix = "h"))
  
  table_maxtime <- tk_df 
  table_maxtime$"Total costs Linac (€)" <- NULL
  table_maxtime <- table_maxtime[, c("Available time MR-Linac in hours", "Total costs MR-Linac (€)", "Difference compared to conventional treatment (€)", "QALYs needed to outweigh extra costs")]
  
  
  #determine total available MRL hours cost difference is 0 
  snijwaarde_benaderd <- approx(tk_df$Diff, tk_df$'Available time MR-Linac in hours', xout = 0)$y
  snijwaarde_maxtime <- ifelse(is.na(snijwaarde_benaderd), "The value for which the costs are equal falls outside the tested range",
                               paste("MRgRT becomes more expensive after", round(snijwaarde_benaderd, 1), "total available MRL hours")
  )
  
  table_maxtime$`QALYs needed to outweigh extra costs`<- data.frame(ifelse(tk_df$Diff * (1 / wtp) < 0, "n/a", format(round(tk_df$Diff * (1 / wtp), 2), nsmall = 2)))
  
  
  print(snijwaarde_maxtime)
  print(table_maxtime)
  print(plot_maxtime)
  
  ##OCCUPANCY RATE
  occ_t_MRL_th <- seq(0.5, 1, by = 0.01)
  
  # Total personnel costs 
  # Conventional treatment
  # Total costs
  tk_arts_L <- (tv_arts_L + tu_arts_L * f_L) * (c_arts/60) 
  tk_fys_L  <- (tv_fys_L +  tu_fys_L * f_L) * (c_fys/60) 
  tk_labo_L <- (tv_labo_L + tu_labo_L * f_L)  * (c_labo/60)
  
  tk_pers_L <- tk_arts_L + tk_fys_L + tk_labo_L  + c_IMD_L  
  tk_pers_L <- rep(tk_pers_L, length(occ_t_MRL_th))
  
  ##MR-Linac 
  # Total treatment costs
  tk_arts_MRL <- (tv_arts_MRL + tu_arts_MRL* f_MRL) * (c_arts/60)
  tk_fys_MRL  <- (tv_fys_MRL +  tu_fys_MRL * f_MRL) * (c_fys/60) 
  tk_labo_MRL <- (tv_labo_MRL + tu_labo_MRL *f_MRL)  * (c_labo/60)
  
  ## Sum of total treatment costs
  tk_pers_MRL <- tk_arts_MRL + tk_fys_MRL + tk_labo_MRL + c_IMD_MRL
  tk_pers_MRL <- rep(tk_pers_MRL, length(occ_t_MRL_th))
  
  # Summarizing table personnel costs
  tk_pers <- cbind(tk_pers_L, tk_pers_MRL)
  tk_pers_df <- tk_pers 
  colnames(tk_pers_df) <- c("Linac", "MRL")
  
  #  Total equipment costs
  tc_L <- ((c_aanschaf_L + c_qaequip_L)/t_ann_app + c_onderhoud_L) + (c_bunk_L/t_ann_bunk)       # costs per year
  tc_L_pm <- tc_L / (max_t_L*occ_t_L)                  # costs per minute Linac 
  tc_app_L <- tc_L_pm * (f_L*tf_L)                     # costs per treatment
  l_tc_app_L <- rep(round(tc_app_L, 2), length(occ_t_MRL_th))  
  
  ### MR-Linac 
  tc_MRL <- ((c_aanschaf_MRL + c_qaequip_MRL)/t_ann_app + c_onderhoud_MRL) + (c_bunk_MRL/t_ann_bunk) # costs per year 
  c_MRL_pm <- tc_MRL / (max_t_MRL*occ_t_MRL_th)           # costs per minute MR-Linac
  tc_app_MRL <- c_MRL_pm * (tf_MRL * f_MRL)          # costs for treatment scheme 
  
  ## equipment cost table
  tk_app_df <- cbind(l_tc_app_L, tc_app_MRL)
  colnames(tk_app_df) <- c("Linac", "MRL" )
  rownames(tk_app_df) <- occ_t_MRL_th          
  
  # Total cost (equipment + personnel) 
  # Table
  tk <- tk_app_df + tk_pers_df
  tk_df <- as.data.frame(tk)
  colnames(tk_df) <- c("Linac", "MRL" )
  rownames(tk_df) <- occ_t_MRL_th
  tk_df$Diff <- tk_df$"MRL" - tk_df$"Linac"
  
  tk_df$QALYs <- tk_df$Diff *(1/wtp)
  
  tk_df$occ <- occ_t_MRL_th
  tk_df$occ_p <- as.integer(occ_t_MRL_th * 100)
  colnames (tk_df) <- c("Total costs Linac (€)", "Total costs MR-Linac (€)", "Difference compared to conventional treatment (€)", "QALYs needed to outweigh extra costs", "Occupancy rate MRL", "Occupancy % MRL")
  
  
  scale_ratio <- max(tk_df$`Difference compared to conventional treatment (€)`) / max(tk_df$`QALYs needed to outweigh extra costs`)
  breaks_QALYs <- seq(from = floor(min(tk_df$`QALYs needed to outweigh extra costs`)), 
                      to = ceiling(max(tk_df$`QALYs needed to outweigh extra costs`)), by = 0.05)
  
  plot_occ <- ggplot(tk_df, aes(x = `Occupancy rate MRL`)) +
    geom_line(aes(y = `Difference compared to conventional treatment (€)`), color = "blue", linewidth = 1) +
    geom_line(aes(y = `QALYs needed to outweigh extra costs` * scale_ratio), color = "blue", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
    labs(x = "Occupancy MRL [%]", y = "Cost difference [euro]", title = "Figure 4. Occupancy MR-Linac") +
    scale_y_continuous(
      name = "Cost difference [euro]",
      sec.axis = sec_axis(
        trans = ~./scale_ratio, 
        name = "QALYs needed to outweigh extra costs",
        breaks = breaks_QALYs
      )
    ) +
    scale_x_continuous(breaks = seq(0, max(occ_t_MRL_th), 0.1), 
                       labels = scales::label_number(scale = 100, suffix = "%"))
  
  table_occ <- tk_df 
  table_occ$"Total costs Linac (€)" <- NULL
  table_occ <- table_occ[, c("Occupancy % MRL", "Total costs MR-Linac (€)", "Difference compared to conventional treatment (€)", "QALYs needed to outweigh extra costs")]
  
  
  #bepalen kosten verschil is 0 
  snijwaarde_benaderd <- approx(tk_df$Diff, tk_df$"Occupancy % MRL", xout = 0)$y
  snijwaarde_occ <- ifelse(  is.na(snijwaarde_benaderd),
                             "The value for which the costs are equal falls outside the tested range",
                             paste("MRgRT becomes more expensive after", round(snijwaarde_benaderd, 1), "% occupancy")
  )
  
  table_occ$`QALYs needed to outweigh extra costs`<- data.frame(ifelse(tk_df$Diff * (1 / wtp) < 0, "n/a", format(round(tk_df$Diff * (1 / wtp), 2), nsmall = 2)))

  print(snijwaarde_occ)
  print(table_occ)
  print(plot_occ)
  
  ##Berekenen prijs per minuut
  ### MR-LINAC COST PER MINUTE PIE CHART IN % 
  tv_arts_MRL_pie <- tv_arts_MRL
  tv_fys_MRL_pie <- tv_fys_MRL
  tv_labo_MRL_pie  <-  tv_labo_MRL 
  c_aanschaf_MRL_pie  <-  c_aanschaf_MRL  
  c_bunk_MRL_pie  <-      c_bunk_MRL     
  c_qaequip_MRL_pie  <-    c_qaequip_MRL   
  c_IMD_MRL_pie  <-      c_IMD_MRL    
  c_onderhoud_MRL_pie  <-  c_onderhoud_MRL
  c_arts_pie   <- c_arts
  c_fys_pie  <- c_fys 
  c_labo_pie   <- c_labo
  perc_arts_MRL_pie  <- perc_arts_MRL
  perc_fys_MRL_pie  <- perc_fys_MRL
  perc_labo_MRL_pie  <- perc_labo_MRL
  tu_labo_MRL_pie <- tf_MRL *n_labo_MRL
  tu_arts_MRL_pie <- tf_MRL *n_arts_MRL 
  tu_fys_MRL_pie <- tf_MRL *n_fys_MRL 
  
  ##Hypothetical scenerio (total cost per minute)
  #Totale kosten personele inzet 
  ##MR-Linac 
  # Total treatment costs
  tk_arts_MRL <- (tv_arts_MRL + tu_arts_MRL* f_MRL) * (c_arts/60)
  tk_fys_MRL  <- (tv_fys_MRL +  tu_fys_MRL * f_MRL) * (c_fys/60) 
  tk_labo_MRL <- (tv_labo_MRL + tu_labo_MRL *f_MRL)  * (c_labo/60)
  
  ## Sum of total treatment costs
  tk_pers_MRL <- tk_arts_MRL + tk_fys_MRL + tk_labo_MRL + c_IMD_MRL
  tk_pers_MRL_pm <- tk_pers_MRL / (f_MRL*tf_MRL)
  
  #  Total equipment costs
  ### MR-Linac 
  tc_MRL <- ((c_aanschaf_MRL + c_qaequip_MRL)/t_ann_app + c_onderhoud_MRL) + (c_bunk_MRL/t_ann_bunk) # costs per year 
  c_MRL_pm <- tc_MRL / (max_t_MRL*occ_t_MRL)                                                         # costs per minute MR-Linac 
  tc_app_MRL <- c_MRL_pm * (tf_MRL * f_MRL)            # costs for treatment scheme 
  
  # Total costs
  AAA <- total_costs_PM <- tk_pers_MRL_pm + c_MRL_pm
  ABB <-total_costs <- tk_pers_MRL + tc_app_MRL
  
  AAA <- round(AAA, 1)
  
  ## Set all staffing variables to 0 
  tv_arts_MRL <- 0 
  tp_arts_MRL <- 0 
  tu_arts_MRL <- 0 
  tv_fys_MRL <- 0
  tp_fys_MRL <- 0
  tu_fys_MRL <- 0 
  ttv_labo_MRL <- 0
  tu_labo_MRL <- 0
  
  ## Set all device related variables to 0
  c_aanschaf_MRL  <-  0 
  c_bunk_MRL      <-  0 
  c_qaequip_MRL   <-  0      
  c_IMD_MRL       <-  0 
  c_onderhoud_MRL <-  0 
  
  
  ## Aquisition costs bunker MR-Linac system (rest = 0)
  BAA <- ((c_aanschaf_MRL_pie/t_ann_app) / (max_t_MRL*occ_t_MRL)) 
  BBB <- ((c_aanschaf_MRL_pie/t_ann_app) / (max_t_MRL*occ_t_MRL)) * (tf_MRL * f_MRL)
  
  BAA <- round(BAA, 1)
  
  ## costs bunker MR-Linac system (rest = 0)
  CAA <- ((c_bunk_MRL_pie/t_ann_bunk) / (max_t_MRL*occ_t_MRL)) 
  CBB <- ((c_bunk_MRL_pie/t_ann_bunk) / (max_t_MRL*occ_t_MRL)) * (tf_MRL * f_MRL)
  
  ## QA MR-Linac system (rest = 0)
  DAA <- ((c_qaequip_MRL_pie/t_ann_app) / (max_t_MRL*occ_t_MRL)) 
  DBB <- ((c_qaequip_MRL_pie/t_ann_app) / (max_t_MRL*occ_t_MRL)) * (tf_MRL * f_MRL)
  
  ## Annual maintenance costs (rest = 0)
  EAA <- (c_onderhoud_MRL_pie) / (max_t_MRL*occ_t_MRL) 
  EBB <- (c_onderhoud_MRL_pie / (max_t_MRL*occ_t_MRL)) * (tf_MRL * f_MRL)
  
  ##Staffing 
  ##RTT's 
  tk_labo_MRL_pie <- (tv_labo_MRL_pie + (tu_labo_MRL_pie*f_MRL*(perc_labo_MRL/100)))  * (c_labo/60)
  
  FAA <- tk_labo_MRL_pie / (tf_MRL * f_MRL) 
  FBB <- tk_labo_MRL_pie 
  
  ## Radiation oncologists 
  tk_arts_MRL_pie <- (tv_arts_MRL_pie + (tu_arts_MRL_pie *f_MRL)) * (perc_arts_MRL/100) * (c_arts/60)
  GAA <- tk_arts_MRL_pie / (tf_MRL * f_MRL) 
  GBB <- tk_arts_MRL_pie 
  
  ## Clinical physicists 
  tk_fys_MRL  <- (tv_fys_MRL_pie + (tu_fys_MRL_pie*f_MRL*(perc_fys_MRL/100))) * (c_fys/60) 
  HAA <- tk_fys_MRL / (tf_MRL * f_MRL) 
  HBB <- tk_fys_MRL 
  
  ## MR-simulation 
  IAA <- c_IMD_MRL_pie/ (tf_MRL * f_MRL) 
  IBB <- c_IMD_MRL_pie
  
  # Dataframe results
  # Total check: 
  Total_check <- BAA + CAA + DAA + EAA + FAA + GAA + HAA + IAA
  Total_MedicalDevice <- BAA + CAA + DAA + EAA 
  Total_Staffing <- FAA + GAA + HAA + IAA 
  
  SUM <- Total_MedicalDevice  + Total_Staffing 
  
  
  Tabel_Pie_MRL <- data.frame(
    Category = c("Medical device", "Bunker construction", "Quality assurance or other related equipment", 
                 "Annual maintenance costs", "Costs radiation oncologists", "Costs clinical physicist", 
                 "Costs radiotherapy technologist", "MR simulation", "total costs per minute", "total costs per minute Medical Device" , "total costs per minute staffing"),
    Cost = c(round(BAA, 1), round(CAA, 1),  round(DAA, 1), round(EAA, 1), round(FAA, 1), round(GAA, 1), round(HAA, 1), round(IAA, 1), round(AAA, 1), round(Total_MedicalDevice, 1), round(Total_Staffing, 1))
  )
  Tabel_Pie_MRL$percentage <- round(((Tabel_Pie_MRL$Cost / AAA) * 100), 0)
  
  Pie_MRL <- Tabel_Pie_MRL[1:8,]
  
  labels <- ifelse(Pie_MRL$percentage == 0, "", paste(Pie_MRL$Category, "\n", Pie_MRL$percentage, "%"))
  pie(Pie_MRL$percentage, labels = labels, main = "Costs per minute per cost category", cex = 0.5) 
  
  colors <- c("Medical device" = "lightblue", 
              "Bunker construction" = "blue", 
              "Quality assurance or other related equipment" = "lightblue4", 
              "Annual maintenance costs" = "deepskyblue",
              "Costs radiation oncologists" = "lightgreen", 
              "Costs clinical physicist" = "green", 
              "Costs radiotherapy technologist" = "darkgreen", 
              "MR simulation" = "yellow")
  
  Pie_MRL$Category <- factor(Pie_MRL$percentage, levels = c("Medical device", 
                                                            "Bunker construction", 
                                                            "Quality assurance or other related equipment", 
                                                            "Annual maintenance costs", 
                                                            "Costs radiation oncologists", 
                                                            "Costs clinical physicist", 
                                                            "Costs radiotherapy technologist", 
                                                            "MR simulation"))
  
  Pie_MRL <- Tabel_Pie_MRL[1:8,]
  
  pie_plot_MRL <- ggplot(Pie_MRL, aes(x = "", y = percentage, fill = Category)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    theme_void() +
    geom_text(aes(label = ifelse(percentage == 0, "", paste0(percentage, "%"))), 
              position = position_stack(vjust = 0.5),  # Alleen position_stack gebruiken
              size = 3,  # Pas de tekstgrootte aan
              vjust = -0.7) +  # Verplaats de tekst verder naar buiten
    labs(title = "Cost breakdown per minute of MR-Linac treatment") +
    scale_fill_manual(values = colors)
  print(pie_plot_MRL)
  
  #A: "Total"					
  #B: "Medical device"
  #C: "Bunker construction"
  #D: "Quality assurance or other related equipment"
  #E: "Annual maintenance costs"
  #F: "Costs radiation oncologists"
  #G: "Costs clinical physicist"
  #H: "Costs radiotherapy technologist"
  #I: "MR simulation"
  #J: "Total medical device"
  #K: "Total staffing"
  
  ### CONVENTIONAL TREATMENT COST PER MINUTE PIE CHART IN % 
  tv_arts_L_pie <- tv_arts_L
  tv_fys_L_pie <- tv_fys_L
  tv_labo_L_pie  <-  tv_labo_L 
  c_aanschaf_L_pie  <-  c_aanschaf_L  
  c_bunk_L_pie  <-      c_bunk_L     
  c_qaequip_L_pie  <-    c_qaequip_L   
  c_IMD_L_pie  <-      c_IMD_L    
  c_onderhoud_L_pie  <-  c_onderhoud_L
  c_arts_pie   <- c_arts
  c_fys_pie  <- c_fys 
  c_labo_pie   <- c_labo
  perc_arts_L_pie  <- perc_arts_L
  perc_fys_L_pie  <- perc_fys_L
  perc_labo_L_pie  <- perc_labo_L
  tu_labo_L_pie <- tf_L *n_labo_L
  tu_arts_L_pie <- tf_L *n_arts_L 
  tu_fys_L_pie <- tf_L *n_fys_L 
  
  # Totale kosten personele inzet 
  ## MR-Linac 
  # Total treatment costs
  tk_arts_L <- (tv_arts_L + tu_arts_L* f_L) * (c_arts/60)
  tk_fys_L  <- (tv_fys_L +  tu_fys_L * f_L) * (c_fys/60) 
  tk_labo_L <- (tv_labo_L + tu_labo_L *f_L)  * (c_labo/60)
  
  ## Sum of total treatment costs
  tk_pers_L <- tk_arts_L + tk_fys_L + tk_labo_L + c_IMD_L
  tk_pers_L_pm <- tk_pers_L / (f_L*tf_L)
  
  # Total equipment costs
  # Conventional treatment 
  tc_L <- ((c_aanschaf_L + c_qaequip_L)/t_ann_app + c_onderhoud_L) + (c_bunk_L/t_ann_bunk) # costs per year 
  c_L_pm <- tc_L / (max_t_L*occ_t_L)                                                         # costs per minute MR-Linac 
  tc_app_L <- c_L_pm * (tf_L * f_L)            # costs for treatment scheme 
  
  # Total costs
  MAA <- total_costs_PM_L <- tk_pers_L_pm + c_L_pm
  MBB <-total_costs_L <- tk_pers_L + tc_app_L
  
  MAA <- round(MAA, 1)
  
  ## Set all staffing variables to 0 
  tv_arts_L <- 0 
  tp_arts_L <- 0 
  tu_arts_L <- 0 
  tv_fys_L <- 0
  tp_fys_L <- 0
  tu_fys_L <- 0 
  ttv_labo_L <- 0
  tu_labo_L <- 0
  
  ## Set all device related variables to 0
  c_aanschaf_L  <-  0 
  c_bunk_L      <-  0 
  c_qaequip_L   <-  0      
  c_IMD_L       <-  0 
  c_onderhoud_L <-  0 
  
  ## Aquisition costs conventional system (rest = 0)
  NAA <- ((c_aanschaf_L_pie/t_ann_app) / (max_t_L*occ_t_L)) 
  NBB <- ((c_aanschaf_L_pie/t_ann_app) / (max_t_L*occ_t_L)) * (tf_L * f_L)
  
  NAA <- round(NAA, 1)
  
  ## Costs bunker MR-Linac system (rest = 0)
  OAA <- ((c_bunk_L_pie/t_ann_bunk) / (max_t_L*occ_t_L)) 
  OBB <- ((c_bunk_L_pie/t_ann_bunk) / (max_t_L*occ_t_L)) * (tf_L * f_L)
  
  ## QA MR-Linac system (rest = 0)
  PAA <- ((c_qaequip_L_pie/t_ann_app) / (max_t_L*occ_t_L)) 
  PBB <- ((c_qaequip_L_pie/t_ann_app) / (max_t_L*occ_t_L)) * (tf_L * f_L)
  
  ## Annual maintenance costs (rest = 0)
  QAA <- (c_onderhoud_L_pie) / (max_t_L*occ_t_L) 
  QBB <- (c_onderhoud_L_pie / (max_t_L*occ_t_L)) * (tf_L * f_L)
  
  ##Staffing 
  ##RTT's 
  tk_labo_L_pie <- (tv_labo_L_pie + (tu_labo_L_pie*f_L*(perc_labo_L/100)))  * (c_labo/60)
  
  RAA <- tk_labo_L_pie / (tf_L * f_L) 
  RBB <- tk_labo_L_pie 
  
  ## Radiation oncologists 
  tk_arts_L_pie <- (tv_arts_L_pie + (tu_arts_L_pie *f_L)) * (perc_arts_L/100) * (c_arts/60)
  SAA <- tk_arts_L_pie / (tf_L * f_L) 
  SBB <- tk_arts_L_pie 
  
  ## Clinical physicists 
  tk_fys_L  <- (tv_fys_L_pie + (tu_fys_L_pie*f_L*(perc_fys_L/100))) * (c_fys/60) 
  TAA <- tk_fys_L / (tf_L * f_L) 
  TBB <- tk_fys_L 
  
  ## MR-simulation 
  UAA <- c_IMD_L_pie/ (tf_L * f_L) 
  UBB <- c_IMD_L_pie
  
  # Dataframe
  # Total check: 
  Total_check <- NAA + OAA + PAA + QAA + RAA + SAA + TAA + UAA
  Total_MedicalDevice_L <- NAA + OAA + PAA + QAA 
  Total_Staffing_L <- RAA + SAA + TAA  
  Total_MR_L <- UAA 
  
  SUM <- Total_MedicalDevice_L  + Total_Staffing_L + Total_MR_L
  
  Tabel_Pie_L <- data.frame(
    Category = c("Medical device", "Bunker construction", "Quality assurance or other related equipment", 
                 "Annual maintenance costs", "Costs radiation oncologists", "Costs clinical physicist", 
                 "Costs radiotherapy technologist", "MR simulation", "total costs per minute", "total costs per minute Medical Device" , "total costs per minute staffing"),
    Cost = c(round(NAA, 1), round(OAA, 1),  round(PAA, 1), round(QAA, 1), round(RAA, 1), round(SAA, 1), round(TAA, 1), round(UAA, 1), round(MAA, 1), round(Total_MedicalDevice_L, 1), round(Total_Staffing_L, 1))
  )
  Tabel_Pie_L$percentage <- round(((Tabel_Pie_L$Cost / MAA) * 100), 0)
  
  Pie_L <- Tabel_Pie_L[1:8,]
  
  labels <- ifelse(Pie_L$percentage == 0, "", paste(Pie_L$Category, "\n", Pie_L$percentage, "%"))
  pie(Pie_L$percentage, labels = labels, main = "Costs per minute per cost category", cex = 0.5) 
  
  colors <- c("Medical device" = "lightblue", 
              "Bunker construction" = "blue", 
              "Quality assurance or other related equipment" = "lightblue4", 
              "Annual maintenance costs" = "deepskyblue",
              "Costs radiation oncologists" = "lightgreen", 
              "Costs clinical physicist" = "green", 
              "Costs radiotherapy technologist" = "darkgreen", 
              "MR simulation" = "yellow")
  
  Pie_L$Category <- factor(Pie_L$percentage, levels = c("Medical device", 
                                                        "Bunker construction", 
                                                        "Quality assurance or other related equipment", 
                                                        "Annual maintenance costs", 
                                                        "Costs radiation oncologists", 
                                                        "Costs clinical physicist", 
                                                        "Costs radiotherapy technologist", 
                                                        "MR simulation"))
  Pie_L <- Tabel_Pie_L[1:8,]
  
  pie_plot_L <- ggplot(Pie_L, aes(x = "", y = percentage, fill = Category)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    theme_void() +
    geom_text(aes(label = ifelse(percentage == 0, "", paste0(percentage, "%"))), 
              position = position_stack(vjust = 0.5),  # Alleen position_stack gebruiken
              size = 3,  # Pas de tekstgrootte aan
              vjust = -0.7) +  # Verplaats de tekst verder naar buiten
    labs(title = "Cost breakdown per minute of conventional treatment") +
    scale_fill_manual(values = colors)
  
  print(pie_plot_L)
  
  
  #M: "Total"					
  #N: "MR-Linac system"
  #O: "Bunker construction"
  #P: "Quality assurance or other related equipment"
  #Q: "Annual maintenance costs"
  #R: "Costs radiation oncologists"
  #S: "Costs clinical physicist"
  #T: "Costs radiotherapy technologist"
  #U: "MR simulation"
  #V: "Total medical device"
  #W: "Total staffing"
  
 
    results <- 
    list(
      plot_fractions = plot_fractions, 
      snijwaarde_fractions = snijwaarde_fractions,
      table_fractions = table_fractions, 
      plot_time = plot_time, 
      snijwaarde_time = snijwaarde_time,
      table_time = table_time, 
      table_invest = table_invest, 
      plot_invest = plot_invest, 
      snijwaarde_invest = snijwaarde_invest, 
      table_maxtime = table_maxtime, 
      plot_maxtime = plot_maxtime, 
      snijwaarde_maxtime = snijwaarde_maxtime, 
      table_occ = table_occ, 
      plot_occ = plot_occ, 
      snijwaarde_occ = snijwaarde_occ, 
      pie_plot_MRL = pie_plot_MRL, 
      pie_plot_L = pie_plot_L)
}

