# Input for shiny application Flexible and generic tool to facilitate early exploration of MR-Linac's value for money (Marike Ulehake)

rm(list = ls())      

## Treatment scheme and number of fractions
# Conventional treatment
f_L <-  20                         # number of fractions
tf_L <- 15                         # minutes per fraction

# MR-Linac treatment
f_MRL_range <- c(1:15)             # range  1-15 fractions

f_MRL <- 5                         # number of fractions
tf_MRL <- 60                       # minutes per fraction 

# Utilization 
a_t_L <-  1813                     # yearly operating hours  

occ_t_L <- 1                       # full capacity = 100%
occ_t_MRL <- 1                     # full capacity = 100%

## Costs (euros)
#  Personnel costs 
c_arts <- 105                      # gross hourly wage
c_fys <- 93                        # gross hourly wage
c_labo <- 49                       # gross hourly wage

c_IMD_MRL <- 358                   # MR simulation 
c_IMD_L <- 358                     # MR simulation 

# Personnel time 
# Conventional treatment  

# Professionals present during treatment 
n_labo_L <- as.numeric(3)          # radiotherapy technologist(s)
n_arts_L <- as.numeric(1)          # radiation oncologist(s)
n_fys_L  <- as.numeric(1)          # clinical physicists(s)

# % of time available
perc_labo_L <- 100                 # radiotherapy technologist(s)
perc_arts_L <- 20                  # radiation oncologist(s)
perc_fys_L <- 20                   # clinical physicists(s)

# Pre-treatment time (per minute, per person)  
tv_labo_L     <- 210               # radiotherapy technologist(s)
tv_arts_L     <- 135               # radiation oncologist(s)                              
tv_fys_L      <- 15                # clinical physicists(s)                  

# MR-Linac 
# Professionals present during treatment 
n_labo_MRL <- as.numeric(3)        # radiotherapy technologist(s)
n_arts_MRL <- as.numeric(1)        # radiation oncologist(s)       
n_fys_MRL  <- as.numeric(1)        # clinical physicists(s)  

# % of time available
perc_labo_MRL <- 100               # radiotherapy technologist(s)
perc_arts_MRL <- 20                # radiation oncologist(s)
perc_fys_MRL <- 20                 # clinical physicists(s)

# Pre-treatment time (per minute, per person)
tv_labo_MRL <- 210                 # radiotherapy technologist(s)
tv_arts_MRL <- 135                 # radiation oncologist(s)             
tv_fys_MRL <- 15                   # clinical physicists(s)

# Medical devices 
t_afschr_app    <-    12           # useful life years 
t_afschr_bunk   <-    20           # useful life years 
p_rente         <-   2.5           # annual interest rate

# Linac 
c_aanschaf_L    <-  2000000       # MR-Linac system 
c_qaequip_L     <-        0       # Quality assurance or other related equipment)
c_onderhoud_L   <-   140000       # Annual maintenance costs
c_bunk_L        <-   500000       # Bunker construction

# MR-Linac 
c_aanschaf_MRL  <-  9500000       # conventional system 
c_qaequip_MRL   <-        0       # quality assurance or other related equipment
c_onderhoud_MRL <-   665000       # annual maintenance costs
c_bunk_MRL     <-   2500000       # bunker construction

# Willingness to pay threshold
wtp <- 80000
