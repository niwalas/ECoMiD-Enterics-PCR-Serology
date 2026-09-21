
#-------------------------------
# 0-config.R
# configure data directories
# source base functions
# load libraries
#-------------------------------

#-------------------------------
# helper packages
#-------------------------------
library(here)
library(tidyverse)
library(kableExtra)
library(skimr)
library(broom)
library(readxl)
library(writexl)
library(openxlsx)
library(ggrepel)
library(forcats)
library(scales)
library(gt)
library(survival)

#-------------------------------
# graphics packages
#-------------------------------
library(patchwork)
library(viridis)
library(cowplot)
library(RColorBrewer)
library(ggtext)
library(ggh4x)
library(corrplot)
library(ggcorrplot)
library(table1)

#-------------------------------
# statistics packages
#-------------------------------
library(sandwich)
library(lmtest)

#------------------------------------
# helper function: calc seroreversion and seroconversion rate (in scripts 5 and 6)
#------------------------------------
est_ir <- function(seroconv, pt) {
  nsc  <- sum(seroconv, na.rm = TRUE)
  ptar <- sum(pt,        na.rm = TRUE)
  if (ptar <= 0) return(NA_real_)
  nsc / ptar
}

#-------------------------------
# define directories
#-------------------------------

#-------------------------------
# custom color palettes
#-------------------------------

# safe color blind palette
# http://jfly.iam.u-tokyo.ac.jp/color/
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# Reference: Bang Wong, Nature Methods 2011: https://www.nature.com/articles/nmeth.1618
# Reference: Bang Wong, Nature Methods 2011: https://www.nature.com/articles/nmeth.1618
cbpal <- c("#CC79A7", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#999999", "#0072B2", "#F0E442")

# NY Times rainbow from the Upshot
# https://www.nytimes.com/interactive/2020/03/21/upshot/coronavirus-deaths-by-country.html
nytpal <- c("#510000", "#AC112D", "#EC6D47", "#F2A058", "#F7D269", "#839772", "#325D8A")

# bright color blind palette:  https://personal.sron.nl/~pault/ 
cblack <- "#000004FF"
cblue <- "#3366AA"
cteal <- "#11AA99"
cgreen <- "#66AA55"
cchartr <- "#CCCC55"
cmagent <- "#992288"
cred <- "#EE3333"
corange <- "#EEA722"
cyellow <- "#FFEE33"
cgrey <- "#777777"
