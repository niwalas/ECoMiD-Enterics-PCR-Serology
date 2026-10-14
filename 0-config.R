
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
#------------------------------------
# helper function: prevalence 95% CI with child-clustered robust SE
# intercept-only logistic model, Wald CI on the logit scale, back-transformed.
# Falls back to exact (Clopper-Pearson) CI when prevalence is 0% or 100%,
# where the logit is undefined.
#------------------------------------
robust_prev_ci <- function(d) {
  n   <- sum(!is.na(d$seropos))
  pos <- sum(d$seropos == 1, na.rm = TRUE)
  if (pos == 0 || pos == n) {
    ci <- binom.test(pos, n)$conf.int
    return(tibble(lower_ci = ci[1], upper_ci = ci[2]))
  }
  m   <- glm(seropos ~ 1, family = binomial(), data = d)
  se  <- sqrt(sandwich::vcovCL(m, cluster = d$childid))[1]
  est <- coef(m)[1]
  tibble(lower_ci = plogis(est - 1.96 * se),
         upper_ci = plogis(est + 1.96 * se))
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
