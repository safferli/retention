rm(list = ls()); gc(); gc()

require(devtools)
library(httr)
set_config(use_proxy(url="10.26.0.16", port=3128))
install_github("ramnathv/slidify")
install_github("ramnathv/slidifyLibraries")

library(slidify)

wd <- "M:/Documents/games/gitlab/internal_presentations/retention"
#wd <- "C:/Users/christoph.safferling/Documents/games/gitlab/internal_presentations/retention"
setwd(wd)
author("retention")

slidify("index.Rmd")
