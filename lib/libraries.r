library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(tmap)
library(shiny)

library(dplyr)
library(readr)
library(XML)
library(raster)
library(gridExtra)
library(caret)
library(scales)
library(kableExtra)


options(gsubfn.engine="R")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding="UTF-8")
