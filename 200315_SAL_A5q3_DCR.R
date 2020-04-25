# A5 script
# RECS analysis file
# D. Cale Reeves
# UT Austin, LBJ School
# SAL2020
# Created: 2020/03/15
#
# 

#---------------#
# Run Parameters 
#---------------#
options("width"=180)

OutFile <- file("200315_SAL_A5q3_sol_DCR_Out.txt")
sink(OutFile, append=FALSE, type=c("output"))
sink(OutFile, append=FALSE, type=c("message"))
#set.seed(8675309)

#setwd("/Users/dcr953/Desktop/Box Sync/Teaching/20_Spring/SAL/Assignments/A5")

#---------------#
# Libraries
#---------------#

library(MASS)
library(ggplot2)
library(stats)
library(gridExtra)

#---------------#
# Make Data
#---------------#

data_gen <- function (n, k, p) {
	simX <<- data.frame(ID=seq(1:n))
	simX$truecluster <<- as.factor(sample(seq(1:k), n, replace = TRUE))
	simX_tmp <<- NULL
	sapply(seq(1:k), function (i) {	
		temp_p <- qr.Q(qr(matrix(rnorm(p^2), p)))
		Sigma_k <- abs(crossprod(temp_p, temp_p*(p:1)))
		simX_tmp_k <<- as.data.frame(mvrnorm(n=nrow(simX[simX$truecluster == i,]), mu=runif(p, min = 0, max = 10), Sigma=Sigma_k))
		colnames(simX_tmp_k) <<- paste0("X", seq(1:p))
		simX_tmp_k$ID <<- simX$ID[simX$truecluster == i]
		simX_tmp <<- rbind(simX_tmp, simX_tmp_k)	
	})
	simData <<- merge(simX, simX_tmp, by="ID")
}

data_gen(100, 4, 2)
excludes <- names(simData) %in% c("ID","truecluster") 

simData[!excludes]
