# Estimate changepoints and extreme events using the BEAST method.


rm(list=ls())

library(rstudioapi)
library(Rbeast)

setwd(dirname(getActiveDocumentContext()$path))

load('./USGS-streamflow_p1.RData');  q1=qDF;
load('./USGS-streamflow_p2.RData');  q2=qDF;

rm("qDF")

T = q1$Date
Q = cbind(q1[,2:ncol(q1)],  q2[,2:ncol(q2)])

rm("q1","q2")
gc()

Qmat      = as.matrix(Q)
dim(Qmat) = c(nrow(Q), ncol(Q))
rm("Q")
LQ        = log1p(Qmat)


#......Start of displaying 'MetaData' ......
metadata                = list()     # metadata is used to interpret the input data
metadata$season         = 'svd' # fit a harmonic model to the periodic component
metadata$deltaTime      = "1/12 yearr"  # 0.0833333 year(s) = 1 month(s) = 30.4167 day(s)
metadata$time           = T
metadata$period         = 1          # 1 year(s) = 12 month(s) = 365 day(s) 
metadata$maxMissingRate = 0.75       # if more than 75% of data is missing, BEAST will skip it.
metadata$hasOutlier     = TRUE
#........End of displaying MetaData ........

#......Start of displaying 'prior' ......
prior                   = list()     # prior is the key model parameters of BEAST
prior$seasonMinOrder    = 1          # sorder.minmax[1]: min harmonic order alllowed
prior$seasonMaxOrder    = 5          # sorder.minmax[2]: max harmonic order alllowed
prior$seasonMinKnotNum  = 0          # scp.minmax[1]   : min num of seasonal chngpts
prior$seasonMaxKnotNum  = 10         # scp.minmax[2]   : max num of seasonal chngpts
prior$seasonMinSepDist  = 24         # sseg.min        : min seasonal segment length in terms of datapoints
prior$trendMinOrder     = 0          # torder.minmax[1]: min trend polynomial order alllowed
prior$trendMaxOrder     = 1          # torder.minmax[2]: max trend polynomial order alllowed

prior$trendMinKnotNum   = 0          # tcp.minmax[1]   : min num of chngpts in trend
prior$trendMaxKnotNum   = 10         # tcp.minmax[2]   : min num of chngpts in trend

prior$trendMinSepDist   = 24          # tseg.min        : min trend segment length in terms of datapoints
prior$precValue         = 1.5        # useful mainly when precPriorType='constant'
prior$modelPriorType    = 1         
prior$precPriorType     = 'uniform'
prior$outlierMaxKnotNum  = 20         # scp.minmax[2]   : max num of seasonal chngpts
#......End of displaying prior ......

#......Start of displaying 'mcmc' ......
mcmc                           = list()     # mcmc is not BEAST parameters but MCMC sampler options
mcmc$seed                      = 0          # A nonzero seed to replicate among runs
mcmc$samples                   = 8000       # Number of samples saved per chain: the larger, the better
mcmc$thinningFactor            = 5          # Thinning the chain: the larger, the better 
mcmc$burnin                    = 200        # Number of inital samples discarded: the larger, the better
mcmc$chainNumber               = 3          # Nunber of chains: the larger, the better
mcmc$maxMoveStepSize           = 12         # Max step of jumping from current changepoint: No need to change
mcmc$trendResamplingOrderProb  = 0.1        # Proposal probability of sampling trend polynominal order 
mcmc$seasonResamplingOrderProb = 0.17       # Proposal probability of sampling seasoanl order 
mcmc$credIntervalAlphaLevel    = 0.95       # The alphal level for Credible Intervals
# Total number of models randomly visited is (burnin+sampples*thinFactor)*chainNumber=120600
#......End of displaying mcmc ......

#......Start of displaying 'extra' ......
extra                      = list()     # extra is used to configure output/computing options
extra$dumpInputData        = TRUE
extra$whichOutputDimIsTime = 1
extra$computeCredible      = FALSE
extra$fastCIComputation    = TRUE
extra$computeSeasonOrder   = TRUE
extra$computeTrendOrder    = TRUE
extra$computeSeasonChngpt  = TRUE
extra$computeTrendChngpt   = TRUE
extra$computeSeasonAmp     = FALSE
extra$computeTrendSlope    = TRUE
extra$tallyPosNegSeasonJump= FALSE
extra$tallyPosNegTrendJump = TRUE
extra$tallyIncDecTrendJump = TRUE
extra$tallyPosNegOutliers  = TRUE
extra$printProgressBar     = TRUE
extra$printOptions         = TRUE



o = beast123( LQ, metadata, prior, mcmc, extra )
