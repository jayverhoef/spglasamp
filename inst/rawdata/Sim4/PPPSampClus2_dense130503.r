library(maptools)
library(spCountSamp)
library(rgeos)
setwd("/media/Hitachi2GB/00NMML/RPackages/spCountSamp_package/spCountSamp/inst/data/Sim4")


#-------------------------------------------------------------------------------
#  SAMPLE PLOTS
#-------------------------------------------------------------------------------

ni <- 26
nj <- 26
PlotSize <- 0.21
offst <- (10-.21*25)/(2*26)
ep <- .01
i <- 1
j <- 1
samples <- NULL
ID <- 1
for(i in 1:ni) {
  for (j in 1:nj) {
    if (i != 2 & i != 4 & j != 3 & j != 6)
    {
        xL <- (i-1)*10/ni + offst
        xU <- (i-1)*10/ni + offst + PlotSize
        yL <- (j-1)*10/nj + offst
        yU <- (j-1)*10/nj + offst + PlotSize
        samples <- c(samples, Polygons(list(Polygon(cbind(c(xL, xU, xU, xL, xL),
                                          c(yL, yL, yU, yU, yL)))), ID = ID))
				ID = ID + 1
    }
  }
}
plots <- SpatialPolygons(samples)
df <- data.frame(pltarea = rep(NA, times = length(plots@polygons)))
for(i in 1:length(plots@polygons)) 
	df[i,"pltarea"] <- plots@polygons[[i]]@Polygons[[1]]@area
row.names(df) <- as.character(1:length(plots@polygons))
plots <- SpatialPolygonsDataFrame(plots, df)
#plot(plots)


loXlim <- 0
upXlim <- 10
loYlim <- 0
upYlim <- 10
outline <- SpatialPolygons(list(Polygons(list(Polygon(
	cbind(c(loXlim, upXlim, upXlim, loXlim, loXlim),
	c(loYlim, loYlim, upYlim, upYlim, loYlim)))), ID = "bnd")))
#plot(outline, add = TRUE)

#-------------------------------------------------------------------------------
#  SIMULATION
#-------------------------------------------------------------------------------

StoreSims <- NULL
iter <- 2
niter <- 1000
Start.Time <- Sys.time()
 for (iter in 1:niter) {
	set.seed(2000 + iter)
  lower.x.bbox <- runif(1, 5.8, 6.2)
  upper.x.bbox <- runif(1, 7.8, 8.2)
  lower.y.bbox <- runif(1, 5.8, 6.2)
  upper.y.bbox <- runif(1, 7.8, 8.2)
  nseed.big <- 75
  nseed.sma <- 15
  Poi.mean.big <- 14
  Poi.mean.sma <- 8
  big.range <- 1
  sma.range <- 0.02
  trend <- TRUE

  PlotSize <- .5
  pcover <- .5
  SampBal <- TRUE

#debug(point.sim.clus)
  Sim <- pointSimClus(nseed.big = nseed.big,
	  nseed.sma = nseed.sma,
	  Poi.mean.big = Poi.mean.big,
	  Poi.mean.sma = Poi.mean.sma,
	  big.range = big.range,
    sma.range = sma.range,
	  lower.x.lim = 0, upper.x.lim = 10,
	  lower.y.lim = 0, upper.y.lim = 10,
	  lower.x.bbox = lower.x.bbox, upper.x.bbox = upper.x.bbox,
	  lower.y.bbox = lower.y.bbox, upper.y.bbox = upper.y.bbox,
    trend = trend)
  SimPts1 <- Sim$SimPts

  lower.x.bbox <- runif(1, 0.8, 1.2)
  upper.x.bbox <- runif(1, 3.8, 4.2)
  lower.y.bbox <- runif(1, 4.8, 5.2)
  upper.y.bbox <- runif(1, 7.8, 8.2)
  nseed.big <- 25
  nseed.sma <- 10
  Poi.mean.big <- 14
  Poi.mean.sma <- 8
  big.range <- .5
  sma.range <- 0.02
  trend <- TRUE

  PlotSize <- .5
  pcover <- .5
  SampBal <- TRUE

  Sim <- pointSimClus(nseed.big = nseed.big,
	  nseed.sma = nseed.sma,
	  Poi.mean.big = Poi.mean.big,
	  Poi.mean.sma = Poi.mean.sma,
	  big.range = big.range,
    sma.range = sma.range,
	  lower.x.lim = 0, upper.x.lim = 10,
	  lower.y.lim = 0, upper.y.lim = 10,
	  lower.x.bbox = lower.x.bbox, upper.x.bbox = upper.x.bbox,
	  lower.y.bbox = lower.y.bbox, upper.y.bbox = upper.y.bbox,
    trend = trend)
  SimPts2 <- Sim$SimPts

  simPts <- rbind(SimPts1, SimPts2)

	coordinates(simPts) <- c("x","y")
#	plot(simPts, add = TRUE, pch = 19, cex = .5)

  TrueAbundance <- length(coordinates(simPts)[,1])

	counts <- rep(NA, times  = length(plots@polygons))
	for(i in 1:length(plots@polygons)) {
		counts[i] <- sum(!is.na(over(simPts, 
			SpatialPolygons(list(plots@polygons[[i]])))))
	}
	# add count data to Photo Plot Outlines
	pltsData <- plots@data
	pltsData[,"counts"] <- counts
	plots@data <- pltsData

# ------------------------------------------------------------------------------
# end standardize x and y coordinates
# ------------------------------------------------------------------------------

  fCnt1 <- floor(.02*length(plots@polygons))
  fCnt1 <- max(fCnt1,1)
  fCnt2 <- floor(.05*length(plots@polygons))
  fCnt2 <- max(fCnt2,1)
  fCnt3 <- floor(.10*length(plots@polygons))
  fCnt3 <- max(fCnt3,1)
  fCnt4 <- floor(.15*length(plots@polygons))
  fCnt4 <- max(fCnt4,1)

  iter.time <- Sys.time()
  plot(c(0,2),c(0,2), type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  text(1,1.5, paste("Simulation", iter), cex = 3)
  text(1,0.5, paste("Elapsed Time", iter.time - Start.Time), cex = 2)

	EstOut1 <- spCountSamp(counts ~ 1, outline, plots, 
		nNodesRequestC = 3, 
		nNodesRequestF = 8, percentZero = 75)

	EstOut2 <- spCountSamp(counts ~ 1, outline, plots, 
		nNodesRequestC = 5, 
		nNodesRequestF = 16, percentZero = 75)

	EstOut3 <- spCountSamp(counts ~ 1, outline, plots, 
		nNodesRequestC = 7, 
		nNodesRequestF = 24, percentZero = 75)

	EstOut4 <- spCountSamp(counts ~ 1, outline, plots, 
		nNodesRequestC = 9, 
		nNodesRequestF = 32, percentZero = 75)

# summary(EstOut4)
    
# ------------------------------------------------------------------------------
# classical random sampling
# ------------------------------------------------------------------------------


  FPCF <- (1-EstOut4$propSurveyed)
  SRSEst <- mean(plots@data[,"counts"]/plots@data[,"pltarea"])*
		outline@polygons[[1]]@Polygons[[1]]@area
  SRSSE <- sqrt(var(plots@data[,"counts"]/plots@data[,"pltarea"]))*
		outline@polygons[[1]]@Polygons[[1]]@area*sqrt(FPCF)/
    sqrt(length(plots@data[,"pltarea"]))
    
  StoreSims <- rbind(StoreSims, data.frame(TrueAbun = TrueAbundance,
    Est1 = EstOut1$estimate, StdErr1 = EstOut1$stdErr,
    ODTrad1 = EstOut1$ODtrad, ODtrim1 = EstOut1$ODtrim,
		StdErrOD1 = EstOut1$stdErrOD, StdErrGT1 = EstOut1$stdErrGT,
		StdErrLR1 = EstOut1$stdErrLR, StdErrLT1 = EstOut1$stdErrLT, 
    Est2 = EstOut2$estimate, StdErr2 = EstOut2$stdErr,
    ODTrad2 = EstOut2$ODtrad, ODtrim2 = EstOut2$ODtrim,
		StdErrOD2 = EstOut2$stdErrOD, StdErrGT2 = EstOut2$stdErrGT,
		StdErrLR2 = EstOut2$stdErrLR, StdErrLT2 = EstOut2$stdErrLT, 
    Est3 = EstOut3$estimate, StdErr3 = EstOut3$stdErr,
    ODTrad3 = EstOut3$ODtrad, ODtrim3 = EstOut1$ODtrim,
		StdErrOD3 = EstOut3$stdErrOD, StdErrGT3 = EstOut3$stdErrGT,
		StdErrLR3 = EstOut3$stdErrLR, StdErrLT3 = EstOut3$stdErrLT, 
    Est4 = EstOut4$estimate, StdErr4 = EstOut4$stdErr,
    ODTrad4 = EstOut4$ODtrad, ODtrim4 = EstOut4$ODtrim,
		StdErrOD4 = EstOut4$stdErrOD, StdErrGT4 = EstOut4$stdErrGT,
		StdErrLR4 = EstOut4$stdErrLR, StdErrLT4 = EstOut4$stdErrLT, 
    EstSRS = SRSEst, StdErrSRS = SRSSE))

  write.table(StoreSims, "StoreSims4_130505.csv", quote = F, sep = ",", row.names = F)

}

#library(spCountSamp)
#setwd("/media/Hitachi2GB/00NMML/RPackages/spCountSamp_package/spCountSamp/inst/data/Sim4")
# SimSummary(read.csv("StoreSims4_130505.csv"), "1", "OD")
#SimSummary(read.csv("StoreSims4_130505.csv"),"4","GT", 
#	failCutEst = 2000, failCutSE = 500)
#SimSummary(read.csv("StoreSims4_130503.csv"),"SRS")

#X11()
#boxplot(read.csv("StoreSims4_130505.csv")[,"StdErrF1"]*sqrt(read.csv("StoreSims4_130505.csv")[,"ODTradF1"]), read.csv("StoreSims4_130505.csv")[,"StdErrFQ1"],read.csv("StoreSims4_130505.csv")[,"StdErrFR1"],read.csv("StoreSims4_130505.csv")[,"StdErrFV1"],read.csv("StoreSims4_130505.csv")[,"StdErrF1"]*sqrt(read.csv("StoreSims4_130505.csv")[,"ODPercF1"]))

#X11()
#boxplot(read.csv("StoreSims4_130505.csv")[,"StdErrF2"]*sqrt(read.csv("StoreSims4_130505.csv")[,"ODTradF2"]), read.csv("StoreSims4_130505.csv")[,"StdErrFQ2"],read.csv("StoreSims4_130505.csv")[,"StdErrFR2"],read.csv("StoreSims4_130505.csv")[,"StdErrFV2"],read.csv("StoreSims4_130505.csv")[,"StdErrF2"]*sqrt(read.csv("StoreSims4_130505.csv")[,"ODPercF2"]))

SimSummary(read.csv("StoreSims4_130505.csv"), "1")[,c("Bias","RMSPE","CIcover90")]

resultsSim4 <- cbind(
c(
	SimSummary(read.csv("StoreSims4_130505.csv"), "1", failCutEst = 2000)[,c("Bias","RMSPE","CIcover90")],
	SimSummary(read.csv("StoreSims4_130505.csv"), "1","OD", failCutEst = 2000)[,"CIcover90"],
	SimSummary(read.csv("StoreSims4_130505.csv"), "1","LR", failCutEst = 2000)[,"CIcover90"],
	SimSummary(read.csv("StoreSims4_130505.csv"), "1","GT", failCutEst = 2000)[,"CIcover90"],
	SimSummary(read.csv("StoreSims4_130505.csv"), "1","LT", failCutEst = 2000)[,"CIcover90"],
	SimSummary(read.csv("StoreSims4_130505.csv"), "1", failCutEst = 2000)[,"failRate"]
),
c(
	SimSummary(read.csv("StoreSims4_130505.csv"), "2", failCutEst = 2000)[,c("Bias","RMSPE","CIcover90")],
	SimSummary(read.csv("StoreSims4_130505.csv"), "2","OD", failCutEst = 2000)[,"CIcover90"],
	SimSummary(read.csv("StoreSims4_130505.csv"), "2","LR", failCutEst = 2000)[,"CIcover90"],
	SimSummary(read.csv("StoreSims4_130505.csv"), "2","GT", failCutEst = 2000)[,"CIcover90"],
	SimSummary(read.csv("StoreSims4_130505.csv"), "2","LT", failCutEst = 2000)[,"CIcover90"],
	SimSummary(read.csv("StoreSims4_130505.csv"), "2", failCutEst = 2000)[,"failRate"]
),
c(
	SimSummary(read.csv("StoreSims4_130505.csv"), "3", failCutEst = 2000)[,c("Bias","RMSPE","CIcover90")],
	SimSummary(read.csv("StoreSims4_130505.csv"), "3","OD", failCutEst = 2000)[,"CIcover90"],
	SimSummary(read.csv("StoreSims4_130505.csv"), "3","LR", failCutEst = 2000)[,"CIcover90"],
	SimSummary(read.csv("StoreSims4_130505.csv"), "3","GT", failCutEst = 2000)[,"CIcover90"],
	SimSummary(read.csv("StoreSims4_130505.csv"), "3","LT", failCutEst = 2000)[,"CIcover90"],
	SimSummary(read.csv("StoreSims4_130505.csv"), "3", failCutEst = 2000)[,"failRate"]
),
c(
	SimSummary(read.csv("StoreSims4_130505.csv"), "4", failCutEst = 2000)[,c("Bias","RMSPE","CIcover90")],
	SimSummary(read.csv("StoreSims4_130505.csv"), "4","OD", failCutEst = 2000)[,"CIcover90"],
	SimSummary(read.csv("StoreSims4_130505.csv"), "4","LR", failCutEst = 2000)[,"CIcover90"],
	SimSummary(read.csv("StoreSims4_130505.csv"), "4","GT", failCutEst = 2000)[,"CIcover90"],
	SimSummary(read.csv("StoreSims4_130505.csv"), "4","LT", failCutEst = 2000)[,"CIcover90"],
	SimSummary(read.csv("StoreSims4_130505.csv"), "4", failCutEst = 2000)[,"failRate"]
)
)
colnames(resultsSim4) <- c("knots1","knots2","knots3","knots4")
rownames(resultsSim4) <- c("bias", "RMSPE","CI90","CI90OD","CI90WR","CI90TG","CI90TL","fail")
library(xtable)
xtable(resultsSim4, digits = 3)
