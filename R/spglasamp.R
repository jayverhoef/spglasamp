#' Abundance estimator from counts in irregularly spaced plot
#'
#' Creates an abundance estimator from counts in spatial plots by 
#' estimating a spatial point pattern intensity surface between observed
#' plots  and then integrating over the intensity surface to estimate
#' abundance for unsampled area.  It then adds to counted plots, thus it
#' has a finite population correction factor
#'
#' @param formula a formula like response ~ 1, where the response variable is
#' on the left.  Currently, this is the only form that is implemented; that is,
#' no covariates are allowed.  The response variable must be a column name in the
#' SpatialPolygonDataFrame of the plots argument.
#' @param outline an outline of a single polygon of sp Class SpatialPolygon
#' @param plots as polygons of sp Class SpatialPolygons
#' @param nNodesRequestC number of node requests at coarse scale
#' @param nNodesRequestF number of node requests at fine scale
#' @param PercentZero The percent of area with zeros to eliminate when
#' computing overdispersion factor, default = 0
#' @param nodeSetSeed random number seed for k-means computation of nodes

#' @return A list with a lot of items.

#' @author Jay Ver Hoef \email{jay.verhoef@@noaa.gov}
#' @export

spglasamp <- function(formula, totalCount, outline, plots, nNodesRequestC, nNodesRequestF,
	percentZero = 0, nodeSetSeed = 101)
{
	StartTime <- Sys.time()
	# name of the response variable
	respVar <- as.character(attr(terms(formula),"variables"))[2]

	# get the plot centroids
	cent = st_coordinates(st_centroid(st_geometry(plots)))
	# find the mean centroids for x and y
	xmean.fix <- mean(cent[,1])
	ymean.fix <- mean(cent[,2])
	# get the average standard deviation of x and y centroids (across both)
	xystdv.fix <- mean(sqrt(var(cent[,1])),
		sqrt(var(cent[,2])))

	# function for affine transformation
	# https://r-spatial.github.io/sf/articles/sf3.html
	stdizePolys = function(poly, xcentr, ycentr, divisor)
	{
		polyg = st_geometry(poly)
		storage = vector('list', dim(poly)[1])
		for(i in 1:dim(poly)[1]) storage[[i]] = 
			st_point(c(xcentr, ycentr))
		centrs = st_sfc(storage)
		polyout = (polyg - centrs)/divisor
		polyout = st_sf(polyout)
		polyout
	}

	#standardize boundary data
	#(this makes it easier to add the prediction grid as it will be relatively
	#constant across different projections)
	bnd = stdizePolys(outline, xmean.fix, ymean.fix, xystdv.fix)
	#get the area of the boundary
	bnd.originalarea = st_area(outline)
	#get the area of transformed boundary
	bnd.area = st_area(bnd)

	# standardize plots
	plts = stdizePolys(plots, xmean.fix, ymean.fix, xystdv.fix)
	plts[,respVar] = st_drop_geometry(plots[,respVar])
	#get the area of the plots
	plts.originalarea = st_area(plots)
	#get the area of transformed plots
	plts.area = st_area(plts)
	
	# Creates grid of prediction points within boundary but outside of plots. 
	# Also computes coarse and fine node points using kmeans.	
	createPredGridOut <- createPredGridCountSamp(bnd, plts, respVar, nNodesRequestC,
		nNodesRequestF, nodeSetSeed)
	GridPtsp <- createPredGridOut$GridPts
	nodesC <- createPredGridOut$nodesC
	nodesF <- createPredGridOut$nodesF
	convexPolyKnotsFine <- createPredGridOut$convexPolyKnotsFine
	# Create distance matrices among knots for starting values and parameter boundaries
	distCC <- distBetween2xySets(nodesC, nodesC)
	distFF <- distBetween2xySets(nodesF, nodesF)
	distC0 <- distCC
	diag(distC0) <- 1e+32
	# set the range parameter (for Gaussian kernels) to 2*minimum distance among
	# coarse knots
	rangeC <- min(distC0)*2
	distF0 <- distFF
	diag(distF0) <- 1e+32
	# set the range parameter (for Gaussian kernels) to 2*minimum distance among
	# fine knots
	rangeF <- min(distF0)*2

	# put bounds on the kernel range parameters so they won't overlap
	# minimum fine range 
	minFrange  <- 0.5*min(distF0)
	# maximum fine range  = minimum coarse range
	maxFrange <- 3*min(distF0)
	# maximum coarse range
	maxCrange <- 3*min(distC0)
	# distance matrices between centroids and nodes
	distDC <- distBetween2xySets(st_coordinates(st_centroid(plts)), nodesC)
	distDF <- distBetween2xySets(st_coordinates(st_centroid(plts)), nodesF)
	# minimize the minus log likelihood for range parameters
	mLL <- function(ranges, respVar, plts.area, distDC, distDF,
		minFrange, maxFrange, maxCrange) {
		# range parameters constrained between bounds
		rangeF <- minFrange + exp(ranges[1])/
			(1 + exp(ranges[1]))*(maxFrange - minFrange)
		rangeC <- maxFrange + exp(ranges[2])/
			(1 + exp(ranges[2]))*(maxCrange - maxFrange)
		# create Gaussian kernel values from distance matrices and range parameters
		GauKernDC <- exp(-(distDC/rangeC)^2)
		GauKernDF <- exp(-(distDF/rangeF)^2)
		# create design matrix from two matrices above
		X <- cbind(GauKernDC, GauKernDF)
		# response variable as a vector
		y <- as.numeric(st_drop_geometry(plts)[,respVar])
		# create a dataset with y and X
  	dat4IWLS <- as.data.frame(cbind(y,X))
		options(warn = -1)
		# Poisson regression can be used to fit the model, e.g., equation (6) in
		# Ver Hoef and Jansen, 2015, JABES 20: 1-27.
		betaHat <- coef(glm(y ~ . - 1, family = poisson, 
			offset = log(plts.area), data = dat4IWLS))
		options(warn = 0)
		# return the value in equation (8) to be optimized
		# Ver Hoef and Jansen, 2015, JABES 20: 1-27.
		sum(plts.area*exp(X %*% betaHat) - y * X %*% betaHat)
	}
	# optimize range parameters as in equation (8)
	# Ver Hoef and Jansen, 2015, JABES 20: 1-27.
	parmest <- optim(c(log(rangeF), log(rangeC) - log(rangeF)), mLL, 
		respVar = respVar, plts.area = plts.area, 
		distDC = distDC, distDF = distDF,
		minFrange = minFrange, maxFrange = maxFrange, maxCrange = maxCrange)
	
	# create fitted design matrices outside of mLL
	rangeF <- minFrange + exp(parmest$par[1])/
			(1 + exp(parmest$par[1]))*(maxFrange - minFrange)
	rangeC <- maxFrange + exp(parmest$par[2])/
			(1 + exp(parmest$par[2]))*(maxCrange - maxFrange)
	GauKernDC <- exp(-(distDC/rangeC)^2)
	GauKernDF <- exp(-(distDF/rangeF)^2)
	X <- cbind(GauKernDC, GauKernDF)

	# beta parameter estimates (after optimizing for kernel range parameters)
	y <- as.numeric(st_drop_geometry(plts)[,respVar])
	dat4IWLS <- as.data.frame(cbind(y,X))
	options(warn = -1)
	betaHatF <- coef(glm(y ~ . - 1, offset = log(plts.area),
		family = poisson, data = dat4IWLS))
	options(warn = 0)
	fits <- exp(X %*% betaHatF)
	# equation (14)
	# Ver Hoef and Jansen, 2015, JABES 20: 1-27.
	SigF <- solve(covB(X, fits))/mean(plts.area)

	# create prediction design matrices
	distPC <- distBetween2xySets(st_coordinates(GridPtsp), nodesC)
	distPF <- distBetween2xySets(st_coordinates(GridPtsp), nodesF)
	GauKernPC <- exp(-(distPC/rangeC)^2)
	GauKernPF <- exp(-(distPF/rangeF)^2)
	XP <- cbind(GauKernPC, GauKernPF)

	#predictions for all unsampled area based on GridPtsp grid
	pred0F <- exp(XP %*% betaHatF)
	
	# Sampled area
	cB = sum(plts.area)
	# Unsampled area
	cU = bnd.area - cB

	# sum predictions * area per prediction + sum observed
	EstF <- mean(pred0F)*as.numeric(cU) + totalCount

	#scaled predictions
	predOrigAreaF <- pred0F*as.numeric(cU)/
		as.numeric((bnd.originalarea - sum(plts.originalarea)))
	predPerGridF <- pred0F*cU/dim(GridPtsp)[1]

	#variance estimators
	#partial derivative vector to be used in delta method
	cvecF <- apply(as.vector(pred0F)*XP,2,mean)*cU
	# Equation (15)
	# Ver Hoef and Jansen, 2015, JABES 20: 1-27.
	varEstF <- mean(pred0F)*cU + t(cvecF) %*% SigF %*% cvecF
	stdErrF <- sqrt(varEstF)

	# Overdispersion Estimators
	Cnt <- y
	plta <- plts.area
	fits0F <- fits*plta
	# traditional estimator
	ODtradF <- sum((Cnt - fits0F)^2/fits0F)/(length(Cnt) - length(X[1,]))
	ODtradF <- max(1,ODtradF)
	# trimmed OD estimator
	fitscutF <- quantile(fits0F, percentZero/100)
	IndFits1F <- fits0F >= fitscutF
	fits1F <- fits0F[IndFits1F]
	Cnt1F <- Cnt[IndFits1F]
	ODpercF <- sum((Cnt1F - fits1F)^2/fits1F)/(length(Cnt1F) - length(X[1,]))
	ODpercF <- max(1,ODpercF)
	# make it local
	predQ <- pred0F
	predQ[pred0F > fitscutF] <- pred0F[pred0F > fitscutF]*ODpercF
	fitsQ <- fits*0 + 1
	fitsQ[fits0F > fitscutF] <- ODpercF
	varEstQ <- mean(predQ)*cU + mean(fitsQ)*t(cvecF) %*% SigF %*% cvecF
	stdErrQ <- sqrt(varEstQ)
	# regression estimator
	fits0R <- fits0F
	fits0R[fits0F < 1e-150] <- 1e-150
	ODFdata <- data.frame(y = (Cnt - fits0R)^2, x = fits0R)
#	ODvaryR <- lm(y ~ x - 1, data = ODFdata, weights = 1/fits0R)$coefficients
	ODvaryR <- max(lm(y ~ x - 1, data = ODFdata, 
		weights = sqrt(fits0R))$coefficients,1)
#	ODvaryR <- max(lm(y ~ x - 1, data = ODFdata)$coefficients,1)
	varEstR <- mean(pred0F*ODvaryR)*cU + 
		ODvaryR*t(cvecF) %*% SigF %*% cvecF
	stdErrR = sqrt(varEstR)
# change prediction grid and convex polygon back to original coordinates
# before exporting
	pred_sf = stdizePolys(GridPtsp, -xmean.fix/xystdv.fix, 
		-ymean.fix/xystdv.fix, 1/xystdv.fix)
	convx_sf = stdizePolys(st_sf(convexPolyKnotsFine), -xmean.fix/xystdv.fix, 
		-ymean.fix/xystdv.fix, 1/xystdv.fix)
	preds = pred_sf
# add intensity and predictions to prediction grid
	preds[,'Intensity'] = predOrigAreaF
	preds[,'Predictions'] = predPerGridF
	
	EndTime <- Sys.time()
	 
	spCntSmp <- list(
		totalCounted = totalCount,
		estimate = EstF, 
		stdErr = stdErrF, 
		stdErrOD = stdErrF*sqrt(ODtradF),
		stdErrGT = stdErrF*sqrt(ODpercF),
		stdErrLR = stdErrR,
		stdErrLT = stdErrQ,
		ODtrad = ODtradF, 
		ODtrim = ODpercF,
		percentZero = percentZero,
		propSurveyed = cB/(cU + cB),
		rangeF = rangeF*xystdv.fix, 
		rangeC = rangeC*xystdv.fix,
		respVar = respVar,
		outline = outline,
		fits = data.frame(fitsFixed = fits0F), 
		plots = plots,
		preds = preds,
		startTime = StartTime,
		elapsedTime = EndTime - StartTime,
		nNodesRequestC = nNodesRequestC, 
		nNodesRequestF = nNodesRequestF, 
		nodeLocationsC = data.frame(x = nodesC[,1]*xystdv.fix + xmean.fix, 
			y = nodesC[,2]*xystdv.fix + ymean.fix),
		nodeLocationsF = data.frame(x = nodesF[,1]*xystdv.fix + xmean.fix, 
			y = nodesF[,2]*xystdv.fix + ymean.fix),
		convexPolyKnotsFine = convx_sf,
    SigF = SigF,
    betaHatF = betaHatF,
    XP = XP,
    cU = cU,
    varEstFpart1 = mean(pred0F)*cU,
    varEstFpart2 = t(cvecF) %*% SigF %*% cvecF,
    parmest = parmest)

	class(spCntSmp) <- "spglasamp"
	spCntSmp

}

