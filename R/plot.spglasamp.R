#' Plots the fitted spglasamp object 
#' 
#' Plots the fitted spglasamp object  
#'
#' @param obj a spglasamp object
#'
#' @return a plot 
#'
#' @author Jay Ver Hoef \email{jay.verhoef@@noaa.gov}
#' @export

plot.spglasamp = function(obj, cex.pred = .3, cex.knots = 2,
	pch.coarse = 3, pch.fine = 4, lwd.knots = 3, nclass = 9)
{
	par.old = par(mar = c(0,0,0,0), bg = 'gray70')
	require(viridis)
	require(classInt)
	plot(st_geometry(obj$outline))
	plot(st_geometry(obj$plots), add = TRUE)
	plot(st_geometry(obj$plots[
		st_drop_geometry(obj$plots)[,obj$respVar] > 0,]), add = TRUE, 
		col = 'red')
	cip = classIntervals(obj$pred$Prediction, style = 'fisher', n = nclass,
		warnLargeN = FALSE)
	palp = viridis(nclass)
	cip_colors = findColours(cip, palp)
	plot(st_geometry(obj$pred), add = TRUE, pch = 19,
		col = cip_colors, cex = cex.pred)
	plot(st_sf(st_geometry(obj$convexPolyKnotsFine)),
		add = TRUE, lwd = 2, border = 'white')
	plot(st_sf(st_geometry(st_as_sf(obj$nodeLocationsC, 
		coords = c("x","y")))), add = TRUE, pch = pch.coarse, cex = cex.knots, 
		lwd = lwd.knots, col = 'orange',)
	plot(st_sf(st_geometry(st_as_sf(obj$nodeLocationsF,
		coords = c("x","y")))), add = TRUE, pch = pch.fine, cex = cex.knots, 
		lwd = lwd.knots, col = 'orange')
	par(par.old)
}


