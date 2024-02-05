#' Creates grid of prediction points within boundary but outside of plots
#' 
#' Creates grid of prediction points within boundary but outside of plots. Also
#' computes coarse and fine node points using kmeans on subsamples of grid points. 
#'
#' @param bnd an outline of a single polygon of sf class
#' @param plts plots as polygons of sf class
#' @param respVar name of response variable
#' @param nNodesRequestC number of coarse nodes
#' @param nNodesRequestF number of fine nodes
#' @param nodeSetSeed a random number seed passed so that the k-means nodes
#' are reproducible
#'
#' @return a list, where the GridPts item contains the prediction grid as a  
#  sf class.  The nodesC item is a data.frame with x,y-coordinates
#' of the coarse nodes, nodesF item is a data.frame with x,y-coordinates 
#' of the fine nodes, and convexPolyKnotsFine is an sf polygon that is a
#' convex hull around all plots with counts greater than zero.
#'
#' @author Jay Ver Hoef \email{jay.verhoef@@noaa.gov}
#' @export

createPredGridCountSamp <- function(bnd, plts, respVar, 
	nNodesRequestC, nNodesRequestF, nodeSetSeed) 
{
	# use bounding box to get limits of boundary file
	lower.x.lim <- st_bbox(bnd)["xmin"]
	upper.x.lim <- st_bbox(bnd)["xmax"]
	lower.y.lim <- st_bbox(bnd)["ymin"]
	upper.y.lim <- st_bbox(bnd)["ymax"]
	# get the x- and y-ranges
	xrange <- upper.x.lim - lower.x.lim
	yrange <- upper.y.lim - lower.y.lim
	# create the number of grid columns according to formula below
	nc <- round(sqrt(xrange/yrange)*200)
	# create number of grid rows so that it scale appropriately to columns
	nr <- round(nc*yrange/xrange)
	# create a grid of points within bounding box
	GridPts <- st_sf(st_sfc(st_centroid(st_make_grid(bnd, n = c(nc,nr)))))
	# clip grid within boundary
	GridPts = st_intersection(GridPts, bnd)
	# k-means uses randomization, so set the seed for reproducibility
	set.seed(nodeSetSeed)
	# create course node locations
	nodesC <- kmeans(as.data.frame(st_coordinates(GridPts)), nNodesRequestC, 
		nstart = 20, iter.max = 500, algorithm="Lloyd")$centers
	# create fine node locations
	# these are on a convex hull among only plots that have non-zero values
	# start by finding centroids of only plots with non-zero values
	centroids.gt0 <- st_centroid(st_geometry(
		plts[st_drop_geometry(plts[,respVar]) > 0,]))
	# now get the convex hull as a polygon
	poly.gt0 <- st_convex_hull(st_union(centroids.gt0))	
	GridPts_fine = st_intersection(GridPts, poly.gt0)
	nodesF <- kmeans(as.data.frame(st_coordinates(GridPts_fine)), nNodesRequestF, 
		nstart = 20, iter.max = 500, algorithm="Lloyd")$centers
	# create grid outside of observed plots
	# gets indices of GridPts within plots
	GridPts_plt_indx = unlist(st_intersects(plts, GridPts))
	# turn those into indicator variables
	ind = (1:dim(GridPts)[1]) %in% GridPts_plt_indx
	# create grid outside of observed plots
	GridPts <- GridPts[!ind,]
	list(GridPts = GridPts, nodesC = nodesC, nodesF = nodesF, 
		convexPolyKnotsFine = poly.gt0)
}

