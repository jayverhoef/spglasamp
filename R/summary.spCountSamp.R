#' Default summary method for spCountSamp objects

#' Default summary method for spCountSamp objects

#' @param ob spglasamp objects

#' @return list of summary statistics, including the estimate, a data.frame
#' of various standard error estimates, a data.frame of proportions of the
#' variance components, and a data.frame of the range parameters.

#' @author Jay Ver Hoef \email{jay.verhoef@@noaa.gov}
#' @export

summary.spglasamp <- function(ob)
{
	li <- list(
		Estimates = list(totalCounted = ob$totalCounted,
			estimate = ob$estimate,
			stdError = data.frame(
				SE = ob$stdErr,
				SE.ODTrad = ob$stdErrOD,
				SE.ODTrimGlobal = ob$stdErrGT,
				SE.ODTrimLocal = ob$stdErrLT,
				SE.ODRegr = ob$stdErrLR)
		),
		rangeParameters = data.frame(
			coarseScale = ob$rangeC,
			fineScale = ob$rangeF
		),
		proportionSurveyed = ob$propSurveyed
	)
	class(li) <- "summary.spglasamp"
	return(li)
}


