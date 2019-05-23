
#' Calculate UTC and local time from Local Solar Time
#' 
#' @description calculate UTC and local time from Local Solar Time, as provided e.g. in MODIS viewtime rasters
#' @param LocST Local solar time vector in mm:hh format or as a viewtime raster in decimal format or decimal format only
#' @param utc UTC day that was provided e.g. via filename or the corresponding UTC day (infered based on the local time zone the
#' local solar time was reported for conversion to UTC time), format = "%Y-%m-%d" or as POSIXlt date
#' @param lon required only if no raster is used for LocST, to provide as decimal point numeric
#' 
#' 
#' @return data frame with lon and lat, as well as decimal format local solar time ("LocST"), hour format local solar time "LocST_h", 
#' UTC date calculated from LocST "utc_from_LocST", the original UTC input provided ("orgUTC") and a time difference between provided and 
#' calculated UTC times in case a POSIXlt format UTC date was provided (available e.g. in case of MODIS L2/swath data)
#' @author Maite Lezama Valdes
#' @examples
#' 
#' 
#' datpath <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/data/example_data/"
#' vt_raster <- raster(paste0(datpath, "vtr_s.tif"))
#' aoi_ext <- extent(-120775.7, -37096.71, 6686761, 6782394)
#' aoiproj <- "+proj=utm +zone=54 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#' 
#' lmvt(fnam="MOD11_L2.A2018019.1350.006.2018020082352.hdf",
#'      viewtime = vt_raster, ra=NULL, aoiproj=NULL, ltz="Australia/ACT")
#' 
#' # with Swath data + optionally: cut to area of interest and calculate also local timezone
#' lmvt(fnam="MOD11_L2.A2018019.1350.006.2018020082352.hdf", 
#'      viewtime = vt_raster, ra=aoi_ext, aoiproj=aoiproj, ltz="Australia/ACT")
#' 
# with MOD11A1 data
#' lmvt(fnam="MOD11A1.A2018019.h29v12.006.2018020085050.hdf", 
#'      viewtime = vt_raster, ra=NULL, aoiproj=NULL, ltz="Australia/ACT")
#' 


lmvt <- function(fnam, viewtime, ra=NULL, aoiproj=NULL, ltz=NULL) {
  
  ###############  prep local solar time raster  ###############################
  
  # read latlon viewtime raster
  vt <- viewtime
  
  # crop to valid range
  vt[vt >= 240] <- NA
  vt[vt < 0] <- NA
  
  if(!is.null(ra)&!is.null(aoiproj)){
    # crop viewtime raster to research area and apply scale factor
    e <- extent(ra)
    e <- as(e, "SpatialPolygons")
    proj4string(e) <- CRS(aoiproj)
    e.geo <- sp::spTransform(e, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    vt <- crop(vt, e.geo)
  }

  
  ###############  prep UTC date  ####################################################
  

  # get UTC date from fnam
  su <- strsplit(fnam, "A")
  su <- su[[1]][length(su[[1]])]
  org <- paste0(substring(su, 1,4), "-01-01")
  utcday <- as.Date((as.numeric(substring(su, 5,7))-1), origin=org)
  
  if(grepl("v", su)){
    utctime <- NULL
    utcdate <- utcday
  } else {
    utctime <- paste0(substring(su, 9, 10), ":", substring(su, 11, 12))
    utcdate <- strptime(paste0(utcday,utctime), format='%Y-%m-%d %H:%M', tz="UTC")
  }
  
  ###############  call LocST_UTC function  ####################################################
  
  lonLocST <- LocST_UTC(LocST=vt, utc=utcdate)
  
  ###############  add local time if desired  ####################################################
  
  # if local timezone format was provided, local time will be calculated from UTC
  if(!is.null(ltz)){
    # translate UTC date from LocST to local time zone format
    local_tz_date <- with_tz(lonLocST$utc_from_LocST, ltz)
    lonLocST$ltz_date <- local_tz_date
  }
  
  
  return(lonLocST)
  
}

