#' Calculate UTC and local time from Local Solar Time
#'
#' @description calculate UTC and local time from Local Solar Time, as provided e.g. in MODIS viewtime rasters
#'
#'
#' @param LocST Local solar time vector in mm:hh format or as a viewtime raster in decimal format or decimal format only
#' @param utc UTC day that was provided e.g. via filename or the corresponding UTC day
#' (infered based on the local time zone the local solar time was reported for conversion
#' to UTC time), format = '\%Y-\%m-\%d' or as POSIXlt date
#' @param lon required only if no raster is used for LocST, to provide as decimal point numeric
#' @return data frame with lon and lat, as well as decimal format local solar time ("LocST"), hour format local solar time "LocST_h",UTC date calculated from LocST "utc_from_LocST", the original UTC input provided ("orgUTC") and a time difference between provided and calculated UTC times in case a POSIXlt format UTC date was provided (available e.g. in case of MODIS L2/swath data)
#' @author Maite Lezama Valdes
#' @examples
#' #using MODIS L2 product's viewtime raster
#' vtr_s <-raster(system.file("extdata", "vtr_s.tif", package="LocST"))
#' utcdate <- strptime("2018-01-19 13:50", format='%Y-%m-%d %H:%M', tz="UTC") # info derived from MOD11_L2 filename A2018019.1350
#' LocST_UTC(LocST=vtr_s, utc=utcdate)
#'
#' #using MODIS viewtime raster from daily products, where no UTC capture time is available
#' vtr_s <- load(vt_raster)
#' LocST_UTC(LocST=vtr_s, utc="2018-01-19")
#'
#' #using LocST without raster, lon input is required
#' LocST_UTC(LocST="23:36", lon=137.8628, utc="2018-01-19")
#' LocST_UTC(LocST=23.6, lon=137.8628, utc="2018-01-19")
#'
#' #using multiple LocST inputs for the same longitude
#' LocST_UTC(LocST=c(23.0, 22.1), lon=137.8628, utc="2018-01-19")
#' @export LocST_UTC
#' @aliases LocST_UTC

LocST_UTC <- function(LocST, utc, lon=NULL){

  library(lubridate)

  #################### CHECK UTC INPUT #######################################################
  if(any(class(utc)=="POSIXlt")){
    utcstat <- 1
    utcday <- format(utc, "%Y-%m-%d")
  } else if (typeof(utc)=="character"|typeof(utc)=="double"){ # i.e.
    utcstat <- 2
    utcday <- utc
  } else {
    print("Please provide a date or day in UTC formated via POSIXlt or as  'YYY-mm-dd' as a character")
    utcstat <- 3
  }

  #################### IMPLEMENTATION FOR LocST RASTER INPUT #################################


  if(class(LocST)=="RasterLayer"){
    # make xy data frame with lon and LocST
    LocST <- LocST*0.1 # apply scale factor (see 3.2. Scientific Data Sets (SDS)
    #in https://lpdaac.usgs.gov/sites/default/files/public/product_documentation/mod11_user_guide.pdf)


    # get pixel location and LocST values
    lonLocST <- as.data.frame(rasterToPoints(LocST))
    LocST <- lonLocST[,3]
    names(lonLocST) <- c("lon", "lat", "LocST")

    # convert numeric to time format
    LocST_h <- dec_time(lonLocST$LocST)

    lonLocST$LocST_h <- LocST_h

    # calculate lon / 15 conversion: hours and minutes
    lon_15 <- lonLocST$lon / 15
    lon_15_h <- dec_time(lon_15)
    hrs <- as.numeric(substring(lon_15_h, 1,2))
    mins <- as.numeric(substring(lon_15_h, 4,5))

    # assignment of local timezone is just to evade getting a completely false one in,
    # actually, this is local solar time
    locdates <- strptime(paste0(utcday, "_", lonLocST$LocST_h), format='%Y-%m-%d_%H:%M')
    lonLocST$LocST_dat <- locdates

    # convert to UTC
    utcconv <- locdates
    utcconv$hour <- utcconv$hour - hrs
    utcconv$min <- utcconv$min - mins
    # if UTC + longitude/15 (=hrs) < 0: +1 day, if >= 24: -1 day
    if(utcconv$hour < 0){
      utcconv$mday <- utcconv$mday +1
    } else if(utcconv$hour >= 24){
      utcconv$mday <- utcconv$mday -1
    } else {
      utcconv$mday <- utcconv$mday
    }

    # force result to be in UTC
    tst <- force_tz(utcconv, tzone = "UTC")
    lonLocST$utc_from_LocST <- tst

    if(utcstat == 1){
      lonLocST$orgUTC <- utc
      lonLocST$utcdiff <- lonLocST$utc_from_LocST - lonLocST$orgUTC
    } else {
      lonLocST$orgUTC <- utcday
    }

    return(lonLocST)

    #################### IMPLEMENTATION FOR LocST NON-RASTER INPUT #################################


  } else if (class(LocST)!="RasterLayer" & is.null(lon)){

    print("Please provide longitude information as decimal numeric or use viewtime raster instead for LocST")

  } else if (class(LocST)=="numeric" & !is.null(lon)){

    # convert numeric to time format
    LocST_h <- dec_time(LocST)

    if(length(lon)==length(LocST)){
      df <- data.frame(lon, LocST_h)
    } else {
      df <- data.frame(lon=rep(lon, length(LocST_h)), LocST_h)
    }

    lonLocST <- df

    # calculate lon / 15 conversion: hours and minutes
    lon_15 <- lonLocST$lon / 15
    lon_15_h <- dec_time(lon_15)
    hrs <- as.numeric(substring(lon_15_h, 1,2))
    mins <- as.numeric(substring(lon_15_h, 4,5))


    locdates <- strptime(paste0(utcday, "_", lonLocST$LocST_h), format='%Y-%m-%d_%H:%M')
    lonLocST$LocST_dat <- locdates

    # convert to UTC
    utcconv <- locdates
    utcconv$hour <- utcconv$hour - hrs
    utcconv$min <- utcconv$min - mins
    # if UTC + longitude/15 (=hrs) < 0: +1 day, if >= 24: -1 day
    if(utcconv$hour < 0){
      utcconv$mday <- utcconv$mday +1
    } else if(utcconv$hour >= 24){
      utcconv$mday <- utcconv$mday -1
    } else {
      utcconv$mday <- utcconv$mday
    }

    # force result to be in UTC
    tst <- force_tz(utcconv, tzone = "UTC")
    lonLocST$utc_from_LocST <- tst

    if(utcstat == 1){
      lonLocST$orgUTC <- utc
      lonLocST$utcdiff <- lonLocST$utc_from_LocST - lonLocST$orgUTC
    } else {
      lonLocST$orgUTC <- utcday
    }

    return(lonLocST)

  } else if (class(LocST)=="character" & !is.null(lon)){

    LocST_h <- LocST

    if(length(lon)==length(LocST)){
      df <- data.frame(lon, LocST_h)
    } else {
      df <- data.frame(lon=rep(lon, length(LocST_h)), LocST_h)
    }

    lonLocST <- df

    # calculate lon / 15 conversion: hours and minutes
    lon_15 <- lonLocST$lon / 15
    lon_15_h <- dec_time(lon_15)
    hrs <- as.numeric(substring(lon_15_h, 1,2))
    mins <- as.numeric(substring(lon_15_h, 4,5))

    # assignment of local timezone is just to evade getting a completely false one in,
    # actually, this is local solar time
    #utcday <- format(utcdate, "%Y-%m-%d")

    locdates <- strptime(paste0(utcday, "_", lonLocST$LocST_h), format='%Y-%m-%d_%H:%M')
    lonLocST$LocST_dat <- locdates

    # convert to UTC
    utcconv <- locdates
    utcconv$hour <- utcconv$hour - hrs
    utcconv$min <- utcconv$min - mins
    # if UTC + longitude/15 (=hrs) < 0: +1 day, if >= 24: -1 day
    if(utcconv$hour < 0){
      utcconv$mday <- utcconv$mday +1
    } else if(utcconv$hour >= 24){
      utcconv$mday <- utcconv$mday -1
    } else {
      utcconv$mday <- utcconv$mday
    }

    # force result to be in UTC
    tst <- force_tz(utcconv, tzone = "UTC")
    lonLocST$utc_from_LocST <- tst


    if(utcstat == 1){
      lonLocST$orgUTC <- utc
      lonLocST$utcdiff <- lonLocST$utc_from_LocST - lonLocST$orgUTC
    } else {
      lonLocST$orgUTC <- utcday
    }
    return(lonLocST)
  }
}




