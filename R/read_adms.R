#' Reads in ADMS result to dataframe
#'
#' Reads in ADMS result (from .nc file) and outputs a dataframe
#' @param datetime (POSIXct) Datetime argument.
#' @param adms_version  (character) e.g. "hkv20b"
#' @param pollutants  (character vector) e.g. c("PM10", "O3"), can be all variables of the .nc file
#' @param base_folder (character) Basefolder of ADMS results. Default is "/home/dataop/data/nmodel/adms"
#' @param adms_points_path (character) Path to the shapefile of ADMS points.
#' @return An object of class \code{data.frame}
#' @keywords adms 
#' @export
# ' @examples 
# ' datetime = lubridate::ymd_h("2021-09-09-17")
# ' adms_df = read_adms(datetime)
# ' @export

read_adms = function(
    datetime = NULL,
    pollutants = c("PM10", "PM2.5", "NO2", "O3", "SO2"),
    adms_version = "hkv20b",
    base_folder = "/home/dataop/data/nmodel/adms",
    full_path = NULL)
    {
    #check for path
    message(datetime)
    if(is.null(datetime) & is.null(full_path)){
        stop("Provide either datetime or a full path")
    }
    
    if(is.character(full_path)){
        path = full_path
    } else {
        path = adms_path(datetime, adms_version)
    }

    message(path)
    if (file.exists(path) == F){message("FILE NOT FOUND: ", path); return(NA)}

    #open nc file
    ncdf4::nc_open(path) -> nc_adms
browser()
    #read in data
    #find max dim
    #max_count = nc_adms$dim |> unlist() |> as.numeric() |> max(na.rm = T)
    max_count = 1203695

    lapply(pollutants, function(pollutant){
        ncdf4::ncvar_get(nc_adms, varid = pollutant, start = 1, count = max_count) |> as.data.frame()
    }) -> results_list

    ncdf4::nc_close(nc_adms)

    do.call(cbind, results_list) -> results_df
    names(results_df) <- pollutants
    results_df$point <- 1:max_count
    results_df$datetime <- datetime

    results_df
}

#' Reads in the shapefile that has the information where ADMS output points are located
#'
#' Reads in ADMS result (from .nc file) and outputs a dataframe
#' @param adms_points_path (character) Path to the shapefile of ADMS points.
#' @return Returns nothing, but sets 'adms_shp_dummy' as a \code{sf} object in the global environment
#' @export
# ' @examples 
# ' read_adms_poins()
# ' @export
read_adms_poins <- function(
    adms_points_path = "/disk/v092.user/tlh/colleagues/jimmy_chan/adms_to_raster/data/lnglat_no_receptors/lnglat_no_receptors_hk_80.shp")
{
    #load dummy points
    if(!exists('adms_shp_dummy')){
      adms_shp_dummy <<- sf::st_read(adms_points_path)
    }
}

#' Reads in ADMS result to shapefile
#'
#' Reads in ADMS result (from .nc file) and outputs a dataframe
#' @param datetime (POSIXct) Datetime argument.
#' @param adms_version  (character) e.g. "hkv20b"
#' @param pollutants  (character vector) e.g. c("PM10", "O3"), can be all variables of the .nc file
#' @param base_folder (character) Basefolder of ADMS results. Default is "/home/dataop/data/nmodel/adms"
#' @param adms_points_path (character) Path to the shapefile of ADMS points.
#' @return An object of class \code{sf}
#' @keywords adms 
#' @export
# ' @examples 
# ' datetime = lubridate::ymd_h("2021-09-09-17")
# ' adms_df = read_adms_shp(datetime)
# ' @export
read_adms_shp <- function(
    datetime,
    adms_version = "hkv20b",
    pollutants = c("PM10", "PM2.5", "NO2", "O3", "SO2"),
    base_folder = "/home/dataop/data/nmodel/adms",
    adms_points_path = "/disk/v092.user/tlh/colleagues/jimmy_chan/adms_to_raster/data/lnglat_no_receptors/lnglat_no_receptors_hk_80.shp")
{
    #read in df
    adms_df <- read_adms(datetime, adms_version, pollutants, base_folder)

    #stop condition
    if(class(adms_df) != "data.frame" && is.na(adms_df)) "Could not read in {datetime}" |> glue::glue() |> stop()

    #load dummy points
    if(!exists('adms_shp_dummy')){
      read_adms_poins(adms_points_path)
    }

    #prepare for merging
    adms_shp_dummy_merge <- adms_shp_dummy
    adms_shp_dummy_merge$point <- adms_shp_dummy_merge$FID + 1
    adms_shp_dummy_merge <- adms_shp_dummy_merge |> dplyr::select(-FID)

    #merge
    if(isTRUE(all.equal(adms_shp_dummy_merge$point, adms_df$point))){
        adms_shp_dummy_merge <- adms_shp_dummy_merge |> dplyr::select(-point)
        adms_shp = cbind(adms_shp_dummy_merge, adms_df)
    } else {
        adms_shp = dplyr::right_join(adms_shp_dummy_merge, adms_df, by = "point")    
    }

    adms_shp
}

#' Reads in ADMS result to shapefile
#'
#' Reads in ADMS result (from .nc file) and outputs a dataframe
#' @param datetime (POSIXct) Datetime argument.
#' @param resolution (numeric) Resolution of raster (in m).
#' @param adms_version  (character) e.g. "hkv20b"
#' @param pollutants  (character vector) e.g. c("PM10", "O3"), can be all variables of the .nc file
#' @param base_folder (character) Basefolder of ADMS results. Default is "/home/dataop/data/nmodel/adms"
#' @param adms_points_path (character) Path to the shapefile of ADMS points.
#' @param cores (numeric) Number of cores for rasterization. Default is one core per pollutant.
#' @return An object of class \code{sf}
#' @keywords adms 
#' @export
# ' @examples 
# ' datetime = lubridate::ymd_h("2021-09-09-17")
# ' adms_df = read_adms_raster(datetime, resolution = 20)
# ' @export
read_adms_raster <- function(
    datetime,
    resolution,
    adms_version = "hkv20b",
    pollutants = c("PM10", "PM2.5", "NO2", "O3", "SO2"),
    base_folder = "/home/dataop/data/nmodel/adms",
    adms_points_path = "/disk/v092.user/tlh/colleagues/jimmy_chan/adms_to_raster/data/lnglat_no_receptors/lnglat_no_receptors_hk_80.shp",
    cores = length(pollutants))  
{
    #read in shp
    adms_shp = read_adms_shp(datetime, adms_version, pollutants, base_folder, adms_points_path)

    #stop condition
    if(class(adms_shp)[1] != "sf" && is.na(adms_shp)) "Could not read in {datetime}" |> glue::glue() |> stop()

    adms_vect = terra::vect(adms_shp)
    dummy_rast = terra::rast(adms_vect, resolution = resolution)

    #rasterize, wrap because of parallel
    parallel::mclapply(pollutants, mc.cores = cores, FUN = function(pollutant){
        adms_rast = terra::rasterize(adms_vect, dummy_rast, field = pollutant, fun = mean, na.rm = T)
        terra::wrap(adms_rast)
    }) -> adms_raster_list

    #unwrap
    lapply(adms_raster_list, terra::rast) -> adms_raster_list

    do.call(c, adms_raster_list) -> adms_raster_combined
    names(adms_raster_combined) <- pollutants

    adms_raster_combined
    #will do interplation later
    #interpolation loop, runs until all NA values are interpolated
    # while(any(values(adms_rast) |> is.na() |> as.vector() == TRUE)){
    #     values(adms_rast) |> is.na() |> table() |> print()
    #     adms_rast <- terra::focal(adms_rast, w = matrix(1, 5, 5), fun = fill.na, na.only=T, na.rm = F)
    #     print('hi')
    # }
    
}

fill.na <- function(x) {

  i <- (length(x) +1 ) / 2


  if( is.na(x)[i] ) {
    return( round(mean(x, na.rm=TRUE),4) )
  } else {
    return( round(x[i],0) )
  }
}


# terra::writeCDF(adms_rast, filename = "/disk/scratch/tlh/aaa.nc", compression= 9, overwrite = T)

