#' Reads in ADMS result to shapefile
#'
#' Reads in ADMS result (from .nc file) and outputs a shapefile
#' @param base_folder (character) Basefolder of ADMS results. Default is "/home/dataop/data/nmodel/adms"
#' @param adms_points_path (character) Path to the shapefile of ADMS points.
#' @param datetime (POSIXct) Datetime argument.
#' @param adms_version  (character) e.g. "hkv20b"
#' @param pollutants  (character vector) e.g. c("PM10", "O3"), can be all variables of the .nc file
#' @return An object of class \code{sfc_multipoint} or \code{NA}
#' @keywords adms shapefile
#' @export
# ' @examples 
# ' example_point = sf::st_point(c(1,2))
# ' example_point = sf::st_sfc(example_point)
# ' example_point = sf::st_sf(example_point)
# ' 
# ' buffer_circle(example_point, 200) -> circular_buffer
# ' plot(circular_buffer)
# ' @export

read_adms(
    base_folder = "/home/dataop/data/nmodel/adms",
    adms_points_path = "/disk/v092.user/tlh/colleagues/jimmy_chan/adms_to_raster/data/lnglat_no_receptors/lnglat_no_receptors_hk_80.shp",
    datetime,
    adms_version,
    pollutants = c("PM10", "NO2", "O3")
){
    
    message(datetime)

    if(is.character(path)){} else{
        path = adms_path(datetime, version)
    }

    message(path)
    if (file.exists(path) == F){message("FILE NOT FOUND: ", path); return(NA)}

    #open nc file
    ncdf4::nc_open(path) -> nc_adms

    #read in data
    #find max dim
    #max_count = nc_adms$dim |> unlist() |> as.numeric() |> max(na.rm = T)
    max_count = 1203695

    lapply(pollutants, function(pollutant){
        ncdf4::ncvar_get(nc_adms, varid = pollutant, start = 1, count = max_count) |> as.data.frame()
    }) -> results_list

    do.call(cbind, results_list) -> results_df
    names(results_df) <- pollutants
    results_df$point <- 1:max_count
    results_df$datetime <- datetime

    results_df
}
