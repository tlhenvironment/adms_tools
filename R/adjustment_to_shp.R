#' Adjustment to shape
#'
#' Reads in adjustment (from .csv) and generates a shapefile
#' @param base_folder (character) Basefolder of tam results. Defaults to /home/dataop/script/EMIS/GMAPTRAFFIC/traffic_adjustment_module_v3/
#' @param adms_points_path (character) Path to the shapefile of ADMS points.
#' @param datetime (POSIXct) Datetime argument
#' @param format  (character) Either "sf" or "terra", depending which package to use
#' @return An object of class \code{sfc_multipoint} or \code{vect}
#' @keywords adjustment shapefile
#' @export
# ' @examples 
# ' example_point = sf::st_point(c(1,2))
# ' example_point = sf::st_sfc(example_point)
# ' example_point = sf::st_sf(example_point)
# ' 
# ' buffer_circle(example_point, 200) -> circular_buffer
# ' plot(circular_buffer)
# ' @export

adjustment_to_shp <- function(
    base_folder = "/home/dataop/script/EMIS/GMAPTRAFFIC/traffic_adjustment_module_v3/",
    adms_points_path = "/disk/v092.user/tlh/colleagues/jimmy_chan/adms_to_raster/data/lnglat_no_receptors/lnglat_no_receptors_hk_80.shp",
    datetime, format){

    #load dummy points
    if(!exists('adms_shp_dummy')){
      adms_shp_dummy <<- sf::st_read(adms_points_path)
    }

    #read in adjustment dataframe as csv
    strftime(datetime, "%Y-%m") -> y_m
    strftime(datetime, "%Y-%m-%d") -> y_m_d
    strftime(datetime, "%Y-%m-%d-%H-%M") -> y_m_d_h_m
    
    adjustment_path = 
      glue::glue("{base_folder}/results/{y_m}/{y_m_d}/{y_m_d_h_m}/model_adjustment/{y_m_d_h_m}_adjustment_result.csv")    

    #read in
    if(!file.exists(adjustment_path)){
      glue::glue("File not found: {adjustment_path}")
      return(NA)
    }

    adjustment_df = data.table::fread(adjustment_path)

    #bind with shapefile
    dplyr::right_join(adms_shp_dummy, adjustment_df, by = "FID")

}
