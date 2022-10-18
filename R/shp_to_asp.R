#' Convert shp to .ASP for ADMS runs
#'
#' Converts a shapefile into a .ASP file, which denotes static ADMS output points
#' @param shp (sf shapefile) Input sf shapefile
#' @param filepath (character) Path for saving file
#' @param height (numeric) Height of the receptor points (in m). Defaults to 3
#' @return No return
#' @keywords adms, asp, monitors
#' @export
# ' @examples 
# ' shp_to_asp(adms_shp_dummy, filepoath)
# ' @export

shp_to_asp <- function(shp, filepath, height = 3){
    shp = sf::st_transform(shp,
        crs = "+proj=lcc +lat_1=15.0 +lat_2=40.0 +lat_0=28.5 +lon_0=114.0 +x_0=0 +y_0=0 +no_defs +a=6370000 +b=6370000 +to_meter=1")

    coords_df = sf::st_coordinates(x = shp)

    df = data.frame(id = 1:nrow(coords_df), coords_df, height = height)

    #write out
    write.table(
        df,
        file = filepath,
        row.names = F,
        col.names = F,
        sep =","
    )

    "Written: {filepath}" |> glue::glue() |> message()

}

