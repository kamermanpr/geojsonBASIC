#' Convert shapefiles to GeoJSON.
#'
#' A convenience wrapper building on functions \link[rgdal]{readOGR}, \link[sp]{spTransform}, \link[rgeos]{gSimplify}, \link[geojsonio]{geojson_json}, \link[geojsonio]{geojson_write}, and \link[geojsonlint]{geojson_lint} to convert shapefiles to geojson files.
#'
#' @param dir.path Path (no trailing '/') to the 'shapefile' folder. This folder must contain, at a bare minimum, files with the extensions .shp, .shx, and .dbf).
#' @param input.file A single character string specifying the filename shared by the files in the shapefile folder.
#' @param output.file A single character string specifying the filename of the output file (no extension, such as '.geojson' is required). The default value is emph{NULL}, and the file will be given the name of the input file name.
#' @param crs A single character string specifying the Coordinate Reference System to use. The default value is '+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'.
#' @param simplify Logical (default value is FALSE) specifying whether to simplify the given geometry using the Douglas-Peuker algorithm.
#' @param tolerance Numerical tolerance value to be used by the Douglas-Peuker algorithm. Greater values produce greater simplification (default value is 0).
#' @param topology Logical specifying whether the Douglas-Peuker algorithm should attempt to preserve the topology of the original geometry (default value is TRUE).
#' @param validate Logical specifying whether to validate the output GeoJSON using the geojsonlint.com web service (default value is FALSE).
#'
#' @export
# Function
geojson_convert <- function(dir.path,
                         input.file,
                         output.file = NULL,
                         crs = '+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0',
                         simplify = FALSE,
                         tolerance = 0,
                         topology = TRUE,
                         validate = FALSE) {

    # Check if path to file exists
    file_path <- paste0(dir.path, '/', input.file, '.shp')

    if(!file.exists(file_path)) {
        stop('Input file name or directory does not exist.')
    }

    # Default action: name
    if(!is.null(output.file)) {
        file_name <- output.file
    } else {
        file_name <- input.file
    }

    ## Add geojson extension to file_name
    file_name_2 <- paste0(file_name, '.geojson')

    # Read in the shape file
    shape <- rgdal::readOGR(dsn = dir.path,
                            layer = input.file)
    # Set coordinate reference system
    shape_1 <- sp::spTransform(x = shape,
                             CRSobj = sp::CRS(crs))
    # Simplify
    if(simplify == TRUE) {
    shape_2 <- rgeos::gSimplify(spgeom = shape_1,
                              tol = tolerance,
                              topologyPreserve = topology)
    } else {
        shape_2 <- shape_1
    }
    # Transform to SpatialPolygonsDataFrame
    shape_3 <- geojsonio::geojson_json(input = shape_2,
                                       geometry = 'polygon')
    # Transform to geojson
    geojsonio::geojson_write(input = shape_3,
                             file = file_name_2)
    # Validate
    if(validate == TRUE) {
        valid <- geojsonlint::geojson_lint(x = geojsonlint::as.location(file_name_2))
                if(valid == 'TRUE') {
                    message('Passes validity check')
                } else {
                    stop('Fails validity check')
                }
    }
}