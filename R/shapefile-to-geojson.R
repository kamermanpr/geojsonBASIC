#' make_geojson
#'
#' A convenience wrapper building on functions from the \link[rgdal]{readOGR}, \link[sp]{spTransform}, \link[rgeos]{gSimplify}, \link[geojsonio]{geojson_json}, \link[geojsonio]{geojson_write}, and \link[geojsonlint]{geojson_lint} to convert shapefiles to geojson files.
#'
#' @export
# Function
make_geojson <- function(dir.path,
                         input.file,
                         output.file = NULL,
                         crs = '+init=epsg:4238',
                         simplify = FALSE,
                         tolerance = 1,
                         topology = TRUE,
                         validate = FALSE) {

    # Default action: name
    if(!is.null(output.file)) {
        file_name <- output.file
    } else {
        file_name <- input.file
    }

    ## Add extension to file_name
    file_name_2 <- paste0(file_name, '.geojson')

    # Read in the shape file
    shape <- rgdal::readOGR(dsn = dir.path,
                            layer = input.file)
    # Set coordinate reference system
    shape_1 <- sp::spTransform(x = shape,
                             CRSobj = CRS(crs))
    # Simplify
    if(simplify == TRUE) {
    shape_2 <- rgeos::gSimplify(spgeom = shape_1,
                              tol = tolerance,
                              topologyPreserve = topology)
    } else {
        shape_2 <- shape_1
    }
    # Transform to SpatialPolygonsDataFrame
    #shape_3 <- sp::SpatialPolygonsDataFrame(Sr = shape_2,
                                        #data = shape_1@data)
    shape_3 <- geojsonio::geojson_json(input = shape_2,
                                       geometry = 'polygon')
    # Transform to geojson
    geojsonio::geojson_write(input = shape_3,
                             file = file_name_2)
    # Validate
    if(validate == TRUE) {
        valid <- geojsonlint::geojson_lint(x = as.location(file_name_2))
                if(valid[1] == 'TRUE') {
                    message('Conversion complete, and file passes validity check')
                } else {
                    stop('Converted file fails validity check')
                }
    }
}
