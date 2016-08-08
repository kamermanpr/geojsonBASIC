# Convert Natural Earth shapefiles to geojson format

# Load packages
library(rgdal) # reading and writing shape files
library(rgeos) # simplify (reduce) file
library(geojsonio) # for conversion to geojson
library(geojsonlint) # for validation of geojson

# Function
make_geojson <- function(dir.path,
                         input.file,
                         output.file = NULL,
                         crs = '+init=epsg:4238',
                         simplify = 1,
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
    shape_2 <- rgeos::gSimplify(spgeom = shape_1,
                              tol = simplify,
                              topologyPreserve = topology)
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

make_geojson(dir.path = './ne_50m_admin_1_states_provinces_lakes',
             input.file = 'ne_50m_admin_1_states_provinces_lakes',
             output.file = './geojson-provinces/provinces_50m',
             validate = TRUE)

