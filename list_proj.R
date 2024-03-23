proj_db <- system.file("proj/proj.db", package = "sf")
# For dynamically linked PROJ, provide the path to proj.db yourself:
# if (proj_db == "") proj_db <- proj_db_path
# crs_table <- sf::read_sf(proj_db, "crs_view") # extracts the "crs_view" table
# subset(crs_table, grepl("Belg|Ostend", name) & auth_name == "EPSG")[2:5]


# library(RSQLite)
# filename <- "your_db_file.db"
# sqlite.driver <- dbDriver("SQLite")
# db <- dbConnect(sqlite.driver, dbname = filename)
# 
# ## Some operations
# dbListTables(db)
# mytable <- dbReadTable(db,"your_table_name")



library(rgdal)
proj_db <- system.file("proj/proj.db", package = "sf")
dblayers <- ogrListLayers(proj_db)


cls <- matrix(ncol = 2, byrow = T,
              c('compound_crs', 'auth_name|code|name',
                'alias_name', '',
                'conversion', 'auth_name|code|name|description|method_name',
                'conversion_method', '', 
                'conversion_param', '', 
                'conversion_table', 'auth_name|code|name|description',
                'coordinate_operation_method', '',
                'coordinate_operation_view', 'table_name|auth_name|code|name|description',
                'coordinate_system', '', 
                'crs_view', '', 
                'extent', '', 
                'projected_crs', 'code|name|coordinate_system_code',
                'usage', ''))
dbtables <- cls[, 1]


ans <- NULL; for(i in 1:length(dbtables)){
  # i = 12
  xx <- readOGR(dsn = proj_db, layer =  dbtables[i], #'crs_view', 
                dropNULLGeometries = FALSE, disambiguateFIDs = TRUE, verbose = FALSE)
  
  selCols <- cls[i, 2]
  if (selCols == ''){
    selCols <- paste(colnames(xx), collapse = '|')
  }
  # strsplit(selCols, '\\|')[[1]]
  # strsplit('*', '\\|')[[1]]
  xx <- xx[, strsplit(selCols, '\\|')[[1]]]
                 #grep(selCols, colnames(xx), value = TRUE)]
  
  cat('\n\n\n  ', dbtables[i],'\n\n')
  print(head(xx))
  cat('\n--\n')
 # print(tail(xx))
  print(dim(xx))
  dim(selTable)
  #ans <- rbind(ans, cbind(dbTabName = dbtables[i], selTable), deparse.level = 0)
  ans[[i]] <- (cbind(dbTabName = dbtables[i], xx))
}

df <- do.call(plyr::rbind.fill, ans)
df$crs_code <- paste0(df$auth_name, ':', df$code)



pos <- apply(df, 2, function(x) grep(pattern = 54004, x ))
pos
df[unique(unlist(pos)), ]
dim(df)
head(df)
tail(df)

table(df$dbTabName)


crs_codes <- df[df$dbTabName == 'crs_view', c('auth_name', 'code', 'name', 'table_name', 'crs_code') ]
dim(crs_codes)


pos <- apply(crs_codes, 2, function(x) grep(pattern = '54004', x ))
unique(unlist(pos))
crs_codes[unique(unlist(pos)), ]
write.csv(crs_codes, file = 'N:/Mi unidad/connectivity-nasa-USFSIP/02_procesed-data/crs_codes_global.csv')


# CRS	ESRI:102028 - Asia_South_Albers_Equal_Area_Conic - Projected
# ESRI:54004 - World_Mercator - Projected
