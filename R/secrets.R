library('RPostgreSQL')
library('RPostgres')
data(mtcars)
con <- dbConnect(drv = dbDriver("PostgreSQL"),
                  dbname = Sys.getenv("DBNAME"),
                  host = Sys.getenv("HOST"),
                  user = Sys.getenv("USER"),
                  password = Sys.getenv("PASSWORD"))
dbWriteTable(con, "test", mtcars, overwrite = T)
