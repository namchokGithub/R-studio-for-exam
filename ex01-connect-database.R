# Connect Database
library(RMySQL)

# utf8
Sys.setlocale("LC_CTYPE", "UTF-8")
# options(encoding="UTF-8")

#------------ Connect database -------------------#

con = dbConnect(MySQL(),
                user="oesuser",
                password="tsp60@oesuser",
                dbname="tsp60_nu_oesdb",
                host="10.80.39.17")

dataFormDB <- dbGetQuery(con, "SELECT * FROM oes_result_exam")

View(dataFormDB)

dbDisconnect(con)

#------------ Connect database -------------------#