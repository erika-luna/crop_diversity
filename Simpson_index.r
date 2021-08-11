#This is an R script
#This is the Simposon index as discussed with Exequiel E. (Calculations_divindex_Exe_solution.xlsx)
#Created by Charlotte and Rodrigo on Aug-19-2018
dwd <- "~Char/Dropbox/AAA_agri_DATA/Database/Sagarpa_data/Agriculture/"
library(readxl)
library(dplyr)

#This ine read the database
dbss <- read_xlsx(path= paste(dwd,"Crop_Yield_03-16_Munb_NoNA_Tons.xlsx",sep = ""), sheet = 1)

#This creates a function "CalcIndiceSimpson"
CalcIndiceSimpson <- function(x,edomunid,yearagricola,newcrop,areacosechada){ #names the function and tells the parameters that the function will have
  edomunid="edo.mun.id"
  yearagricola="YearAgricola"
  newcrop="new.crop"
  areacosechada="Cosechada"
  finaltemp <- vector()
  for (m in unique(x[[edomunid]])){
    y <- filter(x,x[[edomunid]] == m)
    for (ye in unique(y[[yearagricola]])) {
      z <- filter(y,y[[yearagricola]] == ye)
      areatotal <- sum(z[[areacosechada]])
      z$index <- (z[[areacosechada]]/areatotal)^2
      simpson <- 1/sum(z$index)
      simpson2 <- simpson*(90/areatotal)^0.25
      final <- data.frame(id = m, year = ye, area = areatotal, indexS = simpson, indexS2 = simpson2)
      finaltemp <- rbind(finaltemp, final)
      }
  }
  return(finaltemp)
}
indexSimpson <- CalcIndiceSimpson(x = dbss,edomunid="edo.mun.id", yearagricola="YearAgricola",newcrop="new.crop",areacosechada="Cosechada")

write.csv(indexSimpson, file = paste0(dwd, "IndexSimpson.csv"), row.names = F)


#### ERIKA ####
dbss <- SIAP_mun

CalcIndiceSimpson <- function(x,edomunid,yearagricola,newcrop,areacosechada){ #names the function and tells the parameters that the function will have
  edomunid="mun_code"
  yearagricola="year"
  newcrop="crop"
  areacosechada="harvested"
  finaltemp <- vector()
  for (m in unique(x[[edomunid]])){
    y <- filter(x,x[[edomunid]] == m)
    for (ye in unique(y[[yearagricola]])) {
      z <- filter(y,y[[yearagricola]] == ye)
      areatotal <- sum(z[[areacosechada]])
      z$index <- (z[[areacosechada]]/areatotal)^2
      simpson <- 1/sum(z$index)
      simpson2 <- simpson*(90/areatotal)^0.25
      final <- data.frame(id = m, year = ye, area = areatotal, indexS = simpson, indexS2 = simpson2)
      finaltemp <- rbind(finaltemp, final)
    }
  }
  return(finaltemp)
}
indexSimpson <- CalcIndiceSimpson(x = dbss,edomunid="mun_code", yearagricola="year",newcrop="crop",areacosechada="harvested")

write.csv(indexSimpson, file = paste0("IndexSimpson_muncode.csv"), row.names = F)


