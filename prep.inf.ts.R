### prep.inf.ts.R
################################################
### merge long term with current years data
################################################

########################
## load data & packages
########################
require(tidyr);require(dplyr)
load("data/processed/R/inf.dat.long.Rdata")
inf.dat.long.16 <- inf.dat.long; rm(inf.dat.long)
load("data/base/2015Rdata/int.inf.Rdata")
inf.dat.old <- inf.dat; rm(inf.dat)


###################################################
## Convert old data to long format
###################################################
tmp <- gather(data = inf.dat.old, key = taxon, value = abundance,
              Hydrozoa:Ensis, factor_key = TRUE); rm(inf.dat.old)
## trim superfluous names
tmp <- tmp[, !names(tmp) %in% c("sample_old","sample", "station")]

### format 'year'
inf.dat.long.16$year <- as.numeric(as.character(inf.dat.long.16$year))

############
## Merge
############
inf.ts <- dplyr::bind_rows(inf.dat.long.16, tmp) ## merge 2016 & old data together
inf.ts <- as.data.frame(inf.ts) #make it a dataframe

########################
## sort formatting etc
########################
inf.ts$transect <- as.factor(inf.ts$transect)
inf.ts$transect <- factor(inf.ts$transect, levels = c("T1N", "T1","T1S",
                                                      "T4", "T7", "T8",
                                                      "T11", "T12", "T13",
                                                      "T15", "T17","T20",
                                                      "T21", "T22","T23",
                                                      "WA1"))
inf.ts$taxon <- as.factor(inf.ts$taxon)
inf.ts$method <- as.factor(inf.ts$method)
inf.ts$method2 <- inf.ts$method
inf.ts <- mutate(inf.ts,
                 method2 = ifelse(method == "1.0mm"|method == "1.0mm mesh",
                                 "1.0mm", "0.5mm"))
inf.ts$method2 <- as.factor(inf.ts$method2)
inf.ts$method <- inf.ts$method2
inf.ts$method2 <- NULL
inf.ts.L <- inf.ts
rm(inf.ts, inf.dat.long.16, tmp)

########################
## Save LONG version
########################
save(inf.ts.L, file = "data/processed/R/inf.ts.lon.Rdata")
write.csv(inf.ts.L, file = "data/processed/csv/inf.ts.L.csv",row.names=F)
#write.csv(inf.ts.L, file = "C:/Users/ccesar/Desktop/MyFiles - not backed up/[THE LAB]/[temp]/inf.ts.L.csv",row.names=F)
# export unique taxa
write.csv(unique(inf.ts.L$taxon), file = "data/processed/csv/inf.sum.ts.taxa.csv",row.names=F)

########################
## Convert to WIDE
########################
inf.ts.wide <- inf.ts.L %>% spread(taxon, abundance)
rm(inf.ts.L)
## convert NA to 0
inf.ts.wide[,9:length(inf.ts.wide)][is.na(inf.ts.wide[,9:length(inf.ts.wide)])] <- 0

###re-assign variable types to make converted columns numeric
inf.ts.wide <- inf.ts.wide %>%
  mutate_each(funs(type.convert(as.character(.))))

inf.ts.W <- inf.ts.wide; rm(inf.ts.wide)
########################
## Save WIDE version
########################
save(inf.ts.W, file = "data/processed/R/inf.ts.W.Rdata")
#write.csv(inf.ts.W, file = "data/processed/csv/inf.ts.W.csv",row.names=F)
write.csv(inf.ts.W, file = "C:/Users/ccesar/Desktop/MyFiles - not backed up/[THE LAB]/[temp]/inf.ts.W.csv",row.names=F)
#####################################
## Collapse taxon names to pool data
#####################################
### Nephtys
inf.ts.W$temp <- inf.ts.W$Nephtys + inf.ts.W$Nephtys.cirrosa + inf.ts.W$Nephtys.hombergii
inf.ts.W$Nephtys <- NULL; 
inf.ts.W$Nephtys.cirrosa <- NULL; inf.ts.W$Nephtys.hombergii <- NULL
inf.ts.W$Nephtys <- inf.ts.W$temp; inf.ts.W$temp <- NULL
### Arenicola
inf.ts.W$temp <- inf.ts.W$Arenicola + inf.ts.W$Arenicola.marina
inf.ts.W$Arenicola <- NULL; inf.ts.W$Arenicola.marina <- NULL;
inf.ts.W$Arenicola <- inf.ts.W$temp; inf.ts.W$temp <- NULL
### Pontocrates
inf.ts.W$temp <- inf.ts.W$Pontocrates + inf.ts.W$Pontocrates.arcticus +
  inf.ts.W$Pontocrates.arenarius
inf.ts.W$Pontocrates <- NULL; inf.ts.W$Pontocrates.arcticus <- NULL
inf.ts.W$Pontocrates.arenarius <- NULL
inf.ts.W$Pontocrates <- inf.ts.W$temp; inf.dat$temp <- NULL
### Bathyporeia
inf.ts.W$temp <- inf.ts.W$Bathyporeia + inf.ts.W$Bathyporeia.pelagica +
  inf.ts.W$Bathyporeia.sarsi
inf.ts.W$Bathyporeia<- NULL;
inf.dat$Bathyporeia.pelagica <- NULL
inf.ts.W$Bathyporeia.sarsi <- NULL
inf.ts.W$Bathyporeia <- inf.ts.W$temp; inf.ts.W$temp <- NULL
### Corophium
inf.ts.W$temp <- inf.ts.W$Corophium + inf.ts.W$Corophium.volutator + inf.ts.W$Corophium.arenarium
inf.ts.W$Corophium <- NULL; inf.ts.W$Corophium.volutator <- NULL;inf.ts.W$Corophium.arenarium <- NULL;
inf.ts.W$Corophium <- inf.ts.W$temp; inf.ts.W$temp <- NULL
### Mytilidae
inf.ts.W$temp <- inf.ts.W$Mytilidae + inf.ts.W$Mytilus.edulis
inf.ts.W$Mytilidae <- NULL; inf.ts.W$Mytilus.edulis <- NULL
inf.ts.W$Mytilidae <- inf.ts.W$temp; inf.ts.W$temp <- NULL
### Eurydice
inf.ts.W$temp <- inf.ts.W$Eurydice + inf.ts.W$Eurydice.affinis +
  inf.ts.W$Eurydice.pulchra
inf.ts.W$Eurydice <- NULL; inf.ts.W$Eurydice.affinis <- NULL
inf.ts.W$Eurydice.pulchra <- NULL
inf.ts.W$Eurydice <- inf.ts.W$temp; inf.ts.W$temp <- NULL
############################
## Summarise by year & zone
############################
### Swap out 'P' values for a numeric term
tmp.yr <- subset(inf.ts.W, method == "1.0mm")

#convert counts to characters
tmp.yr[10:length(tmp.yr)] <- sapply(tmp.yr[10:length(tmp.yr)],as.character)
## change 'P' values to a numeric
tmp.yr[tmp.yr == "P"] <- "-99999";
##change back
tmp.yr[10:length(tmp.yr)] <- sapply(tmp.yr[10:length(tmp.yr)],as.numeric)

### calculate means by year & shore height
### remove descriptive data
drop <- names(tmp.yr) %in% c("sample", "transect", "rep",
                             "method", "zone2.1", "zone2.2")
tmp.yr <- tmp.yr[!drop]
rm(drop)

tmp.yr <- tmp.yr %>%
  group_by(year, shore, zone1) %>%
  summarise_each(funs(mean))
tmp.yr <- as.data.frame(tmp.yr)
#sort data by mpg (ascending) and cyl (descending)
tmp.yr <- tmp.yr[order(tmp.yr$zone1, tmp.yr$shore, tmp.yr$year),]

##transpose & save
tmp.yr <- t(tmp.yr); tmp.yr <- as.data.frame(tmp.yr)
write.csv(tmp.yr, file = "data/processed/csv/inf.summ.yr.sh.csv")
rm(tmp.yr)

########################
## tidy up
########################
rm(inf.ts, inf.ts.W)
detach("package:tidyr", unload = TRUE)
detach("package:dplyr", unload = TRUE)
