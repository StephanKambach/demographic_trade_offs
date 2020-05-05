######################################################
# read in all the different CTFS census data ---------
load.census.data = function(CTFS.plot){
  
  if(CTFS.plot == "bci"){
    # load in the main census data and compile them in a list
    load("data/bci/inventory/bci.tree1.rdata")
    load("data/bci/inventory/bci.tree2.rdata")
    load("data/bci/inventory/bci.tree3.rdata")
    load("data/bci/inventory/bci.tree4.rdata")
    load("data/bci/inventory/bci.tree5.rdata")
    load("data/bci/inventory/bci.tree6.rdata")
    load("data/bci/inventory/bci.tree7.rdata")
    load("data/bci/inventory/bci.tree8.rdata")
    
    census.list = list(census1 = data.table(bci.tree1),
                       census2 = data.table(bci.tree2),
                       census3 = data.table(bci.tree3),
                       census4 = data.table(bci.tree4),
                       census5 = data.table(bci.tree5),
                       census6 = data.table(bci.tree6),
                       census7 = data.table(bci.tree7),
                       census8 = data.table(bci.tree8))
    
    # keep only the largest stem of each tree
    for(i in 1: length(census.list)){
      census.list[[i]] = census.list[[i]][order(census.list[[i]]$dbh, decreasing = T)]
      census.list[[i]] = census.list[[i]][which(!(duplicated(census.list[[i]]$treeID)))]
      census.list[[i]] = census.list[[i]][order(census.list[[i]]$treeID)]}
    
    # rename status levels so that the only harbor A, D and everything else is M
    for(i in 1: length(census.list)){
      census.list[[i]]$status[census.list[[i]]$status %in% c("AD", "AM", "AR", "M", "")] = "M"}
  }
  
  if(CTFS.plot == "korup"){
    # load in the main census data and compile them in a list
    load("data/korup/census data/korup.full1.rdata")
    load("data/korup/census data/korup.full2.rdata")
    
    census.list = list(census1 = data.table(korup.full1),
                       census2 = data.table(korup.full2))
    
    for(i in 1:length(census.list)){
      names(census.list[[i]]) = c("treeID", "stemID", "tag", "StemTag", "sp", "quadrat",
                                  "gx", "gy", "dbhID", "censusID", "dbh", "pom", "hom", "ExactDate",
                                  "DFstatus", "codes", "nostems", "date", "status", "agb")
      census.list[[i]]$ExactDate = as.Date(census.list[[i]]$ExactDate, format = "%Y-%m-%d")
      census.list[[i]]$pom = as.numeric(census.list[[1]]$pom)
      census.list[[i]]$tag = as.numeric(census.list[[1]]$tag)
      census.list[[i]]$sp = tolower(census.list[[1]]$sp)
      census.list[[i]]$quadrat = as.numeric(census.list[[1]]$quadrat)
      
      # fill with mean date where the date is missing
      census.list[[i]]$ExactDate[which(is.na(census.list[[i]]$ExactDate))] = mean(census.list[[i]]$ExactDate, na.rm = T)
      census.list[[i]]$date[is.na(census.list[[i]]$date)] = mean(census.list[[i]]$date, na.rm = T)}
    
    # keep only the largest stem of each tree
    for(i in 1: length(census.list)){
      census.list[[i]] = census.list[[i]][order(census.list[[i]]$dbh, decreasing = T)]
      census.list[[i]] = census.list[[i]][which(!(duplicated(census.list[[i]]$treeID)))]
      census.list[[i]] = census.list[[i]][order(census.list[[i]]$treeID)]} 
  }
  
  
  if(CTFS.plot == "hkk"){
    # load in the main census data and compile them in a list
    load("data/hkk/census data/hkk.full1.rdata")
    load("data/hkk/census data/hkk.full2.rdata")
    load("data/hkk/census data/hkk.full3.rdata")
    
    census.list = list(census1 = data.table(hkk.full1),
                       census2 = data.table(hkk.full2),
                       census3 = data.table(hkk.full3))
    
    for(i in 1:length(census.list)){
      names(census.list[[i]]) = c("treeID", "stemID", "tag", "StemTag", "sp", "quadrat",
                                  "gx", "gy", "dbhID", "censusID", "dbh", "pom", "hom", "ExactDate",
                                  "DFstatus", "codes", "nostems", "date", "status", "agb")
     
      census.list[[i]]$tag = as.numeric(census.list[[1]]$tag)
      census.list[[i]]$StemTag = as.numeric(census.list[[1]]$StemTag)
      census.list[[i]]$sp = tolower(census.list[[1]]$sp)
      census.list[[i]]$quadrat = as.numeric(census.list[[1]]$quadrat)
      census.list[[i]]$pom = as.numeric(census.list[[1]]$pom)
      census.list[[i]]$ExactDate = as.Date(census.list[[i]]$ExactDate, format = "%Y-%m-%d")
      
      # fill with mean date where the date is missing
      census.list[[i]]$ExactDate[which(is.na(census.list[[i]]$ExactDate))] = mean(census.list[[i]]$ExactDate, na.rm = T)
      census.list[[i]]$date[is.na(census.list[[i]]$date)] = mean(census.list[[i]]$date, na.rm = T)}
    
    # keep only the largest stem of each tree
    for(i in 1: length(census.list)){
      census.list[[i]] = census.list[[i]][order(census.list[[i]]$dbh, decreasing = T)]
      census.list[[i]] = census.list[[i]][which(!(duplicated(census.list[[i]]$treeID)))]
      census.list[[i]] = census.list[[i]][order(census.list[[i]]$treeID)]} 
  }
  
  if(CTFS.plot == "ituri_edoro"){
    # load in the main census data and compile them in a list
    load("data/ituri_edoro/census data/edoro.full1.rdata")
    load("data/ituri_edoro/census data/edoro.full2.rdata")
    load("data/ituri_edoro/census data/edoro.full3.rdata")
    
    census.list = list(census1 = data.table(edoro.full1),
                       census2 = data.table(edoro.full2),
                       census3 = data.table(edoro.full3))
    
  for(i in 1:length(census.list)){
      names(census.list[[i]]) = c("treeID", "stemID", "tag", "StemTag", "sp", "quadrat",
                                  "gx", "gy", "dbhID", "censusID", "dbh", "pom", "hom", "ExactDate",
                                  "DFstatus", "codes", "nostems", "status", "date", "agb", "plot")
      census.list[[i]]$tag = as.numeric(census.list[[1]]$tag)
      census.list[[i]]$StemTag = as.numeric(census.list[[1]]$StemTag)
      census.list[[i]]$sp = tolower(census.list[[1]]$sp)
      census.list[[i]]$quadrat = as.numeric(census.list[[1]]$quadrat)
      census.list[[i]]$pom = as.numeric(census.list[[1]]$pom)
      census.list[[i]]$ExactDate = as.Date(census.list[[i]]$ExactDate, format = "%Y-%m-%d")
      
      # fill with mean date where the date is missing
      census.list[[i]]$ExactDate[which(is.na(census.list[[i]]$ExactDate))] = mean(census.list[[i]]$ExactDate, na.rm = T)
      census.list[[i]]$date[is.na(census.list[[i]]$date)] = mean(census.list[[i]]$date, na.rm = T)}
    
    # keep only the largest stem of each tree
    for(i in 1: length(census.list)){
      census.list[[i]] = census.list[[i]][order(census.list[[i]]$dbh, decreasing = T)]
      census.list[[i]] = census.list[[i]][which(!(duplicated(census.list[[i]]$treeID)))]
      census.list[[i]] = census.list[[i]][order(census.list[[i]]$treeID)]} 
  }
  
  if(CTFS.plot == "ituri_lenda"){
    # load in the main census data and compile them in a list
    load("data/ituri_lenda/census data/lenda.full1.rdata")
    load("data/ituri_lenda/census data/lenda.full2.rdata")
    load("data/ituri_lenda/census data/lenda.full3.rdata")
    
    census.list = list(census1 = data.table(lenda.full1),
                       census2 = data.table(lenda.full2),
                       census3 = data.table(lenda.full3))
    
    for(i in 1:length(census.list)){
      names(census.list[[i]]) = c("treeID", "stemID", "tag", "StemTag", "sp", "quadrat",
                                  "gx", "gy", "dbhID", "censusID", "dbh", "pom", "hom", "ExactDate",
                                  "DFstatus", "codes", "nostems", "status", "date", "agb", "plot")
      census.list[[i]]$tag = as.numeric(census.list[[1]]$tag)
      census.list[[i]]$StemTag = as.numeric(census.list[[1]]$StemTag)
      census.list[[i]]$sp = tolower(census.list[[1]]$sp)
      census.list[[i]]$quadrat = as.numeric(census.list[[1]]$quadrat)
      census.list[[i]]$pom = as.numeric(census.list[[1]]$pom)
      census.list[[i]]$ExactDate = as.Date(census.list[[i]]$ExactDate, format = "%Y-%m-%d")
      
      # fill with mean date where the date is missing
      census.list[[i]]$ExactDate[which(is.na(census.list[[i]]$ExactDate))] = mean(census.list[[i]]$ExactDate, na.rm = T)
      census.list[[i]]$date[is.na(census.list[[i]]$date)] = mean(census.list[[i]]$date, na.rm = T)}
    
    # keep only the largest stem of each tree
    for(i in 1: length(census.list)){
      census.list[[i]] = census.list[[i]][order(census.list[[i]]$dbh, decreasing = T)]
      census.list[[i]] = census.list[[i]][which(!(duplicated(census.list[[i]]$treeID)))]
      census.list[[i]] = census.list[[i]][order(census.list[[i]]$treeID)]} 
  }
  if(CTFS.plot == "fushan"){
    # load in the main census data and compile them in a list
    load("data/fushan/inventory/fs.full.rdata")
    
    census.list = list(census1 = data.table(fs.full1),
                       census2 = data.table(fs.full2),
                       census3 = data.table(fs.full3))
    
    # remove columns that are only in one census
    census.list$census1$code = NULL
    census.list$census2$status0 = NULL
    census.list$census1$DFstatus = NULL
    census.list$census2$DFstatus = NULL
    census.list$census2$code = NULL
    census.list$census3$status0 = NULL
    census.list$census3$DFstatus = NULL
    census.list$census3$code = NULL

    
    for(i in 1:length(census.list)){
      # rename
      names(census.list[[i]])[which(names(census.list[[i]]) == "h")] = "hom"
      names(census.list[[i]])[which(names(census.list[[i]]) == "stemid")] = "treeID" 
      names(census.list[[i]])[which(names(census.list[[i]]) == "date")] = "ExactDate"
      
      # calculate julian date 
      census.list[[i]]$date = as.numeric(as.Date(as.character(census.list[[i]]$ExactDate),"%Y-%m-%d") - as.Date("1960-01-01","%Y-%m-%d"))
      
      # <- dbh in fushan is in cm, thus must be multiplied by 10
      census.list[[i]]$dbh = census.list[[i]]$dbh * 10
      
      # -> remove dbh with dbh < 10 and set status to P for these
      #census.list[[i]]$status[which(census.list[[i]]$dbh < 10)] = "P"
      #census.list[[i]]$dbh[which(census.list[[i]]$dbh < 10)] = NA
      # there are individuals that were measured with dbh <= 10 in census1,  but then only dbh = 9 in census2
    }
    
    # tree ID should be matching numbers
    all.ids.temp = unique(c(census.list$census1$treeID,
                            census.list$census2$treeID,
                            census.list$census3$treeID))
    
    for(i in 1:length(census.list)){
      
      census.list[[i]]$treeID = factor(census.list[[i]]$treeID,
                                       levels = all.ids.temp)
      census.list[[i]]$treeID = as.numeric(census.list[[i]]$treeID)}  
    
    # there are 2 ind that are marked "A" in census2, but are not in census3, I set them to missing in census3
    missing2 = census.list[[2]][status == "A"]
    missing2 = missing2[!(treeID %in% census.list[[3]]$treeID)]
    missing2$dbh = NA; missing2$status = "M"
    missing2$ExactDate = mean(as.Date(census.list$census3$ExactDate), na.rm = T)
    missing2$date = mean(census.list$census3$date, na.rm = T)
    census.list[[3]] = rbind(census.list[[3]],
                             missing2)
  }
  
  if(CTFS.plot == "seeds_fushan"){
    
    # load in the main census data and compile them in a list
    load("data/fushan/seeds_seedlings/fs_seeds_200209-201705.RData")
    seeds.fushan = data.table(trapdata)
    seeds.fushan$sp = tolower(seeds.fushan$sp)
    seeds.fushan$size = ifelse(seeds.fushan$size == "B", "big","small")
    
    length(unique(seeds.fushan$sp))
    table(seeds.fushan$year, seeds.fushan$trap)
    # -> 89-107 are added in 2007, I would not include those to keep the sample size similar across years
    seeds.fushan = seeds.fushan[trap < 89]
    # I need full year data
    seeds.fushan = seeds.fushan[year > 2002 & year < 2017]
    
    seeds.fushan = seeds.fushan[,.(seeds = sum(seeds, na.rm = T),
                                   viability_prop = mean(viability/seeds, na.rm  = T)),
                                by = .(sp, year)]
    seeds.fushan = seeds.fushan[,.(seeds_mean = mean(seeds),
                                   seeds_sd = sd(seeds),
                                   viability_mean = mean(viability_prop, na.rm = T),
                                   viability_sd = sd(viability_prop, na.rm = T)),
                                by = sp]
    seeds.fushan$seeds_mean_log = log(seeds.fushan$seeds_mean)
    return(seeds.fushan)
  }
  
  if(CTFS.plot == "seedlings_fushan"){
    
    # load in the main census data and compile them in a list
    load("data/fushan/seeds_seedlings/seedling_42.RData")
    seedlings.fushan = data.table(seedling)
    seedlings.fushan$sp = tolower(seedlings.fushan$sp)
    table(seedlings.fushan$status)
    
    # recode status
    seedlings.fushan$status[seedlings.fushan$status %in% c("G","N")] = "D"
    seedlings.fushan$status[seedlings.fushan$status == "G"] = "D"
    
    # remove seedlings that crossed the 1cm threshold
    # seedlings.fushan = seedlings.fushan[status != "L"]

    # remove unknown species
    seedlings.fushan = seedlings.fushan[sp != "unk"]
    
    table(seedlings.fushan$year, seedlings.fushan$month)
    
    # only use 2004-2014, so measurements are always in november
    seedlings.fushan = seedlings.fushan[year > 2003 & year < 2015]
    seedlings.fushan = seedlings.fushan[month == 11]
    
    # create column for exact date
    seedlings.fushan$ExactDate = seedlings.fushan$date
    
    # create treeID for growth and survival calculations
    seedlings.fushan$treeID = paste(seedlings.fushan$trap,  seedlings.fushan$plot, sep = "_")
    seedlings.fushan$treeID = paste(seedlings.fushan$treeID,  seedlings.fushan$tag, sep = "_")
    seedlings.fushan$treeID = as.numeric(factor(seedlings.fushan$treeID))
    seedlings.fushan$tree_stemID = seedlings.fushan$treeID
    
    # create an alibi column for height of measurement
    seedlings.fushan$hom = 1.3
    # create alibi column for dbh 
    seedlings.fushan$dbh = seedlings.fushan$ht
    
    # create census list
    census.list = list("census1" = seedlings.fushan[year == 2004],
                       "census2" = seedlings.fushan[year == 2005],
                       "census3" = seedlings.fushan[year == 2006],
                       "census4" = seedlings.fushan[year == 2007],
                       "census5" = seedlings.fushan[year == 2008],
                       "census6" = seedlings.fushan[year == 2009],
                       "census7" = seedlings.fushan[year == 2010],
                       "census8" = seedlings.fushan[year == 2011],
                       "census9" = seedlings.fushan[year == 2012],
                       "census10" = seedlings.fushan[year == 2013],
                       "census11" = seedlings.fushan[year == 2014])
    
    # calculate julian date
    for(i in 1: length(census.list)){
      census.list[[i]]$date = as.numeric(as.Date(as.character(census.list[[i]]$ExactDate),"%Y-%m-%d") - as.Date("1960-01-01","%Y-%m-%d"))}
    
    # add prior individuals
    for(i in 1:(length(census.list)-1)){
      later.census = census.list[[i+1]]
      earlier.census = census.list[[i]]
      
      later.census = later.census[status == "A"]
      later.census = later.census[!(treeID %in% earlier.census$treeID)]
      later.census$ExactDate = mean(earlier.census$ExactDate)
      later.census$date = mean(earlier.census$date)
      later.census$status = "P"
      later.census$ht = NA
      later.census$leafno = NA
      census.list[[i]] = rbind(census.list[[i]], later.census)}
    
    return(census.list)
  }
  
  
  
  if(CTFS.plot == "lambir"){
    # load in the main census data and compile them in a list
    # create census list
    load("data/lambir/inventory/old from nadja/lambir.full1.rdata")
    load("data/lambir/inventory/old from nadja/lambir.full2.rdata")
    load("data/lambir/inventory/old from nadja/lambir.full3.rdata")
    load("data/lambir/inventory/old from nadja/lambir.full4.rdata")
    
    census.list = list(census1 = data.table(lambir.full1),
                       census2 = data.table(lambir.full2),
                       census3 = data.table(lambir.full3),
                       census4 = data.table(lambir.full4))
    
    # replace dates that are the same between censuses with 0000-00-00
    for(i in 2:length(census.list)){
      earlier.census = census.list[[i-1]]
      later.census = census.list[[i]]
      earlier.census$treeID_ExactDate = paste(earlier.census$treeID, as.character(earlier.census$ExactDate), sep = "_")
      later.census$treeID_ExactDate = paste(later.census$treeID, as.character(later.census$ExactDate), sep = "_")
      later.census$ExactDate[which(later.census$treeID_ExactDate %in% earlier.census$treeID_ExactDate)] = "0000-00-00"
      later.census$treeID_ExactDate = NULL
      census.list[[i]] = later.census}
    
    # replace date 0000-00-00 with mean date if status == alive
    for(i in 1:length(census.list)){
      census.list[[i]]$ExactDate[which(census.list[[i]]$ExactDate == "0000-00-00" & census.list[[i]]$status == "A")] = 
        as.character(mean(as.Date(census.list[[i]]$ExactDate), na.rm = T))}
    
    # calculate julian dates
    for(i in 1:length(census.list)){
      census.list[[i]]$date = as.numeric(as.Date(as.character(census.list[[i]]$ExactDate),"%Y-%m-%d") - as.Date("1960-01-01","%Y-%m-%d"))}
    
    # dbh must be multiplied by 10
    for(i in 1:length(census.list)){
      census.list[[i]]$dbh = census.list[[i]]$dbh  * 10}
    
    # make sp names lower cases
    for(i in 1:length(census.list)){
      census.list[[i]]$sp = tolower(census.list[[i]]$sp)}
    
    # dead individuals should not have a dbh measurement
    for(i in 1:length(census.list)){
      census.list[[i]]$dbh[census.list[[i]]$status == "D"] = NA}
    
    # manual cleaning
    census.list[[3]]$status[census.list[[3]]$treeID == 169352] = "D" # dbh = 182 in census2 and 0.2 in census3 and dead in census4
    census.list[[3]]$dbh[census.list[[3]]$treeID == 169352] = NA
    
    census.list[[1]] = census.list[[1]][treeID != 146841] # dbh of 0.6 in census1 and then dead in all folowing censuses
    census.list[[2]] = census.list[[2]][treeID != 146841]
    census.list[[3]] = census.list[[3]][treeID != 146841]
    census.list[[4]] = census.list[[4]][treeID != 146841]
    
    # dbh can go under 10 in the next census
  }
  
  
  if(CTFS.plot == "nanjenshan"){
    # load in the main census data and compile them in a list
    load("data/nanjenshan/inventory/kenting.full1.rdata")
    load("data/nanjenshan/inventory/kenting.full2.rdata")
    load("data/nanjenshan/inventory/kenting.full3.rdata")
    
    census.list = list(census1 = data.table(kenting.full1),
                       census2 = data.table(kenting.full2),
                       census3 = data.table(kenting.full3))
    
    for(i in 1:length(census.list)){
      
      # <- dbh in nanjenshan is in cm and in nanjenshan in mm, so I have to multiply by 10
      census.list[[i]]$dbh = census.list[[i]]$dbh * 10
      
      # make dbh NA when individuals are dead
      census.list[[i]]$dbh[which(census.list[[i]]$status == "D")] = NA
      
      # lower cases sp codes
      census.list[[i]]$sp = tolower(census.list[[i]]$sp)}
    
    # remove dbh measurement < 10 and set status for those at "P"
    census.list[[1]]$status[which(census.list[[1]]$dbh < 10)] = "P"
    census.list[[1]]$dbh[which(census.list[[1]]$dbh < 10)] = NA
    
    # set all trees that have dbh == 0 in census 2 to missing
    errors = census.list[[2]][dbh < 10]
    census.list[[2]]$status[census.list[[2]]$dbh < 10] = "M"
    census.list[[2]]$dbh[census.list[[2]]$dbh < 10] = NA
    
    census.list[[3]]$status[census.list[[3]]$dbh < 10 & census.list[[3]]$status == "A"] = "M"
    census.list[[3]]$dbh[census.list[[3]]$status == "M"] = NA
    
    census.list[[3]]$dbh[census.list[[3]]$status == "D"] = NA
    
    
    #census.list[[1]][treeID %in% errors$treeID]
    #census.list[[2]][treeID %in% errors$treeID]
    #census.list[[3]][treeID %in% errors$treeID]
    
    # it is quite not perfect, not every alive ind has a status "P" or "A" in the previous census,
    # but like this the canopy asignment will be correct and based on all available measurements
    # and the growth, survival and recruitment will be calculated correctly because of data checks then
  }
  
  if(CTFS.plot == "palanan"){
    # load in the main census data and compile them in a list
    
    census.list = list(census1 = fread("data/palanan/inventory/Palanan1.txt"),
                       census2 = fread("data/palanan/inventory/Palanan2.txt"),
                       census3 = fread("data/palanan/inventory/Palanan3.txt"),
                       census4 = fread("data/palanan/inventory/Palanan4.txt"))
    
    # rename
    for(i in 1:length(census.list)){
      names(census.list[[i]]) = c("runningID","full_name","sp","subsp","quadrat","gx", "gy","treeID",
                                  "tag", "stemID", "stemtag", "census", "dbh", "hom", "ExactDate", "codes", "stem", "status")}
    
    # replace date 0000-00-00 with mean date if status == alive
    for(i in 1:length(census.list)){
      census.list[[i]]$ExactDate[which(census.list[[i]]$ExactDate == "0000-00-00" & census.list[[i]]$status == "alive")] = 
        mean(as.Date(census.list[[i]]$ExactDate), na.rm = T)}
    
    # calculate julian dates
    for(i in 1:length(census.list)){
      census.list[[i]]$date = as.numeric(as.Date(as.character(census.list[[i]]$ExactDate),"%Y-%m-%d") - as.Date("1960-01-01","%Y-%m-%d"))}
    
    # replace dbh == 0 with NA
    for(i in 1:length(census.list)){
      census.list[[i]]$dbh[census.list[[i]]$dbh == 0] = NA}
    
    # dbh must be multiplied by 10
    for(i in 1:length(census.list)){
      census.list[[i]]$dbh = census.list[[i]]$dbh  * 10}
    
    # rename status
    for(i in 1:length(census.list)){
      census.list[[i]]$status[census.list[[i]]$status == "alive"] = "A"
      census.list[[i]]$status[census.list[[i]]$status == "dead"] = "D"
      census.list[[i]]$status[census.list[[i]]$status == "missing"] = "M"}
    
    # make sp names lower cases
    for(i in 1:length(census.list)){
      census.list[[i]]$sp = tolower(census.list[[i]]$sp)}
  }
  
  if(CTFS.plot == "pasoh"){
    # load in the main census data and compile them in a list
    load("data/pasoh/inventory/pasoh.full1.rdata")
    load("data/pasoh/inventory/pasoh.full2.rdata")
    load("data/pasoh/inventory/pasoh.full3.rdata")
    load("data/pasoh/inventory/pasoh.full4.rdata")
    load("data/pasoh/inventory/pasoh.full5.rdata")
    load("data/pasoh/inventory/pasoh.full6.rdata")
    
    census.list = list(census1 = data.table(pasoh.full1),
                       census2 = data.table(pasoh.full2),
                       census3 = data.table(pasoh.full3),
                       census4 = data.table(pasoh.full4),
                       census5 = data.table(pasoh.full5),
                       census6 = data.table(pasoh.full6))
    
    # correct incorrect and missing dates
    census.list[[5]]$ExactDate[which(census.list[[5]]$ExactDate == "0000-00-00")] = NA
    census.list[[5]]$ExactDate[is.na(census.list[[5]]$ExactDate)] = mean(as.Date(census.list[[5]]$ExactDate, na.rm = T))
    census.list[[6]]$ExactDate[is.na(census.list[[6]]$ExactDate)] = mean(as.Date(census.list[[6]]$ExactDate, na.rm = T))
    census.list[[6]]$ExactDate[which(as.Date(census.list[[6]]$ExactDate) <= "2010-03-02")] = mean(as.Date(census.list[[6]]$ExactDate, na.rm = T))
    
    # keep only the largest stem of each tree and make species lower case
    for(i in 1: length(census.list)){
      census.list[[i]] = census.list[[i]][order(census.list[[i]]$dbh, decreasing = T)]
      census.list[[i]] = census.list[[i]][which(!(duplicated(census.list[[i]]$treeID)))]
      census.list[[i]] = census.list[[i]][order(census.list[[i]]$treeID)]
      census.list[[i]]$sp = tolower(as.character(census.list[[i]]$sp))}
  }
  
  if(CTFS.plot == "pasoh_before2005"){
    # load in the main census data and compile them in a list
    load("data/pasoh/inventory/pasoh.full1.rdata")
    load("data/pasoh/inventory/pasoh.full2.rdata")
    load("data/pasoh/inventory/pasoh.full3.rdata")
    load("data/pasoh/inventory/pasoh.full4.rdata")
    
    census.list = list(census1 = data.table(pasoh.full1),
                       census2 = data.table(pasoh.full2),
                       census3 = data.table(pasoh.full3),
                       census4 = data.table(pasoh.full4))
    
    # keep only the largest stem of each tree and make species lower case
    for(i in 1: length(census.list)){
      census.list[[i]] = census.list[[i]][order(census.list[[i]]$dbh, decreasing = T)]
      census.list[[i]] = census.list[[i]][which(!(duplicated(census.list[[i]]$treeID)))]
      census.list[[i]] = census.list[[i]][order(census.list[[i]]$treeID)]
      census.list[[i]]$sp = tolower(as.character(census.list[[i]]$sp))}
  }
  
  if(CTFS.plot == "pasoh_after2005"){
    # load in the main census data and compile them in a list
    load("data/pasoh/inventory/pasoh.full5.rdata")
    load("data/pasoh/inventory/pasoh.full6.rdata")
    
    census.list = list(census5 = data.table(pasoh.full5),
                       census6 = data.table(pasoh.full6))
    
    # correct incorrect and missing dates
    census.list$census5$ExactDate[which(census.list$census5$ExactDate == "0000-00-00")] = NA
    census.list$census5$ExactDate[is.na(census.list$census5$ExactDate)] = mean(as.Date(census.list$census5$ExactDate, na.rm = T))
    census.list$census6$ExactDate[is.na(census.list$census6$ExactDate)] = mean(as.Date(census.list$census6$ExactDate, na.rm = T))
    census.list$census6$ExactDate[which(as.Date(census.list$census6$ExactDate) <= "2010-03-02")] = mean(as.Date(census.list$census6$ExactDate[which(as.Date(census.list$census6$ExactDate) > "2010-03-02")], na.rm = T))
    
    # keep only the largest stem of each tree and make species lower case
    for(i in 1: length(census.list)){
      census.list[[i]] = census.list[[i]][order(census.list[[i]]$dbh, decreasing = T)]
      census.list[[i]] = census.list[[i]][which(!(duplicated(census.list[[i]]$treeID)))]
      census.list[[i]] = census.list[[i]][order(census.list[[i]]$treeID)]
      census.list[[i]]$sp = tolower(as.character(census.list[[i]]$sp))}
  }
  
  if(CTFS.plot %in% c("sherman","sherman_old_growth")){
    # load in the main census data and compile them in a list
    census.list = list(census1 = fread("data/sherman/inventory/sherman_1_from_online.txt"),
                       census2 = fread("data/sherman/inventory/sherman_2_from_online.txt"),
                       census3 = fread("data/sherman/inventory/sherman_3_from_online.txt"),
                       census4 = fread("data/sherman/inventory/sherman_4_from_online.txt"))
    
    # rename + format date
    for(i in 1:length(census.list)){
      names(census.list[[i]]) = c("Row_nr", "sp_full_name", "sp", "subsp", "quadrat", "gx", "gy", "treeID", "tag", "stemID",
                                  "stemTag", "census_ID", "dbh" ,"hom" ,"ExactDate", "codes", "stem", "status")
      
      # if only the old-growth part should be analysed
      if(CTFS.plot == "sherman_old_growth"){
        census.list[[i]] = census.list[[i]][gy <= 340]}
      
      census.list[[i]]$date = as.numeric(as.Date(as.character(census.list[[i]]$ExactDate),"%Y-%m-%d") - as.Date("1960-01-01","%Y-%m-%d"))
      
      # rename status 
      census.list[[i]]$status[which(census.list[[i]]$status == "alive")] = "A"
      census.list[[i]]$status[which(census.list[[i]]$status == "dead")] = "D"
      census.list[[i]]$status[which(census.list[[i]]$status == "broken below")] = "D"
      census.list[[i]]$status[which(census.list[[i]]$status == "stem dead")] = "D"
      census.list[[i]]$status[which(census.list[[i]]$status == "missing")] = "M"
    }
    
    # remove duplicates, i.e. multiple stems per tree
    for(i in 1:length(census.list)){
      census.list[[i]]  = census.list[[i]][order(census.list[[i]]$treeID, -abs(census.list[[i]]$dbh) ), ] #sort by id and reverse of abs(dbh)
      census.list[[i]] = census.list[[i]][ !duplicated(census.list[[i]]$treeID), ] # remove duplicates
    }
    
    # add missing trees in census 4
    to.add.missing = census.list[[3]][status == "A" & !(treeID %in% census.list[[4]]$treeID)]
    to.add.missing$dbh = NA
    to.add.missing$status = "M"
    to.add.missing$ExactDate = as.character(mean(as.Date(census.list[[4]]$ExactDate)))
    to.add.missing$date  = mean(census.list[[4]]$date)
    census.list[[4]] = rbind(census.list[[4]], to.add.missing)
    
  }
  
  if(CTFS.plot == "luquillo"){
    
    luquillo.full1 = fread("data/luquillo/inventory/luquillo_1_from_online.txt")
    luquillo.full2 = fread("data/luquillo/inventory/luquillo_2_from_online.txt")
    luquillo.full3 = fread("data/luquillo/inventory/luquillo_3_from_online.txt")
    luquillo.full4 = fread("data/luquillo/inventory/luquillo_4_from_online.txt")
    luquillo.full5 = fread("data/luquillo/inventory/luquillo_5_from_online.txt")
    luquillo.full6 = fread("data/luquillo/inventory/luquillo_6_from_online.txt")
    
    census.list = list(census1 = data.table(luquillo.full1),
                       census2 = data.table(luquillo.full2),
                       census3 = data.table(luquillo.full3),
                       census4 = data.table(luquillo.full4),
                       census5 = data.table(luquillo.full5),
                       census6 = data.table(luquillo.full6))
    
    for(i in 1:length(census.list)){
      
      # rename
      names(census.list[[i]]) = c("line_nr","sp_name_full","sp","is_subsp","quadrat","gx","gy","treeID","tag","stemID",
                                  "stemTag","census","dbh","hom","ExactDate","codes","stem","status")
      
      census.list[[i]]$status[which(census.list[[i]]$status == "alive")] = "A"
      census.list[[i]]$status[which(census.list[[i]]$status == "dead")] = "D"
      census.list[[i]]$status[which(census.list[[i]]$status == "stem dead")] = "D"
      census.list[[i]]$status[which(census.list[[i]]$status == "broken below")] = "D"
      
      # calculate julian date
      census.list[[i]]$date = as.numeric(as.Date(as.character(census.list[[i]]$ExactDate),"%Y-%m-%d") - as.Date("1960-01-01","%Y-%m-%d"))
      
      # keep only the largest stem of each tree
      census.list[[i]] = census.list[[i]][order(census.list[[i]]$dbh, decreasing = T)]
      census.list[[i]] = census.list[[i]][which(!(duplicated(census.list[[i]]$treeID)))]
      census.list[[i]] = census.list[[i]][order(census.list[[i]]$treeID)]
      
      census.list[[i]]$sp = tolower(census.list[[i]]$sp)
      }
  }
  
  if(CTFS.plot == "luquillo_census123"){
    
    luquillo.full1 = fread("data/luquillo/inventory/luquillo_1_from_online.txt")
    luquillo.full2 = fread("data/luquillo/inventory/luquillo_2_from_online.txt")
    luquillo.full3 = fread("data/luquillo/inventory/luquillo_3_from_online.txt")
    
    census.list = list(census1 = data.table(luquillo.full1),
                       census2 = data.table(luquillo.full2),
                       census3 = data.table(luquillo.full3))
    
    for(i in 1:length(census.list)){
      
      # rename
      names(census.list[[i]]) = c("line_nr","sp_name_full","sp","is_subsp","quadrat","gx","gy","treeID","tag","stemID",
                                  "stemTag","census","dbh","hom","ExactDate","codes","stem","status")
      
      census.list[[i]]$status[which(census.list[[i]]$status == "alive")] = "A"
      census.list[[i]]$status[which(census.list[[i]]$status == "dead")] = "D"
      census.list[[i]]$status[which(census.list[[i]]$status == "stem dead")] = "D"
      census.list[[i]]$status[which(census.list[[i]]$status == "broken below")] = "D"
      
      # calculate julian date
      census.list[[i]]$date = as.numeric(as.Date(as.character(census.list[[i]]$ExactDate),"%Y-%m-%d") - as.Date("1960-01-01","%Y-%m-%d"))
      
      # keep only the largest stem of each tree
      census.list[[i]] = census.list[[i]][order(census.list[[i]]$dbh, decreasing = T)]
      census.list[[i]] = census.list[[i]][which(!(duplicated(census.list[[i]]$treeID)))]
      census.list[[i]] = census.list[[i]][order(census.list[[i]]$treeID)]}
  }
  
  if(CTFS.plot == "luquillo_census456"){
    
    luquillo.full4 = fread("data/luquillo/inventory/luquillo_4_from_online.txt")
    luquillo.full5 = fread("data/luquillo/inventory/luquillo_5_from_online.txt")
    luquillo.full6 = fread("data/luquillo/inventory/luquillo_6_from_online.txt")
    
    census.list = list(census4 = data.table(luquillo.full4),
                       census5 = data.table(luquillo.full5),
                       census6 = data.table(luquillo.full6))
    
    for(i in 1:length(census.list)){
      
      # rename
      names(census.list[[i]]) = c("line_nr","sp_name_full","sp","is_subsp","quadrat","gx","gy","treeID","tag","stemID",
                                  "stemTag","census","dbh","hom","ExactDate","codes","stem","status")
      
      census.list[[i]]$status[which(census.list[[i]]$status == "alive")] = "A"
      census.list[[i]]$status[which(census.list[[i]]$status == "dead")] = "D"
      census.list[[i]]$status[which(census.list[[i]]$status == "stem dead")] = "D"
      census.list[[i]]$status[which(census.list[[i]]$status == "broken below")] = "D"
      
      # calculate julian date
      census.list[[i]]$date = as.numeric(as.Date(as.character(census.list[[i]]$ExactDate),"%Y-%m-%d") - as.Date("1960-01-01","%Y-%m-%d"))
      
      # keep only the largest stem of each tree
      census.list[[i]] = census.list[[i]][order(census.list[[i]]$dbh, decreasing = T)]
      census.list[[i]] = census.list[[i]][which(!(duplicated(census.list[[i]]$treeID)))]
      census.list[[i]] = census.list[[i]][order(census.list[[i]]$treeID)]}
  }
  
  
  if(CTFS.plot == "sinharaja"){
    # load in the main census data and compile them in a list
    
    # create census list
    load("data/sinharaja/inventory/sinharaja.stem3.rdata")
    census.list = list(census1 = fread("data/sinharaja/inventory/sinharaja_census1.txt"),
                       census2 = fread("data/sinharaja/inventory/sinharaja_census2.txt"),
                       census3 = data.table(sinharaja.stem3))
    
    names(census.list[[1]]) = c("No","Latin","sp","SubSpecies","quadrat","gx","gy","treeID",
                                "tag","stemID","stemtag","census","dbh","hom","ExactDate","codes",
                                "stem","status")
    names(census.list[[2]]) = c("No","Latin","sp","SubSpecies","quadrat","gx","gy","treeID",
                                "tag","stemID","stemtag","census","dbh","hom","ExactDate","codes",
                                "stem","status")
    names(census.list[[3]]) = c("treeID","stemID", "tag", "stemtag","sp","quadrat","gx","gy","dbhID",
                                "census", "dbh","pom","hom","ExactDate","DFstatus","codes","countPOM","date",
                                "status","agb")
    
    # replace status information with the ususal coding
    for(i in 1:length(census.list)){
      census.list[[i]]$status[which(census.list[[i]]$status == "alive")] = "A"
      census.list[[i]]$status[which(census.list[[i]]$status == "broken below")] = "D"
      census.list[[i]]$status[which(census.list[[i]]$status == "dead")] = "D"
      census.list[[i]]$status[which(census.list[[i]]$status == "missing")] = "M"
      census.list[[i]]$status[which(census.list[[i]]$status == "G")] = "M"}
    
    # remove dead or missing individuals in census 1
    census.list[[1]] = census.list[[1]][status == "A"]
    
    # set status to M where dbh == NA, but status is not explicit D
    for(i in 1:length(census.list)){
      census.list[[i]]$status[which(!(census.list[[i]]$status == "D") & is.na(census.list[[i]]$dbh))] = "M"}
    
    # keep only the largest stem of each tree
    for(i in 1: length(census.list)){
      census.list[[i]] = census.list[[i]][order(census.list[[i]]$dbh, decreasing = T)]
      census.list[[i]] = census.list[[i]][which(!(duplicated(census.list[[i]]$treeID)))]
      census.list[[i]] = census.list[[i]][order(census.list[[i]]$treeID)]}
    
    # add julian Date and replace status with "A", "D", "P" and make sp names lower case
    for(i in 1:length(census.list)){
      census.list[[i]]$date = as.numeric(as.Date(as.character(census.list[[i]]$ExactDate),"%Y-%m-%d") - as.Date("1960-01-01","%Y-%m-%d"))
      census.list[[i]]$status[which(census.list[[i]]$status == "alive")] = "A"
      census.list[[i]]$status[which(census.list[[i]]$status %in% c("dead","broken below"))] = "D"
      census.list[[i]]$sp  = tolower(as.character(census.list[[i]]$sp))}
    
    # make species names lower case
    census.list[[3]]$sp = tolower(census.list[[3]]$sp)
    
    # remove/add dead/missing ind in census 3 that are not in census 2
    to.add = census.list[[1]][treeID == 11]
    to.add$dbh = NA; to.add$status = "M" 
    to.add$ExactDate = as.character(mean(as.Date(census.list[[2]]$ExactDate)))
    to.add$date = mean(census.list[[2]]$date)
    census.list[[2]] = rbind(census.list[[2]], to.add)
    
    census.list[[3]] = census.list[[3]][!(treeID %in% c(11, 10735, 90347, 191427, 299498))]
    
    # there are two individuals that are alive in census1 and census3 but missing from census2
    # I average their growth for census2 because there 600 - 1800 other individuals so 
    # this little inaccurracy will will not have any weight
    to.add1 = census.list[[1]][treeID == 303508]
    to.add1$dbh = mean(c(census.list[[1]][treeID == 303508]$dbh[1], census.list[[3]][treeID == 303508]$dbh[1]))
    to.add1$ExactDate = as.character(mean(as.Date(census.list[[2]]$ExactDate)))
    to.add1$date = mean(census.list[[2]]$date)
    
    to.add2 = census.list[[1]][treeID == 303509]
    to.add2$dbh = mean(c(census.list[[1]][treeID == 303509]$dbh[1], census.list[[3]][treeID == 303509]$dbh[1]))
    to.add2$ExactDate = as.character(mean(as.Date(census.list[[2]]$ExactDate)))
    to.add2$date = mean(census.list[[2]]$date)
    
    census.list[[2]] = rbind(census.list[[2]], to.add1, to.add2)
    
    return(census.list)
  }
  
  if(CTFS.plot == "yasuni"){
    # load in the main census data and compile them in a list
    load("data/yasuni/inventory/census 1-2(CTFS format)/yasuni.full1.rdata")
    load("data/yasuni/inventory/census 1-2(CTFS format)/yasuni.full2.rdata")
    load("data/yasuni/inventory/census 2-3/cns2007hom.Rdata")
    
    census.list = list(census1 = data.table(yasuni.full1),
                       census2 = data.table(yasuni.full2),
                       census3 = data.table(cns2007hom))
    
    # rename
    names(census.list[[3]]) = c("tag","sp","gx","gy","dbh","pom","date","codes","status","hom" )
    
    # sp to lower cases
    for(i in 1: length(census.list)){
      census.list[[i]]$sp = tolower(as.character(census.list[[i]]$sp))}
    
    # add treeID and stemID to 3rd census, based on tags
    census.list[[3]]$treeID = NA
    census.list[[3]]$stemID = NA
    census.list[[3]]$treeID = census.list[[2]]$treeID[match(census.list[[3]]$tag, census.list[[2]]$tag)]
    census.list[[3]]$stemID = census.list[[2]]$stemID[match(census.list[[3]]$tag, census.list[[2]]$tag)]
    census.list[[3]]$treeID[which(is.na(census.list[[3]]$treeID))] = 
      max(census.list[[3]]$treeID, na.rm = T) + seq(from = 1, to = length(which(is.na(census.list[[3]]$treeID))))
    census.list[[3]]$stemID[which(is.na(census.list[[3]]$stemID))] = 1
    census.list[[3]]$stemID[which(duplicated(census.list[[3]][,c("gx","gy")]))] = 2
    
    # rename status and make dbh NA
    census.list[[3]]$status[which(census.list[[3]]$status == "AB")] = "D"
    
    for(i in 1:length(census.list)){
      census.list[[i]]$dbh[which(census.list[[i]]$status == "P")] = NA
      census.list[[i]]$dbh[which(census.list[[i]]$status == "D")] = NA}
    
    # # in yasuni 1 I replace the status of all ind with dbh < 10 with "P" but I do not do this in yasuni 2 and 3
    # still I replace all dbh values < 10 with NA
    census.list[[1]]$status[which(census.list[[1]]$dbh < 10)] = "P"
    
    # remove all P, they will be added later
    for(i in 1: length(census.list)){
      census.list[[i]] = census.list[[i]][which(census.list[[i]]$status != "P")]}
    
    # convert date in yasuni.full3
    census.list[[3]]$ExactDate =  as.character(as.Date(census.list[[3]]$date, origin=as.Date("1960-01-01")))
    
    # keep only the largest stem of each tree
    for(i in 1: length(census.list)){
      census.list[[i]] = census.list[[i]][order(census.list[[i]]$dbh, decreasing = T)]
      census.list[[i]] = census.list[[i]][which(!(duplicated(census.list[[i]]$treeID)))]
      census.list[[i]] = census.list[[i]][order(census.list[[i]]$treeID)]}
    
    # in census 3 the pom is not correct, so I replace all pom with 1.3 if hom = 13
    census.list[[3]]$pom = as.character(census.list[[3]]$pom)
    census.list[[3]]$pom[which(census.list[[3]]$hom == 1.3)] = "1.3"
    
    # add missing ind in census3
    to.add.missing  = census.list[[2]][status == "A"]
    to.add.missing = to.add.missing[!(treeID %in% census.list[[3]]$treeID)]
    to.add.missing$dbh = NA
    to.add.missing$ExactDate = as.character(mean(as.Date(census.list[[3]]$ExactDate)))
    to.add.missing$date = mean(census.list[[3]]$date)
    to.add.missing = to.add.missing[,names(census.list[[3]]), with = F]
    
    census.list[[3]] = rbind(census.list[[3]],
                             to.add.missing,
                             use.names = T)
    # remove individuals that are marked "D" in census2 and census3 but are not in census1
    for(i in length(census.list)){
      census.list[[i]] = census.list[[i]][!(tag %in% c(149990, 90438, 133869, 140124, 155588, 159966))]}
  }
  
  
  
  return(census.list)
  
}

################################################################
# read in all the different CTFS census data specifications ----
load.census.specifications = function(CTFS.plot, census.list){
  
  all.censuses.dt = rbindlist(census.list,fill = T, idcol= "census_id")
  dbh_median = all.censuses.dt[,.(dbh_median = median(dbh, na.rm = T)), by = census_id]
  dbh_cov = all.censuses.dt[,.(dbh_sd = sd(dbh, na.rm = T),
                               dbh_mean = mean(dbh, na.rm = T)), by = census_id]
  dbh_cov$cov = dbh_cov$dbh_sd / dbh_cov$dbh_mean
  dbh_range = all.censuses.dt[,.(dbh_min = min(dbh, na.rm = T),
                                 dbh_max = max(dbh, na.rm = T)), by = census_id]
  dbh_range$dbh_range = abs(dbh_range$dbh_max - dbh_range$dbh_min)
  
  dates = c()
  for(i in 1:length(census.list)){
    dates = c(dates, as.character(mean(as.Date(census.list[[i]]$ExactDate), na.rm = T)))}
  
  if(CTFS.plot == "bci"){
    census.specifications = list(
      nr_of_crown_layers = 4,
      mean_dates = dates,
      dbh_conversion = 1,
      censuses_with_inaccurate_dbh_below_5cm = c("census1","census2"),
      add_dead_ind = F,
      add_prior_ind = F,
      codes_for_dead_trees = NULL,
      subplot_area = 31.25 * 31.25,
      max_dbh_selected = 2000,
      ca_allo_coefficient1 = 0.03615016,
      ca_allo_coefficient2 = 1.2819275,
      nr_species_in_census_list = length(unique(all.censuses.dt$sp)),
      dbh_median = mean(dbh_median$dbh_median, na.rm = T),
      dbh_cov = mean(dbh_cov$cov, na.rm = T),
      dbh_range = mean(dbh_range$dbh_range, na.rm = T))}
  
  if(CTFS.plot == "korup"){
    census.specifications = list(
      nr_of_crown_layers = 7,
      mean_dates = dates,
      dbh_conversion = 1,
      censuses_with_inaccurate_dbh_below_5cm = NULL,
      add_dead_ind = F,
      add_prior_ind = F,
      codes_for_dead_trees = NULL,
      subplot_area = 31.25 * 31.25,
      max_dbh_selected = 2000,
      ca_allo_coefficient1 = 0.03615016,
      ca_allo_coefficient2 = 1.2819275,
      nr_species_in_census_list = length(unique(all.censuses.dt$sp)),
      dbh_median = mean(dbh_median$dbh_median, na.rm = T),
      dbh_cov = mean(dbh_cov$cov, na.rm = T),
      dbh_range = mean(dbh_range$dbh_range, na.rm = T))}
  
  if(CTFS.plot == "hkk"){
    census.specifications = list(
      nr_of_crown_layers = 4,
      mean_dates = dates,
      dbh_conversion = 1,
      censuses_with_inaccurate_dbh_below_5cm = NULL,
      add_dead_ind = F,
      add_prior_ind = F,
      codes_for_dead_trees = NULL,
      subplot_area = 31.25 * 31.25,
      max_dbh_selected = 2000,
      ca_allo_coefficient1 = 0.03615016,
      ca_allo_coefficient2 = 1.2819275,
      nr_species_in_census_list = length(unique(all.censuses.dt$sp)),
      dbh_median = mean(dbh_median$dbh_median, na.rm = T),
      dbh_cov = mean(dbh_cov$cov, na.rm = T),
      dbh_range = mean(dbh_range$dbh_range, na.rm = T))}
  
  if(CTFS.plot == "ituri_edoro"){
    census.specifications = list(
      nr_of_crown_layers = 7,
      mean_dates = dates,
      dbh_conversion = 1,
      censuses_with_inaccurate_dbh_below_5cm = NULL,
      add_dead_ind = F,
      add_prior_ind = F,
      codes_for_dead_trees = NULL,
      subplot_area = 31.25 * 31.25,
      max_dbh_selected = 2000,
      ca_allo_coefficient1 = 0.03615016,
      ca_allo_coefficient2 = 1.2819275,
      nr_species_in_census_list = length(unique(all.censuses.dt$sp)),
      dbh_median = mean(dbh_median$dbh_median, na.rm = T),
      dbh_cov = mean(dbh_cov$cov, na.rm = T),
      dbh_range = mean(dbh_range$dbh_range, na.rm = T))}
  
  if(CTFS.plot == "ituri_lenda"){
    census.specifications = list(
      nr_of_crown_layers = 7,
      mean_dates = dates,
      dbh_conversion = 1,
      censuses_with_inaccurate_dbh_below_5cm = NULL,
      add_dead_ind = F,
      add_prior_ind = F,
      codes_for_dead_trees = NULL,
      subplot_area = 31.25 * 31.25,
      max_dbh_selected = 2000,
      ca_allo_coefficient1 = 0.03615016,
      ca_allo_coefficient2 = 1.2819275,
      nr_species_in_census_list = length(unique(all.censuses.dt$sp)),
      dbh_median = mean(dbh_median$dbh_median, na.rm = T),
      dbh_cov = mean(dbh_cov$cov, na.rm = T),
      dbh_range = mean(dbh_range$dbh_range, na.rm = T))}
  
  if(CTFS.plot == "fushan"){
    census.specifications = list(
      nr_of_crown_layers = 7,
      mean_dates = dates,
      dbh_conversion = 1,
      censuses_with_inaccurate_dbh_below_5cm = NULL,
      add_dead_ind = F,
      add_prior_ind = F,
      codes_for_dead_trees = NULL,
      subplot_area = 31.25 * 31.25,
      max_dbh_selected = 2000,
      ca_allo_coefficient1 = 0.03615016,
      ca_allo_coefficient2 = 1.2819275,
      nr_species_in_census_list = length(unique(all.censuses.dt$sp)),
      dbh_median = mean(dbh_median$dbh_median, na.rm = T),
      dbh_cov = mean(dbh_cov$cov, na.rm = T),
      dbh_range = mean(dbh_range$dbh_range, na.rm = T))}
  
  if(CTFS.plot == "seedlings_fushan"){
    census.specifications = list(
      nr_of_crown_layers = NA,
      mean_dates = dates,
      dbh_conversion = 1,
      censuses_with_inaccurate_dbh_below_5cm = NULL,
      add_dead_ind = F,
      add_prior_ind = F,
      codes_for_dead_trees = NULL,
      subplot_area = 31.25 * 31.25,
      max_dbh_selected = 2000,
      ca_allo_coefficient1 = 0.03615016,
      ca_allo_coefficient2 = 1.2819275,
      nr_species_in_census_list = length(unique(all.censuses.dt$sp)),
      ht_median = mean(dbh_median$dbh_median, na.rm = T),
      ht_cov = mean(dbh_cov$cov, na.rm = T),
      ht_range = mean(dbh_range$dbh_range, na.rm = T))}
  
  if(CTFS.plot == "lambir"){
    census.specifications = list(
      nr_of_crown_layers = 6,
      mean_dates = dates,
      dbh_conversion = 1,
      censuses_with_inaccurate_dbh_below_5cm = NULL,
      add_dead_ind = F,
      add_prior_ind = F,
      codes_for_dead_trees = NULL,
      subplot_area = 31.25 * 31.25,
      max_dbh_selected = 2000,
      ca_allo_coefficient1 = 0.03615016,
      ca_allo_coefficient2 = 1.2819275,
      nr_species_in_census_list = length(unique(all.censuses.dt$sp)),
      dbh_median = mean(dbh_median$dbh_median, na.rm = T),
      dbh_cov = mean(dbh_cov$cov, na.rm = T),
      dbh_range = mean(dbh_range$dbh_range, na.rm = T))}
  
  if(CTFS.plot == "nanjenshan"){
    census.specifications = list(
      nr_of_crown_layers = 5,
      mean_dates = dates,
      dbh_conversion = 1,
      censuses_with_inaccurate_dbh_below_5cm = NULL,
      add_dead_ind = F,
      add_prior_ind = F,
      codes_for_dead_trees = NULL,
      subplot_area = 31.25 * 31.25,
      max_dbh_selected = 2000,
      ca_allo_coefficient1 = 0.03615016,
      ca_allo_coefficient2 = 1.2819275,
      nr_species_in_census_list = length(unique(all.censuses.dt$sp)),
      dbh_median = mean(dbh_median$dbh_median, na.rm = T),
      dbh_cov = mean(dbh_cov$cov, na.rm = T),
      dbh_range = mean(dbh_range$dbh_range, na.rm = T))}
  
  if(CTFS.plot == "palanan"){
    census.specifications = list(
      nr_of_crown_layers = 5,
      mean_dates = dates,
      dbh_conversion = 1,
      censuses_with_inaccurate_dbh_below_5cm = NULL,
      add_dead_ind = F,
      add_prior_ind = T,
      codes_for_dead_trees = NULL,
      subplot_area = 31.25 * 31.25,
      max_dbh_selected = 2000,
      ca_allo_coefficient1 = 0.03615016,
      ca_allo_coefficient2 = 1.2819275,
      nr_species_in_census_list = length(unique(all.censuses.dt$sp)),
      dbh_median = mean(dbh_median$dbh_median, na.rm = T),
      dbh_cov = mean(dbh_cov$cov, na.rm = T),
      dbh_range = mean(dbh_range$dbh_range, na.rm = T))}
  
  if(CTFS.plot %in% c("sherman","sherman_old_growth")){
    census.specifications = list(
      nr_of_crown_layers = 4,
      mean_dates = dates,
      dbh_conversion = 1,
      censuses_with_inaccurate_dbh_below_5cm = NULL,
      add_dead_ind = F,
      add_prior_ind = T,
      codes_for_dead_trees = NULL,
      subplot_area = 31.25 * 31.25,
      max_dbh_selected = 2000,
      ca_allo_coefficient1 = 0.03615016,
      ca_allo_coefficient2 = 1.2819275,
      nr_species_in_census_list = length(unique(all.censuses.dt$sp)),
      dbh_median = mean(dbh_median$dbh_median, na.rm = T),
      dbh_cov = mean(dbh_cov$cov, na.rm = T),
      dbh_range = mean(dbh_range$dbh_range, na.rm = T))}
  
  if(CTFS.plot == "yasuni"){
    census.specifications = list(
      nr_of_crown_layers = 5,
      mean_dates = dates,
      dbh_conversion = 1,
      censuses_with_inaccurate_dbh_below_5cm = NULL,
      add_dead_ind = F,
      add_prior_ind = T,
      codes_for_dead_trees = NULL,
      subplot_area = 31.25 * 31.25,
      max_dbh_selected = 2000,
      ca_allo_coefficient1 = 0.03615016,
      ca_allo_coefficient2 = 1.2819275,
      nr_species_in_census_list = length(unique(all.censuses.dt$sp)),
      dbh_median = mean(dbh_median$dbh_median, na.rm = T),
      dbh_cov = mean(dbh_cov$cov, na.rm = T),
      dbh_range = mean(dbh_range$dbh_range, na.rm = T))}
  
  if(CTFS.plot %in% c("pasoh", "pasoh_before2005")){
    census.specifications = list(
      nr_of_crown_layers = 4,
      mean_dates = dates,
      dbh_conversion = 1,
      censuses_with_inaccurate_dbh_below_5cm = c("census1","census2"),
      add_dead_ind = F,
      add_prior_ind = F,
      codes_for_dead_trees = NULL,
      subplot_area = 31.25 * 31.25,
      max_dbh_selected = 2000,
      ca_allo_coefficient1 = 0.03848731,
      ca_allo_coefficient2 = 1.204,
      nr_species_in_census_list = length(unique(all.censuses.dt$sp)),
      dbh_median = mean(dbh_median$dbh_median, na.rm = T),
      dbh_cov = mean(dbh_cov$cov, na.rm = T),
      dbh_range = mean(dbh_range$dbh_range, na.rm = T))}
  
  if(CTFS.plot == "pasoh_after2005"){
    census.specifications = list(
      nr_of_crown_layers = 4,
      mean_dates = dates,
      dbh_conversion = 1,
      censuses_with_inaccurate_dbh_below_5cm = NA,
      add_dead_ind = F,
      add_prior_ind = F,
      codes_for_dead_trees = NULL,
      subplot_area = 31.25 * 31.25,
      max_dbh_selected = 2000,
      ca_allo_coefficient1 = 0.03848731,
      ca_allo_coefficient2 = 1.204,
      nr_species_in_census_list = length(unique(all.censuses.dt$sp)),
      dbh_median = mean(dbh_median$dbh_median, na.rm = T),
      dbh_cov = mean(dbh_cov$cov, na.rm = T),
      dbh_range = mean(dbh_range$dbh_range, na.rm = T))}
  
  if(CTFS.plot %in% c("luquillo", "luquillo_old_growth")){
    census.specifications = list(
      nr_of_crown_layers = 5,
      mean_dates = dates,
      dbh_conversion = 1,
      censuses_with_inaccurate_dbh_below_5cm = NA,
      add_dead_ind = F,
      add_prior_ind = T,
      codes_for_dead_trees = NULL,
      subplot_area = 31.25 * 31.25,
      max_dbh_selected = 2000,
      ca_allo_coefficient1 = 0.03615016,
      ca_allo_coefficient2 = 1.2819275,
      nr_species_in_census_list = length(unique(all.censuses.dt$sp)),
      dbh_median = mean(dbh_median$dbh_median, na.rm = T),
      dbh_cov = mean(dbh_cov$cov, na.rm = T),
      dbh_range = mean(dbh_range$dbh_range, na.rm = T))}
  
  if(CTFS.plot == "luquillo_census123"){
    census.specifications = list(
      nr_of_crown_layers = 5,
      mean_dates = dates,
      dbh_conversion = 1,
      censuses_with_inaccurate_dbh_below_5cm = NA,
      add_dead_ind = F,
      add_prior_ind = T,
      codes_for_dead_trees = NULL,
      subplot_area = 31.25 * 31.25,
      max_dbh_selected = 2000,
      ca_allo_coefficient1 = 0.03615016,
      ca_allo_coefficient2 = 1.2819275,
      nr_species_in_census_list = length(unique(all.censuses.dt$sp)),
      dbh_median = mean(dbh_median$dbh_median, na.rm = T),
      dbh_cov = mean(dbh_cov$cov, na.rm = T),
      dbh_range = mean(dbh_range$dbh_range, na.rm = T))}
  
  if(CTFS.plot == "luquillo_census456"){
    census.specifications = list(
      nr_of_crown_layers = 5,
      mean_dates = dates,
      dbh_conversion = 1,
      censuses_with_inaccurate_dbh_below_5cm = NA,
      add_dead_ind = F,
      add_prior_ind = T,
      codes_for_dead_trees = NULL,
      subplot_area = 31.25 * 31.25,
      max_dbh_selected = 2000,
      ca_allo_coefficient1 = 0.03615016,
      ca_allo_coefficient2 = 1.2819275,
      nr_species_in_census_list = length(unique(all.censuses.dt$sp)),
      dbh_median = mean(dbh_median$dbh_median, na.rm = T),
      dbh_cov = mean(dbh_cov$cov, na.rm = T),
      dbh_range = mean(dbh_range$dbh_range, na.rm = T))}
  
  if(CTFS.plot == "sinharaja"){
    census.specifications = list(
      nr_of_crown_layers = 7,
      mean_dates = dates,
      dbh_conversion = 1,
      censuses_with_inaccurate_dbh_below_5cm = NA,
      add_dead_ind = F,
      add_prior_ind = T,
      codes_for_dead_trees = NULL,
      subplot_area = 31.25 * 31.25,
      max_dbh_selected = 2000,
      ca_allo_coefficient1 = 0.03615016,
      ca_allo_coefficient2 = 1.2819275,
      nr_species_in_census_list = length(unique(all.censuses.dt$sp)),
      dbh_median = mean(dbh_median$dbh_median, na.rm = T),
      dbh_cov = mean(dbh_cov$cov, na.rm = T),
      dbh_range = mean(dbh_range$dbh_range, na.rm = T))}
  
  return(census.specifications)
}

######################################################
# load in the species lists --------------------------
load.species.list = function(CTFS.plot, census.list){
  
  if(CTFS.plot == "bci"){
    
    load("data/bci/species list/bci.spptable.rdata")
    bci.spptable$sp = tolower(as.character(bci.spptable$sp))
    species.info = list(species_table = bci.spptable[,c("sp","Latin","IDLevel")])
    names(species.info$species_table) = c("sp", "latin", "IDlevel")
    
    species.info$species_to_exclude = 
      unique(c(bci.spptable$sp[which(bci.spptable$Family %in% "Arecaceae")],
               species.info$species_table$sp[c(85:88,91,92,94)],
               species.info$species_table$sp[which(species.info$species_table$IDlevel %in% c("none", "multiple"))],
               "uniden","NA","spp", "tremin"))}
  
  if(CTFS.plot == "hkk"){
    
    load("data/hkk/species list/hkk.spptable.rdata")
    hkk.spptable = data.table(hkk.spptable)
    hkk.spptable$sp = tolower(as.character(hkk.spptable$sp))
    
    species.info = list(species_table = hkk.spptable[,c("sp","Latin","IDlevel")])
    names(species.info$species_table) = c("sp", "latin", "IDlevel")
    
    species.info$species_to_exclude = 
      unique(c(hkk.spptable$sp[which(hkk.spptable$Family %in% "Arecaceae")],
               species.info$species_table$sp[which(species.info$species_table$IDlevel %in% c("multiple","mixed"))],
               "uniden","NA","spp"))}
  
  if(CTFS.plot == "korup"){
    
    load("data/korup/species list/korup.spptable.rdata")
    korup.spptable = data.table(korup.spptable)
    korup.spptable$sp = tolower(as.character(korup.spptable$sp))
    
    species.info = list(species_table = korup.spptable[,c("sp","Latin","IDLevel")])
    names(species.info$species_table) = c("sp", "latin", "IDlevel")
    
    species.info$species_to_exclude = 
      unique(c(korup.spptable$sp[which(korup.spptable$Family %in% "Arecaceae")],
               species.info$species_table$sp[which(species.info$species_table$IDlevel %in% c("multiple","mixed"))],
               "uniden","NA","spp"))}
  
  if(CTFS.plot == "ituri_edoro"){
    
    load("data/ituri_edoro/species list/ituri.spptable.rdata")
    ituri.spptable = data.table(ituri.spptable)
    ituri.spptable$sp = tolower(as.character(ituri.spptable$sp))
    
    species.info = list(species_table = ituri.spptable[,c("sp","Latin","IDlevel")])
    names(species.info$species_table) = c("sp", "latin", "IDlevel")
    
    species.info$species_to_exclude = 
      unique(c(ituri.spptable$sp[which(ituri.spptable$Family %in% "Arecaceae")],
               species.info$species_table$sp[which(species.info$species_table$IDlevel %in% c("multiple","mixed"))],
               "uniden","NA","spp"))}
  
  if(CTFS.plot == "ituri_lenda"){
    
    load("data/ituri_lenda/species list/ituri.spptable.rdata")
    ituri.spptable = data.table(ituri.spptable)
    ituri.spptable$sp = tolower(as.character(ituri.spptable$sp))
    
    species.info = list(species_table = ituri.spptable[,c("sp","Latin","IDlevel")])
    names(species.info$species_table) = c("sp", "latin", "IDlevel")
    
    species.info$species_to_exclude = 
      unique(c(ituri.spptable$sp[which(ituri.spptable$Family %in% "Arecaceae")],
               species.info$species_table$sp[which(species.info$species_table$IDlevel %in% c("multiple","mixed"))],
               "uniden","NA","spp"))}
  
  if(CTFS.plot == "palanan"){
    
    species.table = fread("data/palanan/species list/Palanan species list.txt")
    Arecaceae_genuses = fread("functions/Genuses of Arecaceae_from plant list.txt")
    
    # rename
    names(species.table) = c("runningID", "family", "genus", "sp_only", "subsp", "sp", "IDlevel", "authority", "prior_names", "speciesID")
    species.table$sp = tolower(species.table$sp)
    species.table$latin = paste(species.table$genus, species.table$sp_only, sep = " ")
    
    # check IDlevel
    table(species.table$IDlevel)
    species.info = list(species_table = species.table[,c("sp","latin","IDlevel")])
    
    species.info$species_to_exclude = 
      unique(c(species.info$species_table$sp[which(species.table$genus %in% Arecaceae_genuses$Genus)],
               species.info$species_table$sp[which(species.info$species_table$IDlevel %in% c("multiple"))],
               "uniden","NA","spp"))}
  
  if(CTFS.plot %in% c("sherman","sherman_old_growth")){
    
    species.table = fread("data/sherman/species list/shermansp.txt")
    load("data/bci/species list/bci.spptable.rdata")
    Arecaceae_genuses = fread("functions/Genuses of Arecaceae_from plant list.txt")
    names(species.table) = c("sp", "genus", "species", "family" )
    species.table$sp = tolower(species.table$sp)
    species.table$latin = paste(species.table$genus, species.table$species, sep = " ")
    
    # add IDlevel
    species.table$IDlevel = "species"
    species.table$IDlevel[species.table$genus %in% c("Unidentified")] = "unidentified"
    species.table$IDlevel[species.table$family %in% c("*", "Unidentified")] = "unidentified"
    species.table$IDlevel[species.table$species %in% c("*", "Unidentified", "sp.1","sp.2",
                                                       "sp.3","sp.4","sp.5","sp.6", "species")] = "unidentified"
    
    species.info = list(species_table = species.table[,c("sp","latin","IDlevel")])
    names(species.info$species_table) = c("sp", "latin", "IDlevel")
    
    species.info$species_to_exclude = 
      unique(c(species.info$species_table$sp[which(species.table$genus %in% Arecaceae_genuses$Genus)],
               species.info$species_table$sp[which(species.info$species_table$IDlevel %in% c("unidentified"))],
               bci.spptable$sp[c(85:88,91,92,94)], 
               "uniden","NA","spp", "tremin"))}
  
  if(CTFS.plot == "fushan"){
    
    fushan.spp.table = fread("data/fushan/species list/splist_all.txt")
    Arecaceae_genuses = fread("functions/Genuses of Arecaceae_from plant list.txt")
    fushan.spp.table$sp = tolower(fushan.spp.table$sp)
    
    species.info = list(species_table = fushan.spp.table[,c("sp","fullname")])
    species.info$species_table$IDlevel = NA
    names(species.info$species_table) = c("sp", "latin", "IDlevel")
    
    exlude.arec = fushan.spp.table$sp[which(fushan.spp.table$genus %in% Arecaceae_genuses$Genus)]
    species.info$species_to_exclude = unique(c(exlude.arec,
                                               "uniden","NA","spp","cyatpo","cyatsp","cyatle"))}
  
  if(CTFS.plot == "lambir"){
    
    species.table = fread("data/lambir/species list/Lambir_species_list.txt")
    Arecaceae_genuses = fread("functions/Genuses of Arecaceae_from plant list.txt")
    names(species.table) = c("runningID","family","genus","sp_only","subsp","sp","IDlevel","authority","prior_names","speciesID" )
    species.table$latin = paste(species.table$genus, species.table$sp_only, sep = " ")
    species.table$sp = tolower(species.table$sp)
    
    # create the species.info
    species.info = list(species_table = species.table[,c("sp","latin","IDlevel")])
    names(species.info$species_table) = c("sp", "latin", "IDlevel")
    
    species.info$species_to_exclude = 
      unique(c(species.info$species_table$sp[which(species.table$genus %in% Arecaceae_genuses$Genus)],
               species.info$species_table$sp[which(species.info$species_table$IDlevel %in% c("multiple", "none"))],
               "uniden","NA","spp", "tremin"))}
  
  
  if(CTFS.plot == "nanjenshan"){
    
    spp.table = fread("data/nanjenshan/species list/forestgeo webpage and abbreviations merged.txt")
    Arecaceae_genuses = fread("functions/Genuses of Arecaceae_from plant list.txt")
    spp.table$latin = paste(spp.table$GENUS, spp.table$SPECIES, sep  =" ")
    
    species.info = list(species_table = spp.table[,c("sp","latin")])
    species.info$species_table$IDlevel = NA
    names(species.info$species_table) = c("sp", "latin", "IDlevel")
    
    exlude.arec = spp.table$sp[which(spp.table$genus %in% Arecaceae_genuses$Genus)]
    species.info$species_to_exclude = unique(c(exlude.arec,
                                               "uniden","NA","spp"))}
  
  if(CTFS.plot %in% c("pasoh", "pasoh_before2005","pasoh_after2005")){
    
    load("data/pasoh/species list/pasoh.spptable.rdata")
    Arecaceae_genuses = fread("functions/Genuses of Arecaceae_from plant list.txt")
    species.table = data.table(pasoh.spptable)
    species.table$sp = tolower(species.table$sp)
    
    species.info = list(species_table = species.table[,c("sp","Latin","IDLevel")])
    names(species.info$species_table) = c("sp", "latin", "IDlevel")
    
    nonsense.names = tolower(c("AAAAAA","BBBBBB","CCCCCC","DDDDDD","EEEEEE","FFFFFF","GGGGGG","HHHHHH","IIIIII","JJJJJJ","KKKKKK","LLLLLL","MMMMMM",
                               "NNNNNN","OOOOOO","PPPPPP","QQQQQQ","RRRRRR","SSSSSS","TTTTTT","UUUUUU","VVVVVV","WWWWWW","XXXXXX","YYYYYY","ZZZZZZ"))
    
    species.info$species_to_exclude = unique(c(species.info$species_table$sp[species.info$species_table$Family %in% "Arecaceae"],
                                               species.info$species_table$sp[species.info$species_table$Genus %in% Arecaceae_genuses$Genus],
                                               species.info$species_table$sp[species.info$species_table$IDlevel == "unidenti"],
                                               nonsense.names, "uniden","NA","spp"))
  }

  if(CTFS.plot == "yasuni"){
    
    load("data/yasuni/species list/splist95.rdata")
    load("data/yasuni/species list/splist2007.rdata")
    load("data/yasuni/species list/yasuni.spptable.rdata")
    Arecaceae_genuses = fread("functions/Genuses of Arecaceae_from plant list.txt")
    
    splist95 = data.table(splist95)
    splist2007 = data.table(splist2007)
    yasuni.spptable = data.table(yasuni.spptable)
    
    splist95$sp = tolower(splist95$sp)
    splist2007$sp = tolower(splist2007$sp)
    yasuni.spptable$sp = tolower(yasuni.spptable$sp)
    yasuni.spptable = yasuni.spptable[, c("genus", "specie") := tstrsplit(Latin, " ", fixed=TRUE)][]
    
    which(!(splist95$sp %in% splist2007$sp)) # all 95 sp are in 2007 :)
    which(!(yasuni.spptable$sp %in% splist2007$sp)) # some species are not in 2007 table
    
    species.table = splist2007
    species.table = rbind(data.frame(species.table), data.frame(
      "sp" = yasuni.spptable$sp[which(!(yasuni.spptable$sp %in% splist2007$sp))],
      "family" = yasuni.spptable$Family[which(!(yasuni.spptable$sp %in% splist2007$sp))],
      "genus" = yasuni.spptable$genus[which(!(yasuni.spptable$sp %in% splist2007$sp))],
      "specie" = yasuni.spptable$species[which(!(yasuni.spptable$sp %in% splist2007$sp))],
      "grform" = NA))
    species.table = data.table(species.table)
    
    # create one species colum
    species.table$Latin = paste(species.table$genus, species.table$specie, sep = " ")
    
    # which species to exclude
    to.exclude  = tolower(c("AAAAAA","BBBBBB","CCCCCC","DDDDDD","EEEEEE","FFFFFF","GGGGGG","HHHHHH","IIIIII","JJJJJJ","KKKKKK","LLLLLL","MMMMMM",
                            "NNNNNN","OOOOOO","PPPPPP","QQQQQQ","RRRRRR","SSSSSS","TTTTTT","UUUUUU","VVVVVV","WWWWWW","XXXXXX","YYYYYY","ZZZZZZ",
                            species.table$sp[species.table$grform %in% c("T,H")],
                            species.table$sp[species.table$family %in% "ARECACEAE"],
                            species.table$sp[species.table$genus %in% Arecaceae_genuses$Genus],
                            "uniden","NA","spp"))
    
    species.info = list(species_table = species.table[,c("sp","Latin")])
    names(species.info$species_table) = c("sp", "latin")
    
    species.info$species_to_exclude = to.exclude}
  
  
  if(CTFS.plot %in% c("luquillo","luquillo_old_growth")){
    
    species.table = fread("data/luquillo/species list/Luquillo_species_list.txt")
    Arecaceae_genuses = fread("functions/Genuses of Arecaceae_from plant list.txt")
    species.table$sp = tolower(as.character(species.table$Mnemonic))
    
    # create full species name
    species.table$Latin = paste(species.table$Genus, species.table$Species, sep = " ")
    
    species.info = list(species_table = species.table[,c("sp","Latin","IDlevel")])
    names(species.info$species_table) = c("sp", "latin", "IDlevel")
    
    species.info$species_to_exclude = 
      c(species.table$sp[which(species.table$Family %in% "Arecaceae")],
        species.table$sp[which(species.table$Genus %in% Arecaceae_genuses$Genus)],
        species.table$sp[which(species.table$IDlevel %in% c("none", "multiple"))],
        "premon", "roybor", "uniden", "NA", "spp")}
  
  if(CTFS.plot == "luquillo_census123"){
    
    species.table = fread("data/luquillo/species list/Luquillo_species_list.txt")
    Arecaceae_genuses = fread("functions/Genuses of Arecaceae_from plant list.txt")
    species.table$Species = tolower(as.character(species.table$Species))
    
    # create full species name
    species.table$Latin = paste(species.table$Genus, species.table$Species, sep = " ")
    
    species.info = list(species_table = species.table[,c("Species","Latin","IDlevel")])
    names(species.info$species_table) = c("sp", "latin", "IDlevel")
    
    species.info$species_to_exclude = 
      c(species.info$species_table$sp[which(species.info$species_table$Family %in% "Arecaceae")],
        species.info$species_table$sp[which(species.info$species_table$Genus %in% Arecaceae_genuses$Genus)],
        species.info$species_table$sp[which(species.info$species_table$IDlevel %in% c("none", "multiple"))],
        "premon", "roybor", "uniden", "NA", "spp")}
  
  if(CTFS.plot == "luquillo_census456"){
    
    species.table = fread("data/luquillo/species list/Luquillo_species_list.txt")
    Arecaceae_genuses = fread("functions/Genuses of Arecaceae_from plant list.txt")
    species.table$Species = tolower(as.character(species.table$Species))
    
    # create full species name
    species.table$Latin = paste(species.table$Genus, species.table$Species, sep = " ")
    
    species.info = list(species_table = species.table[,c("Species","Latin","IDlevel")])
    names(species.info$species_table) = c("sp", "latin", "IDlevel")
    
    species.info$species_to_exclude = 
      c(species.info$species_table$sp[which(species.info$species_table$Family %in% "Arecaceae")],
        species.info$species_table$sp[which(species.info$species_table$Genus %in% Arecaceae_genuses$Genus)],
        species.info$species_table$sp[which(species.info$species_table$IDlevel %in% c("none", "multiple"))],
        "premon", "roybor", "uniden", "NA", "spp")}
  
  if(CTFS.plot == "sinharaja"){
    
    species.table = fread("data/sinharaja/species list/species_list_sinharaja.txt")
    Arecaceae_genuses = fread("functions/Genuses of Arecaceae_from plant list.txt")
    species.table$Mnemonic = tolower(as.character(species.table$Mnemonic))
    
    # create full species name
    species.table$Latin = paste(species.table$Genus, species.table$Species, sep = " ")
    
    species.info = list(species_table = species.table[,c("Mnemonic","Latin","IDlevel")])
    names(species.info$species_table) = c("sp", "latin", "IDlevel")
    
    species.info$species_to_exclude = 
      c(species.info$species_table$sp[which(species.info$species_table$Family %in% "Arecaceae")],
        species.info$species_table$sp[which(species.info$species_table$Genus %in% Arecaceae_genuses$Genus)],
        species.info$species_table$sp[which(species.info$species_table$IDlevel == "multiple")],
        "uniden", "NA", "spp")}
  
  return(species.info)
}

############################################################
# load the species traits-----------------------------------

load.species.tratis = function(CTFS.plot){
  
  if(CTFS.plot == "bci"){
    species.traits = fread("data/bci/traits/ppa_full_traits.txt")
    species.traits$V1 = NULL
    species.traits$sp = tolower(as.character(species.traits$sp))
    species.traits = species.traits[,.(sp, MAXHEIGHT_AVG, WSG_AVG, SEED__DRY_MIX)]
    names(species.traits) = c("sp","height_max","wsg","seed_mass")  
  }
  return (species.traits)
}

######################################################
# check and clean the census data --------------------
check.census.list = function(CTFS.plot, census.list, census.specifications){
  
  
  # check the data format
  for(i in 1: length(census.list)){
    if(class(census.list[[i]]$treeID) != "numeric"){
      census.list[[i]]$treeID = as.numeric(as.character(census.list[[i]]$treeID))
      print(paste(c(names(census.list)[i],"$treeID converted to numeric"), collapse = ""))}
    if(class(census.list[[i]]$quadrat) != "numeric"){
      census.list[[i]]$quadrat = as.numeric(as.character(census.list[[i]]$quadrat))
      print(paste(c(names(census.list)[i],"$quadrat converted to numeric"), collapse = ""))}
    if(class(census.list[[i]]$gx) != "numeric"){
      census.list[[i]]$gx = as.numeric(as.character(census.list[[i]]$gx))
      print(paste(c(names(census.list)[i],"$gx converted to numeric"), collapse = ""))}
    if(class(census.list[[i]]$gy) != "numeric"){
      census.list[[i]]$gy = as.numeric(as.character(census.list[[i]]$gy))
      print(paste(c(names(census.list)[i],"$gy converted to numeric"), collapse = ""))}
    
    if(class(census.list[[i]]$stemID) != "numeric"){
      census.list[[i]]$stemID = as.numeric(as.character(census.list[[i]]$stemID))
      print(paste(c(names(census.list)[i],"$stemID converted to numeric"), collapse = ""))}
    
    if(CTFS.plot %in% c("foresgeo")){
      if(class(census.list[[i]]$dbh) != "numeric"){
        census.list[[i]]$dbh = as.numeric(as.character(census.list[[i]]$dbh))
        print(paste(c(names(census.list)[i],"$dbh converted to numeric"), collapse = ""))}}
    
    if(class(census.list[[i]]$ba) != "numeric"){
      census.list[[i]]$ba = as.numeric(as.character(census.list[[i]]$ba))
      print(paste(c(names(census.list)[i],"$ba converted to numeric"), collapse = ""))}
    if(class(census.list[[i]]$hom) != "numeric"){
      census.list[[i]]$hom = as.numeric(as.character(census.list[[i]]$hom))
      print(paste(c(names(census.list)[i],"$hom converted to numeric"), collapse = ""))}
    
    # create a unique combination of tree and stemID
    census.list[[i]]$tree_stemID = paste(census.list[[i]]$treeID, census.list[[i]]$stemID, sep = "_")
    
    # set species names to lower case
    census.list[[i]]$sp = tolower(as.character(census.list[[i]]$sp))
  }
  
  # check all the data requirements
  for(i in 1:length(census.list)){
    
    # check dbh
    if(CTFS.plot %in% c("foresgeo")){
      census.list[[i]]$dbh = census.list[[i]]$dbh * census.specifications$dbh_conversion
      census.list[[i]]$dbh[which(census.list[[i]]$status == "D")] = NA}
    
    if(CTFS.plot %in% c("nizanda", "yucatan")){
      census.list[[i]]$ba = census.list[[i]]$ba * census.specifications$ba_conversion
      census.list[[i]]$ba[which(census.list[[i]]$status == "D")] = NA}
    
    # check ExactDate
    if(!("ExactDate" %in% names(census.list[[i]]))){
      print(paste("no column named ExactDates in census", i))
    }else{
      census.list[[i]]$ExactDate = as.Date(census.list[[i]]$ExactDate)
      
      # check min ExactDate
      if(min(census.list[[i]]$ExactDate, na.rm = T) < "1980-01-01"){
        print(paste(c(length(census.list[[i]]$ExactDate[which(census.list[[i]]$ExactDate < "1980-01-01")]),
                      " ExactDates in census", i, " below 1980, set to: ",
                      mean(census.list[[i]]$ExactDate, na.rm = T)), 
                    collapse =""))
        census.list[[i]]$ExactDate[which(census.list[[i]]$ExactDate < "1980-01-01")] = mean(census.list[[i]]$ExactDate, na.rm = T)}
      
      # check max ExactDate
      if(max(census.list[[i]]$ExactDate, na.rm = T) > "2019-10-21"){
        print(paste(c(length(census.list[[i]]$ExactDate[which(census.list[[i]]$ExactDate > "2019-10-21")]),
                      " ExactDates in census", i, " above Oct 2019, set to: ",
                      mean(census.list[[i]]$ExactDate, na.rm = T)), 
                    collapse =""))
        census.list[[i]]$ExactDate[which(census.list[[i]]$ExactDate > "2019-10-21")] = mean(census.list[[i]]$ExactDate, na.rm = T)}
      
      # check NAs
      census.list[[i]]$ExactDate[which(is.na(census.list[[i]]$ExactDate))] = mean(census.list[[i]]$ExactDate, na.rm = T)
    }
    
    # check date
    if(!("date" %in% names(census.list[[i]]))){
      print(paste("no column named date in census", i))
    }else{
      census.list[[i]]$date = as.numeric(as.character(census.list[[i]]$date))
      
      # check min dates
      if(min(census.list[[i]]$date, na.rm = T) < 0) {
        print(paste(c(length(census.list[[i]]$date[which(census.list[[i]]$date < 0)]),
                      " dates in census", i, " below 0, set to: ",
                      mean(census.list[[i]]$date, na.rm = T)), 
                    collapse =""))
        census.list[[i]]$ExactDate[which(census.list[[i]]$ExactDate < 0)] = mean(census.list[[i]]$date, na.rm = T)}
      
      # check max dates
      if(max(census.list[[i]]$date, na.rm = T) > 21852) {
        print(paste(c(length(census.list[[i]]$date[which(census.list[[i]]$date > 21852)]),
                      " dates in census", i, " above 21852, set to: ",
                      mean(census.list[[i]]$date, na.rm = T)), 
                    collapse =""))
        census.list[[i]]$date[which(census.list[[i]]$date > 21852)] = mean(census.list[[i]]$date, na.rm = T)}
      
      # check NAs
      census.list[[i]]$ExactDate[which(is.na(census.list[[i]]$ExactDate))] = mean(census.list[[i]]$ExactDate, na.rm = T)
    }
    
    # check if ExactDates matches dates
    if("ExactDate" %in% names(census.list[[i]]) & "date" %in% names(census.list[[i]])){
      all.dates = as.numeric(as.Date(as.character(census.list[[i]]$ExactDate),"%Y-%m-%d") - as.Date("1960-01-01","%Y-%m-%d"))
      print(paste(
        length(which(all.dates != census.list[[i]]$date)),
        " dates did not match the Exact date and were re-calculated"))
      census.list[[i]]$date[which(all.dates != census.list[[i]]$date)] = 
        as.numeric(as.Date(as.character(census.list[[i]]$ExactDate[which(all.dates != census.list[[i]]$date)]),"%Y-%m-%d") - as.Date("1960-01-01","%Y-%m-%d"))
    }
    
    # check status
    print(paste(c("census ",i, "status levels: ", unique(census.list[[i]]$status)), collapse = " "))
    
    # recode-status and dbh of dead trees
    if(CTFS.plot %in% c("foresgeo")){
      census.list[[i]]$dbh[which(census.list[[i]]$status %in%  census.specifications$codes_for_dead_trees)] = NA
      census.list[[i]]$status[which(census.list[[i]]$status %in%  census.specifications$codes_for_dead_trees)] = "D"}
    if(CTFS.plot %in% c("nizanda","yucatan")){
      census.list[[i]]$ba[which(census.list[[i]]$status %in%  census.specifications$codes_for_dead_trees)] = NA
      census.list[[i]]$status[which(census.list[[i]]$status %in%  census.specifications$codes_for_dead_trees)] = "D"}
    
    
    # include "D" species, if these are not yet included
    if(census.specifications$add_dead_ind == T){
      if(i > 1){
        previous.census = census.list[[i-1]]
        previous.census = previous.census[which(previous.census$status == "A"),]
        previous.census = previous.census[which(!(previous.census$treeID %in% census.list[[i]]$treeID))]
        if(nrow(previous.census) > 0){
          previous.census$status = "D"
          previous.census$dbh = NA
          previous.census$ExactDate = rep(mean(census.list[[i]]$ExactDate, na.rm = T),
                                          nrow(previous.census))
          previous.census$date = as.integer(mean(census.list[[i]]$date, na.rm = T))
          census.list[[i]] = rbind(census.list[[i]],
                                   previous.census, fill = T)
        }}}
    
    # include "P" individuals, if these are not yet added
    if(census.specifications$add_prior_ind == T){
      if(i < length(census.list)){
        next.census = census.list[[i+1]]
        next.census = next.census[which(next.census$status == "A"),]
        next.census = next.census[which(!(next.census$treeID %in% census.list[[i]]$treeID)),]
        if(nrow(next.census) > 0){
          next.census$status = "P"
          next.census$dbh = NA
          next.census$ExactDate = rep(as.Date(mean(as.Date(census.list[[i]]$ExactDate), na.rm = T)),
                                      nrow(next.census))
          next.census$date = as.integer(mean(census.list[[i]]$date, na.rm = T))
          census.list[[i]] = rbind(census.list[[i]],
                                   next.census, fill =T)
        }}}
    
    # check hom
    if(mean(census.list[[i]]$hom, na.rm = T) < 10){
      census.list[[i]]$hom = census.list[[i]]$hom * 10
      print("hom is multiplied by 10")}
  }
  return(census.list)
}



# check for all thinkable data errors ---------------------------------------------------------
# this function is checking, step-by-step, if there are any potential errors in the census data
check.census.data.set = function(census.list, census.specifications, species.info){
  
  nr.of.censuses = length(census.list)
  
  # 1. check if number of rows is somewhat similar
  check = sapply(census.list, nrow)
  if(min(check / max(check)) < 0.9){print("! number of rows differs by more than 10% between censuses")
  }else{print(paste(c("ok - nrows are similar up to", round(min(check / max(check)), digits = 2)*100, "%"), collapse = " "))}
  
  # 2. check if all "A" ind are "A" or "P" in the previous census
  all.fine = T
  for(i in 2:nr.of.censuses){
    later.census = census.list[[i]][status == "A"]
    earlier.census = census.list[[i-1]][treeID %in% later.census$treeID]
    
    if(nrow(earlier.census) != nrow(later.census)){
      print(paste(c("! - not all alive ind of census ", i, " in earlier census ", i-1), collapse = ""))
      all.fine = F}}
  if(all.fine){print("ok - all alive ind are in previous censuses")}
  
  
  # 3. check if all "A" ind are "A", "D" or "M" is next census
  all.fine = T
  for(i in 1:(nr.of.censuses-1)){
    earlier.census = census.list[[i]][status == "A"]
    earlier.census = earlier.census[!(sp %in% species.info$species_to_exclude)]
    
    later.census = census.list[[i+1]][treeID %in% earlier.census$treeID]
    later.census = later.census[status %in% c("A", "D", "M")]
    later.census = later.census[!(sp %in% species.info$species_to_exclude)]
    
    if(nrow(earlier.census) != nrow(later.census)){
      print(paste(c("! - not all alive ind of census ", i, " in later census ", i+1), collapse = ""))
      all.fine = F}}
  if(all.fine){print("ok - all alive ind are in later censuses")}
  
}

