calculate.growthrates.from.main.censuses = function(CTFS.plot, census.list, census.specifications ){
  # arguments:
  # census.name = the name of the FDP as one string (e.g. "bci")
  # census.list = a list with all censuses, ordered from frist to last census date
  # are.there.multiple.treeIDs = Boolean (T, F), whether there are duplicated treeIDs in the dataset (due to multiple stems)
  # to exclude = a string vector with the species that should be excluded (e.g. c("species1", "species2))
  
  # create an empty list to store the results
  results = list()
  
  for(i in 1:(length(census.list)-1)){
    
    earlier.census = as.data.frame(census.list[[i]])
    later.census = as.data.frame(census.list[[i+1]])
    
    # remove NAs in dbh
    if(CTFS.plot %in% c("costa_rica", "forestgeo")){
      earlier.census = earlier.census[!(is.na(earlier.census$dbh)),]
      later.census = later.census[!(is.na(later.census$dbh)),]}
    if(CTFS.plot %in% c("nizanda","yucatan")){
      earlier.census = earlier.census[!(is.na(earlier.census$ba)),]
      later.census = later.census[!(is.na(later.census$ba)),]}
    
    if(CTFS.plot %in% c("seedlings.fushan")){
      earlier.census = earlier.census[!(is.na(earlier.census$ht)),]
      later.census = later.census[!(is.na(later.census$ht)),]}
    
    # keep only alive
    earlier.census = earlier.census[earlier.census$status == "A",]
    later.census = later.census[later.census$status == "A",]
    
    # exlude individuals with inaccurate dbh measurements
    if(names(census.list)[i] %in% census.specifications$censuses_with_inaccurate_dbh_below_5cm | 
       names(census.list)[i+1] %in% census.specifications$censuses_with_inaccurate_dbh_below_5cm){
      earlier.census = earlier.census[earlier.census$dbh > 50,]
      later.census = later.census[later.census$dbh > 50,]}
    
    # keep only those tree IDs that are in earlier and later census
    earlier.census = earlier.census[which(earlier.census$tree_stemID %in% later.census$tree_stemID),]
    later.census = later.census[which(later.census$tree_stemID %in% earlier.census$tree_stemID),]
    
    # sort both censuses according to treeID
    earlier.census = earlier.census[order(earlier.census$treeID),]
    later.census = later.census[order(later.census$treeID),]
    
    # create empty vector for a species list
    specieslist.temp = c()
    
    # calculate growth, ! use the modified data where indicated with the modify-argument
    # if neither the earlier nor the later census should be modified
    if(CTFS.plot %in% c("costa_rica", "forestgeo")){
      growth.temp = growth(census1 = earlier.census, 
                           census2 = later.census, 
                           split1 = earlier.census$treeID, 
                           method = "I")}
    
    if(CTFS.plot %in% c("nizanda","yucatan")){
      earlier.census$dbh = earlier.census$ba
      later.census$dbh = later.census$ba
      
      growth.temp = growth(census1 = earlier.census, 
                           census2 = later.census, 
                           split1 = earlier.census$treeID, 
                           method = "I", growthcol = "ba", mindbh = 0)}
    
    if(CTFS.plot %in% c("seedlings_fushan")){
      earlier.census$dbh = earlier.census$ht
      later.census$dbh = later.census$ht
      
      growth.temp = growth(census1 = earlier.census, 
                           census2 = later.census, 
                           split1 = earlier.census$treeID, 
                           method = "I", growthcol = "ht", mindbh = 0)}
    
    demography.temp = assemble.demography(growth.temp, type = "g")
    earlier.census.with.demography = data.frame(earlier.census, dinc_per_year = demography.temp[,"rate.1"])
    
    #remove individuals with NAs in growth rate to to different point of measurements (pom) between survey dates
    earlier.census.with.demography = earlier.census.with.demography[!(is.na(earlier.census.with.demography$dinc_per_year)),]
    
    # add information on the earlier and later sampling date to each individual
    names(earlier.census.with.demography)[which(names(earlier.census.with.demography) == "date")] = "date1"
    names(earlier.census.with.demography)[which(names(earlier.census.with.demography) == "ExactDate")] = "ExactDate1"
    
    names(later.census)[which(names(later.census) == "date")] = "date2"
    names(later.census)[which(names(later.census) == "ExactDate")] = "ExactDate2"
    
    earlier.census.with.demography$ExactDate1 = as.Date(earlier.census.with.demography$ExactDate1)
    later.census$ExactDate2 = as.Date(later.census$ExactDate2)
    
    earlier.census.with.demography = merge(earlier.census.with.demography, later.census[,c("treeID","date2","ExactDate2")], by = "treeID")
    
    earlier.census.with.demography$interval_date1_and_2_in_years  = (earlier.census.with.demography$date2 - earlier.census.with.demography$date1) / 365.25
    earlier.census.with.demography$interval_date1_and_2_in_days = earlier.census.with.demography$ExactDate2 - earlier.census.with.demography$ExactDate1
    
    # calculate the modulus-transformed growth
    earlier.census.with.demography$dinc_per_year_modulus = modulus.transformation(earlier.census.with.demography$dinc_per_year, delta = 0.4)
    
    # add the census_id
    earlier.census.with.demography$census_id = paste("census",i,sep = "")
    
    # store results
    results[[i]] = earlier.census.with.demography
    names(results)[i] = paste(c("growth_census_",i,"_",i+1), collapse = "")
    
    print(paste("growthrates between census ",i," and ",i+1," determined"), sep = "")
    print(paste("proportion of dinc per year <= 0: ",
                length(which(results[[i]]$dinc_per_year <=0)) / nrow(results[[i]]), sep = ""))
    
  }
  
  # save results list in global environment
  return(results)
}


#######################################################################################
calculate.survival.from.main.censuses =  function(CTFS.plot,
                                                  census.list,
                                                  species.info){
  
  # create an empty list to store the results
  results = list()
  
  for(i in 1:(length(census.list)-1)){
    
    earlier.census = as.data.frame(census.list[[i]])
    later.census = as.data.frame(census.list[[i+1]])
    
    # keep only those tree IDs that are in earlier and later census
    earlier.census = earlier.census[which(earlier.census$treeID %in% later.census$treeID),]
    later.census = later.census[which(later.census$treeID %in% earlier.census$treeID),]
    
    # sort by treeID
    earlier.census = earlier.census[order(earlier.census$treeID),]
    later.census = later.census[order(later.census$treeID),]
    
    # assign alive and dead
    earlier.census$alive = ifelse(earlier.census$status=="A" & later.census$status=="A",1,
                                  ifelse(earlier.census$status=="A" & later.census$status=="D",0,NA))
    
    # remove NAs
    if(CTFS.plot %in% c("costa_rica", "forestgeo")){
      earlier.census = earlier.census[!(earlier.census$sp %in% species.info$species_to_exclude) &
                                        !is.na(earlier.census$sp) &
                                        !is.na(earlier.census$gx) &
                                        !is.na(earlier.census$gy) &
                                        !is.na(earlier.census$dbh) &
                                        !is.na(earlier.census$alive),]}
    
    if(CTFS.plot %in% c("nizanda")){
      earlier.census = earlier.census[!(earlier.census$sp %in% species.info$species_to_exclude) &
                                        !is.na(earlier.census$sp) &
                                        !is.na(earlier.census$gx) &
                                        !is.na(earlier.census$gy) &
                                        !is.na(earlier.census$ba) &
                                        !is.na(earlier.census$alive),]}
    
    if(CTFS.plot %in% c("yucatan")){
      earlier.census = earlier.census[!(earlier.census$sp %in% species.info$species_to_exclude) &
                                        !is.na(earlier.census$sp) &
                                        !is.na(earlier.census$ba) &
                                        !is.na(earlier.census$alive),]}
    
    
    # add information on the earlier and later sampling date to each individual
    names(earlier.census)[which(names(earlier.census) == "date")] = "date1"
    names(earlier.census)[which(names(earlier.census) == "ExactDate")] = "ExactDate1"
    
    names(later.census)[which(names(later.census) == "date")] = "date2"
    names(later.census)[which(names(later.census) == "ExactDate")] = "ExactDate2"
    
    earlier.census$ExactDate1 = as.Date(earlier.census$ExactDate1)
    later.census$ExactDate2 = as.Date(later.census$ExactDate2)
    
    earlier.census = merge(earlier.census, later.census[,c("treeID","date2","ExactDate2")], by = "treeID")
    
    earlier.census$interval_date1_and_2_in_years  = (earlier.census$date2 - earlier.census$date1) / 365.25
    earlier.census$interval_date1_and_2_in_days = earlier.census$ExactDate2 - earlier.census$ExactDate1
    
    # store results
    results[[i]]  = earlier.census
    names(results[i]) = paste(c("survival_census_",i,"_",i+1), collapse = "")
    
    # print message that results dataframe has been created
    print(paste(c("survival between census ",i," and ",i+1, " calculated"), collapse = ""))
    print(paste("proportion of alive : dead =",
                length(which(results[[i]]$alive == 1)) / nrow(results[[i]])))
  }
  
  # calculate mean survival rate per species
  first.census = data.table(results[[1]])
  last.census = data.table(results[[length(results)]])
  
  surv.summary = first.census[,.(n_ind_all = length(tree_stemID), 
                                 n_ind_surv = length(which(alive == 1)), 
                                 t = mean(interval_date1_and_2_in_years)),
                              by = .( sp)]
  
  surv.summary$surv_rate = surv.summary$n_ind_surv / surv.summary$n_ind_all
  surv.summary$surv_rate = surv.summary$surv_rate ^(1 / surv.summary$t)
  surv.summary$surv_rate = 1 - surv.summary$surv_rate
  
  surv.summary$life_span = 1 / (1- (surv.summary$n_ind_surv / surv.summary$n_ind_all))
  surv.summary$surv_per_year = 1- (1- surv.summary$n_ind_surv / surv.summary$n_ind_all) / surv.summary$t
  
  # print mean and median yearly mortality rates for individuals
  print(paste(c("survival rate (min 10 ind): from = ", 
                round(min(surv.summary$surv_rate[which(surv.summary$n_ind_all >= 10)], na.rm = T), digits = 3),
                " to = ", 
                round(max(surv.summary$surv_rate[which(surv.summary$n_ind_all >= 10)], na.rm = T), digits = 3),
                " median = ",
                round(median(surv.summary$surv_rate[which(surv.summary$n_ind_all >= 10)], na.rm = T), digits = 3),
                " cov = ",
                round(sd(surv.summary$surv_rate[which(surv.summary$n_ind_all >= 10)], na.rm = T) /
                        mean(surv.summary$surv_rate[which(surv.summary$n_ind_all >= 10)], na.rm = T),digits = 3)),
              collapse = ""))
  
  print(paste(c("life span (min 10 ind): from = ", 
                round(min(surv.summary$life_span[which(surv.summary$n_ind_all >= 10)], na.rm = T), digits = 3),
                " to = ", 
                round(max(surv.summary$life_span[which(surv.summary$n_ind_all >= 10)], na.rm = T), digits = 3),
                " median = ",
                round(median(surv.summary$life_span[which(surv.summary$n_ind_all >= 10)], na.rm = T), digits = 3),
                " cov = ",
                round(sd(surv.summary$life_span[which(surv.summary$n_ind_all >= 10)], na.rm = T) /
                        mean(surv.summary$life_span[which(surv.summary$n_ind_all >= 10)], na.rm = T),digits = 3)),
              collapse = ""))
  
  print(paste(c("yearly survival rate (min 10 ind): from = ", 
                round(min(surv.summary$surv_per_year[which(surv.summary$n_ind_all >= 10)], na.rm = T), digits = 3),
                " to = ", 
                round(max(surv.summary$surv_per_year[which(surv.summary$n_ind_all >= 10)], na.rm = T), digits = 3),
                " median = ",
                round(median(surv.summary$surv_per_year[which(surv.summary$n_ind_all >= 10)], na.rm = T), digits = 3),
                " cov = ",
                round(sd(surv.summary$surv_per_year[which(surv.summary$n_ind_all >= 10)], na.rm = T) /
                        mean(surv.summary$surv_per_year[which(surv.summary$n_ind_all >= 10)], na.rm = T),digits = 3)),
              collapse = ""))
  
  # save results list in global environment
  return(results)
}

#####################################################################################
# this function is needed to assign trees to a crown layer, within the current census
helper.function.assign.trees.to.crown.layers = function(crown.cover.corrected, 
                                                        subplot.area, 
                                                        allometry,
                                                        nr.of.layers){
  # arguments:
  # crown.cover.corrected must be a numeric vector that is ordered from high to low values
  # subplot area = area of the subplot used for PPA layer assignment
  # allometry = on which basis should trees be assigned to crown layers:
  # "ppa" = based on crown - dbh allometry
  # "even" = evenly assignment the same number of individuals to every group
  # nr.of.layers = how many layers should be assigned, will only be used for allometry = "even"
  
  crown.layer.assigned = rep(NA, length(crown.cover.corrected))
  
  temp.cover.level = 0
  
  if(allometry == "ppa"){
    while(length(which(is.na(crown.layer.assigned))) > 0){
      
      temp.cover.level = temp.cover.level + 1
      
      # calculate the cummulative sum of all individuals that have not yet been assigned to a crown layer  
      cumsum.temp = rep(NA, length(crown.cover.corrected))
      cumsum.temp[which(is.na(crown.layer.assigned))] = cumsum(crown.cover.corrected[which(is.na(crown.layer.assigned))])
      
      # assign those individuals whose cumsum is < plot area to the same crown layer
      if(length(which(cumsum.temp < subplot.area)) > 0){
        crown.layer.assigned[which(cumsum.temp < subplot.area)] = temp.cover.level
      }else{
        crown.layer.assigned[min(which(is.na(crown.layer.assigned)))] = temp.cover.level}
      
      # stop the loop if all idividuals are already assigned to a crown layer
      if(length(which(is.na(crown.layer.assigned))) == 0){break}
      
      # if more than 50% of the next individual would fit within the current crown layer assign it to this layer
      plot.area.minus.area.covered.by.temp.layer = subplot.area - cumsum.temp[max(which(crown.layer.assigned == temp.cover.level))]
      if((plot.area.minus.area.covered.by.temp.layer / crown.cover.corrected[min(which(is.na(crown.layer.assigned)))]) >= 0.5){
        crown.layer.assigned[min(which(is.na(crown.layer.assigned)))] = temp.cover.level}
    }}
  
  if(allometry == "even"){
    crown.layer.assigned = rep(1:nr.of.layers, each =  round(length(crown.cover.corrected) / nr.of.layers))
    crown.layer.assigned = crown.layer.assigned[1:length(crown.cover.corrected)]
  }
  return(crown.layer.assigned)
}

#######################################################################################
assign.crown.layer.in.single.census = function(CTFS.plot,
                                               census.list, 
                                               species.info,
                                               census.specifications,
                                               nr.of.crown.layers){
  
  # create an empty list to store the results
  results = list()
  
  # store the actual number of calculated crown layers
  crown.layer.counting = c()
  
  for(i in 1:length(census.list)){
    
    # read in one census
    census.temp = data.table(census.list[[i]])
    
    # calculate area of the whole CTFS plot
    if(CTFS.plot %in% c("costa_rica","forestgeo","nizanda")){
      plotdim = sort(c(round(max(census.temp$gx, na.rm = T) - min(census.temp$gx, na.rm = T)),
                       round(max(census.temp$gy, na.rm = T) - min(census.temp$gy, na.rm = T))), decreasing = T)
      plotarea = plotdim[1] * plotdim[2]}
    
    if(CTFS.plot %in% c("yucatan")){
      plotdim = c(10,10)
      plotarea = 100}
    
    # correct crown area, individuals with crown area > subplot.area.to.determine.crown.layer get the value of subplot.area.to.determine.crown.layer
    if(CTFS.plot %in% c("costa_rica", "forestgeo")){
      census.temp$crown_area =  census.specifications$ca_allo_coefficient1 * 
        (census.temp$dbh ^ census.specifications$ca_allo_coefficient2)
      census.temp$crown_area_corrected = census.temp$crown_area
      census.temp$crown_area_corrected[which(census.temp$crown_area_corrected > census.specifications$subplot_area)] = census.specifications$subplot_area}
    
    if(CTFS.plot %in% c("nizanda")){
      census.temp$crown_area[which(is.na(census.temp$crown_area))] =  census.specifications$ca_allo_coefficient1 * 
        (census.temp$ba[which(is.na(census.temp$crown_area))] ^ census.specifications$ca_allo_coefficient2)
      census.temp$crown_area_corrected = census.temp$crown_area
      census.temp$crown_area_corrected[which(census.temp$crown_area_corrected > census.specifications$subplot_area)] = census.specifications$subplot_area}
    
    if(CTFS.plot %in% c("yucatan")){
      census.temp$crown_area =  census.specifications$ca_allo_coefficient1 * 
        (census.temp$ba ^ census.specifications$ca_allo_coefficient2)
      census.temp$crown_area_corrected = census.temp$crown_area
      census.temp$crown_area_corrected[which(census.temp$crown_area_corrected > census.specifications$subplot_area)] = census.specifications$subplot_area}
    
    # calculate the plot index
    if(CTFS.plot %in% c("costa_rica","forestgeo")){
      census.temp$plotindex =  gxgy.to.index(census.temp$gx, 
                                             census.temp$gy, 
                                             gridsize = sqrt(census.specifications$subplot_area),
                                             plotdim = plotdim)
      # remove is.na(plotindex)
      census.temp = census.temp[!is.na(plotindex)]
      
      # these are two plots, so give other indices to the second plot
      census.temp$plotindex = as.numeric(factor(paste(census.temp$plotcode, census.temp$plotindex, sep = "_")))}
    
    # remove all dead, prior and non-located stems
    if(CTFS.plot %in% c("costa_rica","nizanda","forestgeo")){
      census.temp = census.temp[!is.na(crown_area_corrected) & 
                                  !is.na(gx) & 
                                  !is.na(gy) &
                                  !is.na(plotindex) &
                                  status == "A", ]}
    
    if(CTFS.plot %in% c("yucatan")){
      census.temp = census.temp[!is.na(crown_area_corrected) & 
                                  !is.na(plotindex) &
                                  status == "A", ]}
    
    
    # ordering from highest to lowest crown area is important for correct layer assignment 
    census.temp = census.temp[order(census.temp$plotindex, census.temp$crown_area_corrected, decreasing = T),]
    
    # assign the individual trees to crown layers
    cover.assignment.temp = census.temp[,.(tree_stemID,
                                           PPA_assignment = 
                                             helper.function.assign.trees.to.crown.layers(crown.cover.corrected = crown_area_corrected, 
                                                                                          subplot.area = census.specifications$subplot_area, 
                                                                                          allometry = "ppa"),
                                           dbh_assignment2 = 
                                             helper.function.assign.trees.to.crown.layers(crown.cover.corrected = crown_area_corrected, 
                                                                                          subplot.area = census.specifications$subplot_area, 
                                                                                          allometry = "even", 
                                                                                          nr.of.layers = 2),
                                           dbh_assignment4 = 
                                             helper.function.assign.trees.to.crown.layers(crown.cover.corrected = crown_area_corrected, 
                                                                                          subplot.area = census.specifications$subplot_area, 
                                                                                          allometry = "even", 
                                                                                          nr.of.layers = 4),
                                           dbh_assignment6 = 
                                             helper.function.assign.trees.to.crown.layers(crown.cover.corrected = crown_area_corrected, 
                                                                                          subplot.area = census.specifications$subplot_area, 
                                                                                          allometry = "even", 
                                                                                          nr.of.layers = 6)),
                                        by = (plotindex)]
    
    census.temp = merge(census.temp, cover.assignment.temp, by = c("tree_stemID","plotindex"))
    
    # store nr.of.crown.layers
    crown.layer.counting = c(crown.layer.counting,
                             min(cover.assignment.temp$PPA_assignment) : max(cover.assignment.temp$PPA_assignment))
    
    # bring in order once again
    census.temp = census.temp[order(census.temp$plotindex, census.temp$crown_area_corrected, decreasing = T),]
    
    # change the maximum number of crown levels (i.e. change all higher values to the maximum)  
    census.temp$PPA_assignment_max = census.temp$PPA_assignment
    census.temp$PPA_assignment[which(census.temp$PPA_assignment > nr.of.crown.layers)] = nr.of.crown.layers
    census.temp$crown_layer = census.temp$PPA_assignment
    census.temp$PPA_assignment = NULL
    
    # remove unwanted species
    census.temp = census.temp[which(!(census.temp$sp %in% species.info$species_to_exclude)),]
    census.temp = census.temp[!(is.na(census.temp$sp %in% species.info$species_to_exclude)),]
    
    # store results, exlude the species listed in exclude-argument    
    results[[i]] = census.temp
    names(results)[i] = paste("crownlayer_assigned_in_census",i, sep = "")
    
    # print message that results dataframe has been created
    print(paste("crownlayer assigned to census " ,i, sep = ""))
    
  }
  
  # print number of crown layers
  print(paste("Median number of possible crown layers = ", median(crown.layer.counting)))
  print(paste(c("Range of possible crown layers = ", range(crown.layer.counting)), collapse = " "))
  
  # save results list in global environment
  return(results)
}

#######################################################################################
assign.crown.layer.in.single.census.based.on.PPA.and.wright.2010 = function(census.list,
                                                                            growth.between.censuses,
                                                                            survival.between.censuses,
                                                                            species.info,
                                                                            census.specifications,
                                                                            nr.of.crown.layers,
                                                                            wright.lower.percentile,
                                                                            wright.upper.percentile,
                                                                            PPA.or.wright){
  
  # create an empty list to store the results
  crown.layers.PPA = list()
  
  # store the actual number of calculated crown layers
  crown.layer.counting = c()
  
  # 1. assign PPA layers to census.list ---------------
  if(PPA.or.wright == "PPA"){
    for(i in 1:length(census.list)){
      
      # read in one census
      census.temp = data.table(census.list[[i]])
      
      # calculate area of the whole CTFS plot
      plotdim = sort(c(round(max(census.temp$gx, na.rm = T) - min(census.temp$gx, na.rm = T)),
                       round(max(census.temp$gy, na.rm = T) - min(census.temp$gy, na.rm = T))), decreasing = T)
      plotarea = plotdim[1] * plotdim[2]
      
      # correct crown area, individuals with crown area > subplot.area.to.determine.crown.layer get the value of subplot.area.to.determine.crown.layer
      census.temp$crown_area =  census.specifications$ca_allo_coefficient1 * 
        (census.temp$dbh ^ census.specifications$ca_allo_coefficient2)
      census.temp$crown_area_corrected = census.temp$crown_area
      census.temp$crown_area_corrected[which(census.temp$crown_area_corrected > census.specifications$subplot_area)] = census.specifications$subplot_area
      
      # calculate the plot index
      census.temp$plotindex =  gxgy.to.index(census.temp$gx, census.temp$gy, gridsize = sqrt(census.specifications$subplot_area), plotdim = plotdim)
      
      # remove all dead, prior and non-located stems
      census.temp = census.temp[!is.na(crown_area_corrected) & 
                                  !is.na(gx) & 
                                  !is.na(gy) &
                                  !is.na(plotindex) &
                                  status == "A", ]
      
      # ordering from highest to lowest crown area is important for correct layer assignment 
      census.temp = census.temp[order(census.temp$plotindex, census.temp$crown_area_corrected, decreasing = T),]
      
      # assign the individual trees to crown layers
      cover.assignment.temp = census.temp[,.(tree_stemID,
                                             PPA_assignment = 
                                               helper.function.assign.trees.to.crown.layers(crown.cover.corrected = crown_area_corrected, 
                                                                                            subplot.area = census.specifications$subplot_area, 
                                                                                            allometry = "ppa"),
                                             dbh_assignment2 = 
                                               helper.function.assign.trees.to.crown.layers(crown.cover.corrected = crown_area_corrected, 
                                                                                            subplot.area = census.specifications$subplot_area, 
                                                                                            allometry = "even", 
                                                                                            nr.of.layers = 2),
                                             dbh_assignment4 = 
                                               helper.function.assign.trees.to.crown.layers(crown.cover.corrected = crown_area_corrected, 
                                                                                            subplot.area = census.specifications$subplot_area, 
                                                                                            allometry = "even", 
                                                                                            nr.of.layers = 4),
                                             dbh_assignment6 = 
                                               helper.function.assign.trees.to.crown.layers(crown.cover.corrected = crown_area_corrected, 
                                                                                            subplot.area = census.specifications$subplot_area, 
                                                                                            allometry = "even", 
                                                                                            nr.of.layers = 6)),
                                          by = (plotindex)]
      
      census.temp = merge(census.temp, cover.assignment.temp, by = c("tree_stemID","plotindex"))
      
      # store nr.of.crown.layers
      crown.layer.counting = c(crown.layer.counting,
                               min(cover.assignment.temp$PPA_assignment) : max(cover.assignment.temp$PPA_assignment))
      
      # bring in order once again
      census.temp = census.temp[order(census.temp$plotindex, census.temp$crown_area_corrected, decreasing = T),]
      
      # change the maximum number of crown levels (i.e. change all higher values to the maximum)  
      census.temp$PPA_assignment_max = census.temp$PPA_assignment
      census.temp$PPA_assignment[which(census.temp$PPA_assignment > nr.of.crown.layers)] = nr.of.crown.layers
      
      # remove unwanted species
      census.temp = census.temp[which(!(census.temp$sp %in% species.info$species_to_exclude)),]
      census.temp = census.temp[!(is.na(census.temp$sp %in% species.info$species_to_exclude)),]
      
      # rename crown_layer column and only keep those in the top and bottom layer
      census.temp$crown_layer = ifelse(census.temp$PPA_assignment == 1, 1,
                                       ifelse(census.temp$PPA_assignment == max(census.temp$PPA_assignment), 2, NA))
      
      # store results
      crown.layers.PPA[[i]] = census.temp[!is.na(census.temp$crown_layer),]
      names(crown.layers.PPA)[i] = paste("crownlayer_assigned_in_census",i, sep = "")
      
      # print message that results dataframe has been created
      print(paste("PPA crownlayer assigned to census " ,i, sep = ""))
      
    }
    
    # print number of crown layers
    print(paste("Median number of possible crown layers = ", median(crown.layer.counting)))
    print(paste(c("Range of possible crown layers = ", range(crown.layer.counting)), collapse = " "))
    
    
    # 2. append PPA crown layers to growth and survival data ---------------
    growth.between.censuses.PPA = growth.between.censuses
    survival.between.censuses.PPA = survival.between.censuses
    
    for (i in 1:length(growth.between.censuses.PPA)){
      
      growth.census.temp = growth.between.censuses.PPA[[i]]
      survival.census.temp = survival.between.censuses.PPA[[i]]
      crown.layer.temp = crown.layers.PPA[[i]]
      
      # merge growth and crown census
      growth.census.temp.merged = merge(growth.census.temp,
                                        crown.layer.temp[, c("tree_stemID","plotindex",
                                                             "crown_area","crown_area_corrected",
                                                             "crown_layer")],
                                        by = "tree_stemID")
      
      survival.census.temp.merged = merge(survival.census.temp,
                                          crown.layer.temp[, c("tree_stemID","plotindex",
                                                               "crown_area","crown_area_corrected",
                                                               "crown_layer")],
                                          by = "tree_stemID")
      # store results
      growth.between.censuses.PPA[[i]] = growth.census.temp.merged
      survival.between.censuses.PPA[[i]] = survival.census.temp.merged
    }
    assign("growth.between.censuses.PPA", growth.between.censuses.PPA, envir=globalenv())
    assign("survival.between.censuses.PPA", survival.between.censuses.PPA, envir=globalenv())
  }
  
  # 3. assign wright 2010 classification ------------------------------------
  if(PPA.or.wright == "wright"){
    growth.between.censuses.wright = growth.between.censuses
    survival.between.censuses.wright = survival.between.censuses
    
    for(i in 2:length(growth.between.censuses.wright)){
      
      census.temp = growth.between.censuses.wright[[i]]
      previous.census.temp = growth.between.censuses.wright[[i-1]]
      previous.census.temp = previous.census.temp[!(previous.census.temp$sp %in% species.info$species_to_exclude),]
      previous.census.temp = previous.census.temp[previous.census.temp$status == "A",]
      
      sp.with.more.than.200.ind = table(previous.census.temp$sp)
      sp.with.more.than.200.ind = names(sp.with.more.than.200.ind)[which(sp.with.more.than.200.ind >= 200)]
      
      # to store those tree ids in good/bad growing conditions
      trees.in.favourable.conditions = c()
      trees.in.unfavourable.conditions = c()
      
      for(sp.temp in sp.with.more.than.200.ind){
        previous.census.temp.to.append.layers = previous.census.temp
        current.census.temp.to.append.layers = census.temp
        
        # determine percentiles for favourable and unfavourable conditions
        percentile.favourable = quantile(current.census.temp.to.append.layers$dinc_per_year[current.census.temp.to.append.layers$sp == sp.temp], probs = wright.upper.percentile, na.rm = T)
        percentile.unfavourable = quantile(previous.census.temp.to.append.layers$dinc_per_year[previous.census.temp.to.append.layers$sp == sp.temp], probs = wright.lower.percentile, na.rm = T)
        
        trees.in.favourable.conditions = c(trees.in.favourable.conditions, 
                                           current.census.temp.to.append.layers[which(current.census.temp.to.append.layers$sp == sp.temp & current.census.temp.to.append.layers$dinc_per_year > percentile.favourable), "tree_stemID"])
        trees.in.unfavourable.conditions = c(trees.in.unfavourable.conditions,
                                             previous.census.temp.to.append.layers[which(previous.census.temp.to.append.layers$sp == sp.temp & previous.census.temp.to.append.layers$dinc_per_year < percentile.unfavourable), "tree_stemID"])
      }
      
      current.census.temp.to.append.layers$crown_layer = NA
      previous.census.temp.to.append.layers$crown_layer = NA
      
      current.census.temp.to.append.layers$crown_layer[current.census.temp.to.append.layers$tree_stemID %in% trees.in.favourable.conditions] = 1
      previous.census.temp.to.append.layers$crown_layer[previous.census.temp.to.append.layers$tree_stemID %in% trees.in.unfavourable.conditions] = 2
      
      growth.between.censuses.wright[[i]] = merge(growth.between.censuses.wright[[i]],
                                                  current.census.temp.to.append.layers[,c("tree_stemID","crown_layer")],
                                                  by = "tree_stemID", all.x = T)
      survival.between.censuses.wright[[i]] = merge(survival.between.censuses.wright[[i]],
                                                    previous.census.temp.to.append.layers[,c("tree_stemID","crown_layer")],
                                                    by = "tree_stemID", all.x = T)
      
      growth.between.censuses.wright[[i]] = growth.between.censuses.wright[[i]][!(is.na(growth.between.censuses.wright[[i]]$crown_layer)),]
      survival.between.censuses.wright[[i]] = survival.between.censuses.wright[[i]][!(is.na(survival.between.censuses.wright[[i]]$crown_layer)),]
      
      # print message that results dataframe has been created
      print(paste("Wright good/bad conditions assigned to census ", i, sep  = ""))
      
    }
    
    # save results list in global environment
    growth.between.censuses.wright = growth.between.censuses.wright[2: length(growth.between.censuses.wright)]
    survival.between.censuses.wright = survival.between.censuses.wright[2:length(survival.between.censuses.wright)]
    assign("growth.between.censuses.wright", growth.between.censuses.wright, envir=globalenv())
    assign("survival.between.censuses.wright", survival.between.censuses.wright, envir=globalenv())
  }
}

################################################################################
append.crown.layer.assignment.to.growth.and.survival.data = function(growth.between.censuses,
                                                                     survival.between.censuses,
                                                                     crownlayer.assigned){
  
  for (i in 1:length(growth.between.censuses)){
    growth.census.temp = growth.between.censuses[[i]]
    survival.census.temp = survival.between.censuses[[i]]
    crown.layer.temp = crownlayer.assigned[[i]]
    
    # merge growth and crown census
    growth.census.temp.merged = merge(growth.census.temp,
                                      crown.layer.temp[, c("tree_stemID","plotindex",
                                                           "crown_area","crown_area_corrected",
                                                           "crown_layer", "PPA_assignment_max")],
                                      by = "tree_stemID")
    
    survival.census.temp.merged = merge(survival.census.temp,
                                        crown.layer.temp[, c("tree_stemID","plotindex",
                                                             "crown_area","crown_area_corrected",
                                                             "crown_layer", "PPA_assignment_max")],
                                        by = "tree_stemID")
    # store results
    growth.between.censuses[[i]] = growth.census.temp.merged
    survival.between.censuses[[i]] = survival.census.temp.merged
    
    # print message that results dataframe has been created
    print(paste("crown layers appended to census", i, sep  = ""))
  }
  
  results = list("growth.between.censuses" = growth.between.censuses,
                 "survival.between.censuses" = survival.between.censuses)
  # save results list in global environment
  return(results) 
}
################################################################################
save.phi.values = function(CTFS.plot, crownlayer.assigned, output.folder){
  
  longlist = rbindlist(crownlayer.assigned,  fill = T, idcol= "census_id")
  phi.values = longlist[,.(phi = median(sqrt(crown_area_corrected / pi) / (dbh * 0.1))),
                        by = sp]
  print(paste(c("phi values range from ",round(min(phi.values$phi), digits = 3), 
                " to ", round(max(phi.values$phi), digits = 3)), collapse = ""))
  saveRDS(phi.values, paste(c(output.folder, "/",CTFS.plot,"/",CTFS.plot,"_phi_values.rds"), collapse = ""))
  print(paste(c("intermediate output/",CTFS.plot,"/",CTFS.plot,"_phi_values.rds saved"), collapse = ""))
}

#######################################################
create.dbh.growth.survival.per.layer.density.plots = function(CTFS.plot,
                                                              growth.between.censuses,
                                                              survival.between.censuses,
                                                              output.file){
  if(CTFS.plot %in% c("costa_rica","forestgeo")){census.temp = 1}
  if(CTFS.plot %in% c("nizanda")){census.temp = 2}
  if(CTFS.plot %in% c("yucatan")){census.temp = 2}
  
  ########
  # growth
  growth.census1 = data.table(growth.between.censuses[[census.temp]])
  growth.census1 = growth.census1[,.(dinc_per_year_mean = mean(dinc_per_year, na.rm = T)),
                                  by =.(sp, PPA_assignment_max)]
  
  # determine number of species in each layer and append this information also to survival and recruitment
  n.species = growth.census1[,.(n_species = length(sp)),
                             by = PPA_assignment_max]
  
  # append number of species information, so that it will be used for faceting the plot
  growth.census1 = merge(growth.census1, n.species, by = "PPA_assignment_max")
  growth.census1$PPA_assignment_max = paste("Layer ", growth.census1$PPA_assignment_max, sep = "")
  growth.census1$PPA_assignment_max = paste(growth.census1$PPA_assignment_max, ": ", sep = "")
  growth.census1$PPA_assignment_max = paste(growth.census1$PPA_assignment_max, growth.census1$n_species, sep = "")
  growth.census1$PPA_assignment_max = paste(growth.census1$PPA_assignment_max, " sp", sep = "")
  
  ########
  # survival
  survival.census1 = data.table(survival.between.censuses[[census.temp]])
  survival.census1 = survival.census1[,.(alive_mean = mean(alive, na.rm = T)),
                                      by =.(sp, PPA_assignment_max)]
  
  # append number of species information, so that it will be used for faceting the plot
  survival.census1 = merge(survival.census1, n.species, by = "PPA_assignment_max")
  survival.census1$PPA_assignment_max = paste("Layer ", survival.census1$PPA_assignment_max, sep = "")
  survival.census1$PPA_assignment_max = paste(survival.census1$PPA_assignment_max, ": ", sep = "")
  survival.census1$PPA_assignment_max = paste(survival.census1$PPA_assignment_max, survival.census1$n_species, sep = "")
  survival.census1$PPA_assignment_max = paste(survival.census1$PPA_assignment_max, " sp", sep = "")
  
  ########
  # dbh
  dbh.census1 = data.table(growth.between.censuses[[census.temp]])
  if(CTFS.plot %in% c("costa_rica","forestgeo")){
    dbh.census1 = dbh.census1[,.(mean_log_dbh = log(mean(dbh, na.rm = T))),
                              by =.(sp, PPA_assignment_max)]}
  if(CTFS.plot %in% c("nizanda", "yucatan")){
    dbh.census1 = dbh.census1[,.(mean_log_ba = log(mean(ba, na.rm = T))),
                              by =.(sp, PPA_assignment_max)]}
  
  
  # append number of species information, so that it will be used for faceting the plot
  dbh.census1 = merge(dbh.census1, n.species, by = "PPA_assignment_max")
  dbh.census1$PPA_assignment_max = paste("Layer ", dbh.census1$PPA_assignment_max, sep = "")
  dbh.census1$PPA_assignment_max = paste(dbh.census1$PPA_assignment_max, ": ", sep = "")
  dbh.census1$PPA_assignment_max = paste(dbh.census1$PPA_assignment_max, dbh.census1$n_species, sep = "")
  dbh.census1$PPA_assignment_max = paste(dbh.census1$PPA_assignment_max, " sp", sep = "")
  
  if(CTFS.plot %in% c("costa_rica","forestgeo")){
    plot.dbh = ggplot(data = dbh.census1) +
      geom_density(aes(x = mean_log_dbh, fill = factor(PPA_assignment_max)), colour = "grey") +
      facet_grid(PPA_assignment_max ~ ., scales = "free") +
      coord_cartesian(xlim =c(quantile(dbh.census1$mean_log_dbh, probs = c(0.01, 0.99)))) +
      theme(legend.position = "none")}
  
  if(CTFS.plot %in% c("nizanda","yucatan")){
    plot.dbh = ggplot(data = dbh.census1) +
      geom_density(aes(x = mean_log_ba, fill = factor(PPA_assignment_max)), colour = "grey") +
      facet_grid(PPA_assignment_max ~ ., scales = "free") +
      coord_cartesian(xlim =c(quantile(dbh.census1$mean_log_ba, probs = c(0.01, 0.99)))) +
      theme(legend.position = "none")}
  
  plot.growth = ggplot(data = growth.census1) +
    geom_density(aes(x = dinc_per_year_mean, fill = factor(PPA_assignment_max)), colour = "grey") +
    facet_grid(PPA_assignment_max ~ ., scales = "free") +
    coord_cartesian(xlim =c(quantile(growth.census1$dinc_per_year_mean, probs = c(0.01, 0.99)))) +
    theme(legend.position = "none")
  
  plot.survival = ggplot(data = survival.census1) +
    geom_density(aes(x = alive_mean, fill = factor(PPA_assignment_max)), colour = "grey") +
    facet_grid(PPA_assignment_max ~ ., scales = "free") +
    theme(legend.position = "none")
  
  
  full.plot.dbh.growth.survival =
    plot_grid(plot.dbh, plot.growth, plot.survival, 
              align = "h", nrow = 1, 
              rel_widths = c(0.3, 0.3, 0.3))
  
  max.nr.of.layer = length(unique(growth.census1$PPA_assignment))
  
  ggsave(output.file,
         plot = full.plot.dbh.growth.survival,
         height = max.nr.of.layer * 2 , 
         width = 10, 
         limitsize = F)
  print(full.plot.dbh.growth.survival)
}

#####################################
save.number.of.recruits = function(CTFS.plot,
                                   census,
                                   census.specifications,
                                   species.info,
                                   save.recruits.table,
                                   output.file){
  
  # exclude species and individuals status != "A"
  all.censuses.dt = rbindlist(census, fill = T, idcol= "census_id")
  all.censuses.dt = all.censuses.dt[!(all.censuses.dt$sp %in% species.info$species_to_exclude),]
  all.censuses.dt = all.censuses.dt[!(is.na(all.censuses.dt$sp)),]
  all.censuses.dt = all.censuses.dt[status == "A"]
  
  # get maxdbh and reproductive sizes (0.5 * maxdbh)
  if(CTFS.plot %in% c("costa_rica","forestgeo")){
    all.censuses.dt.biggest.ind = all.censuses.dt[order(all.censuses.dt$dbh, decreasing = T),]}
  if(CTFS.plot %in% c("nizanda","yucatan")){
    all.censuses.dt.biggest.ind = all.censuses.dt[order(all.censuses.dt$ba, decreasing = T),]}
  
  all.censuses.dt.biggest.ind = all.censuses.dt.biggest.ind[!(duplicated(all.censuses.dt.biggest.ind$treeID)),]
  
  all.censuses.dt.2.biggest = all.censuses.dt.biggest.ind[, head(.SD, 2), by=sp]
  if(CTFS.plot %in% c("costa_rica","forestgeo")){
    all.censuses.dt.2.biggest = all.censuses.dt.2.biggest[, .(max_2_ind = mean(dbh,na.rm = T)), by = sp]}
  if(CTFS.plot %in% c("nizanda","yucatan")){
    all.censuses.dt.2.biggest = all.censuses.dt.2.biggest[, .(max_2_ind = mean(ba,na.rm = T)), by = sp]}
  
  all.censuses.dt.3.biggest = all.censuses.dt.biggest.ind[, head(.SD, 3), by=sp]
  if(CTFS.plot %in% c("costa_rica","forestgeo")){
    all.censuses.dt.3.biggest = all.censuses.dt.3.biggest[, .(max_3_ind = mean(dbh,na.rm = T)), by = sp]}
  if(CTFS.plot %in% c("nizanda","yucatan")){
    all.censuses.dt.3.biggest = all.censuses.dt.3.biggest[, .(max_3_ind = mean(ba,na.rm = T)), by = sp]}
  
  all.censuses.dt.6.biggest = all.censuses.dt.biggest.ind[, head(.SD, 6), by=sp]
  if(CTFS.plot %in% c("costa_rica","forestgeo")){
    all.censuses.dt.6.biggest = all.censuses.dt.6.biggest[, .(max_6_ind = mean(dbh,na.rm = T)), by = sp]}
  if(CTFS.plot %in% c("nizanda","yucatan")){
    all.censuses.dt.6.biggest = all.censuses.dt.6.biggest[, .(max_6_ind = mean(ba,na.rm = T)), by = sp]}
  
  if(CTFS.plot %in% c("costa_rica","forestgeo")){
    all.maxdbh = all.censuses.dt.biggest.ind[,.(max_dbh = max(dbh, na.rm = T),
                                                max_dbh95 = quantile(dbh, probs = 0.95, na.rm = T),
                                                max_dbh99 = quantile(dbh, probs = 0.99, na.rm = T)), by = (sp)]}
  if(CTFS.plot %in% c("nizanda","yucatan")){
    all.maxdbh = all.censuses.dt.biggest.ind[,.(max_dbh = max(ba, na.rm = T),
                                                max_dbh95 = quantile(ba, probs = 0.95, na.rm = T),
                                                max_dbh99 = quantile(ba, probs = 0.99, na.rm = T)), by = (sp)]}
  
  all.maxdbh = merge(all.maxdbh, all.censuses.dt.2.biggest, by = "sp")
  all.maxdbh = merge(all.maxdbh, all.censuses.dt.3.biggest, by = "sp")
  all.maxdbh = merge(all.maxdbh, all.censuses.dt.6.biggest, by = "sp")
  
  print(paste("species exceeding max dbh:",
              length(unique(all.maxdbh[max_6_ind > census.specifications$max_dbh_selected]$sp))))
  all.maxdbh$max_6_ind[which(all.maxdbh$max_6_ind > census.specifications$max_dbh_selected)] = census.specifications$max_dbh_selected  
  all.maxdbh$repr_size = all.maxdbh$max_6_ind * 0.5
  all.censuses.dt = merge(all.censuses.dt, all.maxdbh[,.(sp, repr_size, max_6_ind)], by = "sp")
  
  # get recruits
  recruits.list = list()
  
  if(CTFS.plot %in% c("costa_rica","forestgeo")){
    
    all.sp.temp = unique(rbindlist(census.list, fill = T)$sp)
    all.sp.temp = all.sp.temp[!(all.sp.temp %in% species.info$species_to_exclude)]
    
    for(i in 2:length(census.list)){
      earlier.census = census.list[[i-1]]
      later.census = census.list[[i]]
      both.censuses = merge(earlier.census[,.(sp, treeID, status, ExactDate, dbh)],later.census[,.(sp, treeID, status, ExactDate)],
                            by = c("sp","treeID"))
      
      # remove unwanted and NA species
      both.censuses = both.censuses[!is.na(sp)]
      both.censuses = both.censuses[!(sp %in% species.info$species_to_exclude)]
      
      # calculate recruitment
      both.censuses$recruited = ifelse(both.censuses$status.x == "P" & both.censuses$status.y == "A", 1,0)
      recruits.temp = both.censuses[,.(recruits = sum(recruited, na.rm =T)), by = "sp"]
      
      sp.to.add = all.sp.temp[which(!(all.sp.temp %in% recruits.temp$sp))]
      if(length(sp.to.add) > 0){
        recruits.temp = rbind(recruits.temp,
                              data.frame(sp = sp.to.add,
                                         recruits = 0))}
      
      recruits.temp$census_earlier = names(census.list)[i-1]
      recruits.temp$census_later = names(census.list)[i]
      
      # add dates
      recruits.temp$census_date_earlier = mean.Date(as.Date(earlier.census$ExactDate))
      recruits.temp$census_date_later = mean.Date(as.Date(later.census$ExactDate))
      recruits.temp$days_between_censuses = as.numeric(difftime(recruits.temp$census_date_later, 
                                                                recruits.temp$census_date_earlier,
                                                                units = "days"))
      recruits.temp$years_between_censuses = recruits.temp$days_between_censuses / 365.25
      
      # calculate log recruits, recruits per year log(recruits) per year
      recruits.temp$log_recruits = log(recruits.temp$recruits)
      recruits.temp$recruits_per_year = recruits.temp$recruits / recruits.temp$years_between_censuses
      recruits.temp$log_recruits_per_year =  log(recruits.temp$recruits_per_year)
      
      recruits.list[[i-1]] = recruits.temp}}
  
  if(CTFS.plot %in% c("nizanda","yucatan")){
    
    all.sp.temp = unique(rbindlist(census.list)$sp)
    all.sp.temp = all.sp.temp[!(all.sp.temp %in% species.info$species_to_exclude)]
    
    for(i in 2:length(census.list)){
      earlier.census = census.list[[i-1]]
      later.census = census.list[[i]]
      both.censuses = merge(earlier.census[,.(sp, treeID, status, ExactDate, ba)],later.census[,.(sp, treeID, status, ExactDate)],
                            by = c("sp","treeID"))
      # remove unwanted and NA species
      both.censuses = both.censuses[!is.na(sp)]
      both.censuses = both.censuses[!(sp %in% species.info$species_to_exclude)]
      
      # calculate recruitment
      both.censuses$recruited = ifelse(both.censuses$status.x == "P" & both.censuses$status.y == "A", 1,0)
      recruits.temp = both.censuses[,.(recruits = sum(recruited, na.rm =T)), 
                                    by = "sp"]
      hist(recruits.temp$recruits)
      
      # add sp that are not in these censuses
      sp.to.add = all.sp.temp[which(!(all.sp.temp %in% recruits.temp$sp))]
      if(length(sp.to.add) > 0){
        recruits.temp = rbind(recruits.temp,
                              data.frame(sp = sp.to.add,
                                         recruits = 0))}
      
      recruits.temp$census_earlier = names(census.list)[i-1]
      recruits.temp$census_later = names(census.list)[i]
      
      # add dates
      recruits.temp$census_date_earlier = mean.Date(as.Date(earlier.census$ExactDate))
      recruits.temp$census_date_later = mean.Date(as.Date(later.census$ExactDate))
      recruits.temp$days_between_censuses = as.numeric(difftime(recruits.temp$census_date_later, 
                                                                recruits.temp$census_date_earlier,
                                                                units = "days"))
      recruits.temp$years_between_censuses = recruits.temp$days_between_censuses / 365.25
      
      # calculate log recruits, recruits per year log(recruits) per year
      recruits.temp$log_recruits = log(recruits.temp$recruits)
      recruits.temp$recruits_per_year = recruits.temp$recruits / recruits.temp$years_between_censuses
      recruits.temp$log_recruits_per_year = log(recruits.temp$recruits_per_year)
      
      if(i == 2){
        recruits.per.interval.temp = recruits.temp
      }else{
        recruits.per.interval.temp = rbind(recruits.per.interval.temp, recruits.temp)}}
    
    recruits.list[[i-1]] = recruits.per.interval.temp}
  
  recruits = rbindlist(recruits.list, fill = T)
  
  # calculate basal area values
  if(CTFS.plot %in% c("costa_rica","forestgeo")){
    all.censuses.dt$ba = (pi * (all.censuses.dt$dbh ^2) / 4 ) / 1000000}
  
  # all individuals                               
  if(CTFS.plot %in% c("costa_rica","forestgeo")){
    all.ind.estimates = all.censuses.dt[,.(nr_ind_all = length(treeID),
                                           ba_sum_all = sum(ba, na.rm = T)),
                                        by = .(sp, census_id)]}
  if(CTFS.plot %in% c("nizanda","yucatan")){
    all.ind.estimates = all.censuses.dt[,.(nr_ind_all = length(treeID),
                                           ba_sum_all = sum(ba, na.rm = T)),
                                        by = .(sp, census_id)]}
  
  # only adult individuals
  adult.estimates = all.censuses.dt
  if(CTFS.plot %in% c("costa_rica","forestgeo")){
    adult.estimates$is_adult = ifelse(adult.estimates$dbh >= adult.estimates$repr_size, 1, 0)}
  if(CTFS.plot %in% c("nizanda","yucatan")){
    adult.estimates$is_adult = ifelse(adult.estimates$ba >= adult.estimates$repr_size, 1, 0)}
  
  adult.estimates = adult.estimates[is_adult == 1]
  if(CTFS.plot %in% c("costa_rica","forestgeo")){
    adult.estimates = adult.estimates[,.(nr_ind_adult = length(treeID),
                                         ba_sum_adult = sum(ba, na.rm = T)), 
                                      by = .(sp, census_id)]}
  if(CTFS.plot %in% c("nizanda","yucatan")){
    adult.estimates = adult.estimates[,.(nr_ind_adult = length(treeID),
                                         ba_sum_adult = sum(ba, na.rm = T)), 
                                      by = .(sp, census_id)]}
  
  # merge with recruits data to have every species in every census
  if(CTFS.plot %in% c("costa_rica","forestgeo")){
    ind.ba.estimates.per.census = data.table(sp = rep(all.maxdbh$sp, length(unique(all.censuses.dt$census_id))),
                                             census_id = rep(names(census.list), each = nrow(all.maxdbh)))
    ind.ba.estimates.per.census = merge(ind.ba.estimates.per.census, all.ind.estimates,
                                        by = c("sp", "census_id"), all.x = T)
    ind.ba.estimates.per.census = merge(ind.ba.estimates.per.census, adult.estimates,
                                        by = c("sp", "census_id"), all.x = T)
    
    # replace is.na(nr_ind, nr_adult, sum_ba_all, sum_ba_adult) with 0
    ind.ba.estimates.per.census$nr_ind_all[is.na(ind.ba.estimates.per.census$nr_ind_all)] = 0
    ind.ba.estimates.per.census$ba_sum_all[is.na(ind.ba.estimates.per.census$ba_sum_all)] = 0
    ind.ba.estimates.per.census$nr_ind_adult[is.na(ind.ba.estimates.per.census$nr_ind_adult)] = 0
    ind.ba.estimates.per.census$ba_sum_adult[is.na(ind.ba.estimates.per.census$ba_sum_adult)] = 0
    
    # calculate mean values
    mean.ind.ba.values = ind.ba.estimates.per.census[,.(mean_nr_ind_all = mean(nr_ind_all),
                                                        mean_ba_sum_all = mean(ba_sum_all),
                                                        mean_nr_ind_adult = mean(nr_ind_adult),
                                                        mean_ba_sum_adult = mean(ba_sum_adult)),
                                                     by = sp]
    
    # merge recruits and mean ind and ba values
    recruits = merge(recruits, mean.ind.ba.values, by = "sp", all.x = T)}
  
  if(CTFS.plot %in% c("nizanda","yucatan")){
    ind.ba.estimates.per.census = data.table(cbind(data.frame(sp = rep(all.maxdbh$sp, nrow(unique(all.censuses.dt[,.(census_id)])))),
                                                   unique(all.censuses.dt[,.(census_id)])))
    ind.ba.estimates.per.census = merge(ind.ba.estimates.per.census, all.ind.estimates,
                                        by = c("sp", "census_id"), all.x = T)
    ind.ba.estimates.per.census = merge(ind.ba.estimates.per.census, adult.estimates,
                                        by = c("sp", "census_id"), all.x = T)
    
    # replace is.na(nr_ind, nr_adult, sum_ba_all, sum_ba_adult) with 0
    ind.ba.estimates.per.census$nr_ind_all[is.na(ind.ba.estimates.per.census$nr_ind_all)] = 0
    ind.ba.estimates.per.census$ba_sum_all[is.na(ind.ba.estimates.per.census$ba_sum_all)] = 0
    ind.ba.estimates.per.census$nr_ind_adult[is.na(ind.ba.estimates.per.census$nr_ind_adult)] = 0
    ind.ba.estimates.per.census$ba_sum_adult[is.na(ind.ba.estimates.per.census$ba_sum_adult)] = 0
    
    # calculate mean values
    mean.ind.ba.values = ind.ba.estimates.per.census[,.(mean_nr_ind_all = mean(nr_ind_all),
                                                        mean_ba_sum_all = mean(ba_sum_all),
                                                        mean_nr_ind_adult = mean(nr_ind_adult),
                                                        mean_ba_sum_adult = mean(ba_sum_adult)),
                                                     by = .(sp)]
    
    # merge recruits and mean ind and ba values
    recruits = merge(recruits, mean.ind.ba.values, by = c("sp"), all.x = T)}
  
  # save results
  if(save.recruits.table == T){
    saveRDS(recruits,
            output.file)}
  
  return(recruits)
}

#######################################################
save_balanced_data = function(CTFS.plot,
                              growth.between.censuses.to.balance,
                              survival.between.censuses.to.balance,
                              save.balanced.tables,
                              output.file.growth,
                              output.file.survival){
  
  # put all data frames together into one
  growth.all.censuses = rbindlist(growth.between.censuses.to.balance,
                                  fill = T,
                                  idcol= "census_id")
  
  survival.all.censuses = rbindlist(survival.between.censuses.to.balance,
                                    fill = T,
                                    idcol= "census_id")
  
  # remove singular species (i.e. with only one individual that is present in only two censecutive censuses)
  growth.all.censuses = growth.all.censuses[,.SD[length(tree_stemID) > 1],
                                            by = sp]
  
  survival.all.censuses = survival.all.censuses[,.SD[length(tree_stemID) > 1],
                                                by = sp]
  
  # for growth: for each species, exclude the highest and lowest 1% of growth for each species
  growth.all.censuses = growth.all.censuses[, .SD[dinc_per_year >= quantile(dinc_per_year, probs = 0.01, type = 3) & 
                                                    dinc_per_year <= quantile(dinc_per_year, probs = 0.99, type = 3)],
                                            by = sp]
  
  ###########################################
  # growth - create balanced data sets
  reduced.growth.sample = growth.all.censuses[,.SD[sample(.N, min(100,.N))],by =.(sp, crown_layer)]
  reduced.growth.sample = reduced.growth.sample[,.(sp,dinc_per_year, crown_layer)]
  
  ###########################################
  # survival - create balanced data sets
  reduced.survival.sample = survival.all.censuses[,.SD[sample(.N, min(1000,.N))],by =.(sp, crown_layer)]
  reduced.survival.sample = reduced.survival.sample[,.(sp, alive, interval_date1_and_2_in_years, crown_layer)]
  
  #####################################
  # save the balanced data sets
  if(save.balanced.tables == T){
    saveRDS(reduced.growth.sample,
            output.file.growth)
    saveRDS(reduced.survival.sample,
            output.file.survival)
  }
  
  
  if(save.balanced.tables == F){
    results = list("survival_balanced" = reduced.survival.sample,
                   "growth_balanced" = reduced.growth.sample)
    return(results)}
  
}

################################################################
# small function to get the reproductive dbh for a given species
fetch.the.reproductive_dbh_for_a_given_species = function(species.temp, species.max.dbh.temp){
  rep_dbh_temp = species.max.dbh.temp$reproductive_dbh[which(species.max.dbh.temp$sp == species.temp)]
  return(rep_dbh_temp)
}

##############################################################################
model.greta.recruits = function(census.name, recruitment.list.from.main.census, 
                                nr.iterations, nr.chains){
  
  # arguments:
  # census.name: string giving the name of the FDP (e.g. "bci")
  # recruitment.list.from.main.census: the list that is created by the function "calculate.number.of.recruits.from.main.census" (e.g. bci.recruitment.main.census)
  # nr.iterations: chain length of the mcmc chain
  # nr.chains: number of mcmc chains to compute
  
  if(missing(nr.iterations)){stop("Please specifiy nr.iterations")}
  if(missing(nr.chains)){stop("Please specifiy nr.chains")}
  
  # compile one large data table, remove last census
  all.censuses.temp = rbindlist(recruitment.list.from.main.census, fill = T, idcol = "uniqueID")
  all.censuses.temp = all.censuses.temp[census_id < max(all.censuses.temp$census_id)]
  
  # get the number of species
  specieslist.temp = unique(as.character(all.censuses.temp$sp))
  nr.species.temp = length(specieslist.temp)
  nr.censuses = length(recruitment.list.from.main.census) - 1 # no recruitment from last census on
  
  # create a results data frame to store the greta results
  results = data.frame("sp" = NA, CI_mean = NA, CI_lb =NA, CI_ub = NA,  
                       gelman_diag_estimate = NA, gelman_diag_CI_up = NA, R_hat = NA)[0,]
  
  # get the confidence intervals for every species
  for(i in 1: length(specieslist.temp)){
    
    # specify the data
    recruits.year.ba.temp = as_data(all.censuses.temp[sp %in% specieslist.temp[i],recruits_div_years_div_mean_adult_sum_ba])
    
    # define the model parameters (priors)
    lambda = uniform(min = 0, max(all.censuses.temp$recruits_div_years_div_mean_adult_sum_ba))
    
    # define the likelihood structure
    distribution(recruits.year.ba.temp) = poisson(lambda)
    
    # define the model for greta
    model.temp = model(lambda)
    
    # check model structure
    #plot(model.temp)
    
    # do the specified number of mcmc chains
    all.chains.temp  = greta::mcmc(model.temp, n_samples = nr.iterations, 
                                   chains = nr.chains, verbose = F)
    
    # give a status report
    #print(paste(c(species.temp, "  ", nr.chains, "  chains  done"), collapse = ""))
    
    # check results
    # mcmc_trace(draws.combined)
    # mcmc_intervals(draws.combined)
    
    # transfer results into results data frame
    results = rbind(results,
                    data.frame("sp" = specieslist.temp[i], 
                               CI_mean = summary(all.chains.temp)$statistics["Mean"],
                               CI_lb = min(summary(all.chains.temp)$quantiles), 
                               CI_ub = max(summary(all.chains.temp)$quantiles), 
                               gelman_diag_estimate = gelman.diag(x = all.chains.temp)$psrf[,"Point est."],
                               gelman_diag_CI_up = gelman.diag(x = all.chains.temp)$psrf[,"Upper C.I."],
                               R_hat = R.hat(matrix(unlist(all.chains.temp), ncol = nr.chains))))
    
    # give status report
    print(paste(c("Finished species  ",i,"   of   ", length(specieslist.temp)), collapse = ""))
  }
  
  # calculate the coefficient of variation, i.e. the mean divided by CI-range
  results$coef_of_variation = (results$CI_ub - results$CI_lb) / results$CI_mean 
  
  # save results data.tablelist in global environment
  assign(paste(c(census.name, ".nr.recruits.year-1.ba-1.posteriors"), collapse=""),
         results, 
         envir=globalenv())
  
  # print message that results list has been created
  print(paste(c(census.name, ".nr.recruits.year-1.ba-1.posteriors","   has been stored"), collapse=""))
}

###############################################################
model.stan.recruits =  function(recruitment.unbalanced, 
                                column.with.recruitment.data,
                                stan.model.file, 
                                nr.iterations, 
                                nr.chains, stan.algorithm){
  
  # get the number of species and censuses
  specieslist.temp = unique(as.character(recruitment.unbalanced$sp))
  nr.species.temp = length(specieslist.temp)
  nr.censuses = length(unique(recruitment.unbalanced$census_id)) # no recruitment from last census on
  
  # create a list with data that will be used by the stan model
  stan.data.temp = list(Nobs = nrow(recruitment.unbalanced),
                        Nsp = nr.species.temp,
                        sp = as.numeric(as.factor(recruitment.unbalanced$sp)),
                        obs = as.integer(recruitment.unbalanced[,get(column.with.recruitment.data)]))
  
  stan.model.temp = stan(file = stan.model.file, 
                         data = stan.data.temp, 
                         iter = nr.iterations, 
                         chains = nr.chains,
                         algorithm = stan.algorithm,
                         control = list(adapt_delta = 0.99, max_treedepth = 30),
                         verbose = F)
  
  stan.summary.temp = summary(stan.model.temp)
  
  # extract predicted mean number of recruits
  stan.mean.predicted.recruits = stan.summary.temp$summary[1:nr.species.temp,which(dimnames(stan.summary.temp$summary)[[2]]=="50%")]      
  stan.mean.predicted.recruits.log = log(stan.mean.predicted.recruits)
  
  # ectract predicted CI for the number of recruits
  stan.CI.predicted.recruits = stan.summary.temp$summary[1:nr.species.temp,which(dimnames(stan.summary.temp$summary)[[2]]=="97.5%")] - stan.summary.temp$summary[1:nr.species.temp,which(dimnames(stan.summary.temp$summary)[[2]]=="2.5%")]
  stan.CI.predicted.recruits.log = log(stan.summary.temp$summary[1:nr.species.temp,which(dimnames(stan.summary.temp$summary)[[2]]=="97.5%")]) - log(stan.summary.temp$summary[1:nr.species.temp,which(dimnames(stan.summary.temp$summary)[[2]]=="2.5%")])
  
  # check maximum R hat statistics, which should be below 1.05
  stan.Rhat.predicted.recruits = stan.summary.temp$summary[1:nr.species.temp,which(dimnames(stan.summary.temp$summary)[[2]]=="Rhat")] 
  print(paste("Rhat =",round(max(stan.Rhat.predicted.recruits),5)))
  
  # calculate weight for the stan.estimates
  stan.weight.recruits =  1 - ((stan.CI.predicted.recruits / stan.mean.predicted.recruits) / quantile((stan.CI.predicted.recruits/stan.mean.predicted.recruits), probs = 0.99))
  stan.weight.recruits.log =  1- ((stan.CI.predicted.recruits.log/stan.mean.predicted.recruits.log) / quantile(stan.CI.predicted.recruits.log/stan.mean.predicted.recruits.log, probs = 0.95))
  stan.weight.recruits[which(stan.weight.recruits <= 0)] = 0.000001
  stan.weight.recruits.log[which(stan.weight.recruits.log <= 0)] = 0.000001
  
  
  # calculate the weigths for the n of total recruits
  stan.weight.recruits = data.frame(
    "sp" = specieslist.temp,
    "stan_mean_nr_recruits" = stan.mean.predicted.recruits,
    "stan_CI_nr_recruits" = stan.CI.predicted.recruits,
    "stan_weights_for_nr_recruits" =stan.weight.recruits,
    "stan_mean_nr_recruits_log" = stan.mean.predicted.recruits.log,
    "stan_CI_nr_recruits_log" = stan.CI.predicted.recruits.log,
    "stan_weights_for_nr_recruits_log" = stan.weight.recruits.log,
    "stan_Rhat_for_nr_recruits" = stan.Rhat.predicted.recruits)
  
  # save the results
  assign("stan.recruitment", stan.weight.recruits, envir=globalenv())
  print("stan.recruitment saved to global environment")
  
  # save the full model for validation and debugging
  assign("stan.recruitment.model", stan.model.temp, envir=globalenv())
  print("stan.recruitment.model saved to global environment")
}

###############################################################
model.stan.survival = function(survival.balanced, 
                               nr.iterations, 
                               stan.init,
                               nr.chains, 
                               stan.algorithm, 
                               stan.model.file, 
                               stan.control){
  
  # determine number of species
  stan.number.of.species = length(unique(survival.balanced$sp))
  specieslist.temp = levels(factor(survival.balanced$sp))
  
  # determine number of layers and the file to use then
  stan.max.crown.layers  = max(survival.balanced$crown_layer)
  stan.model.file = paste(c(stan.model.file, stan.max.crown.layers, ".stan"), collapse = "")
  
  # arrange the data for stand
  stan.data = list(n_sp = stan.number.of.species,
                   n_obs = nrow(survival.balanced),
                   sp = as.numeric(factor(survival.balanced$sp)),
                   tinterval = survival.balanced$interval_date1_and_2_in_years,
                   alive = survival.balanced$alive,
                   cr_layer = survival.balanced$crown_layer)
  
  stan.model.temp = stan(file = stan.model.file,
                         data = stan.data,
                         iter = nr.iterations,
                         chains = nr.chains,
                         algorithm = stan.algorithm,
                         init = stan.init, 
                         refresh = round(nr.iterations/50, digits = 0),
                         control = stan.control,
                         verbose = F)
  
  stan.summary.temp = summary(stan.model.temp)
  
  # sequentially build the results table
  stan.weight.survival = data.frame(
    "sp" = rep(specieslist.temp, stan.max.crown.layers))
  
  # add crown layers
  add.to.stan.weight.survival = c()
  for(i in 1:stan.max.crown.layers){
    add.to.stan.weight.survival = c(add.to.stan.weight.survival, rep(i, length(specieslist.temp)))}
  stan.weight.survival$crown_layer = add.to.stan.weight.survival
  
  # add stan estimates
  stan.weight.survival$stan_mean_surv_rate = stan.summary.temp$summary[1:(stan.max.crown.layers * stan.number.of.species), "50%"]
  stan.weight.survival$stan_CI_surv_rate = stan.summary.temp$summary[1 : (stan.max.crown.layers * stan.number.of.species),"97.5%"] - stan.summary.temp$summary[1 : (stan.max.crown.layers * stan.number.of.species),"2.5%"]
  
  stan.weight.survival$stan_weight_surv_rate = NA
  for(i in 1:stan.max.crown.layers){
    stan.weight.survival$stan_weight_surv_rate[which(stan.weight.survival$crown_layer == i)] = 1- ((stan.weight.survival$stan_CI_surv_rate[which(stan.weight.survival$crown_layer == i)]) /
                                                                                                     quantile(stan.weight.survival$stan_CI_surv_rate[which(stan.weight.survival$crown_layer == i)], probs = 0.99))
  }
  
  # replace zero or negative weights
  stan.weight.survival$stan_weight_surv_rate[which(stan.weight.survival$stan_weight_surv_rate <= 0)] = 0.000001
  stan.weight.survival$Rhat = stan.summary.temp$summary[1:(stan.max.crown.layers * stan.number.of.species),"Rhat"]
  
  # remove the columns stan_CI_div_mean_surv_rate which is no longer needed
  stan.weight.survival$stan_CI_div_mean_surv_rate = NULL
  
  # print the Rhat statistics
  print(paste("Rhat = ", round(max(stan.summary.temp$summary[1:( stan.max.crown.layers * stan.number.of.species),"Rhat"]), 5), sep=""))
  print("Rhat should be close to 1 (not bigger than 1.05)")
  
  # save the results
  assign("stan.survival", stan.weight.survival, envir=globalenv())
  print("stan.survival stored to global environment")
  
  # save the full model for validation and debugging
  assign("stan.survival.model",stan.model.temp, envir=globalenv())
  print("stan.survival.model stored to global environment")
}

###############################################################
modulus.transformation = function(x, delta){
  x2 = x
  x[which(x < 0)] = 0 - x[which(x < 0)]
  x = x ^ delta
  x[which(x2 < 0)] = 0 - x[which(x2 < 0)]
  return(x)
}

##########################################################
# functions to transform dinc_per_year prior to stan model
do.forward.transform = function(x){
  max.for.back.transformation = max(x)
  x =  log(x + max.for.back.transformation)
  mean.for.back.transformation = mean(x)
  x = x - mean(x)
  sd.for.backtransformation = sd(x)
  x = x / sd(x)
  results.list = list("values" = x,
                      "max1" = max.for.back.transformation,
                      "mean2" = mean.for.back.transformation,
                      "sd3" = sd.for.backtransformation)
  return(results.list)
}

do.back.transform = function(x, transform.info){
  x = x * transform.info$sd3
  x = x + transform.info$mean2
  x = exp(x) - transform.info$max1
  return(x)
}

#################################################
# calculate layer means and sigmas from data to use them as model priors
calculate.mean.and.sigma.of.growth.across.species = function(growth.data){
  sp.estimates = growth.data[,.(mean_sp = mean(dinc_trans),
                                sd_sp = sd(dinc_trans), 
                                n = length(dinc_trans)), 
                             by = .(sp, crown_layer)]
  layer.estimates = sp.estimates[,.(mean_of_species_mean = mean(mean_sp, na.rm =T),
                                    sd_of_species_mean = sd(mean_sp, na.rm =T),
                                    mean_of_species_sd = mean(sd_sp, na.rm =T),
                                    sd_of_species_sd = sd(sd_sp, na.rm = T)),
                                 by= crown_layer]
  layer.estimates = layer.estimates[order(layer.estimates$crown_layer),]
  
  results = list("species_values" = sp.estimates,
                 "layer_values" = layer.estimates)
  return(results)}

###############################################################
model.stan.growth = function(growth.data,
                             stan.init,
                             nr.iterations,
                             nr.chains,
                             stan.algorithm,
                             stan.model.file,
                             stan.control,
                             stan.species.list){
  
  # arguments:
  # column.name.of.growth.data: the name of the column with the growth data (included if data is transformed and thus has different column name)
  # growth.data.long.form: specify the data file (with path) where the balanced growth data has been stored (e.g. "test\\bci_growth_balanced_data.txt")
  # stan.model.file: specify the data file that specifies the stand model structure (e.g. "test\\growth_SK.stan")
  # nr.iterations: chain length of the mcmc chain
  # nr.chains: number of mcmc chains to compute
  # stan.control = control arguments that can be supplied to the stan-function
  # stan.init = init-argument that can be supplied to the pstan/rstan-function
  
  # determine species list
  stan.species.list = as.character(levels(factor(growth.data$sp)))
  
  # determine median values to speed up stan
  prior.values = calculate.mean.and.sigma.of.growth.across.species(growth.data)
  
  # determine maximum number of crown layers
  stan.max.crown.layers = max(growth.data$crown_layer)
  stan.model.file = paste(c(stan.model.file,max(growth.data$crown_layer),".stan"), collapse = "")
  
  # contruct stan.data
  stan.data  = list(n_obs = nrow(growth.balanced),
                    n_sp = length(unique(growth.balanced$sp)),
                    sp = as.numeric(factor(growth.balanced$sp)),
                    dinc = growth.balanced$dinc_trans,
                    cr_layer = growth.balanced$crown_layer,
                    mean_growth_lb = quantile(prior.values$species_values$mean_sp, probs = 0.05),
                    mean_growth_ub = quantile(prior.values$species_values$mean_sp, probs = 0.95),
                    sigma_of_mean_growth_lb = 0, 
                    sigma_of_mean_growth_ub = 2,
                    log_mean_sigma_lb = quantile(log(prior.values$species_values$sd_sp[is.finite(prior.values$species_values$sd_sp)]), probs = 0.05),
                    log_mean_sigma_ub = quantile(log(prior.values$species_values$sd_sp[is.finite(prior.values$species_values$sd_sp)]), probs = 0.95),
                    sigma_of_mean_sigma_lb = 0,
                    sigma_of_mean_sigma_ub = 2)
  
  stan.model.temp = stan(file = stan.model.file,
                         data = stan.data, 
                         iter = nr.iterations,
                         chains = nr.chains,
                         init = stan.init,
                         algorithm = stan.algorithm,
                         refresh = round(nr.iterations/50, digits = 0),
                         control = stan.control,
                         verbose = F)
  
  stan.summary.temp = summary(stan.model.temp)
  
  # sequentially build the results table
  stan.weight.growth = data.frame(
    "sp" = rep(stan.species.list, stan.max.crown.layers))
  
  # add crown layers
  add.to.stan.weight.growth = c()
  for(i in 1:stan.max.crown.layers){
    add.to.stan.weight.growth = c(add.to.stan.weight.growth, rep(i, length(stan.species.list)))}
  stan.weight.growth$crown_layer = add.to.stan.weight.growth
  
  # add stan estimates
  stan.weight.growth$stan_mean_growth_rate = stan.summary.temp$summary[1:( stan.max.crown.layers * stan.data$n_sp), "50%"]
  stan.weight.growth$stan_CI_growth_rate = stan.summary.temp$summary[1 : ( stan.max.crown.layers * stan.data$n_sp),"97.5%"] - stan.summary.temp$summary[1 : (stan.max.crown.layers * stan.data$n_sp),"2.5%"]
  
  stan.weight.growth$stan_weight_growth_rate = NA
  for(i in 1:stan.max.crown.layers){
    stan.weight.growth$stan_weight_growth_rate[which(stan.weight.growth$crown_layer == i)] = 1- ((stan.weight.growth$stan_CI_growth_rate[which(stan.weight.growth$crown_layer == i)]) /
                                                                                                   quantile(stan.weight.growth$stan_CI_growth_rate[which(stan.weight.growth$crown_layer == i)], probs = 0.99))
  }
  
  stan.weight.growth$stan_weight_growth_rate[which(stan.weight.growth$stan_weight_growth_rate <= 0)] = 0.000001
  stan.weight.growth$Rhat = stan.summary.temp$summary[1:(stan.max.crown.layers * stan.data$n_sp),"Rhat"]
  
  # remove the columns stan_CI_div_mean_growth_rate which is no longer needed
  stan.weight.growth$stan_CI_div_mean_growth_rate = NULL
  
  # print the Rhat statistics
  print(paste("Rhat = ", round(max(stan.summary.temp$summary[1:(stan.max.crown.layers * stan.data$n_sp),"Rhat"]), 5), sep=""))
  print("Rhat should be close to 1 (not bigger than 1.05)")
  
  # save the results
  assign("stan.growth", stan.weight.growth, envir=globalenv())  
  print("stan.growth stored in global environment")
  
  # save the full model for validation and debugging
  assign("stan.growth.model",stan.model.temp, envir=globalenv())
  print("stan.growth.model stored in global environment")
}

###################################################
create.data.table.to.compare.stan.growth.estimates = function(growth.balanced,
                                                              stan.growth.model,
                                                              stan.growth,
                                                              transform.info,
                                                              nr.of.crown.layers){
  
  stan.model.summary = summary(stan.growth.model)$summary                  
  stan.model.summary[,"mean"] = stan.model.summary[,"mean"]
  stan.model.summary[,"2.5%"] = stan.model.summary[,"2.5%"]
  stan.model.summary[,"97.5%"] =stan.model.summary[,"97.5%"]
  
  growth.prior.medians = calculate.mean.and.sigma.of.growth.across.species(growth.balanced)
  
  growth.vector = paste("mean_growth", 1:nr.of.crown.layers, sep = "")
  
  print(paste(c("correlation between model layer mean and data layer median estimates: ",
                round(cor(stan.model.summary[growth.vector,"mean"],
                          growth.prior.medians$layer_values$mean_of_species_mean[order(growth.prior.medians$layer_values$crown_layer, decreasing = F)]),
                      digits = 3)),
              collapse = ""))
  
  species.mean.growth.from.data = 
    growth.balanced[,.(data_trans_mean = mean(dinc_trans), 
                       data_trans_sd = sd(dinc_trans),
                       data_trans_n = length(dinc_trans),
                       data_mean = mean(dinc_per_year), 
                       data_sd = sd(dinc_per_year),
                       data_n = length(dinc_per_year)), by = .(sp, crown_layer)]
  
  data.to.compare = data.frame("sp" = levels(factor(growth.balanced$sp)),
                               "layer" = rep(1:nr.of.crown.layers, each =length(levels(factor(growth.balanced$sp)))),
                               "stan_trans_mean" = stan.model.summary[1: (nr.of.crown.layers * length(levels(factor(growth.balanced$sp)))), "mean"],
                               "stan_trans_mean_CI" = abs( stan.model.summary[1: (nr.of.crown.layers * length(levels(factor(growth.balanced$sp)))), "97.5%"] - stan.model.summary[1: (nr.of.crown.layers * length(levels(factor(growth.balanced$sp)))), "2.5%"]),
                               "stan_mean" = do.back.transform(stan.model.summary[1: (nr.of.crown.layers * length(levels(factor(growth.balanced$sp)))), "mean"], transform.info = transform.info),
                               "stan_mean_CI" = do.back.transform(abs( stan.model.summary[1: (nr.of.crown.layers * length(levels(factor(growth.balanced$sp)))), "97.5%"] - stan.model.summary[1: (nr.of.crown.layers * length(levels(factor(growth.balanced$sp)))), "2.5%"]), transform.info = transform.info),
                               
                               "weights" = stan.growth$stan_weight_growth_rate,
                               "stan_sigma" = stan.model.summary[((nr.of.crown.layers * length(levels(factor(growth.balanced$sp)))) + 1) :
                                                                   (2 * (nr.of.crown.layers * length(levels(factor(growth.balanced$sp))))), "mean"])
  
  data.to.compare = data.table(merge(data.to.compare, species.mean.growth.from.data, 
                                     by.x = c("sp","layer"), by.y = c("sp","crown_layer"), all.x = T))
  
  # append layer model means
  data.to.compare$model_layer_mean = NA
  
  for(i in 1: nr.of.crown.layers){
    data.to.compare$model_layer_mean[which(data.to.compare$layer == i)] = stan.model.summary[c(growth.vector[i]),"mean"]}
  
  # calculate absolute difference in model and data estimates
  data.to.compare$diff_data_and_est = abs(data.to.compare$stan_mean - data.to.compare$data_mean)
  
  return(data.to.compare)
}

#####################################################
# compile all stan estimates in one list for the wPCA
compile.stan.estimates.for.wPCA = function(CTFS.plots, 
                                           per.plot.or.per.census.interval, 
                                           folder.of.data.to.compile,
                                           data.to.omit.in.plotting){
  
  stan.recruitment = list(); stan.survival = list(); stan.growth = list()
  all.stan.estimates = list(); species.names = list(); transform.info = list()
  
  #################################################
  if(per.plot.or.per.census.interval == "per plot"){
  # load stan estimates
    for(i in 1:nrow(CTFS.plots)){
      plot.temp = CTFS.plots$CTFS_plot[i]
      
      stan.recruitment[[plot.temp]] = data.table(readRDS(paste(c(folder.of.data.to.compile,"/",plot.temp,"/",plot.temp,"_stan_recruitment.rds"), collapse = "")))
      
      # survival + mark which estimates are based on data
      stan.survival[[plot.temp]] = data.table(readRDS(paste(c(folder.of.data.to.compile,"/", plot.temp,"/",plot.temp,"_stan_survival.rds"), collapse = "")))
      stan.survival[[plot.temp]]$sp_layer_comb = paste(stan.survival[[plot.temp]]$sp, stan.survival[[plot.temp]]$crown_layer, sep = "_")
      survival.data = data.table(readRDS(paste(c(folder.of.data.to.compile,"/",plot.temp,"/",plot.temp,"_balanced_survival.rds"), collapse = "")))
      survival.data$sp_layer_comb = paste(survival.data$sp, survival.data$crown_layer, sep = "_")
      stan.survival[[plot.temp]]$data_available = ifelse(stan.survival[[plot.temp]]$sp_layer_comb %in% survival.data$sp_layer_comb, "yes", "no")
      stan.survival[[plot.temp]]$sp_layer_comb = NULL
      
      stan.growth[[plot.temp]] = data.table(readRDS(paste(c(folder.of.data.to.compile,"/", plot.temp,"/",plot.temp,"_stan_growth.rds"), collapse = "")))
      stan.growth[[plot.temp]]$sp_layer_comb = paste(stan.growth[[plot.temp]]$sp, stan.growth[[plot.temp]]$crown_layer, sep = "_")
      growth.data = data.table(readRDS(paste(c(folder.of.data.to.compile,"/",plot.temp,"/",plot.temp,"_balanced_growth.rds"), collapse = "")))
      growth.data$sp_layer_comb = paste(growth.data$sp, growth.data$crown_layer, sep = "_")
      stan.growth[[plot.temp]]$data_available = ifelse(stan.growth[[plot.temp]]$sp_layer_comb %in% growth.data$sp_layer_comb, "yes", "no")
      stan.growth[[plot.temp]]$sp_layer_comb = NULL
      
      species.names[[plot.temp]] = stan.growth[[plot.temp]]$sp}
    
    # compile data.tables with all estimates
    stan.recruitment = rbindlist(stan.recruitment, use.names = T, idcol = "CTFS_plot")
    stan.survival = rbindlist(stan.survival, use.names = T, idcol = "CTFS_plot")
    stan.growth = rbindlist(stan.growth, use.names = T, idcol = "CTFS_plot")}

  # calculate the number of recruits per individuals
  stan.recruitment$stan_yearly_recruits_per_capita = stan.recruitment$stan_mean_nr_recruits / stan.recruitment$mean_nr_ind_all
  
  # transform recruitment - log
  stan.recruitment$stan_yearly_recruits_per_mean_adult_ba_trans = log(stan.recruitment$stan_yearly_recruits_per_mean_adult_ba)
  stan.recruitment$stan_yearly_recruits_per_capita_trans = log(stan.recruitment$stan_yearly_recruits_per_capita)
  
  # transform survival - lifespan
  stan.survival$stan_mean_surv_rate_trans = log(1/(1-stan.survival$stan_mean_surv_rate))
  
  # transform growth - + log(abs(min(growth)) + 0.01)
  stan.growth$stan_mean_growth_rate_trans = log(stan.growth$stan_mean_growth_rate + abs(min(stan.growth$stan_mean_growth_rate)) + 0.01)
  
  all.stan.estimates[["recruitment"]] = stan.recruitment
  all.stan.estimates[["survival"]] = stan.survival
  all.stan.estimates[["growth"]] = stan.growth
  
  return(all.stan.estimates)
  
}

###############################################################
calculate.wPCAs = function(CTFS.plots, all.stan.estimates, nr.of.max.crown.layers){
  
  results = list()
  #unweighted.pcas = list()
  
  stan.recruitment.estimates = all.stan.estimates$recruitment[,.(CTFS_plot, sp, stan_yearly_recruits_per_mean_adult_ba_trans, stan_yearly_recruits_per_capita_trans)]
  stan.recruitment.weights = all.stan.estimates$recruitment[,.(CTFS_plot, sp, stan_weights_for_nr_recruits, stan_weights_for_nr_recruits)]
  names(stan.recruitment.estimates) = c("CTFS_plot", "sp", "recruitment_per_ba", "recruitment_per_capita")
  names(stan.recruitment.weights) = c("CTFS_plot", "sp", "weights_recruitment_per_ba", "weights_recruitment_per_capita")
  
  stan.survival.estimates = dcast(all.stan.estimates$survival,  sp + CTFS_plot ~ crown_layer,  value.var = "stan_mean_surv_rate_trans")
  stan.survival.weights = dcast(all.stan.estimates$survival,  sp + CTFS_plot~ crown_layer,  value.var = "stan_weight_surv_rate")
  names(stan.survival.estimates) = c("sp", "CTFS_plot", paste("survival_layer", 1 : max(all.stan.estimates$survival$crown_layer), sep = ""))
  names(stan.survival.weights) = c("sp", "CTFS_plot", paste("weights_survival_layer", 1 : max(all.stan.estimates$survival$crown_layer), sep = ""))
  
  stan.growth.estimates = dcast(all.stan.estimates$growth,  sp + CTFS_plot ~ crown_layer,  value.var = "stan_mean_growth_rate_trans")
  stan.growth.weights = dcast(all.stan.estimates$growth,  sp + CTFS_plot~ crown_layer,  value.var = "stan_weight_growth_rate")
  names(stan.growth.estimates) = c("sp", "CTFS_plot", paste("growth_layer", 1 : max(all.stan.estimates$growth$crown_layer), sep = ""))
  names(stan.growth.weights) = c("sp", "CTFS_plot", paste("weights_growth_layer", 1 : max(all.stan.estimates$growth$crown_layer), sep = ""))
  
  # put them all in one data.frame
  stan.estimates.compiled = merge(stan.recruitment.estimates, stan.survival.estimates, by = c("CTFS_plot","sp"))
  stan.estimates.compiled = merge(stan.estimates.compiled, stan.growth.estimates, by = c("CTFS_plot","sp"))
  stan.estimates.compiled$sp = as.character(stan.estimates.compiled$sp)
  
  stan.weights.compiled = merge(stan.recruitment.weights, stan.survival.weights, by = c("CTFS_plot", "sp"))
  stan.weights.compiled = merge(stan.weights.compiled, stan.growth.weights, by = c("CTFS_plot", "sp"))
  stan.weights.compiled$sp = as.character(stan.weights.compiled$sp)
  
  # additional infos
  plot.species.info = stan.estimates.compiled[,.(CTFS_plot, sp)]
  nr.of.crown.layers = length(grep(pattern = "growth", names(stan.estimates.compiled)))
  
  #####################################
  # Analyse demographic rates of x layers with a weighted PCA
  # Initial script by Benjamin Rosenbaum, Dr. Nadja Rueger (nadja.rueger@idiv.de)
  # restructured by Stephan Kambach (stephan.kambach@idiv.de)
  # see L. Delchambre. Weighted principal component analysis: a weighted covariance eigendecomposition approach. Mon. Not. R. Astron. Soc. 446(2), 3545-3555, 2014. 
  #   data.estimates: table with rows=observations, cols = variables
  #   data.weights: table with the corresponding weights
  
  # scaled by plot-level sds
  for(i in 1:nrow(CTFS.plots)){
    
    plot.temp = CTFS.plots$CTFS_plot[i]
    
    X = stan.estimates.compiled[CTFS_plot == plot.temp]
    W = stan.weights.compiled[CTFS_plot == plot.temp]
    
    X.save = X
    W.save = W
    
    columns = c(
      match(c("recruitment_per_ba","recruitment_per_capita"), names(stan.estimates.compiled)),
      match(paste("survival_layer", 1:nr.of.max.crown.layers, sep = ""), names(stan.estimates.compiled)),
      match(paste("growth_layer", 1:nr.of.max.crown.layers, sep = ""), names(stan.estimates.compiled)))
    
    columns = columns[!is.na(columns)]
    
    X = data.frame(X)[,c(columns)]
    W = data.frame(W)[,c(columns)]
    
    # remove crown layers with NA
    X = X[,!(is.na(colSums(X)))]
    W = W[,!(is.na(colSums(W)))]
    colnames.of.X = names(X)
    
    unweighted.pcas[[plot.temp]] = plot(PCA(X), choix = "var", title = plot.temp)
    
    # all weights = 1 for test purpose
    # W[,] = 1
    
    X <- t(as.matrix(X))
    W <- t(as.matrix(W))
    
    # replace zero weights
    W[W==0] <- 1.0e-6
    
    n <- as.numeric(ncol(X))
    d <- as.numeric(nrow(X))
    
    get.nr.crown.layers = 
    nr.of.crown.layers.on.this.plot = max(suppressWarnings(as.numeric(substr(colnames.of.X, 
                                                 start = nchar(colnames.of.X),
                                                 stop = nchar(colnames.of.X)))), na.rm = T)
    
    # substract weighted mean, save original data
    Xorig <- X
    
    # save sds and center for for back-transformation later
    centers = rowMeans(W*X)/rowMeans(W)
    sds.within.plot = centers
    names(sds.within.plot) = rownames(X)
    
    X <- X - rowMeans(W*X)/rowMeans(W) # X = centered and scaled within plots
    
    # scale by weighted sd in each dimension
    # X sds per plot
    for (j in 1:d){
      sds.within.plot[j] <- sqrt(sum(W[j, ]^2*X[j, ]^2)/sum(W[j, ]^2))
      X[j, ] <- X[j, ] / sds.within.plot[j]}
    
    # weighted covariance matrix S
    S <- (W*X)%*%t(W*X) / (W%*%t(W))
    
    #  eigenvalues (in decreasing order) and corresponding eigenvectors (in columns) 
    EV <- eigen(S)
    
    #----------------------------------------------------------------
    # step 2: loop through ranks to compute true explained variance
    #----------------------------------------------------------------
    Chi2 <- vector(length=d) # explained variance
    
    for(rank in 1:nrow(X)){
      
      # matrix of principal components (new basis)
      P <- EV$vectors[, 1:rank]
      
      # matrix of coefficients (coordinates in new basis)
      C <- matrix(data=NA, nrow=rank, ncol=n)
      
      for (j in 1:n){
        w <- diag(W[, j]^2)
        
        C[, j] <- solve(t(P)%*%w%*%P, t(P)%*%w%*%X[, j])}
      
      # matrix of projections of old data (PC approx X)
      PC <- P%*%C
      
      # explained variance
      Chi2[rank] <- sum(sum((W*PC)^2)) / sum(sum((W*X)^2))}
    
    # subtract expl variances to get the individual CHi2 of every principal component
    for(rank in 2:nrow(X)){
      Chi2[rank] = Chi2[rank] - sum(Chi2[1:(rank-1)])}
    
    #----------------------------------------------------------------
    # step 3: decide for a rank based on the explained variance 
    #         and compute data for output (same steps as above)
    #----------------------------------------------------------------
    rank <- nrow(X)
    
    P <- EV$vectors[, 1:rank]
    
    C <- matrix(data=NA, nrow=rank, ncol=n)
    
    for (j in 1:n){
      w <- diag(W[,j]^2) 
      C[, j] <- solve(t(P)%*%w%*%P, t(P)%*%w%*%X[, j])}
    
    PC <- P%*%C
    
    
    # re-order the principal components from largest to smallest explained variance
    P = P[,order(Chi2, decreasing = T)]
    C = C[order(Chi2, decreasing = T),]
    Chi2 = Chi2[order(Chi2, decreasing = T)]
    
    # Output is a list with 6 entries:
    #   chi2: explained variance
    #   principal_components:    matrix of principal components (new basis)
    #   pca_coordinates:    matrix of coefficients (coordinates in new basis)
    #   projections_old_data:   matrix of projections of old data (PC approx X)
    #   centers
    #   sds
    
    # re-arrange data for easier later analysis
    names(Chi2) = paste("PCA", 1:length(Chi2), sep = "")
    
    P = data.frame(colnames.of.X, P)
    names(P) = c("demographic_rate", paste("PCA", 1:length(Chi2), sep = ""))
    
    C = data.frame(plot.species.info[CTFS_plot == plot.temp]$sp, t(C))
    names(C) = c("sp", paste("PCA", 1:length(Chi2), sep = ""))
    
    # calculate demographic centroids (i.e. mean axis loadings)
    centroid.recruitment.per.adult.ba = c(P$PCA1[1], P$PCA2[1])
    centroid.recruitment.per.capita = c(P$PCA1[2], P$PCA2[2])
    
    centroid.survival = c(mean(P$PCA1[grep(pattern = "survival", x = P$demographic_rate)]), mean(P$PCA2[grep(pattern = "survival", x = P$demographic_rate)]))
    centroid.growth = c(mean(P$PCA1[grep(pattern = "growth", x = P$demographic_rate)]), mean(P$PCA2[grep(pattern = "growth", x = P$demographic_rate)]))

    centroids = rbind(centroid.recruitment.per.adult.ba,
                       centroid.recruitment.per.capita,
                       centroid.survival,
                       centroid.growth)
    
    centroids = data.frame(c("recruitment_per_adult_ba", "recruitment_per_capita", "survival", "growth"), centroids)
    names(centroids) = c("demography", "PCA1", "PCA2")
    rownames(centroids) = NULL
    
    results[[plot.temp]] = 
        list("expl_var" = Chi2,
             "factor_loadings" = P,
             "pca_coordinates" = C,
             "demography_centroids" = centroids,
             "centers" = centers,
             "sds" = sds.within.plot,
             "raw_data" = X.save,
             "raw_weights" = W.save)
    
      results[[paste(plot.temp, sep = "")]]$species = 
        as.character(plot.species.info[CTFS_plot == plot.temp,]$sp)
  }
  
  # pdf("results/per plot_PPA/all_unweighted_pcas_2_layers.pdf")
  # for(i in 1:length(unweighted.pcas)){
  #   plot(unweighted.pcas[[i]])}
  # dev.off()
  
  return(results)
}

###############################################################
calculate.wPCAs.two.layers = function(CTFS.plots, all.stan.estimates, nr.of.max.crown.layers, per.plot.or.per.census.interval, top.layer, bottom.layer){
  
  results = list()
  #unweighted.pcas = list()
  
  # only select the second highest and second lowest layer
  for(i in 1:nrow(CTFS.plots)){
    
    plot.temp = CTFS.plots$CTFS_plot[i]
    growth.temp = all.stan.estimates$growth[CTFS_plot == plot.temp]
    survival.temp = all.stan.estimates$survival[CTFS_plot == plot.temp]
    recruitment.temp = all.stan.estimates$recruitment[CTFS_plot == plot.temp]
    
    # determine top and bottom layers
    if(bottom.layer == "second lowest"){
      bottom.layer = max(growth.temp$crown_layer)-1}
    if(bottom.layer == "lowest"){
      bottom.layer = max(growth.temp$crown_layer)}
    if(bottom.layer == "middle"){
      bottom.layer = floor(median(growth.temp$crown_layer))+1}
    if(top.layer == "middle"){
      top.layer = floor(median(growth.temp$crown_layer))}
    
    
    growth.top = growth.temp[crown_layer == top.layer, .(sp, growth_layer1 = stan_mean_growth_rate_trans)]
    growth.weight.top = growth.temp[crown_layer == top.layer, .(sp, weight_growth_layer1 = stan_weight_growth_rate)]
    growth.bottom = growth.temp[crown_layer == bottom.layer, .(sp, growth_layer2 = stan_mean_growth_rate_trans)]
    growth.weight.bottom = growth.temp[crown_layer == bottom.layer, .(sp, weight_growth_layer2 = stan_weight_growth_rate)]
    
    survival.top = survival.temp[crown_layer == top.layer, .(sp, survival_layer1 = stan_mean_surv_rate_trans)]
    survival.weight.top = survival.temp[crown_layer == top.layer, .(sp, weight_survival_layer1 = stan_weight_surv_rate)]
    survival.bottom = survival.temp[crown_layer == bottom.layer, .(sp, survival_layer2 = stan_mean_surv_rate_trans)]
    survival.weight.bottom = survival.temp[crown_layer == bottom.layer, .(sp, weight_survival_layer2= stan_weight_surv_rate)]
    
    recruitment.estimates.temp = recruitment.temp[,.(sp, recruitment_per_ba = stan_yearly_recruits_per_mean_adult_ba_trans,
                                                     recruitment_per_capita = stan_yearly_recruits_per_capita_trans)]
    recruitment.weight.temp = recruitment.temp[,.(sp, 
                                                  weigth_recruitment_per_ba = stan_weights_for_nr_recruits,
                                                  weight_recruitment_per_capita = stan_weights_for_nr_recruits)]
    
    # put all together
    estimates.all.temp = merge(recruitment.estimates.temp, growth.top, by = "sp")
    estimates.all.temp = merge(estimates.all.temp, growth.bottom, by = "sp")
    estimates.all.temp = merge(estimates.all.temp, survival.top, by = "sp")
    estimates.all.temp = merge(estimates.all.temp, survival.bottom, by = "sp")
    
    weight.all.temp = merge(recruitment.weight.temp, growth.weight.top, by = "sp")
    weight.all.temp = merge(weight.all.temp, growth.weight.bottom, by = "sp")
    weight.all.temp = merge(weight.all.temp, survival.weight.top, by = "sp")
    weight.all.temp = merge(weight.all.temp, survival.weight.bottom, by = "sp")
    
    estimates.all.temp$CTFS_plot = plot.temp
    weight.all.temp$CTFS_plot = plot.temp
    
    if(i == 1){
      all.estimates = estimates.all.temp
      all.weights = weight.all.temp
    }else{
      all.estimates  = rbind(all.estimates, estimates.all.temp)
      all.weights  = rbind(all.weights, weight.all.temp)}}
  
  # additional infos
  plot.species.info = all.estimates[,.(CTFS_plot, sp)]
  nr.of.crown.layers = 2
  
  #####################################
  # Analyse demographic rates of x layers with a weighted PCA
  # Initial script by Benjamin Rosenbaum, Dr. Nadja Rueger (nadja.rueger@idiv.de)
  # restructured by Stephan Kambach (stephan.kambach@idiv.de)
  # see L. Delchambre. Weighted principal component analysis: a weighted covariance eigendecomposition approach. Mon. Not. R. Astron. Soc. 446(2), 3545-3555, 2014. 
  #   data.estimates: table with rows=observations, cols = variables
  #   data.weights: table with the corresponding weights
  
  # scaled by plot-level sds
  for(i in 1:nrow(CTFS.plots)){
    
    plot.temp = CTFS.plots$CTFS_plot[i]
    census.interval.temp = CTFS.plots$census_interval[i]
    
    X = all.estimates[CTFS_plot == plot.temp]
    W = all.weights[CTFS_plot == plot.temp]
    
    X.save = X
    W.save = W
    
    X$sp = NULL; X$CTFS_plot = NULL; X = data.frame(X)
    W$sp = NULL; W$CTFS_plot = NULL; W = data.frame(W)
    
    colnames.of.X = names(X)
    # unweighted analysis for tests
    #PCA(X)
    
    #W[,] = 1
    
    X <- t(as.matrix(X))
    W <- t(as.matrix(W))
    
    # replace zero weights
    W[W==0] <- 1.0e-6
    
    n <- as.numeric(ncol(X))
    d <- as.numeric(nrow(X))
    
    # substract weighted mean, save original data
    Xorig <- X
    
    # save sds and center for for back-transformation later
    centers = rowMeans(W*X)/rowMeans(W)
    sds.within.plot = centers
    names(sds.within.plot) = rownames(X)
    
    X <- X - rowMeans(W*X)/rowMeans(W) # X = centered and scaled within plots
    
    # scale by weighted sd in each dimension
    # X sds per plot
    for (j in 1:d){
      sds.within.plot[j] <- sqrt(sum(W[j, ]^2*X[j, ]^2)/sum(W[j, ]^2))
      X[j, ] <- X[j, ] / sds.within.plot[j]}
    
    # weighted covariance matrix S
    S <- (W*X)%*%t(W*X) / (W%*%t(W))
    
    #  eigenvalues (in decreasing order) and corresponding eigenvectors (in columns) 
    EV <- eigen(S)
    
    #----------------------------------------------------------------
    # step 2: loop through ranks to compute true explained variance
    #----------------------------------------------------------------
    Chi2 <- vector(length=d) # explained variance
    
    for(rank in 1:nrow(X)){
      
      # matrix of principal components (new basis)
      P <- EV$vectors[, 1:rank]
      
      # matrix of coefficients (coordinates in new basis)
      C <- matrix(data=NA, nrow=rank, ncol=n)
      
      for (j in 1:n){
        w <- diag(W[, j]^2)
        
        C[, j] <- solve(t(P)%*%w%*%P, t(P)%*%w%*%X[, j])}
      
      # matrix of projections of old data (PC approx X)
      PC <- P%*%C
      
      # explained variance
      Chi2[rank] <- sum(sum((W*PC)^2)) / sum(sum((W*X)^2))}
    
    # subtract expl variances to get the individual CHi2 of every principal component
    for(rank in 2:nrow(X)){
      Chi2[rank] = Chi2[rank] - sum(Chi2[1:(rank-1)])}
    
    #----------------------------------------------------------------
    # step 3: decide for a rank based on the explained variance 
    #         and compute data for output (same steps as above)
    #----------------------------------------------------------------
    rank <- nrow(X)
    
    P <- EV$vectors[, 1:rank]
    
    C <- matrix(data=NA, nrow=rank, ncol=n)
    
    for (j in 1:n){
      w <- diag(W[,j]^2) 
      C[, j] <- solve(t(P)%*%w%*%P, t(P)%*%w%*%X[, j])}
    
    PC <- P%*%C
    
    
    # re-order the principal components from largest to smallest explained variance
    P = P[,order(Chi2, decreasing = T)]
    C = C[order(Chi2, decreasing = T),]
    Chi2 = Chi2[order(Chi2, decreasing = T)]
    
    # Output is a list with 6 entries:
    #   chi2: explained variance
    #   principal_components:    matrix of principal components (new basis)
    #   pca_coordinates:    matrix of coefficients (coordinates in new basis)
    #   projections_old_data:   matrix of projections of old data (PC approx X)
    #   centers
    #   sds
    
    # re-arrange data for easier later analysis
    names(Chi2) = paste("PCA", 1:length(Chi2), sep = "")
    
    P = data.frame(colnames.of.X, P)
    names(P) = c("demographic_rate", paste("PCA", 1:length(Chi2), sep = ""))
    
    C = data.frame(plot.species.info[CTFS_plot == plot.temp]$sp, t(C))
    names(C) = c("sp", paste("PCA", 1:length(Chi2), sep = ""))
    
    # calculate demographic centroids (i.e. mean axis loadings)
    centroid.recruitment.per.adult.ba = c(P$PCA1[1], P$PCA2[1])
    centroid.recruitment.per.capita = c(P$PCA1[2], P$PCA2[2])
    
    centroid.growth = c(mean(P$PCA1[grep(pattern = "growth", x = P$demographic_rate)]), mean(P$PCA2[grep(pattern = "growth", x = P$demographic_rate)]))
    
    centroid.survival = c(mean(P$PCA1[grep(pattern = "survival", x = P$demographic_rate)]), mean(P$PCA2[grep(pattern = "survival", x = P$demographic_rate)]))
    
    centroids = rbind(centroid.recruitment.per.adult.ba,
                      centroid.recruitment.per.capita,
                      centroid.survival,
                      centroid.growth)
    
    centroids = data.frame(c("recruitment_per_adult_ba", "recruitment_per_capita", "survival", "growth"), centroids)
    names(centroids) = c("demography", "PCA1", "PCA2")
    rownames(centroids) = NULL
    
    results[[plot.temp]] = 
      list("expl_var" = Chi2,
           "factor_loadings" = P,
           "pca_coordinates" = C,
           "demography_centroids" = centroids,
           "centers" = centers,
           "sds" = sds.within.plot,
           "raw_data" = X.save,
           "raw_weights" = W.save)
    
    results[[paste(plot.temp, census.interval.temp, sep = "")]]$species = 
      as.character(plot.species.info[CTFS_plot == plot.temp,]$sp)
  }
  
  #pdf("results/per plot_PPA/PCA_2 layers/all_unweighted_pcas_2_layers.pdf")
  #for(i in 1:length(unweighted.pcas)){
  #  plot(unweighted.pcas[[i]])}
  #dev.off()
  
  return(results)
}

###############################################################
calculate.wPCAs.with.max.dbh = function(CTFS.plots, all.stan.estimates, nr.of.max.crown.layers, per.plot.or.per.census.interval, top.layer, bottom.layer,max.dbh){
  
  results = list()
  #unweighted.pcas = list()
  
  stan.recruitment.estimates = all.stan.estimates$recruitment[,.(CTFS_plot, sp, stan_yearly_recruits_per_mean_adult_ba_trans, stan_yearly_recruits_per_capita_trans)]
  stan.recruitment.weights = all.stan.estimates$recruitment[,.(CTFS_plot, sp, stan_weights_for_nr_recruits, stan_weights_for_nr_recruits)]
  names(stan.recruitment.estimates) = c("CTFS_plot", "sp", "recruitment_per_ba", "recruitment_per_capita")
  names(stan.recruitment.weights) = c("CTFS_plot", "sp", "weights_recruitment_per_ba", "weights_recruitment_per_capita")
  
  stan.survival.estimates = dcast(all.stan.estimates$survival,  sp + CTFS_plot ~ crown_layer,  value.var = "stan_mean_surv_rate_trans")
  stan.survival.weights = dcast(all.stan.estimates$survival,  sp + CTFS_plot~ crown_layer,  value.var = "stan_weight_surv_rate")
  names(stan.survival.estimates) = c("sp", "CTFS_plot", paste("survival_layer", 1 : max(all.stan.estimates$survival$crown_layer), sep = ""))
  names(stan.survival.weights) = c("sp", "CTFS_plot", paste("weights_survival_layer", 1 : max(all.stan.estimates$survival$crown_layer), sep = ""))
  
  stan.growth.estimates = dcast(all.stan.estimates$growth,  sp + CTFS_plot ~ crown_layer,  value.var = "stan_mean_growth_rate_trans")
  stan.growth.weights = dcast(all.stan.estimates$growth,  sp + CTFS_plot~ crown_layer,  value.var = "stan_weight_growth_rate")
  names(stan.growth.estimates) = c("sp", "CTFS_plot", paste("growth_layer", 1 : max(all.stan.estimates$growth$crown_layer), sep = ""))
  names(stan.growth.weights) = c("sp", "CTFS_plot", paste("weights_growth_layer", 1 : max(all.stan.estimates$growth$crown_layer), sep = ""))
  
  # put them all in one data.frame
  stan.estimates.compiled = merge(stan.recruitment.estimates, stan.survival.estimates, by = c("CTFS_plot","sp"))
  stan.estimates.compiled = merge(stan.estimates.compiled, stan.growth.estimates, by = c("CTFS_plot","sp"))
  stan.estimates.compiled$sp = as.character(stan.estimates.compiled$sp)
  
  stan.weights.compiled = merge(stan.recruitment.weights, stan.survival.weights, by = c("CTFS_plot", "sp"))
  stan.weights.compiled = merge(stan.weights.compiled, stan.growth.weights, by = c("CTFS_plot", "sp"))
  stan.weights.compiled$sp = as.character(stan.weights.compiled$sp)
  
  # add max_dbh
  stan.estimates.compiled = merge(stan.estimates.compiled, max.dbh[,.(CTFS_plot, sp, max_dbh)], by = c("CTFS_plot","sp"))
  stan.weights.compiled = merge(stan.weights.compiled, max.dbh[,.(CTFS_plot, sp, max_dbh_weight)], by = c("CTFS_plot","sp"))
  
  # additional infos
  plot.species.info = stan.estimates.compiled[,.(CTFS_plot, sp)]
  nr.of.crown.layers = length(grep(pattern = "growth", names(stan.estimates.compiled)))
  
  #####################################
  # Analyse demographic rates of x layers with a weighted PCA
  # Initial script by Benjamin Rosenbaum, Dr. Nadja Rueger (nadja.rueger@idiv.de)
  # restructured by Stephan Kambach (stephan.kambach@idiv.de)
  # see L. Delchambre. Weighted principal component analysis: a weighted covariance eigendecomposition approach. Mon. Not. R. Astron. Soc. 446(2), 3545-3555, 2014. 
  #   data.estimates: table with rows=observations, cols = variables
  #   data.weights: table with the corresponding weights
  
  # scaled by plot-level sds
  for(i in 1:nrow(CTFS.plots)){
    
    plot.temp = CTFS.plots$CTFS_plot[i]
    
    X = stan.estimates.compiled[CTFS_plot == plot.temp]
    W = stan.weights.compiled[CTFS_plot == plot.temp]
    
    X.save = X
    W.save = W
    
    columns = c(
      match(c("max_dbh"), names(stan.estimates.compiled)),
      match(c("recruitment_per_ba","recruitment_per_capita"), names(stan.estimates.compiled)),
      match(paste("survival_layer", 1:nr.of.max.crown.layers, sep = ""), names(stan.estimates.compiled)),
      match(paste("growth_layer", 1:nr.of.max.crown.layers, sep = ""), names(stan.estimates.compiled)))
    
    columns = columns[!is.na(columns)]
    
    X = data.frame(X)[,c(columns)]
    W = data.frame(W)[,c(columns)]
    
    # remove crown layers with NA
    X = X[,!(is.na(colSums(X)))]
    W = W[,!(is.na(colSums(W)))]
    colnames.of.X = names(X)
    
    PCA(X)
    
    # all weights = 1 for test purpose
    # W[,] = 1
    
    X <- t(as.matrix(X))
    W <- t(as.matrix(W))
    
    # replace zero weights
    W[W==0] <- 1.0e-6
    
    n <- as.numeric(ncol(X))
    d <- as.numeric(nrow(X))
    
    get.nr.crown.layers = 
      nr.of.crown.layers.on.this.plot = max(suppressWarnings(as.numeric(substr(colnames.of.X, 
                                                                               start = nchar(colnames.of.X),
                                                                               stop = nchar(colnames.of.X)))), na.rm = T)
    
    # substract weighted mean, save original data
    Xorig <- X
    
    # save sds and center for for back-transformation later
    centers = rowMeans(W*X)/rowMeans(W)
    sds.within.plot = centers
    names(sds.within.plot) = rownames(X)
    
    X <- X - rowMeans(W*X)/rowMeans(W) # X = centered and scaled within plots
    
    # scale by weighted sd in each dimension
    # X sds per plot
    for (j in 1:d){
      sds.within.plot[j] <- sqrt(sum(W[j, ]^2*X[j, ]^2)/sum(W[j, ]^2))
      X[j, ] <- X[j, ] / sds.within.plot[j]}
    
    # weighted covariance matrix S
    S <- (W*X)%*%t(W*X) / (W%*%t(W))
    
    #  eigenvalues (in decreasing order) and corresponding eigenvectors (in columns) 
    EV <- eigen(S)
    
    #----------------------------------------------------------------
    # step 2: loop through ranks to compute true explained variance
    #----------------------------------------------------------------
    Chi2 <- vector(length=d) # explained variance
    
    for(rank in 1:nrow(X)){
      
      # matrix of principal components (new basis)
      P <- EV$vectors[, 1:rank]
      
      # matrix of coefficients (coordinates in new basis)
      C <- matrix(data=NA, nrow=rank, ncol=n)
      
      for (j in 1:n){
        w <- diag(W[, j]^2)
        
        C[, j] <- solve(t(P)%*%w%*%P, t(P)%*%w%*%X[, j])}
      
      # matrix of projections of old data (PC approx X)
      PC <- P%*%C
      
      # explained variance
      Chi2[rank] <- sum(sum((W*PC)^2)) / sum(sum((W*X)^2))}
    
    # subtract expl variances to get the individual CHi2 of every principal component
    for(rank in 2:nrow(X)){
      Chi2[rank] = Chi2[rank] - sum(Chi2[1:(rank-1)])}
    
    #----------------------------------------------------------------
    # step 3: decide for a rank based on the explained variance 
    #         and compute data for output (same steps as above)
    #----------------------------------------------------------------
    rank <- nrow(X)
    
    P <- EV$vectors[, 1:rank]
    
    C <- matrix(data=NA, nrow=rank, ncol=n)
    
    for (j in 1:n){
      w <- diag(W[,j]^2) 
      C[, j] <- solve(t(P)%*%w%*%P, t(P)%*%w%*%X[, j])}
    
    PC <- P%*%C
    
    
    # re-order the principal components from largest to smallest explained variance
    P = P[,order(Chi2, decreasing = T)]
    C = C[order(Chi2, decreasing = T),]
    Chi2 = Chi2[order(Chi2, decreasing = T)]
    
    # Output is a list with 6 entries:
    #   chi2: explained variance
    #   principal_components:    matrix of principal components (new basis)
    #   pca_coordinates:    matrix of coefficients (coordinates in new basis)
    #   projections_old_data:   matrix of projections of old data (PC approx X)
    #   centers
    #   sds
    
    # re-arrange data for easier later analysis
    names(Chi2) = paste("PCA", 1:length(Chi2), sep = "")
    
    P = data.frame(colnames.of.X, P)
    names(P) = c("demographic_rate", paste("PCA", 1:length(Chi2), sep = ""))
    
    C = data.frame(plot.species.info[CTFS_plot == plot.temp]$sp, t(C))
    names(C) = c("sp", paste("PCA", 1:length(Chi2), sep = ""))
    
    # calculate demographic centroids (i.e. mean axis loadings)
    centroid.max.dbh = c(P$PCA1[1], P$PCA2[1])
    centroid.recruitment.per.adult.ba = c(P$PCA1[2], P$PCA2[2])
    centroid.recruitment.per.capita = c(P$PCA1[3], P$PCA2[3])
    
    centroid.survival = c(mean(P$PCA1[grep(pattern = "survival", x = P$demographic_rate)]), mean(P$PCA2[grep(pattern = "survival", x = P$demographic_rate)]))
    centroid.growth = c(mean(P$PCA1[grep(pattern = "growth", x = P$demographic_rate)]), mean(P$PCA2[grep(pattern = "growth", x = P$demographic_rate)]))
    
    centroids = rbind(centroid.max.dbh,
                      centroid.recruitment.per.adult.ba,
                      centroid.recruitment.per.capita,
                      centroid.survival,
                      centroid.growth)
    
    centroids = data.frame(c("max_dhb","recruitment_per_adult_ba", "recruitment_per_capita", "survival", "growth"), centroids)
    names(centroids) = c("demography", "PCA1", "PCA2")
    rownames(centroids) = NULL
    
    results[[plot.temp]] = 
      list("expl_var" = Chi2,
           "factor_loadings" = P,
           "pca_coordinates" = C,
           "demography_centroids" = centroids,
           "centers" = centers,
           "sds" = sds.within.plot,
           "raw_data" = X.save,
           "raw_weights" = W.save)
    
    results[[paste(plot.temp, sep = "")]]$species = 
      as.character(plot.species.info[CTFS_plot == plot.temp,]$sp)
  }
  
  # pdf("results/per plot_PPA/all_unweighted_pcas_2_layers.pdf")
  # for(i in 1:length(unweighted.pcas)){
  #   plot(unweighted.pcas[[i]])}
  # dev.off()
  
  return(results)
}

###############################################################
calculate.wPCAs.no.rec.ba = function(CTFS.plots, all.stan.estimates, nr.of.max.crown.layers, per.plot.or.per.census.interval){
  
  results = list()
  
  stan.survival.estimates = dcast(all.stan.estimates$survival,  sp + CTFS_plot ~ crown_layer,  value.var = "stan_mean_surv_rate_trans")
  stan.survival.weights = dcast(all.stan.estimates$survival,  sp + CTFS_plot~ crown_layer,  value.var = "stan_weight_surv_rate")
  names(stan.survival.estimates) = c("sp", "CTFS_plot", paste("survival_layer", 1 : max(all.stan.estimates$survival$crown_layer), sep = ""))
  names(stan.survival.weights) = c("sp", "CTFS_plot", paste("weights_survival_layer", 1 : max(all.stan.estimates$survival$crown_layer), sep = ""))
  
  stan.growth.estimates = dcast(all.stan.estimates$growth,  sp + CTFS_plot ~ crown_layer,  value.var = "stan_mean_growth_rate_trans")
  stan.growth.weights = dcast(all.stan.estimates$growth,  sp + CTFS_plot~ crown_layer,  value.var = "stan_weight_growth_rate")
  names(stan.growth.estimates) = c("sp", "CTFS_plot", paste("growth_layer", 1 : max(all.stan.estimates$growth$crown_layer), sep = ""))
  names(stan.growth.weights) = c("sp", "CTFS_plot", paste("weights_growth_layer", 1 : max(all.stan.estimates$growth$crown_layer), sep = ""))
  
  # put them all in one data.frame
  stan.estimates.compiled = merge(stan.survival.estimates, stan.growth.estimates, by = c("CTFS_plot","sp"))
  stan.estimates.compiled$sp = as.character(stan.estimates.compiled$sp)
  
  stan.weights.compiled = merge(stan.survival.weights, stan.growth.weights, by = c("CTFS_plot", "sp"))
  stan.weights.compiled$sp = as.character(stan.weights.compiled$sp)
  
  # additional infos
  plot.species.info = stan.estimates.compiled[,.(CTFS_plot, sp)]
  
  nr.of.crown.layers = length(grep(pattern = "growth", names(stan.estimates.compiled)))
  
  #####################################
  # Analyse demographic rates of x layers with a weighted PCA
  # Initial script by Benjamin Rosenbaum, Dr. Nadja Rueger (nadja.rueger@idiv.de)
  # restructured by Stephan Kambach (stephan.kambach@idiv.de)
  # see L. Delchambre. Weighted principal component analysis: a weighted covariance eigendecomposition approach. Mon. Not. R. Astron. Soc. 446(2), 3545-3555, 2014. 
  #   data.estimates: table with rows=observations, cols = variables
  #   data.weights: table with the corresponding weights
  
  # scaled by plot-level sds
  for(i in 1:nrow(CTFS.plots)){
    
    plot.temp = CTFS.plots$CTFS_plot[i]
    census.interval.temp = CTFS.plots$census_interval[i]
    
    X = stan.estimates.compiled[CTFS_plot == plot.temp]
    W = stan.weights.compiled[CTFS_plot == plot.temp]
    
    X.save = X
    W.save = W
    
    columns = c(
      match(paste("survival_layer", 1:nr.of.max.crown.layers, sep = ""), names(stan.estimates.compiled)),
      match(paste("growth_layer", 1:nr.of.max.crown.layers, sep = ""), names(stan.estimates.compiled)))
    
    # in case that not all layers should be analysed
    columns = columns[!is.na(columns)]
    
    X = data.frame(X)[,c(columns)]
    W = data.frame(W)[,c(columns)]
    
    # remove crown layers with NA
    X = X[,!(is.na(colSums(X)))]
    W = W[,!(is.na(colSums(W)))]
    colnames.of.X = names(X)
    
    X <- t(as.matrix(X))
    W <- t(as.matrix(W))
    
    # replace zero weights
    W[W==0] <- 1.0e-6
    
    n <- as.numeric(ncol(X))
    d <- as.numeric(nrow(X))
    
    get.nr.crown.layers = 
      nr.of.crown.layers.on.this.plot = max(suppressWarnings(as.numeric(substr(colnames.of.X, 
                                                                               start = nchar(colnames.of.X),
                                                                               stop = nchar(colnames.of.X)))), na.rm = T)
    
    # substract weighted mean, save original data
    Xorig <- X
    
    # save sds and center for for back-transformation later
    centers = rowMeans(W*X)/rowMeans(W)
    sds.within.plot = centers
    names(sds.within.plot) = rownames(X)
    
    X <- X - rowMeans(W*X)/rowMeans(W) # X = centered and scaled within plots
    
    # scale by weighted sd in each dimension
    # X sds per plot
    for (j in 1:d){
      sds.within.plot[j] <- sqrt(sum(W[j, ]^2*X[j, ]^2)/sum(W[j, ]^2))
      X[j, ] <- X[j, ] / sds.within.plot[j]}
    
    # weighted covariance matrix S
    S <- (W*X)%*%t(W*X) / (W%*%t(W))
    
    #  eigenvalues (in decreasing order) and corresponding eigenvectors (in columns) 
    EV <- eigen(S)
    
    #----------------------------------------------------------------
    # step 2: loop through ranks to compute true explained variance
    #----------------------------------------------------------------
    Chi2 <- vector(length=d) # explained variance
    
    for(rank in 1:nrow(X)){
      
      # matrix of principal components (new basis)
      P <- EV$vectors[, 1:rank]
      
      # matrix of coefficients (coordinates in new basis)
      C <- matrix(data=NA, nrow=rank, ncol=n)
      
      for (j in 1:n){
        w <- diag(W[, j]^2)
        
        C[, j] <- solve(t(P)%*%w%*%P, t(P)%*%w%*%X[, j])}
      
      # matrix of projections of old data (PC approx X)
      PC <- P%*%C
      
      # explained variance
      Chi2[rank] <- sum(sum((W*PC)^2)) / sum(sum((W*X)^2))}
    
    # subtract expl variances to get the individual CHi2 of every principal component
    for(rank in 2:nrow(X)){
      Chi2[rank] = Chi2[rank] - sum(Chi2[1:(rank-1)])}
    
    #----------------------------------------------------------------
    # step 3: decide for a rank based on the explained variance 
    #         and compute data for output (same steps as above)
    #----------------------------------------------------------------
    rank <- nrow(X)
    
    P <- EV$vectors[, 1:rank]
    
    C <- matrix(data=NA, nrow=rank, ncol=n)
    
    for (j in 1:n){
      w <- diag(W[,j]^2) 
      C[, j] <- solve(t(P)%*%w%*%P, t(P)%*%w%*%X[, j])}
    
    PC <- P%*%C
    
    # re-order the principal components from largest to smallest explained variance
    P = P[,order(Chi2, decreasing = T)]
    C = C[order(Chi2, decreasing = T),]
    Chi2 = Chi2[order(Chi2, decreasing = T)]
    
    # Output is a list with 6 entries:
    #   chi2: explained variance
    #   principal_components:    matrix of principal components (new basis)
    #   pca_coordinates:    matrix of coefficients (coordinates in new basis)
    #   projections_old_data:   matrix of projections of old data (PC approx X)
    #   centers
    #   sds
    
    # re-arrange data for easier later analysis
    names(Chi2) = paste("PCA", 1:length(Chi2), sep = "")
    
    P = data.frame(colnames.of.X, P)
    names(P) = c("demographic_rate", paste("PCA", 1:length(Chi2), sep = ""))
    
    C = data.frame(plot.species.info[CTFS_plot == plot.temp]$sp, t(C))
    names(C) = c("sp", paste("PCA", 1:length(Chi2), sep = ""))
    
    # calculate demographic centroids (i.e. mean axis loadings)
    centroid.survival = c(mean(P$PCA1[grep(pattern = "survival", x = P$demographic_rate)]), mean(P$PCA2[grep(pattern = "survival", x = P$demographic_rate)]))
    centroid.growth = c(mean(P$PCA1[grep(pattern = "growth", x = P$demographic_rate)]), mean(P$PCA2[grep(pattern = "growth", x = P$demographic_rate)]))
    
    centroids = rbind(centroid.survival,
                      centroid.growth)
    
    centroids = data.frame(c("survival", "growth"), centroids)
    names(centroids) = c("demography", "PCA1", "PCA2")
    rownames(centroids) = NULL
    
    results[[plot.temp]] = 
        list("expl_var" = Chi2,
             "factor_loadings" = P,
             "pca_coordinates" = C,
             "demography_centroids" = centroids,
             "centers" = centers,
             "sds" = sds.within.plot,
             "raw_data" = X.save,
             "raw_weights" = W.save)
    
    results[[paste(plot.temp, census.interval.temp, sep = "")]]$species = 
        as.character(plot.species.info[CTFS_plot == plot.temp,]$sp)
    
  }
  return(results)
}

###########################################################
# create pca plots, with and without species names
plot.pcas.and.return.explained.variation = function(CTFS.plots, 
                                                    pcas.to.plot,
                                                    axes.switches,
                                                    output.folder, 
                                                    plot.pcas.with.species.names){
  
  
  # create empty lists to store the plots
  pca.plot.list = list()
  pca.plot.with.species.names.list.all.layers = list()
  colors = list()
  
  explained.variation = data.frame("CTFS_plot" = NA, "census_interval" = NA,  "species_number" = NA,
                                   "PCA1_var_expl" = NA, "PCA1_broken_stick" = NA, "PCA1_significant" = NA ,
                                   "PCA2_var_expl" = NA, "PCA2_broken_stick" = NA, "PCA2_significant" = NA,
                                   "sum_expl_variation" = NA, "significant_axes" = NA)[0,]
  
  for(i in 1:nrow(CTFS.plots)){
    
    plot.temp = CTFS.plots$CTFS_plot[i]
    census.interval.temp = CTFS.plots$census_interval[i]
    
    # load data
    pca = pcas.to.plot[[i]]
    nr.of.species  = length(unique(pca$species))
    
    # get the plotable growth and survival layers
    survival.layers = names(pca$sds)[grep(pattern = "survival", x = names(pca$sds))]
    survival.layers = as.numeric(substr(survival.layers, nchar(survival.layers),nchar(survival.layers)))
    
    growth.layers = names(pca$sds)[grep(pattern = "growth", x = names(pca$sds))]
    growth.layers = as.numeric(substr(growth.layers, nchar(growth.layers),nchar(growth.layers)))
    
    # re-arrange factor levels for plotting
    pca$factor_loadings$demographic_rate = factor(as.character(pca$factor_loadings$demographic_rate),
                                                  levels = as.character(pca$factor_loadings$demographic_rate))
    
    # assign colors for the factor loadings    
    colors[[plot.temp]] =  
      c("black",
        '#348757',"#A7F146",
        c('#153251','#435A79','#0b6794','#4a89b8','#CCDAF7','#A1B9C9','#D9E3E9'),
        c('#680000','#a32020','#e0301e','#dc6900','#eb8c00','#F3BA66','#FBEAD1'))
    names(colors[[plot.temp]]) = c("max_dbh", "recruitment_per_ba","recruitment_per_capita",
                                   paste("survival_layer",1:7, sep = ""),
                                   paste("growth_layer",1:7, sep = ""))
    colors[[plot.temp]] = colors[[plot.temp]][names(colors[[plot.temp]]) %in% as.character(pca$factor_loadings$demographic_rate)]
    
    # switch PCA-axes where necessary
    pca$factor_loadings[which(names(pca$factor_loadings) %in% axes.switches[[i]])] = 0 - pca$factor_loadings[which(names(pca$factor_loadings) %in% axes.switches[[i]])]
    
    # determine significance of explained variation of PCA1 and PCA2
    explained.variation = rbind(explained.variation,
                data.frame("CTFS_plot" = plot.temp,
                           "census_interval" = ifelse(is.null(census.interval.temp), NA, census.interval.temp),
                           "species_number" = nr.of.species,
                           "PCA1_var_expl" = pca$expl_var[1],
                           "PCA1_broken_stick" = brokenStick(1:2,nrow(pca$factor_loadings))[1],
                           "PCA1_significant" = ifelse(pca$expl_var[1] > brokenStick(1:2,nrow(pca$factor_loadings))[1], "*",""),
                           "PCA2_var_expl" = pca$expl_var[2],
                           "PCA2_broken_stick" = brokenStick(1:2,nrow(pca$factor_loadings))[2],
                           "PCA2_significant" = ifelse(pca$expl_var[2] > brokenStick(1:2,nrow(pca$factor_loadings))[2], "*",""),
                           "sum_expl_variation" = pca$expl_var[1] + pca$expl_var[2],
                           "significant_axes" = paste(c(names(pca$expl_var)[which(pca$expl_var > brokenStick(1:nrow(pca$factor_loadings),nrow(pca$factor_loadings)))]),
                                                      collapse = "_")))
    
    # convert significances to character for later plotting
    explained.variation$PCA1_significant = as.character(explained.variation$PCA1_significant)
    explained.variation$PCA2_significant = as.character(explained.variation$PCA2_significant)
    
    # get axis limits, same limits for all plots
    xlim.temp = c()
    ylim.temp = c()
    
    for(j in 1:nrow(CTFS.plots)){
      xlim.temp = c(xlim.temp, range(pcas.to.plot[[j]]$pca_coordinates$PCA1))
      ylim.temp = c(ylim.temp, range(pcas.to.plot[[j]]$pca_coordinates$PCA2))}
    
    xlim.temp = c(- max(abs(xlim.temp)), max(abs(xlim.temp)))
    ylim.temp = c(- max(abs(ylim.temp)), max(abs(ylim.temp)))
    
    PCA1.plot.text = data.frame("PCA1" = rep(xlim.temp[[1]] + 2, 3),
                                "PCA2" = c(ylim.temp[[1]] + c(2,3,4)),
                                "label" = c(paste(c("PCA1:", round(explained.variation$PCA1_var_expl[explained.variation$CTFS_plot == plot.temp] * 100), 
                                                    "%", explained.variation$PCA1_significant[explained.variation$CTFS_plot == plot.temp]), collapse = " "),
                                            paste(c("PCA2:", round(explained.variation$PCA2_var_expl[explained.variation$CTFS_plot == plot.temp] * 100), 
                                                    "%", explained.variation$PCA2_significant[explained.variation$CTFS_plot == plot.temp]), collapse = " "),
                                            "nr_of_species" = paste(nr.of.species, "species")))
    
    # get muliplicator for arrow length
    max.loading = c()
    for(j in 1:nrow(CTFS.plots)){
      max.loading = c(max.loading, max(abs(pcas.to.plot[[j]]$factor_loadings$PCA2)))}
    
    max.loading = max(max.loading)
    arrow.multi = floor(ylim.temp[2] / max.loading)
    
    # plotting, no factor loadings if values are scaled across plots
    plot.of.plot.temp =
      ggplot()+ 
      geom_hline(yintercept = 0, linetype = "dotted") +
      geom_vline(xintercept = 0, linetype = "dotted") +
      geom_point(data = pca$pca_coordinates, aes(x = PCA1, y = PCA2), color = CTFS.plots$color[CTFS.plots$CTFS_plot == CTFS.plots$CTFS_plot[i]], alpha = 0.3, pch = 16) +
      annotate("text", rep(-Inf,3), rep(-Inf,3), label = PCA1.plot.text$label, hjust =- 0.1, vjust = c(-5,-3,-1)) + 
      #geom_text(data = PCA1.plot.text, aes(x = PCA1, y = PCA2, label = label)) +
      coord_cartesian(xlim = xlim.temp, ylim = ylim.temp) + 
      scale_color_manual(name = "Demographic rate", values= colors[[CTFS.plots$CTFS_plot[i]]]) +
      xlab("PCA1") + ylab("PCA2") + ggtitle(paste(plot.temp, census.interval.temp, sep = "")) +
      theme(legend.position = "none")
  
    # add factor loading and 2-d density plot if values are scaled across plots
    if(length(pcas.to.plot) > 1){
      if(length(pcas.to.plot) > 1 & all(pcas.to.plot[[1]]$sds == pcas.to.plot[[2]]$sds, na.rm = T)){
      plot.of.plot.temp  = plot.of.plot.temp + 
        geom_segment(data = pca$factor_loadings, aes(x = mean(pca$pca_coordinates$PCA1), 
                                                     y = mean(pca$pca_coordinates$PCA2),
                                                     xend = mean(pca$pca_coordinates$PCA1) + PCA1*arrow.multi, 
                                                     yend = mean(pca$pca_coordinates$PCA2) + PCA2*arrow.multi, color = demographic_rate),
                     arrow = arrow(), size = 2) 
        stat_density_2d(data = pca$pca_coordinates, aes(x = PCA1, y = PCA2, fill = ..level..), 
                        geom = "polygon", fill = CTFS.plots$color[CTFS.plots$CTFS_plot == CTFS.plots$CTFS_plot[i]], alpha = 0.3)
    }else{
      plot.of.plot.temp = plot.of.plot.temp +
      geom_segment(data = pca$factor_loadings, aes(x = 0, y = 0, xend = PCA1*arrow.multi, yend = PCA2*arrow.multi, color = demographic_rate),
                   arrow = arrow(), size = 2)}
      
    }else{
      plot.of.plot.temp = plot.of.plot.temp +
        geom_segment(data = pca$factor_loadings, aes(x = 0, y = 0, xend = PCA1*8, yend = PCA2*8, color = demographic_rate),
                     arrow = arrow(), size = 2)}
    
    # plotting with species names
    plot.of.plot.temp.with.species.names =
      ggplot()+ 
      geom_hline(yintercept = 0, linetype = "dotted") +
      geom_vline(xintercept = 0, linetype = "dotted") +
      geom_segment(data = pca$factor_loadings, aes(x = 0, y = 0, xend = PCA1*arrow.multi, yend = PCA2*arrow.multi, color = demographic_rate),
                   arrow = arrow(), alpha = 0.6, size = 2) +
      geom_text(data = pca$pca_coordinates, aes(x = PCA1, y = PCA2, label = sp), alpha = 0.5) +
      scale_color_manual(name = "Demographic rate", values= colors[[CTFS.plots$CTFS_plot[i]]]) +
      xlab("PCA1") + ylab("PCA2") + ggtitle(paste(plot.temp, census.interval.temp, sep = "")) 
    
    # store plots in one list
    pca.plot.list[[paste(plot.temp, census.interval.temp, sep = "")]] = plot.of.plot.temp
    
    # save plots with species names
    if(plot.pcas.with.species.names == T){
      ggsave(filename = paste(c(output.folder,"/pcas with species names/",
                                paste(plot.temp, census.interval.temp, sep = ""),
                                "_pca_", growth.layers, "_layers_species_names.pdf"),collapse = ""),
             plot =  plot.of.plot.temp.with.species.names, height = 14, width= 15)}
    }
  
  # to make shure that the assembling of multiple plots works
  graphics.off()
  
  svg(filename = paste(c(output.folder, "/all_pcas.svg", sep = ""), collapse = ""),
      height = 15, width = 15)
  grid.arrange(grobs = pca.plot.list, ncol = 3)
  graphics.off()
  
  # one plot to see the coloring
  ggsave(filename = paste(output.folder, "/check_colours.svg",sep = ""), 
         plot = pca.plot.list[["fushan"]] + theme(legend.position = "left"))
  
  # display pcas if obtained per plot
  grid.arrange(grobs = pca.plot.list, ncol = 3)
  
  return(explained.variation)
}

###########################
# create axis loading plots
plot.pca.axis.loadings = function(CTFS.plots, explained.variation.to.plot, exclude.rec = T,
                                  pcas.to.plot, four.or.all.layers,
                                  per.plot.or.per.census.interval, output.folder){
  
  
  # create empty lists to store the plots
  pca.plot.list = list()
  max.nr.of.crown.layers = c()
  
  for(i in 1:nrow(CTFS.plots)){
    
    plot.temp = CTFS.plots$CTFS_plot[i]
    census.interval.temp = CTFS.plots$census_interval[i]
    
    # load data
    pca = pcas.to.plot[[i]]
    nr.of.species  = length(pca$species)
  
    # get the plotable growth and survival layers
    survival.layers = names(pca$sds)[grep(pattern = "survival", x = names(pca$sds))]
    survival.layers = as.numeric(substr(survival.layers, nchar(survival.layers),nchar(survival.layers)))
    
    growth.layers = names(pca$sds)[grep(pattern = "growth", x = names(pca$sds))]
    growth.layers = as.numeric(substr(growth.layers, nchar(growth.layers),nchar(growth.layers)))
    
    # determine.max.number of crown layers
    max.nr.of.crown.layers = c(max.nr.of.crown.layers, 
                               survival.layers, growth.layers)
    
    # re-arrange factor levels for plotting
    layer.colors = data.frame("demographic_rate" = c("growth_layer1", "growth_layer2", "growth_layer3", "growth_layer4", "growth_layer5", "growth_layer6", "growth_layer7",
                                                     "survival_layer1", "survival_layer2", "survival_layer3", "survival_layer4", "survival_layer5", "survival_layer6", "survival_layer7",
                                                     "recruitment_per_ba", "recruitment_per_capita"),
                              "color" = c('#680000','#a32020','#e0301e','#dc6900','#eb8c00','#F3BA66','#FBEAD1',
                                          '#153251','#435A79','#0b6794','#4a89b8','#CCDAF7','#A1B9C9','#D9E3E9',
                                          '#348757',"#A7F146"))
    
    if(exclude.rec == T){layer.colors = layer.colors[-(which(layer.colors$demographic_rate == "recruitment_per_ba"))]}
    
    # assign colors for the factor loadings    
    pca$factor_loadings = merge(pca$factor_loadings, layer.colors, by = "demographic_rate")
    
    # order factor levels for plotting
    pca$factor_loadings = pca$factor_loadings[match(layer.colors$demographic_rate, pca$factor_loadings$demographic_rate),]
    pca$factor_loadings = pca$factor_loadings[complete.cases(pca$factor_loadings),]
    
    pca$factor_loadings$demographic_rate = factor(as.character(pca$factor_loadings$demographic_rate),
                                                  levels = as.character(pca$factor_loadings$demographic_rate))
    pca$factor_loadings$color = as.character(pca$factor_loadings$color)
    
    # switch PCA-axes where necessary
    if(per.plot.or.per.census.interval == "per plot" & four.or.all.layers == "four"){
      if(CTFS.plots[CTFS_plot == plot.temp]$switch_axis1_loadings_4_layers == T){
        pca$factor_loadings$PCA1 = 0 - pca$factor_loadings$PCA1
        pca$pca_coordinates$PCA1 = 0 - pca$pca_coordinates$PCA1}
      if(CTFS.plots[CTFS_plot == plot.temp]$switch_axis2_loadings_4_layers == T){
        pca$factor_loadings$PCA2 = 0 - pca$factor_loadings$PCA2
        pca$pca_coordinates$PCA2 = 0 - pca$pca_coordinates$PCA2}}
    
    if(per.plot.or.per.census.interval == "per plot" & four.or.all.layers == "all"){
      if(CTFS.plots[CTFS_plot == plot.temp]$switch_axis1_loadings_all_layers == T){
        pca$factor_loadings$PCA1 = 0 - pca$factor_loadings$PCA1
        pca$pca_coordinates$PCA1 = 0 - pca$pca_coordinates$PCA1}
      if(CTFS.plots[CTFS_plot == plot.temp]$switch_axis2_loadings_all_layers == T){
        pca$factor_loadings$PCA2 = 0 - pca$factor_loadings$PCA2
        pca$pca_coordinates$PCA2 = 0 - pca$pca_coordinates$PCA2}}
    
    # plot PCA1 axis
    plot.PCA1 =
      ggplot()+ 
      geom_hline(yintercept = -0.2, linetype = "dotted") +
      geom_hline(yintercept = 0.2, linetype = "dotted") +
      geom_bar(data = pca$factor_loadings, aes(x = demographic_rate, y = PCA1, fill = demographic_rate), stat = "identity", color = "white") +
      geom_hline(yintercept = 0) +
      coord_cartesian(ylim = c(-0.6, 0.6)) + 
      scale_y_continuous(breaks = c(-0.6, -0.3, 0, 0.3, 0.6)) +
      scale_fill_manual(name = "Demographic rate", values= pca$factor_loadings$color) +
      xlab(paste(c("PCA1\nexpl var:", round(pca$expl_var[1]*100), "% ", explained.variation.to.plot$PCA1_significant[i]), collapse = " ")) + 
      ylab("") +
      theme_bw()+
      theme(axis.line.x = element_blank(), panel.grid = element_blank(),
            legend.position = "none", axis.text.x = element_blank(), axis.ticks.x = element_blank(),
            plot.margin = margin(0, 0, 1, 1, "cm"), panel.border = element_rect(color = "black")) 
    
    # plot PCA2 axis
    plot.PCA2 =
      ggplot()+ 
      geom_hline(yintercept = -0.2, linetype = "dotted") +
      geom_hline(yintercept = 0.2, linetype = "dotted") +
      geom_bar(data = pca$factor_loadings, aes(x = demographic_rate, y = PCA2, fill = demographic_rate), stat = "identity", color = "white") +
      geom_hline(yintercept = 0) +
      coord_cartesian(ylim = c(-0.6, 0.6)) + 
      scale_y_continuous(breaks = c(-0.6, -0.3, 0, 0.3, 0.6)) +
      scale_fill_manual(name = "Demographic rate", values= pca$factor_loadings$color) +
      xlab(paste(c("PCA2\nexpl var:", round(pca$expl_var[2]*100), "% ", explained.variation.to.plot$PCA2_significant[i]), collapse = " ")) + 
      ylab("") +
      theme_bw()+
      theme(axis.line.x = element_blank(), panel.grid = element_blank(),
            legend.position = "none", axis.text.x = element_blank(), axis.ticks.x = element_blank(),
            plot.margin = margin(0, 1, 1, 0, "cm"), panel.border = element_rect(color = "black")) 
    
    # save plots
    pca.plot.list[[paste(plot.temp, census.interval.temp, sep = "")]] = 
      arrangeGrob(plot.PCA1, plot.PCA2, 
                  ncol = 2, top = textGrob(paste(plot.temp, census.interval.temp, sep = ""), gp=gpar(fontsize=20))) 
    }
  
  if(exclude.rec == F){
    svg(filename = paste(output.folder, "/all_factor_loadings.svg", sep = ""), 
        height = 20, width = 12)
    grid.arrange(grobs = pca.plot.list, ncol = 2)
    graphics.off()}
  
  if(exclude.rec == T){
    svg(filename = paste(output.folder, "/all_factor_loadings_no_rec_ba.svg", sep = ""), 
        height = 20, width = 12)
    grid.arrange(grobs = pca.plot.list, ncol = 2)
    graphics.off()}
  
    grid.arrange(grobs = pca.plot.list, ncol = 3)
  
}

#####################################################################################
# plot relationships between two demographies from the per-census interval estimates
plot.one.demography.against.each.other = function(CTFS.plots,
                                                  all.stan.estimates.per.census.interval,
                                                  demography.x,
                                                  layer.x,
                                                  demography.y,
                                                  layer.y,
                                                  per.plot.or.per.census.interval,
                                                  data.to.omit.in.plotting){
 
  # store results
  results_per_interval = CTFS.plots[,.(CTFS_plot, census_interval)]
  results_per_plot = CTFS.plots[,.(CTFS_plot)]
  results_per_interval$nr_species = NA
  results_per_interval$mcp80_area = NA
  results_per_interval$lm_coef = NA
  results_per_interval$lm_p = NA
  results_per_interval$lm_R2 = NA
  
  results_per_plot$mcp80_overlap_mean = NA
  
  # select data to plot
  # if recruitment
  if(demography.x == "recruitment_per_ba"){
    stan.estimates.x = all.stan.estimates.per.census.interval[["recruitment"]][,.(CTFS_plot, census_interval, crown_layer = NA, sp, stan_estimates_x = stan_yearly_recruits_per_mean_adult_ba_trans)]}
  if(demography.x == "recruitment_per_capita"){
    stan.estimates.x = all.stan.estimates.per.census.interval[["recruitment"]][,.(CTFS_plot, census_interval, crown_layer = NA, sp, stan_estimates_x = stan_yearly_recruits_per_capita_trans)]}
  if(demography.y == "recruitment_per_ba"){
    stan.estimates.y = all.stan.estimates.per.census.interval[["recruitment"]][,.(CTFS_plot, census_interval, crown_layer = NA, sp, stan_estimates_y = stan_yearly_recruits_per_mean_adult_ba_trans)]}
  if(demography.y == "recruitment_per_capita"){
    stan.estimates.y = all.stan.estimates.per.census.interval[["recruitment"]][,.(CTFS_plot, census_interval, crown_layer = NA, sp, stan_estimates_y = stan_yearly_recruits_per_capita_trans)]}
  
  # if survival
  if(demography.x == "survival"){
    # get the canopy layers for each CTFS plot
    if(layer.x == "bottom"){
      survival.temp = all.stan.estimates.per.census.interval[["survival"]]
      stan.estimates.x = survival.temp[0,]
      for(i in 1:nrow(CTFS.plots)){
        bottom.layer.temp = survival.temp[CTFS_plot == CTFS.plots$CTFS_plot[i] &
                                          census_interval == CTFS.plots$census_interval[i]]
        bottom.layer.temp = bottom.layer.temp[crown_layer == max(bottom.layer.temp$crown_layer)]
        stan.estimates.x = rbind(stan.estimates.x,
                                 bottom.layer.temp)}
      
        stan.estimates.x = stan.estimates.x[,.(CTFS_plot, census_interval, crown_layer,sp, stan_mean_surv_rate_trans)]
      
    }else{
        stan.estimates.x = all.stan.estimates.per.census.interval[["survival"]][crown_layer == layer.x & data_available == "yes",
                                                                            .(CTFS_plot, census_interval, crown_layer,sp, stan_mean_surv_rate_trans)]}
    # remove inaccurate stan estimates
    data.to.omit.temp = data.to.omit.in.plotting[demography == "survival"]
    if(nrow(data.to.omit.temp) > 0){
      for(i in 1:nrow(data.to.omit.temp)){
        stan.estimates.x = stan.estimates.x[!(stan.estimates.x$CTFS_plot == data.to.omit.temp$CTFS_plot[i] &
                                            stan.estimates.x$census_interval == data.to.omit.temp$census_interval[i] &
                                            stan.estimates.x$crown_layer == data.to.omit.temp$crown_layer[i]),]
      }}
    
    names(stan.estimates.x)[5] =  "stan_estimates_x"}
  
  if(demography.y == "survival"){
    # get the canopy layers for each CTFS plot
    if(layer.y == "bottom"){
      survival.temp = all.stan.estimates.per.census.interval[["survival"]]
      stan.estimates.y = survival.temp[0,]
      for(i in 1:nrow(CTFS.plots)){
        bottom.layer.temp = survival.temp[CTFS_plot == CTFS.plots$CTFS_plot[i] &
                                            census_interval == CTFS.plots$census_interval[i]]
        bottom.layer.temp = bottom.layer.temp[crown_layer == max(bottom.layer.temp$crown_layer)]
        stan.estimates.y = rbind(stan.estimates.y,
                                 bottom.layer.temp)}
      
      stan.estimates.y = stan.estimates.y[,.(CTFS_plot, census_interval, crown_layer,sp, stan_mean_surv_rate_trans)]
      
    }else{
      stan.estimates.y = all.stan.estimates.per.census.interval[["survival"]][crown_layer == layer.y & data_available == "yes",
                                                                              .(CTFS_plot, census_interval, crown_layer,sp, stan_mean_surv_rate_trans)]}
    # remove inaccurate stan estimates
    data.to.omit.temp = data.to.omit.in.plotting[demography == "survival"]
    if(nrow(data.to.omit.temp) > 0){
      for(i in 1:nrow(data.to.omit.temp)){
        stan.estimates.y = stan.estimates.y[!(stan.estimates.y$CTFS_plot == data.to.omit.temp$CTFS_plot[i] &
                                                stan.estimates.y$census_interval == data.to.omit.temp$census_interval[i] &
                                                stan.estimates.y$crown_layer == data.to.omit.temp$crown_layer[i]),]
      }}
    
    names(stan.estimates.y)[5] =  "stan_estimates_y"}
  
  # if growth
  if(demography.x == "growth"){
    # get the canopy layers for each CTFS plot
    if(layer.x == "bottom"){
      growth.temp = all.stan.estimates.per.census.interval[["growth"]]
      stan.estimates.x = growth.temp[0,]
      for(i in 1:nrow(CTFS.plots)){
        bottom.layer.temp = growth.temp[CTFS_plot == CTFS.plots$CTFS_plot[i] &
                                            census_interval == CTFS.plots$census_interval[i]]
        bottom.layer.temp = bottom.layer.temp[crown_layer == max(bottom.layer.temp$crown_layer)]
        stan.estimates.x = rbind(stan.estimates.x,
                                 bottom.layer.temp)}
      
      stan.estimates.x = stan.estimates.x[,.(CTFS_plot, census_interval, crown_layer,sp, stan_mean_growth_rate_trans)]
      
    }else{
      stan.estimates.x = all.stan.estimates.per.census.interval[["growth"]][crown_layer == layer.x & data_available == "yes",
                                                                              .(CTFS_plot, census_interval, crown_layer,sp, stan_mean_growth_rate_trans)]}
    # remove inaccurate stan estimates
    data.to.omit.temp = data.to.omit.in.plotting[demography == "growth"]
    if(nrow(data.to.omit.temp) > 0){
      for(i in 1:nrow(data.to.omit.temp)){
        stan.estimates.x = stan.estimates.x[!(stan.estimates.x$CTFS_plot == data.to.omit.temp$CTFS_plot[i] &
                                                stan.estimates.x$census_interval == data.to.omit.temp$census_interval[i] &
                                                stan.estimates.x$crown_layer == data.to.omit.temp$crown_layer[i]),]
      }}
    
    names(stan.estimates.x)[5] =  "stan_estimates_x"}
  
  if(demography.y == "growth"){
    # get the canopy layers for each CTFS plot
    if(layer.y == "bottom"){
      growth.temp = all.stan.estimates.per.census.interval[["growth"]]
      stan.estimates.y = growth.temp[0,]
      for(i in 1:nrow(CTFS.plots)){
        bottom.layer.temp = growth.temp[CTFS_plot == CTFS.plots$CTFS_plot[i] &
                                            census_interval == CTFS.plots$census_interval[i]]
        bottom.layer.temp = bottom.layer.temp[crown_layer == max(bottom.layer.temp$crown_layer)]
        stan.estimates.y = rbind(stan.estimates.y,
                                 bottom.layer.temp)}
      
      stan.estimates.y = stan.estimates.y[,.(CTFS_plot, census_interval, crown_layer,sp, stan_mean_growth_rate_trans)]
      
    }else{
      stan.estimates.y = all.stan.estimates.per.census.interval[["growth"]][crown_layer == layer.y & data_available == "yes",
                                                                              .(CTFS_plot, census_interval, crown_layer,sp, stan_mean_growth_rate_trans)]}
    # remove inaccurate stan estimates
    data.to.omit.temp = data.to.omit.in.plotting[demography == "growth"]
    if(nrow(data.to.omit.temp) > 0){
      for(i in 1:nrow(data.to.omit.temp)){
        stan.estimates.y = stan.estimates.y[!(stan.estimates.y$CTFS_plot == data.to.omit.temp$CTFS_plot[i] &
                                                stan.estimates.y$census_interval == data.to.omit.temp$census_interval[i] &
                                                stan.estimates.y$crown_layer == data.to.omit.temp$crown_layer[i]),]
      }}
    
    names(stan.estimates.y)[5] =  "stan_estimates_y"}
  
  # if(demography.y == "stature"){
  #   
  #   stan.estimates.growth1 = all.stan.estimates.per.census.interval[["growth"]][crown_layer == 1 & data_available == "yes",
  #                                                                               .(CTFS_plot, census_interval, sp, stan_mean_growth_rate_trans)]
  #   stan.estimates.survival1 = all.stan.estimates.per.census.interval[["survival"]][crown_layer == 1 & data_available == "yes",
  #                                                                                   .(CTFS_plot, census_interval, sp, stan_mean_surv_rate_trans)]
  #   stan.estimates.rec_ba = all.stan.estimates.per.census.interval[["recruitment"]][data_available == "yes",
  #                                                                                   .(CTFS_plot, census_interval, sp, stan_yearly_recruits_per_mean_adult_ba_trans)]
  #   
  #   stan.estimates.y = merge(stan.estimates.growth1, stan.estimates.survival1, by=  c("CTFS_plot", "census_interval", "sp"))
  #   stan.estimates.y = merge(stan.estimates.y, stan.estimates.rec_ba, by=  c("CTFS_plot", "census_interval", "sp"))
  #   
  #   stan.estimates.y = stan.estimates.y[,.(sp = sp,  growth = stan_mean_growth_rate_trans, suv = stan_mean_surv_rate_trans,
  #                                          rank_growth = rank(stan_mean_growth_rate_trans),
  #                                          rank_survival = rank(stan_mean_surv_rate_trans),
  #                                          rank_stature = 0.5 * (rank(stan_mean_growth_rate_trans) + rank(stan_mean_surv_rate_trans)),
  #                                          rank_recruitment = rank(stan_yearly_recruits_per_mean_adult_ba_trans)),
  #                                       by = ,.(CTFS_plot, census_interval)]
  #   stan.estimates.y = stan.estimates.y[,.(CTFS_plot, census_interval, sp,rank_stature)]
  #   names(stan.estimates.y)[4] =  "stan_estimates_y"}
  
  
  # merge data
  plot.data = merge(stan.estimates.x, stan.estimates.y, by = c("CTFS_plot", "census_interval","sp"))
  
  # get the 80% mean convex polygons
  mcps80 = data.frame("CTFS_plot" = NA, "census_interval" = NA, "stan_estimates_x" = NA, "stan_estimates_y" = NA, "area" = NA )[0,]
  unique.comb = data.table(unique(plot.data[,.(CTFS_plot, census_interval)]))
  unique.comb$CTFS_plot = as.character(unique.comb$CTFS_plot)
  
  # global 80% mcp of all species (x and y estimates averaged per species)
  average.x.y.estimates.per.species = plot.data[,.(stan_estimates_x = mean(stan_estimates_x), 
                                                   stan_estimates_y = mean(stan_estimates_y)),
                                                by = .(CTFS_plot,sp)][,.(stan_estimates_x, stan_estimates_y)]
  
  mean.x.estimate.for.plot = mean(average.x.y.estimates.per.species$stan_estimates_x)
  mean.y.estimate.for.plot = mean(average.x.y.estimates.per.species$stan_estimates_y)
  mcp80all =  mcp(SpatialPoints(average.x.y.estimates.per.species), percent = 80)
  mcp80all.points = as.data.frame(mcp80all@polygons[[1]]@Polygons[[1]]@coords)
  mcp80all.area = mcp80all@polygons[[1]]@Polygons[[1]]@area
  
  # get the mcp80s for every plot
  results_per_interval = plot.data[,.(nr_species = length(stan_estimates_x),
                                      crown_layer_x = mean(crown_layer.x),
                                      crown_layer_y = mean(crown_layer.y),
                                      lm_coef = lm(stan_estimates_y ~ stan_estimates_x)$coefficients[[2]],
                                      lm_R2 = as.numeric(summary(lm(stan_estimates_y ~ stan_estimates_x))["r.squared"]),
                                      lm_p = anova(lm(stan_estimates_y ~ stan_estimates_x))$`Pr(>F)`[1],
                                      mcp80_area = NA,
                                      overlap_with_next_census = NA),
                                   by = ,.(CTFS_plot, census_interval)]
  list.of.mcps80 = list()
  
  mcps80.points = data.frame(CTFS_plot = NA, census_interval = NA, stan_estimates_x = NA, 
                             stan_estimates_y = NA, variables = NA)[0,]
                             
  for(i in 1:nrow(results_per_interval)){
    
    data.temp = plot.data[CTFS_plot == results_per_interval$CTFS_plot[i] & 
                          census_interval == results_per_interval$census_interval[i]]
    
    mcp80.temp = mcp(SpatialPoints(data.temp[,.(stan_estimates_x, stan_estimates_y)]),
                   percent = 80)
    results_per_interval$mcp80_area[i] = gArea(mcp80.temp) / mcp80all.area
    
    list.of.mcps80[[as.character(results_per_interval$CTFS_plot[i])]][[as.numeric(results_per_interval$census_interval[i])]] = 
      data.frame(mcp80.temp@polygons[[1]]@Polygons[[1]]@coords)
    
    mcps80.points = rbind(mcps80.points, 
                          data.frame(CTFS_plot = results_per_interval$CTFS_plot[i],
                                     census_interval = results_per_interval$census_interval[i],
                                     stan_estimates_x = list.of.mcps80[[as.character(results_per_interval$CTFS_plot[i])]][[as.numeric(results_per_interval$census_interval[i])]]$stan_estimates_x,
                                     stan_estimates_y = list.of.mcps80[[as.character(results_per_interval$CTFS_plot[i])]][[as.numeric(results_per_interval$census_interval[i])]]$stan_estimates_y,
                                     variables = paste(c(demography.y, layer.y, "~", demography.x, layer.x), collapse = "")))}
  
  mcps80.points = data.table(mcps80.points)
  
  # calculate species richness, mpc80 area, correlation coef and lm coef + sds per plot/interval
  
  for(i in 1:nrow(results_per_interval)){
    earlier.census = list.of.mcps80[[as.character(results_per_interval$CTFS_plot)[i]]][[as.numeric(results_per_interval$census_interval[i])]]
    earlier.census = rbind(earlier.census, earlier.census[nrow(earlier.census),])
    earlier.census = Polygons(list(Polygon(earlier.census)), "earlier_census")
  
    # calculate MCP overlap between subsequent censuses
    if(i < nrow(results_per_interval)){
      
      if(results_per_interval$CTFS_plot[i] == results_per_interval$CTFS_plot[i + 1] &
         results_per_interval$census_interval[i] +1 == results_per_interval$census_interval[i+1]){
        
        later.census = list.of.mcps80[[as.character(results_per_interval$CTFS_plot)[i+1]]][[as.numeric(results_per_interval$census_interval[i+1])]]
        later.census  = rbind(later.census, later.census[nrow(later.census),])
        later.census = Polygons(list(Polygon(later.census)), "later_census")
        shape = SpatialPolygons(list(earlier.census, later.census))
        
        overlap.temp = gIntersection(shape["earlier_census"], shape["later_census"])
        
        results_per_interval$overlap_with_next_census[i] = gArea(overlap.temp) / min(c(earlier.census@area, later.census@area))
        
      }
    }
  }
  
  # assign factor levels for the order in the plot
  plot.data$CTFS_plot = factor(plot.data$CTFS_plot, 
                               levels = c("bci", "lambir", "pasoh", "yasuni", 
                                          "fushan", "luquillo", "nanjenshan", "palanan", 
                                          "sherman", "sinharaja"))
  
  # create plot
  plot.to.save =
    ggplot() +
    geom_point(data = plot.data, aes(x = stan_estimates_x, y  = stan_estimates_y), fill = "grey", alpha = 0.1, pch = 21) +
    geom_hline(yintercept = mean.y.estimate.for.plot, linetype = "dotted", color = "black") +
    geom_vline(xintercept = mean.x.estimate.for.plot, linetype = "dotted", color = "black") +
    geom_polygon(data = mcps80.points, aes(x = stan_estimates_x, y = stan_estimates_y, fill = factor(census_interval), color = factor(census_interval)), alpha = 0.1)  +
    geom_polygon(data = mcp80all.points, aes(x = stan_estimates_x, y = stan_estimates_y), fill = NA, color = "black", linetype = "dashed") +
    geom_smooth(data = plot.data, aes(x = stan_estimates_x, y = stan_estimates_y, color = factor(census_interval)), fill = NA, method = "lm", size = 1.1) +
      
    facet_wrap(CTFS_plot ~ ., ncol = 3, dir = "v") +
    
    scale_fill_manual(name = "Census interval", values = c("#FFBF00","#eb6841", "#B23333", "#AF50E7", "#4CA4FF", "#8AE78A","#D0E78A")) +
    scale_color_manual(name = "Census interval", values = c("#FFBF00", "#eb6841", "#B23333", "#AF50E7", "#4CA4FF", "#8AE78A","#D0E78A")) +
    
    coord_cartesian(xlim = c(min(mcps80.points$stan_estimates_x), max(mcps80.points$stan_estimates_x)),
                    ylim = c(min(mcps80.points$stan_estimates_y), max(mcps80.points$stan_estimates_y))) +
    
    xlab(paste(demography.x, layer.x, sep = "")) +
    ylab(paste(demography.y, layer.y, sep = "")) +
    theme_bw() + 
    
    theme(legend.position = "bottom" ,
          panel.grid = element_blank(),
          text = element_text(size = 15))
  
  ggsave(paste(c("intermediate output/all census intervals/demography relationships between censuses/mcps_", demography.x,layer.x,"_",demography.y, layer.y,"_per_census.interval.svg"),collapse = ""),
         plot = plot.to.save,
         height = 12, width = 9)
  
  print(plot.to.save)
  
  results = data.table(results_per_interval)
  return(results)
}


#####################################################################################
# calculate R2 from procrustes analyses between either plot pcas or single census pcas
calculate.procrustes.R2.between.pcas = function(CTFS.plots, pcas.to.plot,
                                                per.plot.or.per.census.interval){
  
  pca.list = list()
  
  # assemble and switch all factor loadings
  for(i in 1:nrow(CTFS.plots)){
    
    plot.temp = CTFS.plots$CTFS_plot[i]
    census.interval.temp = CTFS.plots$census_interval[i]
    pca.temp = pcas.to.plot[[i]]$factor_loadings[,c("PCA1", "PCA2")]
    
    # switch PCA-axes where necessary
    if(per.plot.or.per.census.interval == "per plot"){
      if(CTFS.plots[CTFS_plot == plot.temp]$switch_pca1_4_layers == T){
        pca.temp$PCA1 = 0 - pca.temp$PCA1
        pca.temp$PCA1 = 0 - pca.temp$PCA1}
      if(CTFS.plots[CTFS_plot == plot.temp]$switch_pca2_4_layers == T){
        pca.temp$PCA2 = 0 - pca.temp$PCA2
        pca.temp$PCA2 = 0 - pca.temp$PCA2}}
    
    if(per.plot.or.per.census.interval == "per census interval"){
      if(CTFS.plots[CTFS_plot == plot.temp & census_interval == census.interval.temp]$switch_pca1_4_layers == T){
        pca.temp$PCA1 = 0 - pca.temp$PCA1
        pca.temp$PCA1 = 0 - pca.temp$PCA1}
      if(CTFS.plots[CTFS_plot == plot.temp & census_interval == census.interval.temp]$switch_pca2_4_layers == T){
        pca.temp$PCA2 = 0 - pca.temp$PCA2
        pca.temp$PCA2 = 0 - pca.temp$PCA2}}
    
    pca.list[[paste(plot.temp, census.interval.temp, sep = "")]] = pca.temp
  }
  
  # create a table with the R� values between every plot combination
  if(per.plot.or.per.census.interval == "per plot"){
    
    results = data.frame("plot1" = NA, "plot2" = NA, "procrustes_R2" = NA)[0,]
    
    for(plot.temp1 in unique(CTFS.plots$CTFS_plot)){
      for(plot.temp2 in unique(CTFS.plots$CTFS_plot)){
        
        pca1 = pca.list[[grep(pattern = plot.temp1, names(pca.list))]]
        pca2 = pca.list[[grep(pattern = plot.temp2, names(pca.list))]]
        protest.temp = protest(X = pca1, Y = pca2, scale = F)
        
        results = rbind(results, 
                        data.frame("plot1" = plot.temp1,
                                   "plot2" = plot.temp2,
                                   "procrustes_R2" = protest.temp$t0))}}}
  
  # create a table with the R� values between each census of the same plot
  if(per.plot.or.per.census.interval == "per census interval"){
    
    results = data.frame("CTFS_plot" = NA, "census_comparison" = NA, "procrustes_R2" = NA)[0,]
    
    for(plot.temp in unique(CTFS.plots$CTFS_plot)){
      
      pca.list.temp = pca.list[grep(pattern = plot.temp, names(pca.list))]
      
      for(i in 1:(length(pca.list.temp)-1)){
        
        if(nrow(pca.list.temp[[i]]) ==  nrow(pca.list.temp[[i+1]])){
          protest.temp = protest(X = pca.list.temp[[i]], Y = pca.list.temp[[i + 1]], scale = F)
          protest.temp$t0
          results = rbind(results, 
                          data.frame("CTFS_plot" = plot.temp, 
                                     "census_comparison" = paste(i, i+1, sep = ":"), 
                                     "procrustes_R2" = protest.temp$t0))
        }else{
          results = rbind(results, 
                          data.frame("CTFS_plot" = plot.temp, 
                                     "census_comparison" = paste(i, i+1, sep = ":"), 
                                     "procrustes_R2" = NA))}}}}
  
  if(per.plot.or.per.census.interval == "per plot"){
    
    # create a correlation matrix
    results.matrix = matrix(results$procrustes_R2, 
                                 ncol = length(unique(results$plot1)))
    rownames(results.matrix) = unique(results$plot1)
    colnames(results.matrix) = unique(results$plot1)
    col.temp <- colorRampPalette(c("#FFFFFF","#FFFFFF", "#000000"))
    
    svg(filename = "results/all pcas/procrustes_R2_scaled_per_plot.svg", height = 5, width = 5)
    corrplot(results.matrix, order = "hclust",
             addrect = 3, is.corr = FALSE, col = col.temp(20))
    graphics.off()
    corrplot(results.matrix, order = "hclust",
             addrect = 3, is.corr = FALSE, col = col.temp(20))}
  
  if(per.plot.or.per.census.interval == "per census interval"){
    ggsave(filename = "intermediate output/all census intervals/all pcas/procrustes_R2_boxplot.svg",
      ggplot(data = results) +
        geom_text(aes(x = CTFS_plot, y = procrustes_R2, label = census_comparison), alpha = 0.4) +
        geom_boxplot(aes(x = CTFS_plot, y = procrustes_R2, fill = CTFS_plot, color = CTFS_plot), alpha = 0.2) +
        scale_fill_manual(name = "ForestGeo plot", values = CTFS.plots$color[CTFS.plots$CTFS_plot %in% CTFS.plots$CTFS_plot]) +
        scale_color_manual(name = "ForestGeo plot", values = CTFS.plots$color[CTFS.plots$CTFS_plot %in% CTFS.plots$CTFS_plot]) +
        theme(legend.position = "none"),
      height = 3, width = length(unique(results$CTFS_plot)))}
  
  return(results)
}

################################################################
# assing each species one of 5 demographic groups (based on PCA)
determine.species.demographic.groups = function(pca.species.assignment,
                                                pca.demographic.group.names){
  
  pca.species.assignment$group_NN = NA
  pca.species.assignment$group_Einheitskreis = NA
  
  pca.centroid.fast.species = quantile(pca.species.assignment$PCA1, probs = 0.1)
  pca.centroid.slow.species = quantile(pca.species.assignment$PCA1, probs = 0.9)
  pca.centroid.slb.species = quantile(pca.species.assignment$PCA2, probs = 0.1)
  pca.centroid.llp.species = quantile(pca.species.assignment$PCA2, probs = 0.9)
  
  # scale PCA values to Einheitskreis
  pca.border.fast.to.scale = pca.centroid.fast.species / 2
  pca.border.slow.to.scale = pca.centroid.slow.species / 2
  pca.border.slb.to.scale = pca.centroid.slb.species / 2
  pca.border.llp.to.scale = pca.centroid.llp.species / 2
  
  pca.centroids.demographic.groups = 
    data.frame("PCA1" = c(pca.centroid.fast.species, pca.centroid.slow.species, 0, 0, 0),
               "PCA2" = c(0, 0, pca.centroid.slb.species, pca.centroid.llp.species, 0),
               "group" = pca.demographic.group.names)
  
  # save the Eimheitskreis_distance for plotting
  Einheitskreis_dist = c()
  # determine group belongs by the shortest distance to the centroids
  for(i in 1:nrow(pca.species.assignment)){
    dist.temp =dist(rbind(pca.species.assignment[i,c("PCA1","PCA2")],
                          pca.centroids.demographic.groups[,c("PCA1","PCA2")]), 
                    method = "euclidean")
    pca.species.assignment$group_NN[i] =  pca.demographic.group.names[which(dist.temp[1:5] == min(dist.temp[1:5]))]
    
    # Nadjas Einheitskreis Solution
    x = ifelse(pca.species.assignment$PCA1[i]>0, 
               pca.species.assignment$PCA1[i]/abs(pca.border.slow.to.scale), 
               pca.species.assignment$PCA1[i]/abs(pca.border.fast.to.scale))
    y = ifelse(pca.species.assignment$PCA2[i]>0, 
               pca.species.assignment$PCA2[i]/abs(pca.border.llp.to.scale), 
               pca.species.assignment$PCA2[i]/abs(pca.border.slb.to.scale))
    coords = matrix(c(0,0,x,y), nrow=2, byrow=T)
    if(y<0 && abs(y)>abs(x)) pca.species.assignment$group_Einheitskreis[i] = pca.demographic.group.names[3]
    if(x>0 && abs(x)>abs(y)) pca.species.assignment$group_Einheitskreis[i] = pca.demographic.group.names[2]
    if(y>0 && abs(y)>abs(x)) pca.species.assignment$group_Einheitskreis[i] = pca.demographic.group.names[4]
    if(x<0 && abs(x)>abs(y)) pca.species.assignment$group_Einheitskreis[i] = pca.demographic.group.names[1]
    if(dist(coords) < 1){
      pca.species.assignment$group_Einheitskreis[i] = pca.demographic.group.names[5]
      Einheitskreis_dist  = c(Einheitskreis_dist, dist(coords))}
  }
  Einheitskreis_dist = max(Einheitskreis_dist)
  max.pca.score = max(abs(c(pca.species.assignment$PCA1, pca.species.assignment$PCA2)))
  
  circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))}
  dat <- circleFun(c(0,0),2,npoints = 100)
  
  
  plot.group.assignment = ggplot()+ 
    geom_point(data = pca.species.assignment, aes(x = PCA1, y = PCA2, color = group_NN), alpha = 0.4, size = 4) +
    geom_text(data = pca.species.assignment, aes(x = PCA1, y = PCA2, label = sp)) +
    #geom_point(data = pca.centroids.demographic.groups, aes(x = PCA1, y = PCA2, color = group)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_path(data = dat, aes(x,y)) + 
    scale_color_manual(values = c("#E69F00","#d42121","#009E73","#0072B2","#CC79A7")) + 
    xlim(-max.pca.score, max.pca.score) + ylim(-max.pca.score, max.pca.score) +
    theme_bw()
  
  print(plot.group.assignment)
  
  results = list(pca.species.assignment = pca.species.assignment,
                 plot.group.assignment = plot.group.assignment,
                 group.centroids = data.frame("group" = pca.demographic.group.names,
                                              "PCA1_switched" = c(pca.centroid.fast.species, pca.centroid.slow.species, 0, 0, 0),
                                              "PCA2_switched" = c(0, 0, pca.centroid.slb.species, pca.centroid.llp.species,0)))
  
  return(results)
}


#######################################################
calculate.basal.area.per.hectare.for.the.five.demographic.groups = function(pca.species.assignment, 
                                                                            census.list, 
                                                                            group.column){
  
  # create a data frame to store the results
  results = data.frame("group" = NA, "census_id" = NA, "subplot_id" = NA, "basal_area_sum" = NA)[0,]
  
  # calculate area of the whole CTFS plot
  plotdim.temp = sort(c(round(max(census.list[[1]]$gx, na.rm = T) - min(census.list[[1]]$gx, na.rm = T)),
                        round(max(census.list[[1]]$gy, na.rm = T) - min(census.list[[1]]$gy, na.rm = T))), decreasing = T)
  plotarea.temp = plotdim.temp[1] * plotdim.temp[2]
  
  for(i in 1:length(census.list)){
    
    # select census
    census.temp = census.list[[i]]
    
    # divide into 1 ha subplots
    census.temp$plotindex = factor(gxgy.to.index(census.temp$gx, census.temp$gy, gridsize = 100, plotdim = plotdim.temp))
    
    # assing demographic groups
    census.temp = merge(census.temp, 
                        pca.species.assignment$pca.species.assignment[,c(which(names(pca.species.assignment$pca.species.assignment) %in% c("sp",group.column)))], 
                        by = "sp")
    
    names(census.temp)[which(names(census.temp) == group.column)] = "group"
    census.temp$group = factor(census.temp$group)
    
    # calculate basal area per group and subplot
    basal.area.temp = abundance(census.temp, type = "ba", alivecode = "A", mindbh = 10, dbhunit = "mm",
                                split1 = census.temp$plotindex, split2 = census.temp$group)
    
    results = rbind(results, data.frame(
      "group" =  rep(names(basal.area.temp$ba), each = nrow(basal.area.temp$ba)),
      "census_id" = paste(c(CTFS.plot,"_",i), collapse= ""),
      "subplot_id" = 1: nrow(basal.area.temp$ba), 
      "basal_area_sum" = c(basal.area.temp$b[,1],basal.area.temp$b[,2],basal.area.temp$b[,3],basal.area.temp$b[,4],basal.area.temp$b[,5])))
  }
  
  # only use mean values across the censuses to avoid temporal autocorrelation
  results = data.table(results)
  nr.censuses = length(unique(results$census_id))
  results = results[,.(mean_ba = sum(basal_area_sum, na.rm = T) / nr.censuses),
                    by = .(group, subplot_id)]
  
  return(results)
}

##################################################################
# solve the switched pca to get the demographies at the 5 group centroids (Fast, Slow, SLB, LLP, Int)
get.solved.backtransformed.pca.centroid.values = function(centroids.matrix,
                                                          components.matrix,
                                                          pca.species.assignment,
                                                          pca,
                                                          transformations.prior.to.pca){
  # to store results
  centroid.demographies = list()
  
  centroid.demographies$pca_solved = centroids.matrix  %*% components.matrix
  centroid.demographies$pca_solved = t(centroid.demographies$pca_solved) * pca$sds # de-scale
  centroid.demographies$pca_solved = t(centroid.demographies$pca_solved + pca$centers) # de-center
  
  # backtransform recruitments # must be divided by basal area because is nr recruits per year now
  centroid.demographies$recruitment = exp(centroid.demographies$pca_solved[,1])
  centroid.demographies$recruitment = data.frame("group" = as.character(pca.species.assignment$group.centroids$group), 
                                                 "recruits" = centroid.demographies$recruitment)
  
  # backtransform survival
  centroid.demographies$survival = exp(centroid.demographies$pca_solved[,c(2:5)]) 
  centroid.demographies$survival = 1 / (centroid.demographies$survival) 
  centroid.demographies$survival = as.data.frame(1 - centroid.demographies$survival)
  centroid.demographies$survival = data.frame("group" = as.character(pca.species.assignment$group.centroids$group),
                                              as.data.frame(centroid.demographies$survival))
  
  # back-transform growth
  centroid.demographies$growth = exp(centroid.demographies$pca_solved[,c(6:9)])
  centroid.demographies$growth = centroid.demographies$growth - transformations.prior.to.pca$growth$value_added
  centroid.demographies$growth = data.frame("group" = as.character(pca.species.assignment$group.centroids$group),
                                            as.data.frame(centroid.demographies$growth))
  
  return(centroid.demographies)
}


################################################################
# create histograms of all stan estimates of all forestgeo plots
create.histograms.of.all.stan.estimates = function(CTFS.plots, nr.of.max.crown.layers, all.stan.estimates, per.plot.or.per.census.interval){
  
  # store results in a list
  all.estimates.list = list()
  survival.list = list()
  growth.list = list()
  recruitment.list = list()
  
  summary.stats = list()
  
  # only import those stan estimates for which data was available
  for(plot.temp in CTFS.plots$CTFS_plot){
    
    # load survival and growth data and only keep those sp-canopy layer combinations for which there is data
    survival.data = data.table(readRDS(paste(c("intermediate output/", plot.temp, "/", plot.temp, "_balanced_survival.rds"), collapse = "")))
    survival.data = survival.data[,.(sp, crown_layer, sp_layer_comb = paste(sp, crown_layer, sep = "_"))]
    
    survival.list[[plot.temp]] = all.stan.estimates$survival[CTFS_plot == plot.temp]
    survival.list[[plot.temp]] = survival.list[[plot.temp]][,.(sp, crown_layer, stan_mean_surv_rate, stan_weight_surv_rate, stan_mean_surv_rate_trans,
                                                               sp_layer_comb = paste(sp, crown_layer, sep = "_"))]
    survival.list[[plot.temp]]$data_available = ifelse(survival.list[[plot.temp]]$sp_layer_comb %in% survival.data$sp_layer_comb, "yes", "no")
    
    growth.data = data.table(readRDS(paste(c("intermediate output/", plot.temp, "/", plot.temp, "_balanced_growth.rds"), collapse = "")))
    growth.data = growth.data[,.(sp, crown_layer, sp_layer_comb = paste(sp, crown_layer, sep = "_"))]
    
    growth.list[[plot.temp]] = all.stan.estimates$growth[CTFS_plot == plot.temp]
    growth.list[[plot.temp]] = growth.list[[plot.temp]][,.(sp, crown_layer, stan_mean_growth_rate, stan_weight_growth_rate, stan_mean_growth_rate_trans,
                                                     sp_layer_comb = paste(sp, crown_layer, sep = "_"))]
    growth.list[[plot.temp]]$data_available = ifelse(growth.list[[plot.temp]]$sp_layer_comb %in% growth.data$sp_layer_comb, "yes", "no")
  
    recruitment.list[[plot.temp]] = all.stan.estimates$recruitment[CTFS_plot == plot.temp]
    recruitment.list[[plot.temp]]  = recruitment.list[[plot.temp]][,.(sp, stan_mean_nr_recruits, stan_weights = stan_weights_for_nr_recruits, 
                                                                      recruitment_per_ba = stan_yearly_recruits_per_mean_adult_ba, recruitment_per_capita = stan_yearly_recruits_per_capita,
                                                                      recruitment_per_ba_trans = stan_yearly_recruits_per_mean_adult_ba_trans, recruitment_per_capita_trans = stan_yearly_recruits_per_capita_trans)]
  }
  
  # collect all estimates in 3 tables
  recruitment.all = rbindlist(recruitment.list, idcol = "CTFS_plot")  
  survival.all = rbindlist(survival.list, idcol = "CTFS_plot")  
  growth.all = rbindlist(growth.list, idcol = "CTFS_plot")  
  
  # calculate median and sd of demographic rates per crown layer
  # all estimates
  summary.stats[["all_estimates"]]$recruitment_per_ba = recruitment.all[,.(mean = mean(recruitment_per_ba), sd = sd(recruitment_per_ba),
                                                                           ci_lb = t.test(recruitment_per_ba)$conf.int[1], ci_ub = t.test(recruitment_per_ba)$conf.int[2], 
                                                        demography = "recruitment_per_ba", crown_layer = NA), by = CTFS_plot]
  summary.stats[["all_estimates"]]$recruitment_per_capita = recruitment.all[,.(mean = mean(recruitment_per_capita), sd = sd(recruitment_per_capita),
                                                                               ci_lb = t.test(recruitment_per_capita)$conf.int[1], ci_ub = t.test(recruitment_per_capita)$conf.int[2],
                                                            demography = "recruitment_per_capita", crown_layer = NA), by = CTFS_plot]
  summary.stats[["all_estimates"]]$log_recruitment_per_ba = recruitment.all[,.(mean = mean(recruitment_per_ba_trans), sd = sd(recruitment_per_ba_trans),
                                                                               ci_lb = t.test(recruitment_per_ba_trans)$conf.int[1], ci_ub = t.test(recruitment_per_ba_trans)$conf.int[2],
                                                            demography = "recruitment_per_ba_trans", crown_layer = NA), by = CTFS_plot]
  summary.stats[["all_estimates"]]$log_recruitment_per_capita = recruitment.all[,.(mean = mean(recruitment_per_capita_trans), sd = sd(recruitment_per_capita_trans),
                                                                                   ci_lb = t.test(recruitment_per_capita_trans)$conf.int[1], ci_ub = t.test(recruitment_per_capita_trans)$conf.int[2],
                                                                demography = "recruitment_per_capita_trans", crown_layer = NA), by = CTFS_plot]
  
  summary.stats[["all_estimates"]]$mortality = survival.all[,.(mean = mean(1 - stan_mean_surv_rate), sd = sd(1 - stan_mean_surv_rate),
                                                               ci_lb = t.test(1 - stan_mean_surv_rate)$conf.int[1], ci_ub = t.test(1 - stan_mean_surv_rate)$conf.int[2],
                                            demography = "mortality"), by = .(CTFS_plot, crown_layer)]
  
  summary.stats[["all_estimates"]]$log_lifespan = survival.all[,.(mean = mean(stan_mean_surv_rate_trans), sd = sd(stan_mean_surv_rate_trans),
                                                                  ci_lb = t.test(stan_mean_surv_rate_trans)$conf.int[1], ci_ub = t.test(stan_mean_surv_rate_trans)$conf.int[2],
                                               demography = "log_lifespan"), by = .(CTFS_plot, crown_layer)]
  
  summary.stats[["all_estimates"]]$growth = growth.all[,.(mean = mean(stan_mean_growth_rate), sd = sd(stan_mean_growth_rate),
                                                          ci_lb = t.test(stan_mean_growth_rate)$conf.int[1], ci_ub = t.test(stan_mean_growth_rate)$conf.int[2],
                                       demography = "growth"), by = .(CTFS_plot, crown_layer)]
  
  summary.stats[["all_estimates"]]$log_growth = growth.all[,.(mean = mean(stan_mean_growth_rate_trans), sd = sd(stan_mean_growth_rate_trans),
                                                              ci_lb = t.test(stan_mean_growth_rate_trans)$conf.int[1], ci_ub = t.test(stan_mean_growth_rate_trans)$conf.int[2],
                                           demography = "log_growth"), by = .(CTFS_plot, crown_layer)]
  
  
  summary.stats[["data_estimates"]]$recruitment_per_ba = recruitment.all[,.(mean = mean(recruitment_per_ba), sd = sd(recruitment_per_ba),
                                                                            ci_lb = t.test(recruitment_per_ba)$conf.int[1], ci_ub = t.test(recruitment_per_ba)$conf.int[2],
                                                                           demography = "recruitment_per_ba", crown_layer = NA), by = CTFS_plot]
  summary.stats[["data_estimates"]]$recruitment_per_capita = recruitment.all[,.(mean = mean(recruitment_per_capita), sd = sd(recruitment_per_capita),
                                                                                ci_lb = t.test(recruitment_per_capita)$conf.int[1], ci_ub = t.test(recruitment_per_capita)$conf.int[2],
                                                                               demography = "recruitment_per_capita", crown_layer = NA), by = CTFS_plot]
  summary.stats[["data_estimates"]]$log_recruitment_per_ba = recruitment.all[,.(mean = mean(recruitment_per_ba_trans), sd = sd(recruitment_per_ba_trans),
                                                                                ci_lb = t.test(recruitment_per_ba_trans)$conf.int[1], ci_ub = t.test(recruitment_per_ba_trans)$conf.int[2],
                                                                               demography = "recruitment_per_ba_trans", crown_layer = NA), by = CTFS_plot]
  summary.stats[["data_estimates"]]$log_recruitment_per_capita = recruitment.all[,.(mean = mean(recruitment_per_capita_trans), sd = sd(recruitment_per_capita_trans),
                                                                                    ci_lb = t.test(recruitment_per_capita_trans)$conf.int[1], ci_ub = t.test(recruitment_per_capita_trans)$conf.int[2],
                                                                                   demography = "recruitment_per_capita_trans", crown_layer = NA), by = CTFS_plot]
  
  summary.stats[["data_estimates"]]$mortality = survival.all[data_available == "yes",.(mean = mean(1 - stan_mean_surv_rate), sd = sd(1 - stan_mean_surv_rate),
                                                                                       ci_lb = t.test(1 - stan_mean_surv_rate)$conf.int[1], ci_ub = t.test(1 - stan_mean_surv_rate)$conf.int[2],
                                                               demography = "mortality"), by = .(CTFS_plot, crown_layer)]
  
  summary.stats[["data_estimates"]]$log_lifespan = survival.all[data_available == "yes",.(mean = mean(stan_mean_surv_rate_trans), sd = sd(stan_mean_surv_rate_trans),
                                                                                          ci_lb = t.test(stan_mean_surv_rate_trans)$conf.int[1], ci_ub = t.test(stan_mean_surv_rate_trans)$conf.int[2],
                                                                  demography = "log_lifespan"), by = .(CTFS_plot, crown_layer)]
  
  summary.stats[["data_estimates"]]$growth = growth.all[data_available == "yes",.(mean = mean(stan_mean_growth_rate), sd = sd(stan_mean_growth_rate),
                                                                                  ci_lb = t.test(stan_mean_growth_rate)$conf.int[1], ci_ub = t.test(stan_mean_growth_rate)$conf.int[2],
                                                          demography = "growth"), by = .(CTFS_plot, crown_layer)]
  
  summary.stats[["data_estimates"]]$log_growth = growth.all[data_available == "yes",.(mean = mean(stan_mean_growth_rate_trans), sd = sd(stan_mean_growth_rate_trans),
                                                                                      ci_lb = t.test(stan_mean_growth_rate_trans)$conf.int[1], ci_ub = t.test(stan_mean_growth_rate_trans)$conf.int[2],
                                                              demography = "log_growth"), by = .(CTFS_plot, crown_layer)]
  
  
  results = list()
  results[["all_estimates"]] = rbindlist(summary.stats[["all_estimates"]], use.names= T)
  results[["data_estimates"]] = rbindlist(summary.stats[["data_estimates"]], use.names= T)
  
  # limit to the nr.of.crown.layers
  survival.all = survival.all[crown_layer <= nr.of.max.crown.layers]
  growth.all = growth.all[crown_layer <= nr.of.max.crown.layers]
  
  
  #######################################################################
  # plot all stan estimates
  
  # recruitment
  ggsave(filename = paste(c("results/density plots/density_recruitment_per_ba.png"), collapse = ""),
         height = 5, width = 12,
         plot = ggplot(data = recruitment.all, aes(x = recruitment_per_ba, color = CTFS_plot, linetype = CTFS_plot)) + 
           geom_line(size = 1, stat = "density") + 
           scale_color_manual(values = CTFS.plots$color) + 
           scale_linetype_manual(values = CTFS.plots$linetype) + 
           xlab("Yearly recruits per adult ba"))
 
  ggsave(filename = paste(c("results/density plots/density_recruitment_per_capita.png"), collapse = ""),
         height = 5, width = 12,
         plot = ggplot(data = recruitment.all, aes(x = recruitment_per_capita, color = CTFS_plot, linetype = CTFS_plot)) + 
           geom_line(size = 1, stat = "density") + 
           scale_color_manual(values = CTFS.plots$color) + 
           scale_linetype_manual(values = CTFS.plots$linetype) + 
           xlab("Yearly recruits per capita"))
  
  ggsave(filename = paste(c("results/density plots/density_recruitment_per_ba_log.png"), collapse = ""),
         height = 5, width = 12,
         plot = ggplot(data = recruitment.all, aes(x = recruitment_per_ba_trans, color = CTFS_plot, linetype = CTFS_plot)) + 
           geom_line(size = 1, stat = "density") + 
           scale_color_manual(values = CTFS.plots$color) + 
           scale_linetype_manual(values = CTFS.plots$linetype) + 
           xlab("Log yearly recruits per adult ba"))
  
  ggsave(filename = paste(c("results/density plots/density_recruitment_per_capita_log.png"), collapse = ""),
         height = 5, width = 12,
         plot = ggplot(data = recruitment.all, aes(x = recruitment_per_capita_trans, color = CTFS_plot, linetype = CTFS_plot)) + 
           geom_line(size = 1, stat = "density") + 
           scale_color_manual(values = CTFS.plots$color) + 
           scale_linetype_manual(values = CTFS.plots$linetype) + 
           xlab("Log yearly recruits per capita"))
  
  
  # survival
  ggsave(filename = paste(c("results/density plots/density_survival_all_data.png"), collapse = ""),
         height = 20, width = 12,
         plot = ggplot(data = survival.all, aes(x = stan_mean_surv_rate, color = CTFS_plot, linetype = CTFS_plot)) + 
           geom_line(size = 1, stat = "density") +
           facet_wrap(crown_layer~., ncol = 1, dir = "v", scales = "free_y") +
           scale_color_manual(values = CTFS.plots$color) + 
           scale_linetype_manual(values = CTFS.plots$linetype) + 
           xlab("Survival rate (0.9 percentile displayed)") +
           theme(legend.position = "bottom"))
  
  plot.data = survival.all[stan_mean_surv_rate_trans >= quantile(survival.all$stan_mean_surv_rate_trans, probs = 0.05) & 
                           stan_mean_surv_rate_trans <= quantile(survival.all$stan_mean_surv_rate_trans, probs = 0.95)]
  ggsave(filename = paste(c("results/density plots/density_lifespan_all_data.png"), collapse = ""),
         height = 20, width = 12,
         plot = ggplot(data = plot.data, aes(x = stan_mean_surv_rate_trans, color = CTFS_plot, linetype = CTFS_plot)) + 
           geom_line(size = 1, stat = "density") +
           facet_wrap(crown_layer~., ncol = 1, dir = "v", scales = "free_y") +
           scale_color_manual(values = CTFS.plots$color) + 
           scale_linetype_manual(values = CTFS.plots$linetype) + 
           xlab("Log lifespan (0.05 - 0.95 percentile displayed)") +
           theme(legend.position = "bottom"))
  
  
  # growth
  plot.data = growth.all[stan_mean_growth_rate >= quantile(growth.all$stan_mean_growth_rate, 0.01) &
                         stan_mean_growth_rate <= quantile(growth.all$stan_mean_growth_rate, 0.99)]
  ggsave(filename = paste(c("results/density plots/density_growth_all_data.png"), collapse = ""),
         height = 20, width = 12,
         plot = ggplot(data = plot.data , aes(x = stan_mean_growth_rate, color = CTFS_plot, linetype = CTFS_plot)) + 
           geom_line(size = 1, stat = "density") +
           facet_wrap(crown_layer~., ncol = 1, dir = "v", scales = "free_y") +
           scale_color_manual(values = CTFS.plots$color) + 
           scale_linetype_manual(values = CTFS.plots$linetype) + 
           xlab("Growth rate (0.01 - 0.99 percentile displayed)") +
           theme(legend.position = "bottom"))
  
  plot.data = growth.all[stan_mean_growth_rate_trans >= quantile(growth.all$stan_mean_growth_rate_trans, 0.01) &
                         stan_mean_growth_rate_trans <= quantile(growth.all$stan_mean_growth_rate_trans, 0.99)]
  ggsave(filename = paste(c("results/density plots/density_growth_log_all_data.png"), collapse = ""),
         height = 20, width = 12,
         plot = ggplot(data = plot.data , aes(x = stan_mean_growth_rate_trans, color = CTFS_plot, linetype = CTFS_plot)) + 
           geom_line(size = 1, stat = "density") +
           facet_wrap(crown_layer~., ncol = 1, dir = "v", scales = "free_y") +
           scale_color_manual(values = CTFS.plots$color) + 
           scale_linetype_manual(values = CTFS.plots$linetype) + 
           xlab("Log growth rate (0.01 - 0.99 percentile displayed)") +
           theme(legend.position = "bottom"))
  
  
  ################################################
  # plot only those stan estimates, that have data
  
  # survival
  ggsave(filename = paste(c("results/density plots/density_survival_available_data.png"), collapse = ""),
         height = 20, width = 12,
         plot = ggplot(data = survival.all[data_available == "yes"], aes(x = stan_mean_surv_rate, color = CTFS_plot, linetype = CTFS_plot)) + 
           geom_line(size = 1, stat = "density") +
           facet_wrap(crown_layer~., ncol = 1, dir = "v", scales = "free_y") +
           scale_color_manual(values = CTFS.plots$color) + 
           scale_linetype_manual(values = CTFS.plots$linetype) + 
           xlab("Survival rate (0.9 percentile displayed)") +
           theme(legend.position = "bottom"))
  
  plot.data = survival.all[stan_mean_surv_rate_trans >= quantile(survival.all$stan_mean_surv_rate_trans, probs = 0.05) & 
                           stan_mean_surv_rate_trans <= quantile(survival.all$stan_mean_surv_rate_trans, probs = 0.95) &
                           data_available == "yes"]
  ggsave(filename = paste(c("results/density plots/density_lifespan_available_data.png"), collapse = ""),
         height = 20, width = 12,
         plot = ggplot(data = plot.data, aes(x = stan_mean_surv_rate_trans, color = CTFS_plot, linetype = CTFS_plot)) + 
           geom_line(size = 1, stat = "density") +
           facet_wrap(crown_layer~., ncol = 1, dir = "v", scales = "free_y") +
           scale_color_manual(values = CTFS.plots$color) + 
           scale_linetype_manual(values = CTFS.plots$linetype) + 
           xlab("Log lifespan (0.05 - 0.95 percentile displayed)") +
           theme(legend.position = "bottom"))
  
  
  # growth
  plot.data = growth.all[stan_mean_growth_rate >= quantile(growth.all$stan_mean_growth_rate, 0.01) &
                         stan_mean_growth_rate <= quantile(growth.all$stan_mean_growth_rate, 0.99) &
                         data_available == "yes"]
  ggsave(filename = paste(c("results/density plots/density_growth_available_data.png"), collapse = ""),
         height = 20, width = 12,
         plot = ggplot(data = plot.data , aes(x = stan_mean_growth_rate, color = CTFS_plot, linetype = CTFS_plot)) + 
           geom_line(size = 1, stat = "density") +
           facet_wrap(crown_layer~., ncol = 1, dir = "v", scales = "free_y") +
           scale_color_manual(values = CTFS.plots$color) + 
           scale_linetype_manual(values = CTFS.plots$linetype) + 
           xlab("Growth rate (0.01 - 0.99 percentile displayed)") +
           theme(legend.position = "bottom"))
  
  plot.data = growth.all[stan_mean_growth_rate_trans >= quantile(growth.all$stan_mean_growth_rate_trans, 0.01) &
                         stan_mean_growth_rate_trans <= quantile(growth.all$stan_mean_growth_rate_trans, 0.99) &
                         data_available == "yes"]
  ggsave(filename = paste(c("results/density plots/density_growth_log_available_data.png"), collapse = ""),
         height = 20, width = 12,
         plot = ggplot(data = plot.data , aes(x = stan_mean_growth_rate_trans, color = CTFS_plot, linetype = CTFS_plot)) + 
           geom_line(size = 1, stat = "density") +
           facet_wrap(crown_layer~., ncol = 1, dir = "v", scales = "free_y") +
           scale_color_manual(values = CTFS.plots$color) + 
           scale_linetype_manual(values = CTFS.plots$linetype) + 
           xlab("Log growth rate (0.01 - 0.99 percentile displayed)") +
           theme(legend.position = "bottom"))
  
  
 # save summary results
  return(results)
}


#############################################################
# plot means and sds of the distributions of the recruitment, survival and growth estimates
# separated by all stan estimates or only those estimates that are backed by data
plot.distribution.summaries = function(CTFS.plots, distribution.summaries, nr.of.max.crown.layers){
  
  # all data
  plot.data = distribution.summaries$all_estimates[demography %in% c("recruitment_per_ba_trans", "recruitment_per_capita_trans", "log_lifespan","log_growth")]
  plot.data = plot.data[is.na(crown_layer) | crown_layer <= nr.of.max.crown.layers]
  
  # need to add invisible points to make the y scale similar for all growth and similar for all survival plots
  helper.df = plot.data[,.(max_mean = max(mean), min_mean = min(mean), max_sd = max(sd), min_sd = min(sd)), by = .(demography)]
  plot.data = merge(plot.data, helper.df, by = "demography")
  
  ggsave(filename = paste(c("results/density plots/distribution_means_all_data.png"), collapse = ""),
         height = 8, width = 10,
         plot = ggplot(data = plot.data) +
           geom_point(aes(x = CTFS_plot, y = mean, fill = CTFS_plot), pch = 21, size = 3) +
           geom_linerange(aes(x = CTFS_plot, ymin =  min_mean, ymax = max_mean), alpha = 0)  +
           facet_wrap(demography ~ crown_layer, ncol = 4, scales = "free") +
           theme_bw() +
           theme(legend.position = "none", panel.border=element_rect(fill = NA), axis.text.x = element_text(angle = 90, hjust = 1)) +
           ggtitle("Mean values of demographic rates "))
  
  ggsave(filename = paste(c("results/density plots/distribution_sds_all_data.png"), collapse = ""),
         height = 8, width = 10,
         plot = ggplot(data = plot.data) +
           geom_point(aes(x = CTFS_plot, y = sd, fill = CTFS_plot), pch = 21, size = 3) +
           geom_linerange(aes(x = CTFS_plot, ymin =  min_sd, ymax = max_sd), alpha = 0)  +
           facet_wrap(demography ~ crown_layer, ncol = 4, scales = "free") +
           theme_bw() +
           theme(legend.position = "none", panel.border=element_rect(fill = NA), axis.text.x = element_text(angle = 90, hjust = 1)) +
           ggtitle("SD values of demographic rates "))
  
  
  # only those estimates that came from available data
  plot.data = distribution.summaries$data_estimates[demography %in% c("recruitment_per_ba_trans", "recruitment_per_capita_trans", "log_lifespan","log_growth")]
  plot.data = plot.data[is.na(crown_layer) | crown_layer <= nr.of.max.crown.layers]
  
  # need to add invisible points to make the y scale similar for all growth and similar for all survival plots
  helper.df = plot.data[,.(max_mean = max(mean), min_mean = min(mean), max_sd = max(sd), min_sd = min(sd)), by = .(demography)]
  plot.data = merge(plot.data, helper.df, by = "demography")
  
  ggsave(filename = paste(c("results/density plots/distribution_means_available_data.png"), collapse = ""),
         height = 8, width = 10,
         plot = ggplot(data = plot.data) +
           geom_point(aes(x = CTFS_plot, y = mean, fill = CTFS_plot), pch = 21, size = 3) +
           geom_linerange(aes(x = CTFS_plot, ymin =  min_mean, ymax = max_mean), alpha = 0)  +
           facet_wrap(demography ~ crown_layer, ncol = 4, scales = "free") +
           theme_bw() +
           theme(legend.position = "none", panel.border=element_rect(fill = NA), axis.text.x = element_text(angle = 90, hjust = 1)) +
           ggtitle("Mean values of demographic rates "))
  
  ggsave(filename = paste(c("results/density plots/distribution_sds_available_data.png"), collapse = ""),
         height = 8, width = 10,
         plot = ggplot(data = plot.data) +
           geom_point(aes(x = CTFS_plot, y = sd, fill = CTFS_plot), pch = 21, size = 3) +
           geom_linerange(aes(x = CTFS_plot, ymin =  min_sd, ymax = max_sd), alpha = 0)  +
           facet_wrap(demography ~ crown_layer, ncol = 4, scales = "free") +
           theme_bw() +
           theme(legend.position = "none", panel.border=element_rect(fill = NA), axis.text.x = element_text(angle = 90, hjust = 1)) +
           ggtitle("SD values of demographic rates "))
}

#############################################################
# create correlation plots of all stan estimates of all plots
create.correlation.plot.of.all.stan.estimates = function(CTFS.plots, nr.of.max.crown.layers){
  
  # function to calculate se for cor.test
  cor.test.plus <- function(x) {
    list(x, Standard.Error = unname(sqrt((1 - x$estimate^2)/x$parameter)))}
  
  # store results in a list
  nr.of.crown.layers = c()
  recruitment.estimates = list()
  survival.estimates = list()
  growth.estimates = list()
  
  recruitment.weights = list()
  survival.weights = list()
  growth.weights = list()
  
  recruitment.n.obs = list()
  survival.n.obs = list()
  growth.n.obs = list()
  
  # get and transform the stan estimates + maximum nr of canopy layers
  for(i in CTFS.plots$CTFS_plot){
    recruitment.estimates[[i]] = data.table(readRDS(paste(c("intermediate output/", i, "/",i, "_stan_recruitment.rds"), collapse = "")))
    survival.estimates[[i]] = data.table(readRDS(paste(c("intermediate output/", i, "/",i, "_stan_survival.rds"), collapse = "")))
    growth.estimates[[i]] = data.table(readRDS(paste(c("intermediate output/", i, "/",i, "_stan_growth.rds"), collapse = "")))
    
    recruitment.estimates[[i]]$log_yearly_recruits_per_ba = log(recruitment.estimates[[i]]$stan_yearly_recruits_per_mean_adult_ba)
    recruitment.estimates[[i]]$log_yearly_recruits_per_capita = log(recruitment.estimates[[i]]$stan_mean_nr_recruits /  recruitment.estimates[[i]]$mean_nr_ind_all)
    survival.estimates[[i]]$log_lifespan = log(1 / (1 - survival.estimates[[i]]$stan_mean_surv_rate))
    growth.estimates[[i]]$log_growth = log(growth.estimates[[i]]$stan_mean_growth_rate + abs(min(growth.estimates[[i]]$stan_mean_growth_rate)) + 0.001)
    
    # only keep those for which data is available
    survival.data = data.table(readRDS(paste(c("intermediate output/", i, "/",i, "_balanced_survival.rds"), collapse = "")))
    growth.data = data.table(readRDS(paste(c("intermediate output/", i, "/",i, "_balanced_growth.rds"), collapse = "")))
    
    survival.data = survival.data[,.(n = length(alive)), by = .(sp, crown_layer)]
    growth.data = growth.data[,.(n = length(dinc_per_year)), by = .(sp, crown_layer)]
    
    survival.estimates[[i]] = merge(survival.data, survival.estimates[[i]], by =c("sp","crown_layer"))
    growth.estimates[[i]] = merge(growth.data, growth.estimates[[i]], by =c("sp","crown_layer"))
    
    # get number of crown layers
    nr.of.crown.layers = c(nr.of.crown.layers, max(growth.estimates[[i]]$crown_layer))
    
    # create list of values and a list of weights
    recruitment.weights[[i]] = recruitment.estimates[[i]][,.(sp, stan_weights_for_nr_recruits)]
    
    survival.weights[[i]] = dcast(survival.estimates[[i]], sp ~ crown_layer, value.var = "stan_weight_surv_rate")
    names(survival.weights[[i]])[2:ncol(survival.weights[[i]])] = paste("weight_surv",1:(ncol(survival.weights[[i]])- 1), sep = "")
    growth.weights[[i]] = dcast(growth.estimates[[i]], sp ~ crown_layer, value.var = "stan_weight_growth_rate")
    names(growth.weights[[i]])[2:ncol(growth.weights[[i]])] = paste("weight_growth",1:(ncol(growth.weights[[i]])- 1), sep = "")
    
    recruitment.n.obs[[i]] = recruitment.estimates[[i]][,.(sp, n = 1)]
    survival.n.obs[[i]] = dcast(survival.estimates[[i]], sp ~ crown_layer, value.var = "n")
    survival.n.obs[[i]][is.na(survival.n.obs[[i]])] = 0
    names(survival.n.obs[[i]])[2:ncol(survival.n.obs[[i]])] = paste("n_surv",1:(ncol(survival.n.obs[[i]])- 1), sep = "")
    growth.n.obs[[i]] = dcast(growth.estimates[[i]], sp ~ crown_layer, value.var = "n")
    growth.n.obs[[i]][is.na(growth.n.obs[[i]])] = 0
    names(growth.n.obs[[i]])[2:ncol(growth.n.obs[[i]])] = paste("n_growth",1:(ncol(growth.n.obs[[i]])- 1), sep = "")
    
    recruitment.estimates[[i]] = recruitment.estimates[[i]][,.(sp, log_yearly_recruits_per_ba, log_yearly_recruits_per_capita)]
    survival.estimates[[i]] = dcast(survival.estimates[[i]], sp ~ crown_layer, value.var = "log_lifespan")
    names(survival.estimates[[i]])[2:ncol(survival.estimates[[i]])] = paste("surv",1:(ncol(survival.estimates[[i]])- 1), sep = "")
    growth.estimates[[i]] = dcast(growth.estimates[[i]], sp ~ crown_layer, value.var = "log_growth")
    names(growth.estimates[[i]])[2:ncol(growth.estimates[[i]])] = paste("growth",1:(ncol(growth.estimates[[i]])- 1), sep = "")}
  
  # get maximum number of canopy layers
  nr.of.crown.layers = max(nr.of.crown.layers)

  # calculate all weighted and unweighted correlations (where every sp is weighted equally)
  all.correlations = data.frame("CTFS_plot" = NA, "demography1" = NA, "demography2" = NA, 
                                "cor_weighted" = NA, "se_weighted" = NA, "cor_unweighted" = NA, "se_unweighted" = NA)[0,]
  
  for(i in CTFS.plots$CTFS_plot){
    
    # all growth correlations
    for(j in 1:(ncol(growth.estimates[[i]]) -2)){ # first layer to compare (starts at second column)
      for(k in (j+1):(ncol(growth.estimates[[i]])-1)){ # second layer to compare (starts at third column)
        
        # subset the estimates and sample size weights
        growth.temp = growth.estimates[[i]][, !"sp"]
        growth.temp = growth.temp[, c(..j, ..k)]
        growth.weights.temp = growth.weights[[i]][, !"sp"]
        growth.weights.temp = growth.weights.temp[, c(..j, ..k)]
        
        growth.weights.temp = growth.weights.temp[complete.cases(growth.temp),]
        growth.temp = growth.temp[complete.cases(growth.temp),]
        growth.weights.mean.temp = (growth.weights.temp[[1]] + growth.weights.temp[[2]]) / 2
        
        # weighted correlation
        weighted.cor.temp = wtd.cor(x = growth.temp[,1],
                                    y = growth.temp[,2],
                                    weight = growth.weights.mean.temp)
        
        # unweighted correlation
        unweighted.cor.temp = cor.test.plus(cor.test(growth.temp[[1]],
                                                     growth.temp[[2]]))
        
        all.correlations = 
          rbind(all.correlations,
                data.frame("CTFS_plot" = i, 
                           "demography1" = paste("growth_",j, sep = ""), 
                           "demography2" = paste("growth_",k, sep = ""), 
                           "cor_weighted" = weighted.cor.temp[,"correlation"],
                           "se_weighted" = weighted.cor.temp[,"std.err"],
                           "cor_unweighted" = unweighted.cor.temp[[1]]$estimate,
                           "se_unweighted" = unweighted.cor.temp$Standard.Error))
      }}
    
    # all survival correlations
    for(j in 1:(ncol(survival.estimates[[i]]) -2)){ # first layer to compare (starts at second column)
      for(k in (j+1):(ncol(survival.estimates[[i]])-1)){ # second layer to compare (starts at third column)
        
        # subset the estimates and sample size weights
        survival.temp = survival.estimates[[i]][, !"sp"]
        survival.temp = survival.temp[, c(..j, ..k)]
        survival.weights.temp = survival.weights[[i]][, !"sp"]
        survival.weights.temp = survival.weights.temp[, c(..j, ..k)]
        
        survival.weights.temp = survival.weights.temp[complete.cases(survival.temp),]
        survival.temp = survival.temp[complete.cases(survival.temp),]
        survival.weights.mean.temp = (survival.weights.temp[[1]] + survival.weights.temp[[2]] ) / 2
        
        # weighted correlation
        weighted.cor.temp = wtd.cor(x = survival.temp[,1],
                                    y = survival.temp[,2],
                                    weight = survival.weights.mean.temp)
        
        # unweighted correlation
        unweighted.cor.temp = cor.test.plus(cor.test(survival.temp[[1]],
                                                     survival.temp[[2]]))
        
        all.correlations = 
          rbind(all.correlations,
                data.frame("CTFS_plot" = i, 
                           "demography1" = paste("survival_",j, sep = ""), 
                           "demography2" = paste("survival_",k, sep = ""), 
                           "cor_weighted" = weighted.cor.temp[,"correlation"],
                           "se_weighted" = weighted.cor.temp[,"std.err"],
                           "cor_unweighted" = unweighted.cor.temp[[1]]$estimate,
                           "se_unweighted" = unweighted.cor.temp$Standard.Error))
      }}
    
    # all growth - survival correlations
    for(j in 1:(ncol(growth.estimates[[i]]) -1)){ # growth layers
      for(k in 1:(ncol(growth.estimates[[i]]) -1)){ # survival layers
        
        # subset the estimates and sample size weights
        growth.survival.merged.temp = merge(
          growth.estimates[[i]], survival.estimates[[i]], by = c("sp"))
        growth.survival.merged.temp = growth.survival.merged.temp[,!"sp"]
        growth.survival.weights.merged.temp = merge(
          growth.weights[[i]], survival.weights[[i]], by = c("sp"))
        growth.survival.weights.merged.temp = growth.survival.weights.merged.temp[,!"sp"]
        
        survival.col = (ncol(growth.survival.merged.temp) / 2) + k
        
        growth.survival.temp = cbind(growth.survival.merged.temp[,c(..j)],
                                     growth.survival.merged.temp[,c(..survival.col)])
        
        growth.survival.weights.temp = cbind(growth.survival.weights.merged.temp[,c(..j)],
                                           growth.survival.weights.merged.temp[,c(..survival.col)])
        growth.survival.weights.temp = (growth.survival.weights.temp[[1]] + growth.survival.weights.temp[[2]]) / 2
        growth.survival.weights.temp = growth.survival.weights.temp[complete.cases(growth.survival.temp)]
        growth.survival.temp = growth.survival.temp[complete.cases(growth.survival.temp),]
        
        # weighted correlation
        weighted.cor.temp = wtd.cor(x = growth.survival.temp[[1]],
                                    y = growth.survival.temp[[2]],
                                    weight = growth.survival.weights.temp)
        
        # unweighted correlation
        unweighted.cor.temp = cor.test.plus(cor.test(growth.survival.temp[[1]],
                                                     growth.survival.temp[[2]]))
        
        all.correlations = 
          rbind(all.correlations,
                data.frame("CTFS_plot" = i, 
                           "demography1" = paste("growth_",j, sep = ""), 
                           "demography2" = paste("survival_",k, sep = ""), 
                           "cor_weighted" = weighted.cor.temp[,"correlation"],
                           "se_weighted" = weighted.cor.temp[,"std.err"],
                           "cor_unweighted" = unweighted.cor.temp[[1]]$estimate,
                           "se_unweighted" = unweighted.cor.temp$Standard.Error))
      }}
    
    # all growth/survival - recruitment correlations
    for(j in 1:(ncol(growth.estimates[[i]]) -2)){ # first layer to compare (starts at second column)

        # merge growth/survival and recruitment data
        growth.recruitment.merged = merge(growth.estimates[[i]], recruitment.estimates[[i]], by = "sp")[,!"sp"]
        growth.recruitment.weights.merged = merge(growth.weights[[i]],recruitment.estimates[[i]], by = "sp")[,!"sp"]
        survival.recruitment.merged = merge(survival.estimates[[i]], recruitment.estimates[[i]], by = "sp")[,!"sp"]
        survival.recruitment.weights.merged = merge(survival.weights[[i]], recruitment.estimates[[i]], by = "sp")[,!"sp"]
        
        growth.survival.recruitment.merged = merge(growth.estimates[[i]], survival.estimates[[i]], by = "sp")
        growth.survival.recruitment.merged = merge(growth.survival.recruitment.merged, recruitment.estimates[[i]], by = "sp")[,!"sp"]
        growth.survival.recruitment.weights.merged = merge(growth.weights[[i]], survival.weights[[i]], by = "sp")
        growth.survival.recruitment.weights.merged = merge(growth.survival.recruitment.weights.merged, recruitment.estimates[[i]], by = "sp")
        growth.survival.recruitment.weights.merged = growth.survival.recruitment.weights.merged[,!"sp"]
        
        # get the cols of recruitment
        recruitment.col.ba = (ncol(growth.estimates[[i]])-1) + 1
        recruitment.col.capita = (ncol(growth.estimates[[i]])-1) + 2
        survival.col = (ncol(growth.estimates[[i]])-1) + j
        
        recruitment.col.ba2 = ncol(growth.survival.recruitment.merged) - 1 # for the growth/survival vs recruitment
        recruitment.col.capita2 = ncol(growth.survival.recruitment.merged) # for the growth/survival vs recruitment
        
        # get only those columns whose correlation should be tested
        growth.recruitment.merged.ba = cbind(growth.recruitment.merged[,..j], growth.recruitment.merged[,..recruitment.col.ba])
        growth.recruitment.weights.merged.ba = cbind(growth.recruitment.weights.merged[,..j], growth.recruitment.merged[,..recruitment.col.ba])
        growth.recruitment.merged.capita = cbind(growth.recruitment.merged[,..j], growth.recruitment.merged[,..recruitment.col.capita])
        growth.recruitment.weights.merged.capita = cbind(growth.recruitment.weights.merged[,..j], growth.recruitment.merged[,..recruitment.col.capita])
        
        survival.recruitment.merged.ba = cbind(survival.recruitment.merged[,..j], survival.recruitment.merged[,..recruitment.col.ba])
        survival.recruitment.weights.merged.ba = cbind(survival.recruitment.weights.merged[,..j], survival.recruitment.merged[,..recruitment.col.ba])
        survival.recruitment.merged.capita = cbind(survival.recruitment.merged[,..j], survival.recruitment.merged[,..recruitment.col.capita])
        survival.recruitment.weights.merged.capita = cbind(survival.recruitment.weights.merged[,..j], survival.recruitment.merged[,..recruitment.col.capita])
        
        growth.survival.recruitment.merged.ba = cbind(growth.survival.recruitment.merged[,..j],
                                                      growth.survival.recruitment.merged[,..survival.col],
                                                      growth.survival.recruitment.merged[,..recruitment.col.ba2])
        growth.survival.recruitment.weights.merged.ba = cbind(growth.survival.recruitment.weights.merged[,..j],
                                                            growth.survival.recruitment.weights.merged[,..survival.col],
                                                            growth.survival.recruitment.weights.merged[,..recruitment.col.ba2])
        
        growth.survival.recruitment.merged.capita = cbind(growth.survival.recruitment.merged[,..j],
                                                      growth.survival.recruitment.merged[,..survival.col],
                                                      growth.survival.recruitment.merged[,..recruitment.col.capita2])
        growth.survival.recruitment.weights.merged.capita = cbind(growth.survival.recruitment.weights.merged[,..j],
                                                            growth.survival.recruitment.weights.merged[,..survival.col],
                                                            growth.survival.recruitment.weights.merged[,..recruitment.col.capita2])
        
        # only keep complete cases
        growth.recruitment.weights.merged.ba = growth.recruitment.weights.merged.ba[complete.cases(growth.recruitment.merged.ba)]
        growth.recruitment.merged.ba = growth.recruitment.merged.ba[complete.cases(growth.recruitment.merged.ba)]
        growth.recruitment.weights.merged.capita = growth.recruitment.weights.merged.capita[complete.cases(growth.recruitment.merged.capita)]
        growth.recruitment.merged.capita = growth.recruitment.merged.capita[complete.cases(growth.recruitment.merged.capita)]
        
        survival.recruitment.weights.merged.ba = survival.recruitment.weights.merged.ba[complete.cases(survival.recruitment.merged.ba)]
        survival.recruitment.merged.ba = survival.recruitment.merged.ba[complete.cases(survival.recruitment.merged.ba)]
        survival.recruitment.weights.merged.capita = survival.recruitment.weights.merged.capita[complete.cases(survival.recruitment.merged.capita)]
        survival.recruitment.merged.capita = survival.recruitment.merged.capita[complete.cases(survival.recruitment.merged.capita)]
        
        growth.survival.recruitment.weights.merged.ba = growth.survival.recruitment.weights.merged.ba[complete.cases(growth.survival.recruitment.merged.ba)]
        growth.survival.recruitment.merged.ba = growth.survival.recruitment.merged.ba[complete.cases(growth.survival.recruitment.merged.ba)]
        growth.survival.recruitment.weights.merged.capita = growth.survival.recruitment.weights.merged.capita[complete.cases(growth.survival.recruitment.merged.capita)]
        growth.survival.recruitment.merged.capita = growth.survival.recruitment.merged.capita[complete.cases(growth.survival.recruitment.merged.capita)]
        
        # calculate weights
        growth.recruitment.weights.ba = growth.recruitment.weights.merged.ba[[1]]
        growth.recruitment.weights.capita = growth.recruitment.weights.merged.capita[[1]]
        survival.recruitment.weights.ba = survival.recruitment.weights.merged.ba[[1]]
        survival.recruitment.weights.capita = survival.recruitment.weights.merged.capita[[1]]
        
        growth.survival.recruitment.weights.ba = (growth.survival.recruitment.weights.merged.ba[[1]] + growth.survival.recruitment.weights.merged.ba[[2]]) / 2
        growth.survival.recruitment.weights.capita = (growth.survival.recruitment.weights.merged.capita[[1]]/100) + (growth.survival.recruitment.weights.merged.capita[[2]]) / 2
        
        # calculate ranks for joint growth/survival versus recruitment trade-off
        growth.survival.recruitment.merged.ba$growth_rank = rank(growth.survival.recruitment.merged.ba[[1]])
        growth.survival.recruitment.merged.ba$survival_rank = rank(growth.survival.recruitment.merged.ba[[2]])
        growth.survival.recruitment.merged.ba$joint_rank = (growth.survival.recruitment.merged.ba$growth_rank + growth.survival.recruitment.merged.ba$survival_rank) / 2
        
        growth.survival.recruitment.merged.capita$growth_rank = rank(growth.survival.recruitment.merged.capita[[1]])
        growth.survival.recruitment.merged.capita$survival_rank = rank(growth.survival.recruitment.merged.capita[[2]])
        growth.survival.recruitment.merged.capita$joint_rank = (growth.survival.recruitment.merged.capita$growth_rank + growth.survival.recruitment.merged.capita$survival_rank) / 2
        
        # weighted correlation
        weighted.cor.growth.ba = wtd.cor(x = growth.recruitment.merged.ba[[1]],
                                         y = growth.recruitment.merged.ba[[2]],
                                         weight = growth.recruitment.weights.ba)
        weighted.cor.growth.capita = wtd.cor(x = growth.recruitment.merged.capita[[1]],
                                         y = growth.recruitment.merged.capita[[2]],
                                         weight = growth.recruitment.weights.capita)
        
        weighted.cor.survival.ba = wtd.cor(x = survival.recruitment.merged.ba[[1]],
                                         y = survival.recruitment.merged.ba[[2]],
                                         weight = survival.recruitment.weights.ba)
        weighted.cor.survival.capita = wtd.cor(x = survival.recruitment.merged.capita[[1]],
                                             y = survival.recruitment.merged.capita[[2]],
                                             weight = survival.recruitment.weights.capita)
        
        weighted.cor.growth.survival.ba = wtd.cor(x = growth.survival.recruitment.merged.ba$joint_rank,
                                                  y = growth.survival.recruitment.merged.ba$log_yearly_recruits_per_ba,
                                                  weight = growth.survival.recruitment.weights.ba)
        weighted.cor.growth.survival.capita = wtd.cor(x = growth.survival.recruitment.merged.capita$joint_rank,
                                                  y = growth.survival.recruitment.merged.capita$log_yearly_recruits_per_capita,
                                                  weight = growth.survival.recruitment.weights.capita)
        
        # unweighted correlation
        unweighted.cor.growth.ba = cor.test.plus(cor.test(growth.recruitment.merged.ba[[1]],
                                                               growth.recruitment.merged.ba[[2]]))
        unweighted.cor.growth.capita = cor.test.plus(cor.test(growth.recruitment.merged.capita[[1]],
                                                                   growth.recruitment.merged.capita[[2]]))
        
        unweighted.cor.survival.ba = cor.test.plus(cor.test(survival.recruitment.merged.ba[[1]],
                                                               survival.recruitment.merged.ba[[2]]))
        unweighted.cor.survival.capita = cor.test.plus(cor.test(survival.recruitment.merged.capita[[1]],
                                                                   survival.recruitment.merged.capita[[2]]))
        
        unweighted.cor.growth.survival.ba = cor.test.plus(cor.test(growth.survival.recruitment.merged.ba$joint_rank,
                                                                        growth.survival.recruitment.merged.ba$log_yearly_recruits_per_ba))
        unweighted.cor.growth.survival.capita = cor.test.plus(cor.test(growth.survival.recruitment.merged.capita$joint_rank,
                                                                   growth.survival.recruitment.merged.capita$log_yearly_recruits_per_capita))
        
        # fill results data frame
        all.correlations = 
          rbind(all.correlations,
                data.frame("CTFS_plot" = i, 
                           "demography1" = paste(c("growth", "growth","survival","survival","joint_growth_survival", "joint_growth_survival"),j, sep = "_"),
                           "demography2" = c("recruitment_ba", "recruitment_capita", "recruitment_ba", "recruitment_capita", "recruitment_ba", "recruitment_capita"), 
                           "cor_weighted" = c(weighted.cor.growth.ba[,"correlation"], weighted.cor.growth.capita[,"correlation"],
                                              weighted.cor.survival.ba[,"correlation"], weighted.cor.survival.capita[,"correlation"],
                                              weighted.cor.growth.survival.ba[,"correlation"], weighted.cor.growth.survival.capita[,"correlation"]),
                           "se_weighted" = c(weighted.cor.growth.ba[,"std.err"], weighted.cor.growth.capita[,"std.err"],
                                             weighted.cor.survival.ba[,"std.err"], weighted.cor.survival.capita[,"std.err"],
                                             weighted.cor.growth.survival.ba[,"std.err"], weighted.cor.growth.survival.capita[,"std.err"]),
                           "cor_unweighted" = c(unweighted.cor.growth.ba[[1]]$estimate, unweighted.cor.growth.capita[[1]]$estimate,
                                                unweighted.cor.survival.ba[[1]]$estimate, unweighted.cor.survival.capita[[1]]$estimate,
                                                unweighted.cor.growth.survival.ba[[1]]$estimate, unweighted.cor.growth.survival.capita[[1]]$estimate),
                           "se_unweighted" = c(unweighted.cor.growth.ba$Standard.Error, unweighted.cor.growth.capita$Standard.Error,
                                               unweighted.cor.survival.ba$Standard.Error, unweighted.cor.survival.capita$Standard.Error,
                                               unweighted.cor.growth.survival.ba$Standard.Error, unweighted.cor.growth.survival.capita$Standard.Error)))

      }}
    
  # create data.table
  all.correlations = data.table(all.correlations)
  
  # plotting growth-growth - unweighted
  plot.subset = all.correlations[demography1 %in% paste("growth", 1:nr.of.max.crown.layers, sep = "_") &
                                          demography2 %in% paste("growth", 1:nr.of.max.crown.layers, sep = "_")]
  
  ggplot(data = plot.subset) +
    geom_bar(aes( x = CTFS_plot,
                  y = cor_unweighted,
                  fill = CTFS_plot), stat = "identity", alpha = 0.4) +
    
    geom_linerange(aes( x = CTFS_plot,
                        ymin = cor_unweighted - (1.96 * se_unweighted), 
                        ymax = cor_unweighted + (1.96 * se_unweighted),
                        color = CTFS_plot), size = 2) +
    geom_hline(yintercept = 0) + 
    facet_grid(demography1 ~  demography2, switch = "y") +
    scale_color_manual(values = CTFS.plots$color) +
    scale_fill_manual(values = CTFS.plots$color) +
    ylim(c(-1,1)) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
    theme_bw() +
    ggtitle("Unweighted correlation growth-growth")
  
  ggsave("results/all correlations/cor_growth_growth_unweighted.svg", 
         height = nr.of.max.crown.layers * 2,
         width = nr.of.max.crown.layers * 2)
  
  # plotting growth-growth - weighted
  plot.subset = all.correlations[demography1 %in% paste("growth", 1:nr.of.max.crown.layers, sep = "_") &
                                            demography2 %in% paste("growth", 1:nr.of.max.crown.layers, sep = "_")]
  
  ggplot(data = plot.subset) +
    geom_bar(aes( x = CTFS_plot,
                  y = cor_weighted,
                  fill = CTFS_plot), stat = "identity", alpha = 0.4) +
    
    geom_linerange(aes( x = CTFS_plot,
                        ymin = cor_weighted - (1.96 * se_weighted), 
                        ymax = cor_weighted + (1.96 * se_weighted),
                        color = CTFS_plot), size = 2) +
    geom_hline(yintercept = 0) + 
    facet_grid(demography1 ~  demography2, switch = "y") +
    scale_color_manual(values = CTFS.plots$color) +
    scale_fill_manual(values = CTFS.plots$color) +
    ylim(c(-1,1)) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
    theme_bw() +
    ggtitle("Weighted correlation growth-growth")
  
  ggsave("results/all correlations/cor_growth_growth_weighted.svg", 
         height = nr.of.max.crown.layers * 2,
         width = nr.of.max.crown.layers * 2)
  
  # plotting survival-survival - unweighted
  plot.subset = all.correlations[demography1 %in% paste("survival", 1:nr.of.max.crown.layers, sep = "_") &
                                   demography2 %in% paste("survival", 1:nr.of.max.crown.layers, sep = "_")]
  
  ggplot(data = plot.subset) +
    geom_bar(aes( x = CTFS_plot,
                  y = cor_unweighted,
                  fill = CTFS_plot), stat = "identity", alpha = 0.4) +
    
    geom_linerange(aes( x = CTFS_plot,
                        ymin = cor_unweighted - (1.96 * se_unweighted), 
                        ymax = cor_unweighted + (1.96 * se_unweighted),
                        color = CTFS_plot), size = 2) +
    geom_hline(yintercept = 0) + 
    facet_grid(demography1 ~  demography2, switch = "y") +
    scale_color_manual(values = CTFS.plots$color) +
    scale_fill_manual(values = CTFS.plots$color) +
    ylim(c(-1,1)) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
    theme_bw() +
    ggtitle("Unweighted correlation survival-survival")
  
  ggsave("results/all correlations/cor_survival_survival_unweighted.svg", 
         height = nr.of.max.crown.layers * 2,
         width = nr.of.max.crown.layers * 2)
  
  # plotting survival-survival - weighted
  plot.subset = all.correlations[demography1 %in% paste("survival", 1:nr.of.max.crown.layers, sep = "_") &
                                   demography2 %in% paste("survival", 1:nr.of.max.crown.layers, sep = "_")]
  
  ggplot(data = plot.subset) +
    geom_bar(aes( x = CTFS_plot,
                  y = cor_weighted,
                  fill = CTFS_plot), stat = "identity", alpha = 0.4) +
    
    geom_linerange(aes( x = CTFS_plot,
                        ymin = cor_weighted - (1.96 * se_weighted), 
                        ymax = cor_weighted + (1.96 * se_weighted),
                        color = CTFS_plot), size = 2) +
    geom_hline(yintercept = 0) + 
    facet_grid(demography1 ~  demography2, switch = "y") +
    scale_color_manual(values = CTFS.plots$color) +
    scale_fill_manual(values = CTFS.plots$color) +
    ylim(c(-1,1)) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
    theme_bw() +
    ggtitle("Weighted correlation survival-survival")
  
  ggsave("results/all correlations/cor_survival_survival_weighted.svg", 
         height = nr.of.max.crown.layers * 2,
         width = nr.of.max.crown.layers * 2)
  
  # plotting growth-survival - weighted
  plot.subset = all.correlations[demography1 %in% paste("growth", 1:nr.of.max.crown.layers, sep = "_") &
                                   demography2 %in% paste("survival", 1:nr.of.max.crown.layers, sep = "_")]
  plot.subset$demography1 = factor(plot.subset$demography1, levels = unique(plot.subset$demography1))
  plot.subset$demography2 = factor(plot.subset$demography2, levels = unique(plot.subset$demography2))
  
  ggplot(data = plot.subset) +
    geom_bar(aes( x = CTFS_plot,
                  y = cor_weighted,
                  fill = CTFS_plot), stat = "identity", alpha = 0.4) +
    
    geom_linerange(aes( x = CTFS_plot,
                        ymin = cor_weighted - (1.96 * se_weighted), 
                        ymax = cor_weighted + (1.96 * se_weighted),
                        color = CTFS_plot), size = 2) +
    geom_hline(yintercept = 0) + 
    facet_grid(demography1 ~  demography2, switch = "y") +
    scale_color_manual(values = CTFS.plots$color) +
    scale_fill_manual(values = CTFS.plots$color) +
    ylim(c(-1,1)) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
    theme_bw() +
    ggtitle("Unweighted correlation growth-survival")
  
  ggsave("results/all correlations/cor_growth_survival_weighted.svg", 
         height = nr.of.max.crown.layers * 2,
         width = nr.of.max.crown.layers * 2)
  
  # plotting growth-survival - unweighted
  plot.subset = all.correlations[demography1 %in% paste("growth", 1:nr.of.max.crown.layers, sep = "_") &
                                   demography2 %in% paste("survival", 1:nr.of.max.crown.layers, sep = "_")]
  plot.subset$demography1 = factor(plot.subset$demography1, levels = unique(plot.subset$demography1))
  plot.subset$demography2 = factor(plot.subset$demography2, levels = unique(plot.subset$demography2))
  
  ggplot(data = plot.subset) +
    geom_bar(aes( x = CTFS_plot,
                  y = cor_unweighted,
                  fill = CTFS_plot), stat = "identity", alpha = 0.4) +
    
    geom_linerange(aes( x = CTFS_plot,
                        ymin = cor_unweighted - (1.96 * se_unweighted), 
                        ymax = cor_unweighted + (1.96 * se_unweighted),
                        color = CTFS_plot), size = 2) +
    geom_hline(yintercept = 0) + 
    facet_grid(demography1 ~  demography2, switch = "y") +
    scale_color_manual(values = CTFS.plots$color) +
    scale_fill_manual(values = CTFS.plots$color) +
    ylim(c(-1,1)) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
    theme_bw() +
    ggtitle("Unweighted correlation growth-survival")
  
  ggsave("results/all correlations/cor_growth_survival_unweighted.svg", 
         height = nr.of.max.crown.layers * 2,
         width = nr.of.max.crown.layers * 2)
  
  
  # plotting growth-recruitment - weighted
  plot.subset = all.correlations[demography1 %in% paste("growth", 1:nr.of.max.crown.layers, sep = "_") &
                                   demography2 %in% c("recruitment_ba", "recruitment_capita")]
  
  ggplot(data = plot.subset) +
    geom_bar(aes( x = CTFS_plot,
                  y = cor_weighted,
                  fill = CTFS_plot), stat = "identity", alpha = 0.4) +
    
    geom_linerange(aes( x = CTFS_plot,
                        ymin = cor_weighted - (1.96 * se_weighted), 
                        ymax = cor_weighted + (1.96 * se_weighted),
                        color = CTFS_plot), size = 2) +
    geom_hline(yintercept = 0) + 
    facet_grid(demography1 ~  demography2, switch = "y") +
    scale_color_manual(values = CTFS.plots$color) +
    scale_fill_manual(values = CTFS.plots$color) +
    ylim(c(-1,1)) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
    theme_bw() +
    ggtitle("Weighted correlation growth-recruitment")
  
  ggsave("results/all correlations/cor_growth_recruitment_weighted.svg", 
         height = nr.of.max.crown.layers * 2,
         width = 5)
  
  # plotting growth-recruitment - unweighted
  plot.subset = all.correlations[demography1 %in% paste("growth", 1:nr.of.max.crown.layers, sep = "_") &
                                   demography2 %in% c("recruitment_ba", "recruitment_capita")]
  
  ggplot(data = plot.subset) +
    geom_bar(aes( x = CTFS_plot,
                  y = cor_unweighted,
                  fill = CTFS_plot), stat = "identity", alpha = 0.4) +
    
    geom_linerange(aes( x = CTFS_plot,
                        ymin = cor_unweighted - (1.96 * se_unweighted), 
                        ymax = cor_unweighted + (1.96 * se_unweighted),
                        color = CTFS_plot), size = 2) +
    geom_hline(yintercept = 0) + 
    facet_grid(demography1 ~  demography2, switch = "y") +
    scale_color_manual(values = CTFS.plots$color) +
    scale_fill_manual(values = CTFS.plots$color) +
    ylim(c(-1,1)) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
    theme_bw() +
    ggtitle("Unweighted correlation growth-recruitment")
  
  ggsave("results/all correlations/cor_growth_recruitment_unweighted.svg", 
         height = nr.of.max.crown.layers * 2,
         width = 5)
  
  # plotting survival-recruitment - weighted
  plot.subset = all.correlations[demography1 %in% paste("survival", 1:nr.of.max.crown.layers, sep = "_") &
                                   demography2 %in% c("recruitment_ba", "recruitment_capita")]
  
  ggplot(data = plot.subset) +
    geom_bar(aes( x = CTFS_plot,
                  y = cor_weighted,
                  fill = CTFS_plot), stat = "identity", alpha = 0.4) +
    
    geom_linerange(aes( x = CTFS_plot,
                        ymin = cor_weighted - (1.96 * se_weighted), 
                        ymax = cor_weighted + (1.96 * se_weighted),
                        color = CTFS_plot), size = 2) +
    geom_hline(yintercept = 0) + 
    facet_grid(demography1 ~  demography2, switch = "y") +
    scale_color_manual(values = CTFS.plots$color) +
    scale_fill_manual(values = CTFS.plots$color) +
    ylim(c(-1,1)) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
    theme_bw() +
    ggtitle("Weighted correlation survival-recruitment")
  
  ggsave("results/all correlations/cor_survival_recruitment_weighted.svg", 
         height = nr.of.max.crown.layers * 2,
         width = 5)
  
  # plotting survival-recruitment - unweighted
  plot.subset = all.correlations[demography1 %in% paste("survival", 1:nr.of.max.crown.layers, sep = "_") &
                                   demography2 %in% c("recruitment_ba", "recruitment_capita")]
  
  ggplot(data = plot.subset) +
    geom_bar(aes( x = CTFS_plot,
                  y = cor_unweighted,
                  fill = CTFS_plot), stat = "identity", alpha = 0.4) +
    
    geom_linerange(aes( x = CTFS_plot,
                        ymin = cor_unweighted - (1.96 * se_unweighted), 
                        ymax = cor_unweighted + (1.96 * se_unweighted),
                        color = CTFS_plot), size = 2) +
    geom_hline(yintercept = 0) + 
    facet_grid(demography1 ~  demography2, switch = "y") +
    scale_color_manual(values = CTFS.plots$color) +
    scale_fill_manual(values = CTFS.plots$color) +
    ylim(c(-1,1)) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
    theme_bw() +
    ggtitle("Unweighted correlation survival-recruitment")
  
  ggsave("results/all correlations/cor_survival_recruitment_unweighted.svg", 
         height = nr.of.max.crown.layers * 2,
         width = 5)
  
  # plotting joint growth/survival-recruitment - weighted
  plot.subset = all.correlations[demography1 %in% paste("joint_growth_survival", 1:nr.of.max.crown.layers, sep = "_") &
                                   demography2 %in% c("recruitment_ba", "recruitment_capita")]
  
  ggplot(data = plot.subset) +
    geom_bar(aes( x = CTFS_plot,
                  y = cor_weighted,
                  fill = CTFS_plot), stat = "identity", alpha = 0.4) +
    
    geom_linerange(aes( x = CTFS_plot,
                        ymin = cor_weighted - (1.96 * se_weighted), 
                        ymax = cor_weighted + (1.96 * se_weighted),
                        color = CTFS_plot), size = 2) +
    geom_hline(yintercept = 0) + 
    facet_grid(demography1 ~  demography2, switch = "y") +
    scale_color_manual(values = CTFS.plots$color) +
    scale_fill_manual(values = CTFS.plots$color) +
    ylim(c(-1,1)) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
    theme_bw() +
    ggtitle("Weighted correlation mean rank(growth/survival)-recruitment")
  
  ggsave("results/all correlations/cor_joint_growthsurvival_recruitment_weighted.svg", 
         height = nr.of.max.crown.layers * 2,
         width = 6)
  
  # plotting survival-survival - unweighted
  plot.subset = all.correlations[demography1 %in% paste("joint_growth_survival", 1:nr.of.max.crown.layers, sep = "_") &
                                   demography2 %in% c("recruitment_ba", "recruitment_capita")]
  
  ggplot(data = plot.subset) +
    geom_bar(aes( x = CTFS_plot,
                  y = cor_unweighted,
                  fill = CTFS_plot), stat = "identity", alpha = 0.4) +
    
    geom_linerange(aes( x = CTFS_plot,
                        ymin = cor_unweighted - (1.96 * se_unweighted), 
                        ymax = cor_unweighted + (1.96 * se_unweighted),
                        color = CTFS_plot), size = 2) +
    geom_hline(yintercept = 0) + 
    facet_grid(demography1 ~  demography2, switch = "y") +
    scale_color_manual(values = CTFS.plots$color) +
    scale_fill_manual(values = CTFS.plots$color) +
    ylim(c(-1,1)) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
    theme_bw() +
    ggtitle("Unweighted correlation mean rank(growth/survival)-recruitment")
  
  ggsave("results/all correlations/cor_joint_growthsurvival_recruitment_unweighted.svg", 
         height = nr.of.max.crown.layers * 2,
         width = 6)
  
  return(all.correlations)
}

################################################################################################
# calculate correlation between the amount of explained variance and the sample size in the PCAS
get.the.sample.sizes = function(CTFS.plots){
 
  sample.sizes = data.frame("CTFS_plot" = NA, "median_surv_obs" = NA, "median_growth_obs" = NA)[0,]
  
  for(i in CTFS.plots$CTFS_plot){
      
      # load data
      survival.data = data.table(readRDS(paste(c("intermediate output/", i, "/",i, "_balanced_survival.rds"), collapse = "")))
      growth.data = data.table(readRDS(paste(c("intermediate output/", i, "/",i, "_balanced_growth.rds"), collapse = "")))
      nr.of.crown.layers = max(survival.data$crown_layer)
      
      # get all posible species and layers
      all.sp.layer.combinations = data.frame("sp" = rep(unique(c(survival.data$sp, growth.data$sp)), each = nr.of.crown.layers))
      all.sp.layer.combinations$crown_layer = 1:nr.of.crown.layers
      
      # calculate number of observations
      survival.data = survival.data[,.(nr_replicates = length(alive)), by =.(sp, crown_layer)]
      growth.data = growth.data[,.(nr_replicates = length(dinc_per_year)), by =.(sp, crown_layer)]
      
      # merge data
      survival.data = merge(all.sp.layer.combinations, survival.data,
                            by = c("sp","crown_layer"), all.x = T)
      growth.data = merge(all.sp.layer.combinations, growth.data,
                            by = c("sp","crown_layer"), all.x = T)
      
      # replace NA with zero
      survival.data$nr_replicates[is.na(survival.data$nr_replicates)] = 0
      growth.data$nr_replicates[is.na(growth.data$nr_replicates)] = 0
      
      sample.sizes = rbind(sample.sizes, data.frame(
        "CTFS_plot" = i, 
        "median_surv_obs" = median(survival.data$nr_replicates), 
        "median_growth_obs" = median(growth.data$nr_replicates)))
      
  }
  
  sample.sizes$percent_surv_obs_from_maximum = sample.sizes$median_surv_obs / 1000
  sample.sizes$percent_growth_obs_from_maximum = sample.sizes$median_growth_obs / 100
  sample.sizes$mean_of_growth_survival_percentages = (sample.sizes$percent_surv_obs_from_maximum + 
                                                        sample.sizes$percent_growth_obs_from_maximum) / 2
  return(sample.sizes)
}
  


###############################################################################
# calculate the main correlations of interest (i.e. the trade-offs of interest)

get.the.main.trade.off.correlations = function(CTFS.plots, all.stan.estimates){
  
  results = data.frame("CTFS_plot" = NA, 
                       "growth1_surv4" = NA,
                       "growth1surv1_rec_ba")[0,]
  
  for(i in 1:nrow(CTFS.plots)){
    
    # load recruitment
    stan.recruitment = all.stan.estimates$recruitment[CTFS_plot == CTFS.plots$CTFS_plot[i]]
    
    # load survival and only keep those sp-canopy layer combinations for which there is data
    stan.survival = all.stan.estimates$survival[CTFS_plot == CTFS.plots$CTFS_plot[i]]
    stan.survival = stan.survival[data_available == "yes"]
    
    # load growth and only keep those sp-canopy layer combinations for which there is data
    stan.growth = all.stan.estimates$growth[CTFS_plot == CTFS.plots$CTFS_plot[i]]
    stan.growth = stan.growth[data_available == "yes"]
    
    # merge
    growth1_surv4.merged = merge(stan.growth[crown_layer == 1], stan.survival[crown_layer == 4],
                                 by = "sp")
    growth1_surv4.merged$growth1_rank = rank(growth1_surv4.merged$stan_mean_growth_rate_trans)
    growth1_surv4.merged$surv1_rank = rank(growth1_surv4.merged$stan_mean_surv_rate_trans)
    
    growth1surv1_rec_ba.merged = merge(stan.growth[crown_layer == 1], stan.survival[crown_layer == 1],
                                       by = "sp")
    growth1surv1_rec_ba.merged = merge(growth1surv1_rec_ba.merged, stan.recruitment,
                                       by = "sp")
    growth1surv1_rec_ba.merged$growth1_rank = rank(growth1surv1_rec_ba.merged$stan_mean_growth_rate_trans)
    growth1surv1_rec_ba.merged$surv1_rank = rank(growth1surv1_rec_ba.merged$stan_mean_surv_rate_trans)
    growth1surv1_rec_ba.merged$rec_ba_rank = rank(growth1surv1_rec_ba.merged$stan_yearly_recruits_per_mean_adult_ba)
    growth1surv1_rec_ba.merged$mean_growth1_surv1_rank = (growth1surv1_rec_ba.merged$growth1_rank +  growth1surv1_rec_ba.merged$surv1_rank) / 2
    
    results = rbind(results, 
                    data.frame("CTFS_plot" = CTFS.plots$CTFS_plot[i],
                               "growth1_surv4" = cor(growth1_surv4.merged$growth1_rank,
                                                     growth1_surv4.merged$surv1_rank, 
                                                     method = "spearman"),
                               "growth1surv1_rec_ba" = cor(growth1surv1_rec_ba.merged$mean_growth1_surv1_rank,
                                                           growth1surv1_rec_ba.merged$rec_ba_rank,
                                                           method = "spearman")))
  }
  return(results)
  
}

##################################################################################
# calculate / approximate the mortality rates of small (<= 10 cm) and large (> 10 cm) ind in every plot
quantify.mortality.rates.in.all.CTFS.plots = function(CTFS.plots){
  
  results = data.frame("CTFS_plot" = NA, "mortality_small" = NA, "mortality_large" = NA, 
                       "mortality_all" = NA, "mortality_canopy" = NA)[0,]
  CTFS.plots.temp = CTFS.plots[match(CTFS.plots$CTFS_plot, CTFS.plots$CTFS_plot),]
  
  for(i in 1:nrow(CTFS.plots)){
    plot.temp = CTFS.plots$CTFS_plot[i]
    survival.temp = readRDS(paste(c("intermediate output/per plot_PPA/", plot.temp, "/", plot.temp, "_all_survival_for_supplement.rds"), collapse = ""))
    survival.temp = rbindlist(survival.temp, use.names = T, fill = T)
    
    # determine sizes of large or canopy trees
    #hist(survival.temp[crown_layer == 1]$dbh, breaks = 50)
    survival.temp$more_than_10_cm = ifelse(survival.temp$dbh >= 100, "yes","no")
    survival.temp$more_than_canopy_dbh = ifelse(survival.temp$crown_layer == 1, "yes","no")
    
    survival.small.temp = survival.temp[more_than_10_cm == "no",.(alive_small = sum(alive), all_ind_small = length(alive), years_mean_small = mean(interval_date1_and_2_in_years))]
    survival.large.temp = survival.temp[more_than_10_cm == "yes",.(alive_large = sum(alive), all_ind_large = length(alive), years_mean_large = mean(interval_date1_and_2_in_years))]
    survival.canopy.temp = survival.temp[more_than_canopy_dbh == "yes",.(alive_canopy = sum(alive), all_ind_canopy = length(alive), years_mean_large = mean(interval_date1_and_2_in_years))]
    survival.all.temp = survival.temp[,.(alive_all = sum(alive), all_ind_all = length(alive), years_mean_all = mean(interval_date1_and_2_in_years))]
    
    survival.small.temp = 1 - ((survival.small.temp$alive_small / survival.small.temp$all_ind_small)^(1/survival.small.temp$years_mean_small))
    survival.large.temp = 1 - ((survival.large.temp$alive_large / survival.large.temp$all_ind_large)^(1/survival.large.temp$years_mean_large))
    survival.canopy.temp = 1 - ((survival.canopy.temp$alive_canopy / survival.canopy.temp$all_ind_canopy)^(1/survival.canopy.temp$years_mean_large))
    survival.all.temp = 1 - ((survival.all.temp$alive_all / survival.all.temp$all_ind_all)^(1/survival.all.temp$years_mean_all))
    
    results = rbind(results, 
                    data.frame("CTFS_plot" = plot.temp, 
                               "mortality_small" = survival.small.temp,
                               "mortality_large" = survival.large.temp,
                               "mortality_canopy" = survival.canopy.temp,
                               "mortality_all" = survival.all.temp))
  }
  
  return(results)
}


###########################################################
# determine high Rhat values in single census stan analyses
# saves and returns a data table
determine.censuses.with.high.Rhat = function(CTFS.plots, output.file){
  
  CTFS.plots.temp = CTFS.plots
  
  CTFS.plots.temp$CTFS_plot = NULL
  CTFS.plots.temp$census_interval = NULL
  
  for(i in 1:nrow(CTFS.plots.temp)){
    stan.recruitment.temp = readRDS(paste(c("intermediate output/per census interval_PPA/", CTFS.plots$CTFS_plot[i], "/",
                                            CTFS.plots$CTFS_plot[i], "_stan_recruitment",CTFS.plots$census_interval[i], ".rds"), collapse = ""))
    stan.survival.temp = readRDS(paste(c("intermediate output/per census interval_PPA/", CTFS.plots$CTFS_plot[i], "/",
                                         CTFS.plots$CTFS_plot[i], "_stan_survival",CTFS.plots$census_interval[i], ".rds"), collapse = ""))
    stan.growth.temp = readRDS(paste(c("intermediate output/per census interval_PPA/", CTFS.plots$CTFS_plot[i], "/",
                                       CTFS.plots$CTFS_plot[i], "_stan_growth",CTFS.plots$census_interval[i], ".rds"), collapse = ""))
    
    CTFS.plots.temp$recruitment_Rhat[i] = max(stan.recruitment.temp$stan_Rhat_for_nr_recruits)
    
    if(length(stan.survival.temp$Rhat[stan.survival.temp$crown_layer == 1]) > 1){
      CTFS.plots.temp$survival1_Rhat[i] = max(stan.survival.temp$Rhat[stan.survival.temp$crown_layer == 1])}
    if(length(stan.survival.temp$Rhat[stan.survival.temp$crown_layer == 2]) > 1){
      CTFS.plots.temp$survival2_Rhat[i] = max(stan.survival.temp$Rhat[stan.survival.temp$crown_layer == 2])}
    if(length(stan.survival.temp$Rhat[stan.survival.temp$crown_layer == 3]) > 1){
      CTFS.plots.temp$survival3_Rhat[i] = max(stan.survival.temp$Rhat[stan.survival.temp$crown_layer == 3])}
    if(length(stan.survival.temp$Rhat[stan.survival.temp$crown_layer == 4]) > 1){
      CTFS.plots.temp$survival4_Rhat[i] = max(stan.survival.temp$Rhat[stan.survival.temp$crown_layer == 4])}
    if(length(stan.survival.temp$Rhat[stan.survival.temp$crown_layer == 5]) > 1){
      CTFS.plots.temp$survival5_Rhat[i] = max(stan.survival.temp$Rhat[stan.survival.temp$crown_layer == 5])}
    if(length(stan.survival.temp$Rhat[stan.survival.temp$crown_layer == 6]) > 1){
      CTFS.plots.temp$survival6_Rhat[i] = max(stan.survival.temp$Rhat[stan.survival.temp$crown_layer == 6])}
    if(length(stan.survival.temp$Rhat[stan.survival.temp$crown_layer == 7]) > 1){
      CTFS.plots.temp$survival7_Rhat[i] = max(stan.survival.temp$Rhat[stan.survival.temp$crown_layer == 7])}
    
    if(length(stan.growth.temp$Rhat[stan.growth.temp$crown_layer == 1]) > 1){
      CTFS.plots.temp$growth1_Rhat[i] = max(stan.growth.temp$Rhat[stan.growth.temp$crown_layer == 1])}
    if(length(stan.growth.temp$Rhat[stan.growth.temp$crown_layer == 2]) > 1){
      CTFS.plots.temp$growth2_Rhat[i] = max(stan.growth.temp$Rhat[stan.growth.temp$crown_layer == 2])}
    if(length(stan.growth.temp$Rhat[stan.growth.temp$crown_layer == 3]) > 1){
      CTFS.plots.temp$growth3_Rhat[i] = max(stan.growth.temp$Rhat[stan.growth.temp$crown_layer == 3])}
    if(length(stan.growth.temp$Rhat[stan.growth.temp$crown_layer == 4]) > 1){
      CTFS.plots.temp$growth4_Rhat[i] = max(stan.growth.temp$Rhat[stan.growth.temp$crown_layer == 4])}
    if(length(stan.growth.temp$Rhat[stan.growth.temp$crown_layer == 5]) > 1){
      CTFS.plots.temp$growth5_Rhat[i] = max(stan.growth.temp$Rhat[stan.growth.temp$crown_layer == 5])}
    if(length(stan.growth.temp$Rhat[stan.growth.temp$crown_layer == 6]) > 1){
      CTFS.plots.temp$growth6_Rhat[i] = max(stan.growth.temp$Rhat[stan.growth.temp$crown_layer == 6])}
    if(length(stan.growth.temp$Rhat[stan.growth.temp$crown_layer == 7]) > 1){
      CTFS.plots.temp$growth7_Rhat[i] = max(stan.growth.temp$Rhat[stan.growth.temp$crown_layer == 7])}
  }
  
  
  high.Rhat.values = as.data.frame(which(CTFS.plots.temp > 1.3, arr.ind=T) )
  high.Rhat.values = data.table(data.frame("CTFS_plot" = CTFS.plots$CTFS_plot[high.Rhat.values$row],
                                           "census_interval" = CTFS.plots$census_interval[high.Rhat.values$row],
                                           "crown_layer" = names(CTFS.plots.temp)[high.Rhat.values$col],
                                           "demography" = "survival"))
  high.Rhat.values$crown_layer = gsub(pattern = "_Rhat", replacement = "", x = high.Rhat.values$crown_layer)
  high.Rhat.values$crown_layer = gsub(pattern = "survival", replacement = "", x = high.Rhat.values$crown_layer)
  high.Rhat.values$crown_layer = as.numeric(high.Rhat.values$crown_layer)
  saveRDS(high.Rhat.values,
          output.file)
  return(high.Rhat.values)
}

############################################

############################################
# calculate.wPCAs.only.growth.surv.trade.off = function(CTFS.plots, all.stan.estimates){
#   
#   results = list()
#   
#   # get the max crown_layer per plot
#   max_crown_layers = all.stan.estimates$survival[,.(max_crown_layer = max(crown_layer)), by = CTFS_plot]
#   
#   # survival estimates in bottom layer
#   stan.survival.estimates = all.stan.estimates$survival[,.(CTFS_plot, sp, survival_layer2 = stan_mean_surv_rate_trans, crown_layer = crown_layer)]
#   stan.survival.weights = all.stan.estimates$survival[,.(CTFS_plot, sp, weights_survival_layer2 = stan_weight_surv_rate, crown_layer = crown_layer)]
#   stan.survival.estimates = merge(stan.survival.estimates, max_crown_layers, 
#                                   by.x = c("CTFS_plot","crown_layer"), by.y = c("CTFS_plot","max_crown_layer"))
#   stan.survival.weights = merge(stan.survival.weights, max_crown_layers, 
#                                 by.x = c("CTFS_plot","crown_layer"), by.y = c("CTFS_plot","max_crown_layer"))
# 
#   # growth estimates in top layer
#   stan.growth.estimates = all.stan.estimates$growth[crown_layer == 1,.(CTFS_plot, sp, growth_layer1 = stan_mean_growth_rate_trans)]
#   stan.growth.weights = all.stan.estimates$growth[crown_layer == 1,.(CTFS_plot, sp, weights_growth_layer1 = stan_weight_growth_rate)]
#     
#   # put them all in one data.frame
#   stan.estimates.compiled = merge(stan.survival.estimates, stan.growth.estimates, by = c("CTFS_plot","sp"))
#   stan.estimates.compiled$sp = as.character(stan.estimates.compiled$sp)
#     
#   stan.weights.compiled = merge(stan.survival.weights, stan.growth.weights, by = c("CTFS_plot", "sp"))
#   stan.weights.compiled$sp = as.character(stan.weights.compiled$sp)
#   
#   # additional infos
#   plot.species.info = stan.estimates.compiled[,.(CTFS_plot, sp)]
#   
#   nr.of.crown.layers = length(grep(pattern = "growth", names(stan.estimates.compiled)))
#   
#   #####################################
#   # Analyse demographic rates of x layers with a weighted PCA
#   # Initial script by Benjamin Rosenbaum, Dr. Nadja Rueger (nadja.rueger@idiv.de)
#   # restructured by Stephan Kambach (stephan.kambach@idiv.de)
#   # see L. Delchambre. Weighted principal component analysis: a weighted covariance eigendecomposition approach. Mon. Not. R. Astron. Soc. 446(2), 3545-3555, 2014. 
#   #   data.estimates: table with rows=observations, cols = variables
#   #   data.weights: table with the corresponding weights
#   
#   # scaled by plot-level sds
#   for(i in 1:nrow(CTFS.plots)){
#     
#     plot.temp = CTFS.plots$CTFS_plot[i]
#     census.interval.temp = CTFS.plots$census_interval[i]
#     
#     X = stan.estimates.compiled[CTFS_plot == plot.temp]
#     W = stan.weights.compiled[CTFS_plot == plot.temp]
#     
#     columns = c(match("survival_layer2", names(stan.estimates.compiled)),
#                 match("growth_layer1", names(stan.estimates.compiled)))
#     
#     columns = columns[!is.na(columns)]
#     
#     X = data.frame(X)[,c(columns)]
#     W = data.frame(W)[,c(columns)]
#     
#     # remove crown layers with NA
#     X = X[,!(is.na(colSums(X)))]
#     W = W[,!(is.na(colSums(W)))]
#     colnames.of.X = names(X)
#     
#     X <- t(as.matrix(X))
#     W <- t(as.matrix(W))
#     
#     # replace zero weights
#     W[W==0] <- 1.0e-6
#     
#     n <- as.numeric(ncol(X))
#     d <- as.numeric(nrow(X))
#     
#     get.nr.crown.layers = 
#       nr.of.crown.layers.on.this.plot = max(suppressWarnings(as.numeric(substr(colnames.of.X, 
#                                                                                start = nchar(colnames.of.X),
#                                                                                stop = nchar(colnames.of.X)))), na.rm = T)
#     
#     # substract weighted mean, save original data
#     Xorig <- X
#     
#     # save sds and center for for back-transformation later
#     centers = rowMeans(W*X)/rowMeans(W)
#     sds.within.plot = centers
#     names(sds.within.plot) = rownames(X)
#     
#     X <- X - rowMeans(W*X)/rowMeans(W) # X = centered and scaled within plots
#     
#     # scale by weighted sd in each dimension
#     # X sds per plot
#     for (j in 1:d){
#       sds.within.plot[j] <- sqrt(sum(W[j, ]^2*X[j, ]^2)/sum(W[j, ]^2))
#       X[j, ] <- X[j, ] / sds.within.plot[j]}
#     
#     # weighted covariance matrix S
#     S <- (W*X)%*%t(W*X) / (W%*%t(W))
#     
#     #  eigenvalues (in decreasing order) and corresponding eigenvectors (in columns) 
#     EV <- eigen(S)
#     
#     #----------------------------------------------------------------
#     # step 2: loop through ranks to compute true explained variance
#     #----------------------------------------------------------------
#     Chi2 <- vector(length=d) # explained variance
#     
#     for(rank in 1:nrow(X)){
#       
#       # matrix of principal components (new basis)
#       P <- EV$vectors[, 1:rank]
#       
#       # matrix of coefficients (coordinates in new basis)
#       C <- matrix(data=NA, nrow=rank, ncol=n)
#       
#       for (i in 1:n){
#         w <- diag(W[, i]^2)
#         
#         C[, i] <- solve(t(P)%*%w%*%P, t(P)%*%w%*%X[, i])}
#       
#       # matrix of projections of old data (PC approx X)
#       PC <- P%*%C
#       
#       # explained variance
#       Chi2[rank] <- sum(sum((W*PC)^2)) / sum(sum((W*X)^2))}
#     
#     # subtract expl variances to get the individual CHi2 of every principal component
#     for(rank in 2:nrow(X)){
#       Chi2[rank] = Chi2[rank] - sum(Chi2[1:(rank-1)])}
#     
#     #----------------------------------------------------------------
#     # step 3: decide for a rank based on the explained variance 
#     #         and compute data for output (same steps as above)
#     #----------------------------------------------------------------
#     rank <- nrow(X)
#     
#     P <- EV$vectors[, 1:rank]
#     
#     C <- matrix(data=NA, nrow=rank, ncol=n)
#     
#     for (i in 1:n){
#       w <- diag(W[,i]^2) 
#       C[, i] <- solve(t(P)%*%w%*%P, t(P)%*%w%*%X[, i])}
#     
#     PC <- P%*%C
#     
#     
#     # re-order the principal components from largest to smallest explained variance
#     P = P[,order(Chi2, decreasing = T)]
#     C = C[order(Chi2, decreasing = T),]
#     Chi2 = Chi2[order(Chi2, decreasing = T)]
#     
#     # Output is a list with 6 entries:
#     #   chi2: explained variance
#     #   principal_components:    matrix of principal components (new basis)
#     #   pca_coordinates:    matrix of coefficients (coordinates in new basis)
#     #   projections_old_data:   matrix of projections of old data (PC approx X)
#     #   centers
#     #   sds
#     
#     # re-arrange data for easier later analysis
#     names(Chi2) = paste("PCA", 1:length(Chi2), sep = "")
#     
#     P = data.frame(colnames.of.X, 
#                    P)
#     names(P) = c("demographic_rate", paste("PCA", 1:length(Chi2), sep = ""))
#     
#     C = data.frame(plot.species.info[CTFS_plot == plot.temp]$sp, t(C))
#     
#     names(C) = c("sp", paste("PCA", 1:length(Chi2), sep = ""))
#     
#     # calculate demographic centroids (i.e. mean axis loadings)
#     centroid.survival = c(mean(P$PCA1[grep(pattern = "survival", x = P$demographic_rate)]), mean(P$PCA2[grep(pattern = "survival", x = P$demographic_rate)]))
#     centroid.growth = c(mean(P$PCA1[grep(pattern = "growth", x = P$demographic_rate)]), mean(P$PCA2[grep(pattern = "growth", x = P$demographic_rate)]))
#     
#     centroids = rbind(centroid.survival,
#                       centroid.growth)
#     
#     centroids = data.frame(c("survival", "growth"), centroids)
#     names(centroids) = c("demography", "PCA1", "PCA2")
#     rownames(centroids) = NULL
#     
#     results[[paste(plot.temp, census.interval.temp, sep = "")]] = 
#       list("expl_var" = Chi2,
#            "factor_loadings" = P,
#            "pca_coordinates" = C,
#            "demography_centroids" = centroids,
#            "centers" = centers,
#            "sds" = sds.within.plot)
#     
#     results[[paste(plot.temp, census.interval.temp, sep = "")]]$species = 
#         as.character(plot.species.info[CTFS_plot == plot.temp,]$sp)
#   }
#   return(results)
# }
# 
#####################################################
calculate.mcps.per.plot = function(all.stan.estimates,
                                   CTFS.plots,
                                   output.folder){
  
  results = list("summary" =  data.frame("CTFS_plot" = CTFS.plots$CTFS_plot, "region" = NA,
                                         "min10_growth" = NA, "min10_survival" = NA, "min10_rec_ba" = NA, "min10_rec_capita" = NA,
                                         "max90_growth" = NA, "max90_survival" = NA, "max90_rec_ba" = NA, "max90_rec_capita" = NA,
                                         "range80_growth" = NA, "range80_survival" = NA, "range80_rec_ba" = NA, "range80_rec_capita" = NA,
                                         "mcp80_area_growth_survival" = NA, 
                                         "mcp80_area_growth_rec_ba" = NA, "mcp80_area_growth_rec_capita" = NA,
                                         "mcp80_area_survival_rec_ba" = NA, "mcp80_area_survival_rec_capita" = NA,
                                         "mcp80_R2_growth_survival" = NA, 
                                         "mcp80_R2_growth_rec_ba" = NA, "mcp80_R2_growth_rec_capita" = NA,
                                         "mcp80_R2_survival_rec_ba" = NA, "mcp80_R2_survival_rec_capita" = NA),
                 "mcp80s" = data.frame("CTFS_plot" = NA, "region" = NA,
                                       "x_rate" = NA, "y_rate" = NA, 
                                       "x_coord" = NA,"y_coord" = NA)[0,])
  
  for(i in 1:nrow(CTFS.plots)){
    
    plot.temp = CTFS.plots$CTFS_plot[i]
    
    growth.temp = all.stan.estimates$growth[,.(CTFS_plot, sp, crown_layer, 
                                               growth = stan_mean_growth_rate,
                                               growth_scaled = scale(stan_mean_growth_rate))]
    names(growth.temp) = c("CTFS_plot","sp","crown_layer","growth","growth_scaled")
    growth.temp = growth.temp[CTFS_plot == plot.temp & crown_layer == 2]
    growth.temp$crown_layer = NULL
    
    survival.temp = all.stan.estimates$survival[,.(CTFS_plot, sp, crown_layer, 
                                               survival = stan_mean_surv_rate,
                                               survival_scaled = scale(stan_mean_surv_rate))]
    names(survival.temp) = c("CTFS_plot","sp","crown_layer","survival","survival_scaled")
    survival.temp = survival.temp[CTFS_plot == plot.temp]
    survival.temp = survival.temp[crown_layer == max(crown_layer)- 1]
    survival.temp$crown_layer = NULL
    
    rec.temp.ba = all.stan.estimates$recruitment[,.(CTFS_plot, sp, 
                                                    rec_ba_log = stan_yearly_recruits_per_mean_adult_ba_trans,
                                                    rec_ba_log_scaled = stan_yearly_recruits_per_mean_adult_ba_trans)]
    rec.temp.ba = rec.temp.ba[CTFS_plot == plot.temp]
    
    rec.temp.capita = all.stan.estimates$recruitment[,.(CTFS_plot, sp, 
                                                    rec_capita_log = stan_yearly_recruits_per_capita_trans,
                                                    rec_capita_log_scaled = stan_yearly_recruits_per_capita_trans)]
    rec.temp.capita = rec.temp.capita[CTFS_plot == plot.temp]
    
    all.estimates.temp = merge(growth.temp, survival.temp, by = c("CTFS_plot","sp"))
    all.estimates.temp = merge(all.estimates.temp, rec.temp.ba, by = c("CTFS_plot","sp"))
    all.estimates.temp = merge(all.estimates.temp, rec.temp.capita, by = c("CTFS_plot","sp"))
    
    if(plot.temp == "ituri_lenda"){
      gilbde = all.estimates.temp[sp == "gilbde"]
    }
    # region
    results$summary$region[i] = CTFS.plots$region[i]
    
    # min 10% quantiles
    results$summary$min10_growth[i] = quantile(all.estimates.temp$growth, probs = 0.1)
    results$summary$min10_survival[i] = quantile(all.estimates.temp$survival, probs = 0.1)
    results$summary$min10_rec_ba[i] = quantile(all.estimates.temp$rec_ba_log, probs = 0.1)
    results$summary$min10_rec_capita[i] = quantile(all.estimates.temp$rec_capita_log, probs = 0.1)
    
    # max 90% quantiles
    results$summary$max90_growth[i] = quantile(all.estimates.temp$growth, probs = 0.9)
    results$summary$max90_survival[i] = quantile(all.estimates.temp$survival, probs = 0.9)
    results$summary$max90_rec_ba[i] = quantile(all.estimates.temp$rec_ba_log, probs = 0.9)
    results$summary$max90_rec_capita[i] = quantile(all.estimates.temp$rec_capita_log, probs = 0.9)
    
    # 80% quantiles
    results$summary$range80_growth[i] = quantile(all.estimates.temp$growth, probs = 0.9) - quantile(all.estimates.temp$growth, probs = 0.1)
    results$summary$range80_survival[i] = quantile(all.estimates.temp$survival, probs = 0.9) - quantile(all.estimates.temp$survival, probs = 0.1)
    results$summary$range80_rec_ba[i] = quantile(all.estimates.temp$rec_ba_log, probs = 0.9) - quantile(all.estimates.temp$rec_ba_log, probs = 0.1)
    results$summary$range80_rec_capita[i] = quantile(all.estimates.temp$rec_capita_log, probs = 0.9) - quantile(all.estimates.temp$rec_capita_log, probs = 0.1)
    
    # growth-survival mcp80s
    mcp80 = mcp(SpatialPoints(all.estimates.temp[,.(growth_scaled, survival_scaled)]), percent = 80)
    mcp80.points = as.numeric(row.names(mcp80@polygons[[1]]@Polygons[[1]]@coords))
    mcp80.points = mcp80.points[c(1:(length(mcp80.points)-1),1)]
    mcp80.points = all.estimates.temp[mcp80.points,.(growth, survival)]
    polygon.area.temp = mcp80@polygons[[1]]@Polygons[[1]]@area
    
    # get R� within polygon
    all.estimates.temp$in_poly  = point.in.polygon(point.x = all.estimates.temp$growth, 
                                                   point.y = all.estimates.temp$survival, 
                                                   pol.x = mcp80.points$growth, 
                                                   pol.y = mcp80.points$survival, 
                                                   mode.checked=FALSE)
    stan.estimates.in.mcp.temp = rbind(mcp80.points[-nrow(mcp80.points),],
                                       all.estimates.temp[in_poly == 1, .(growth, survival)])
    polygon.R2.temp = summary(lm(stan.estimates.in.mcp.temp$survival ~ stan.estimates.in.mcp.temp$growth))$adj.r.squared
    
    results$mcp80s = rbind(results$mcp80s,
                           data.frame("CTFS_plot" = plot.temp, "region" = CTFS.plots$region[i],
                                      "x_rate" = "growth", "y_rate" = "survival",
                                      "x_coord" = c(mcp80.points$growth),
                                      "y_coord" = c(mcp80.points$survival)))
    
    results$summary$mcp80_area_growth_survival[i] = polygon.area.temp
    results$summary$mcp80_R2_growth_survival[i] = polygon.R2.temp
    
    # growth-recruitment ba mcp80s
    mcp80 = mcp(SpatialPoints(all.estimates.temp[,.(growth_scaled, rec_ba_log_scaled)]), percent = 80)
    mcp80.points = as.numeric(row.names(mcp80@polygons[[1]]@Polygons[[1]]@coords))
    mcp80.points = mcp80.points[c(1:(length(mcp80.points)-1),1)]
    mcp80.points = all.estimates.temp[mcp80.points,.(growth, rec_ba_log)]
    polygon.area.temp = mcp80@polygons[[1]]@Polygons[[1]]@area
    
    # get R� within polygon
    all.estimates.temp$in_poly  = point.in.polygon(point.x = all.estimates.temp$growth, 
                                                   point.y = all.estimates.temp$rec_ba_log, 
                                                   pol.x = mcp80.points$growth, 
                                                   pol.y = mcp80.points$rec_ba_log, 
                                                   mode.checked=FALSE)
    stan.estimates.in.mcp.temp = rbind(mcp80.points[-nrow(mcp80.points),],
                                       all.estimates.temp[in_poly == 1, .(growth, rec_ba_log)])
    polygon.R2.temp = summary(lm(stan.estimates.in.mcp.temp$rec_ba_log ~ stan.estimates.in.mcp.temp$growth))$adj.r.squared
    
    results$mcp80s = rbind(results$mcp80s,
                           data.frame("CTFS_plot" = plot.temp, "region" = CTFS.plots$region[i],
                                      "x_rate" = "growth", "y_rate" = "rec_ba_log",
                                      "x_coord" = c(mcp80.points$growth),
                                      "y_coord" = c(mcp80.points$rec_ba_log)))
    
    results$summary$mcp80_area_growth_rec_ba[i] = polygon.area.temp
    results$summary$mcp80_R2_growth_rec_ba[i] = polygon.R2.temp
    
    # growth-recruitment capita mcp80s
    mcp80 = mcp(SpatialPoints(all.estimates.temp[,.(growth_scaled, rec_capita_log_scaled)]), percent = 80)
    mcp80.points = as.numeric(row.names(mcp80@polygons[[1]]@Polygons[[1]]@coords))
    mcp80.points = mcp80.points[c(1:(length(mcp80.points)-1),1)]
    mcp80.points = all.estimates.temp[mcp80.points,.(growth, rec_capita_log)]
    polygon.area.temp = mcp80@polygons[[1]]@Polygons[[1]]@area
    
    # get R� within polygon
    all.estimates.temp$in_poly  = point.in.polygon(point.x = all.estimates.temp$growth, 
                                                   point.y = all.estimates.temp$rec_ba_capita_log, 
                                                   pol.x = mcp80.points$growth, 
                                                   pol.y = mcp80.points$rec_ba_capita_log, 
                                                   mode.checked=FALSE)
    stan.estimates.in.mcp.temp = rbind(mcp80.points[-nrow(mcp80.points),],
                                       all.estimates.temp[in_poly == 1, .(growth, rec_capita_log)])
    polygon.R2.temp = summary(lm(stan.estimates.in.mcp.temp$rec_capita_log ~ stan.estimates.in.mcp.temp$growth))$adj.r.squared
    
    results$mcp80s = rbind(results$mcp80s,
                           data.frame("CTFS_plot" = plot.temp, "region" = CTFS.plots$region[i],
                                      "x_rate" = "growth", "y_rate" = "rec_capita_log",
                                      "x_coord" = c(mcp80.points$growth, mcp80.points$growth[length(mcp80.points$growth)]),
                                      "y_coord" = c(mcp80.points$rec_capita_log, mcp80.points$rec_capita_log[length(mcp80.points$rec_capita_log)])))
    
    results$summary$mcp80_area_growth_rec_capita[i] = polygon.area.temp
    results$summary$mcp80_R2_growth_rec_capita[i] = polygon.R2.temp
    
    # survival-recruitment ba mcp80s
    mcp80 = mcp(SpatialPoints(all.estimates.temp[,.(survival_scaled, rec_ba_log_scaled)]), percent = 80)
    mcp80.points = as.numeric(row.names(mcp80@polygons[[1]]@Polygons[[1]]@coords))
    mcp80.points = mcp80.points[c(1:(length(mcp80.points)-1),1)]
    mcp80.points = all.estimates.temp[mcp80.points,.(survival, rec_ba_log)]
    polygon.area.temp = mcp80@polygons[[1]]@Polygons[[1]]@area
    
    # get R� within polygon
    all.estimates.temp$in_poly  = point.in.polygon(point.x = all.estimates.temp$survival, 
                                                   point.y = all.estimates.temp$rec_ba_log, 
                                                   pol.x = mcp80.points$survival, 
                                                   pol.y = mcp80.points$rec_ba_log, 
                                                   mode.checked=FALSE)
    stan.estimates.in.mcp.temp = rbind(mcp80.points[-nrow(mcp80.points),],
                                       all.estimates.temp[in_poly == 1, .(survival, rec_ba_log)])
    polygon.R2.temp = summary(lm(stan.estimates.in.mcp.temp$rec_ba_log ~ stan.estimates.in.mcp.temp$survival))$adj.r.squared
    
    results$mcp80s = rbind(results$mcp80s,
                           data.frame("CTFS_plot" = plot.temp, "region" = CTFS.plots$region[i],
                                      "x_rate" = "survival", "y_rate" = "rec_ba_log",
                                      "x_coord" = c(mcp80.points$survival, mcp80.points$survival[length(mcp80.points$survival)]),
                                      "y_coord" = c(mcp80.points$rec_ba_log, mcp80.points$rec_ba_log[length(mcp80.points$rec_ba_log)])))
    
    results$summary$mcp80_area_survival_rec_ba[i] = polygon.area.temp
    results$summary$mcp80_R2_survival_rec_ba[i] = polygon.R2.temp
    
    # survival-recruitment capita mcp80s
    mcp80 = mcp(SpatialPoints(all.estimates.temp[,.(survival_scaled, rec_capita_log_scaled)]), percent = 80)
    mcp80.points = as.numeric(row.names(mcp80@polygons[[1]]@Polygons[[1]]@coords))
    mcp80.points = mcp80.points[c(1:(length(mcp80.points)-1),1)]
    mcp80.points = all.estimates.temp[mcp80.points,.(survival, rec_capita_log)]
    polygon.area.temp = mcp80@polygons[[1]]@Polygons[[1]]@area
    
    # get R� within polygon
    all.estimates.temp$in_poly  = point.in.polygon(point.x = all.estimates.temp$survival, 
                                                   point.y = all.estimates.temp$rec_ba_capita_log, 
                                                   pol.x = mcp80.points$survival, 
                                                   pol.y = mcp80.points$rec_ba_capita_log, 
                                                   mode.checked=FALSE)
    stan.estimates.in.mcp.temp = rbind(mcp80.points[-nrow(mcp80.points),],
                                       all.estimates.temp[in_poly == 1, .(survival, rec_capita_log)])
    polygon.R2.temp = summary(lm(stan.estimates.in.mcp.temp$rec_capita_log ~ stan.estimates.in.mcp.temp$survival))$adj.r.squared
    
    results$mcp80s = rbind(results$mcp80s,
                           data.frame("CTFS_plot" = plot.temp, "region" = CTFS.plots$region[i],
                                      "x_rate" = "survival", "y_rate" = "rec_capita_log",
                                      "x_coord" = c(mcp80.points$survival, mcp80.points$survival[length(mcp80.points$survival)]),
                                      "y_coord" = c(mcp80.points$rec_capita_log, mcp80.points$rec_capita_log[length(mcp80.points$rec_capita_log)])))
    
    results$summary$mcp80_area_survival_rec_capita[i] = polygon.area.temp
    results$summary$mcp80_R2_survival_rec_capita[i] = polygon.R2.temp
  }
  
  data.to.plot = data.table(results$mcp80s)
  data.to.plot = merge(data.to.plot, CTFS.plots[,.(CTFS_plot, color)], by = "CTFS_plot")
  data.to.plot$CTFS_plot = factor(data.to.plot$CTFS_plot, levels = CTFS.plots$CTFS_plot)
  
  # plotting growth versus survival
  data.to.anotate = data.table(results$summary)[,.(CTFS_plot, 
                                                   expl_var = round(mcp80_R2_growth_survival * 100, digits = 1), label_xpos = NA, label_ypos = NA)]
  data.to.anotate$expl_var[data.to.anotate$expl_var < 0] = 0
  
  for(i in 1:nrow(data.to.anotate)){
    plot.temp = as.character(data.to.anotate$CTFS_plot[i])
    data.to.plot.temp = data.to.plot[x_rate == "growth" & y_rate == "survival" & CTFS_plot == plot.temp]
    most.right.low.point.comb = which((length(data.to.plot.temp$x_coord) - rank(data.to.plot.temp$x_coord)) + rank(data.to.plot.temp$y_coord) == 
                                        (length(data.to.plot.temp$x_coord) - max(rank(data.to.plot.temp$x_coord)) + rank(data.to.plot.temp$y_coord)))[1]
    data.to.anotate$label_xpos[i] = data.to.plot.temp$x_coord[most.right.low.point.comb]
    data.to.anotate$label_ypos[i] = data.to.plot.temp$y_coord[most.right.low.point.comb]}
  
  data.to.plot.now  = data.to.plot[x_rate == "growth" & y_rate == "survival"]
  data.to.plot.now = merge(data.to.plot.now, data.to.anotate, by = "CTFS_plot")
  
  results[["plots"]][["growth_surv"]] =
    ggplot(data = data.to.plot.now) +
    geom_polygon(aes(x = x_coord, y = y_coord , fill = CTFS_plot, color = CTFS_plot), size = 1.5, alpha = 0.01) +
    geom_point(aes(x = gilbde$growth, y = gilbde$survival), size = 2) +  
    geom_text(aes(x = gilbde$growth, y = gilbde$survival , label = "Gilb. dew."), size = 3) +  
    #  geom_label(aes(x = label_xpos,y = label_ypos, label = expl_var, fill = CTFS_plot), color = "white", position = "jitter")  +
    scale_fill_manual(name = "Forest", values = unique(data.to.plot.now$color)) + 
    scale_color_manual(name = "Forest", values = unique(data.to.plot.now$color))  +
    xlab("Annual growth in canopy (mm dbh)") + ylab("Annual survival in understorey") +
    theme_bw() +
    facet_grid(region ~ .)   +
    theme(legend.position = "none")
  
  # plotting growth versus rec_ba
  data.to.anotate = data.table(results$summary)[,.(CTFS_plot, 
                                                   expl_var = round(mcp80_R2_growth_rec_ba * 100, digits = 1), label_xpos = NA, label_ypos = NA)]
  data.to.anotate$expl_var[data.to.anotate$expl_var < 0] = 0
  
  for(i in 1:nrow(data.to.anotate)){
    plot.temp = as.character(data.to.anotate$CTFS_plot[i])
    data.to.plot.temp = data.to.plot[x_rate == "growth" & y_rate == "rec_ba_log" & CTFS_plot == plot.temp]
    most.right.low.point.comb = which((length(data.to.plot.temp$x_coord) - rank(data.to.plot.temp$x_coord)) + rank(data.to.plot.temp$y_coord) == 
                                        (length(data.to.plot.temp$x_coord) - max(rank(data.to.plot.temp$x_coord)) + rank(data.to.plot.temp$y_coord)))[1]
    data.to.anotate$label_xpos[i] = data.to.plot.temp$x_coord[most.right.low.point.comb]
    data.to.anotate$label_ypos[i] = data.to.plot.temp$y_coord[most.right.low.point.comb]}
  
  data.to.plot.now  = data.to.plot[x_rate == "growth" & y_rate == "rec_ba_log"]
  data.to.plot.now = merge(data.to.plot.now, data.to.anotate, by = "CTFS_plot")
  
  results[["plots"]][["growth_rec_ba_log"]] = 
    ggplot(data = data.to.plot.now) +
    geom_polygon(aes(x = x_coord, y = y_coord , fill = CTFS_plot, color = CTFS_plot), size = 1.5, alpha = 0.05) +
    #geom_text(aes(x = gilbde$growth, y = gilbde$rec_ba_log , label = "Gilb. dew."), size = 3, alpha = 0.01) +  
    # geom_label(aes(x = label_xpos,y = label_ypos, label = expl_var, fill = CTFS_plot), color = "white", position = "jitter")  +
    scale_fill_manual(name = "Forest", values = unique(data.to.plot.now$color)) + 
    scale_color_manual(name = "Forest", values = unique(data.to.plot.now$color))  + 
    scale_y_continuous(breaks = c(log(0.1), log(1), log(10), log(100), log(1000)), labels = c(0.1, 1, 10, 100, 1000)) + 
    xlab("Annual growth in canopy (mm dbh)") + ylab("Annual recruitment per adult BA)") +
    theme_bw() + 
    facet_grid(region ~ .) + 
    theme(legend.position = "none")
  
  # plotting growth versus rec_capita
  data.to.anotate = data.table(results$summary)[,.(CTFS_plot, 
                                                   expl_var = round(mcp80_R2_growth_rec_capita * 100, digits = 1), label_xpos = NA, label_ypos = NA)]
  data.to.anotate$expl_var[data.to.anotate$expl_var < 0] = 0
  
  for(i in 1:nrow(data.to.anotate)){
    plot.temp = as.character(data.to.anotate$CTFS_plot[i])
    data.to.plot.temp = data.to.plot[x_rate == "growth" & y_rate == "rec_capita_log" & CTFS_plot == plot.temp]
    most.right.low.point.comb = which((length(data.to.plot.temp$x_coord) - rank(data.to.plot.temp$x_coord)) + rank(data.to.plot.temp$y_coord) == 
                                        (length(data.to.plot.temp$x_coord) - max(rank(data.to.plot.temp$x_coord)) + rank(data.to.plot.temp$y_coord)))[1]
    data.to.anotate$label_xpos[i] = data.to.plot.temp$x_coord[most.right.low.point.comb]
    data.to.anotate$label_ypos[i] = data.to.plot.temp$y_coord[most.right.low.point.comb]}
  
  data.to.plot.now  = data.to.plot[x_rate == "growth" & y_rate == "rec_capita_log"]
  data.to.plot.now = merge(data.to.plot.now, data.to.anotate, by = "CTFS_plot")
  
  results[["plots"]][["growth_rec_capita_log"]] = 
    ggplot(data = data.to.plot.now) +
    geom_polygon(aes(x = x_coord, y = y_coord , fill = CTFS_plot, color = CTFS_plot), size = 1.5, alpha = 0.05) +
   # geom_label(aes(x = label_xpos,y = label_ypos, label = expl_var, fill = CTFS_plot), color = "white", position = "jitter")  +
    scale_fill_manual(name = "Forest", values = unique(data.to.plot.now$color)) + 
    scale_color_manual(name = "Forest", values = unique(data.to.plot.now$color))  + 
    scale_y_continuous(breaks = c(log(0.005), log(0.01),log(0.02), log(0.05), log(0.1), log(0.5)), labels = c(0.5, 1,2, 5, 10, 50)) + 
    xlab("Annual growth in canopy (mm dbh)") + ylab("Annual recruitment per 100 individuals") +
    theme_bw() + 
    facet_grid(region ~ .) + 
    theme(legend.position = "none")
  
  # plotting survival versus rec_ba
  data.to.anotate = data.table(results$summary)[,.(CTFS_plot, 
                                                   expl_var = round(mcp80_R2_survival_rec_ba * 100, digits = 1), label_xpos = NA, label_ypos = NA)]
  data.to.anotate$expl_var[data.to.anotate$expl_var < 0] = 0
  
  for(i in 1:nrow(data.to.anotate)){
    plot.temp = as.character(data.to.anotate$CTFS_plot[i])
    data.to.plot.temp = data.to.plot[x_rate == "survival" & y_rate == "rec_ba_log" & CTFS_plot == plot.temp]
    most.right.low.point.comb = which(rank(data.to.plot.temp$x_coord) + rank(data.to.plot.temp$y_coord) == 
                                        min(rank(data.to.plot.temp$x_coord) + rank(data.to.plot.temp$y_coord)))[1]
    data.to.anotate$label_xpos[i] = data.to.plot.temp$x_coord[most.right.low.point.comb]
    data.to.anotate$label_ypos[i] = data.to.plot.temp$y_coord[most.right.low.point.comb]}
  
  data.to.plot.now  = data.to.plot[x_rate == "survival" & y_rate == "rec_ba_log"]
  data.to.plot.now = merge(data.to.plot.now, data.to.anotate, by = "CTFS_plot")
  
  results[["plots"]][["surv_rec_ba_log"]] = 
    ggplot(data = data.to.plot.now) +
    geom_polygon(aes(x = x_coord, y = y_coord , fill = CTFS_plot, color = CTFS_plot), size = 1.5, alpha = 0.05) +
    #geom_label(aes(x = label_xpos,y = label_ypos, label = expl_var, fill = CTFS_plot), color = "white", position = "jitter")  +
    scale_fill_manual(name = "Forest", values = unique(data.to.plot.now$color)) + 
    scale_color_manual(name = "Forest", values = unique(data.to.plot.now$color))  + 
    scale_y_continuous(breaks = c(log(0.1), log(1), log(10), log(100), log(1000)), labels = c(0.1, 1, 10, 100, 1000)) + 
    xlab("Annual survival in understorey") + ylab("Annual recruitment per adult BA)") +
    theme_bw() + 
    facet_grid(region ~ .) + 
    theme(legend.position = "none")
  
  # plotting survival versus rec_capita
  data.to.anotate = data.table(results$summary)[,.(CTFS_plot, 
                                                   expl_var = round(mcp80_R2_survival_rec_capita * 100, digits = 1), label_xpos = NA, label_ypos = NA)]
  data.to.anotate$expl_var[data.to.anotate$expl_var < 0] = 0
  
  for(i in 1:nrow(data.to.anotate)){
    plot.temp = as.character(data.to.anotate$CTFS_plot[i])
    data.to.plot.temp = data.to.plot[x_rate == "survival" & y_rate == "rec_capita_log" & CTFS_plot == plot.temp]
    most.right.low.point.comb = which(rank(data.to.plot.temp$x_coord) + rank(data.to.plot.temp$y_coord) == 
                                        min(rank(data.to.plot.temp$x_coord) + rank(data.to.plot.temp$y_coord)))[1]
    data.to.anotate$label_xpos[i] = data.to.plot.temp$x_coord[most.right.low.point.comb]
    data.to.anotate$label_ypos[i] = data.to.plot.temp$y_coord[most.right.low.point.comb]}
  
  data.to.plot.now  = data.to.plot[x_rate == "survival" & y_rate == "rec_capita_log"]
  data.to.plot.now = merge(data.to.plot.now, data.to.anotate, by = "CTFS_plot")
  
  results[["plots"]][["surv_rec_capita_log"]] = 
    ggplot(data = data.to.plot.now) +
    geom_polygon(aes(x = x_coord, y = y_coord , fill = CTFS_plot, color = CTFS_plot), size = 1.5, alpha = 0.05) +
    #geom_label(aes(x = label_xpos,y = label_ypos, label = expl_var, fill = CTFS_plot), color = "white", position = "jitter")  +
    scale_fill_manual(name = "Forest", values = unique(data.to.plot.now$color)) + 
    scale_color_manual(name = "Forest", values = unique(data.to.plot.now$color))  + 
    scale_y_continuous(breaks = c(log(0.005), log(0.01),log(0.02), log(0.05), log(0.1), log(0.5)), labels = c(0.5, 1,2, 5, 10, 50)) +     
    xlab("Annual survival in understorey") + ylab("Annual recruitment per 100 individuals") +
    theme_bw() + 
    facet_grid(region ~ .) + 
    theme(legend.position = "none")
  
  results[["plots"]][["legend"]] = ggplot(data = data.to.plot[x_rate == "survival" & y_rate == "rec_capita_log"]) +
    geom_polygon(aes(x = x_coord, y = y_coord , fill = CTFS_plot, color = CTFS_plot), size = 1.5, alpha = 0.05) +
    scale_fill_manual(name = "Forest", values = CTFS.plots$color[CTFS.plots$CTFS_plot %in% data.to.plot$CTFS_plot]) + 
    scale_color_manual(name = "Forest", values = CTFS.plots$color[CTFS.plots$CTFS_plot %in% data.to.plot$CTFS_plot]) +
    xlab("Annual survival in understorey") + ylab("Annual recruitment (per capita)") +
    theme_bw() + 
    facet_grid(region ~ .) 

  plot.list1 = list("growth_surv" = results[["plots"]][["growth_surv"]],
                    "growth_rec_ba" = results[["plots"]][["growth_rec_ba_log"]],
                    "surv_rec_ba" = results[["plots"]][["surv_rec_ba_log"]])
  plot.list2 = list("growth_surv" = results[["plots"]][["growth_surv"]],
                    "growth_rec_capita" = results[["plots"]][["growth_rec_capita_log"]],
                    "surv_rec_capita" = results[["plots"]][["surv_rec_capita_log"]])
  
  
  
  # save 
  ggsave(plot = plot_grid(plotlist = plot.list1, nrow = 1), filename = paste(output.folder, "/growth_surv_rec_ba_from_R.svg", sep =""), height = 7, width = 9)
  ggsave(plot = plot_grid(plotlist = plot.list1, nrow = 1), filename = paste(output.folder, "/growth_surv_rec_capita_from_R.svg", sep =""), height = 7, width = 9)
  
  ggsave(plot = results[["plots"]][["legend"]], filename = paste(output.folder, "/legend.svg", sep =""), height = 7, width = 3.5)

  return(results)
}
###################################################
plot.hypervolumes = function(hypervolume.results,
                             CTFS.plots,
                             recruitment.measure){
  
  hypervolume.results$mcps_to_plot_3d = list("growth_survival" = list(),
                                             "growth_recruitment" = list(),
                                             "survival_recruitment" = list())
  hypervolume.results$mcp_plots_3d = list()
  hypervolume.results$mcp_area_3d = data.frame("CTFS_plot" = as.character(CTFS.plots$CTFS_plot), 
                                               "growth_survival" = NA,
                                               "growth_recruitment" = NA, 
                                               "survival_recruitment" = NA)
  
  for(i in 1:nrow(CTFS.plots)){
    
    plot.temp = CTFS.plots$CTFS_plot[i]
    hypervolume.results$mcp_area_3d$CTFS_plot[i] = plot.temp
    
    # get mcps
    # growth survival
    randompoints.temp = hypervolume.results$random_points[[plot.temp]][,.(growth_top, survival_bottom)]
    mcp.temp = mcp(SpatialPoints(randompoints.temp), percent = 100)
    points.temp = as.numeric(row.names(mcp.temp@polygons[[1]]@Polygons[[1]]@coords))
    points.temp = points.temp[c(1:(length(points.temp)-1),1)]
    hypervolume.results$mcps_to_plot_3d$growth_survival[[plot.temp]] = randompoints.temp[points.temp,]
    hypervolume.results$mcp_area_3d$growth_survival[i] = mcp.temp@polygons[[1]]@Polygons[[1]]@area
    
    # growth recruitment ba log
    randompoints.temp = hypervolume.results$random_points[[plot.temp]][,.(growth_top, recruitment)]
    mcp.temp = mcp(SpatialPoints(randompoints.temp), percent = 100)
    points.temp = as.numeric(row.names(mcp.temp@polygons[[1]]@Polygons[[1]]@coords))
    points.temp = points.temp[c(1:(length(points.temp)-1),1)]
    hypervolume.results$mcps_to_plot_3d$growth_recruitment[[plot.temp]] = randompoints.temp[points.temp,]
    hypervolume.results$mcp_area_3d$growth_recruitment[i] = mcp.temp@polygons[[1]]@Polygons[[1]]@area
    
    # survival recruitment ba log
    randompoints.temp = hypervolume.results$random_points[[plot.temp]][,.(survival_bottom, recruitment)]
    mcp.temp = mcp(SpatialPoints(randompoints.temp), percent = 100)
    points.temp = as.numeric(row.names(mcp.temp@polygons[[1]]@Polygons[[1]]@coords))
    points.temp = points.temp[c(1:(length(points.temp)-1),1)]
    hypervolume.results$mcps_to_plot_3d$survival_recruitment[[plot.temp]] = randompoints.temp[points.temp,]
    hypervolume.results$mcp_area_3d$survival_recruitment[i] = mcp.temp@polygons[[1]]@Polygons[[1]]@area
    
    }

  # arrange data for plotting
  centroids.to.plot = rbindlist(hypervolume.results$centroids, idcol = "CTFS_plot")
  centroids.to.plot = merge(centroids.to.plot, CTFS.plots[,.(CTFS_plot, region)], by = "CTFS_plot")
  
  # growth-survival
  data.to.plot  = rbindlist(hypervolume.results$mcps_to_plot_3d$growth_survival, idcol = "CTFS_plot")
  data.to.plot = merge(data.to.plot, CTFS.plots, by = "CTFS_plot")
  data.to.plot$CTFS_plot = factor(data.to.plot$CTFS_plot, levels = CTFS.plots$CTFS_plot)
  
 hypervolume.results$mcp_plots_3d$growth_survival =
    ggplot(data = data.to.plot) +
    geom_polygon(aes(x =  growth_top, y = survival_bottom, fill = CTFS_plot, color = CTFS_plot), size = 1.5, alpha = 0.01) +
    geom_point(data = centroids.to.plot, aes(x = growth_top, y = survival_bottom, fill = CTFS_plot), size = 2, pch = 21, alpha = 0.7) +
    scale_fill_manual(name = "Forest", values = unique(data.to.plot$color)) + 
    scale_color_manual(name = "Forest", values = unique(data.to.plot$color))  +
    xlab("Growth in canopy (mm dbh)") +
    ylab("Survival in understorey") +
    theme_bw() +
    facet_grid(region ~ .) +
    theme(legend.position = "none")
  
  # growth-recruitment
  data.to.plot  = rbindlist(hypervolume.results$mcps_to_plot_3d$growth_recruitment, idcol = "CTFS_plot")
  data.to.plot = merge(data.to.plot, CTFS.plots, by = "CTFS_plot")
  data.to.plot$CTFS_plot = factor(data.to.plot$CTFS_plot, levels = CTFS.plots$CTFS_plot)
  
  hypervolume.results$mcp_plots_3d$growth_recruitment =
    ggplot(data = data.to.plot) +
    geom_polygon(aes(x = growth_top, y =  recruitment, fill = CTFS_plot, color = CTFS_plot), size = 1.5, alpha = 0.05) +
    geom_point(data = centroids.to.plot, aes(x = growth_top, y =  recruitment, fill = CTFS_plot), size = 2, pch = 21, alpha = 0.7) +
    scale_fill_manual(name = "Forest", values = unique(data.to.plot$color)) + 
    scale_color_manual(name = "Forest", values = unique(data.to.plot$color))  + 
    scale_y_continuous(breaks = c(log(0.1), log(1), log(10), log(100), log(1000)), labels = c(0.1, 1, 10, 100, 1000)) + 
    xlab("Growth in canopy (mm dbh)") +
    theme_bw() + 
    facet_grid(region ~ .) + 
    theme(legend.position = "none")
  
  if(recruitment.measure == "stan_yearly_recruits_per_mean_adult_ba_trans"){
    hypervolume.results$mcp_plots_3d$growth_recruitment = 
      hypervolume.results$mcp_plots_3d$growth_recruitment +
      ylab("Recruitment per unit of reproductive basal area")}
  if(recruitment.measure == "stan_yearly_recruits_per_capita_trans"){
    hypervolume.results$mcp_plots_3d$growth_recruitment = 
      hypervolume.results$mcp_plots_3d$growth_recruitment +
      ylab("Recruitment per capita")}
  
  # plot survival-recruitment
  data.to.plot  = rbindlist(hypervolume.results$mcps_to_plot_3d$survival_recruitment, idcol = "CTFS_plot")
  data.to.plot = merge(data.to.plot, CTFS.plots, by = "CTFS_plot")
  data.to.plot$CTFS_plot = factor(data.to.plot$CTFS_plot, levels = CTFS.plots$CTFS_plot)
  
  hypervolume.results$mcp_plots_3d$survival_recruitment =
    ggplot(data = data.to.plot) +
    geom_polygon(aes(x = survival_bottom, y = recruitment, fill = CTFS_plot, color = CTFS_plot), size = 1.5, alpha = 0.01) +
    geom_point(data = centroids.to.plot, aes(x = survival_bottom, y = recruitment, fill = CTFS_plot), size = 2, pch = 21, alpha = 0.7) +
    scale_fill_manual(name = "Forest", values = unique(data.to.plot$color)) + 
    scale_color_manual(name = "Forest", values = unique(data.to.plot$color))  +
    scale_y_continuous(breaks = c(log(0.1), log(1), log(10), log(100), log(1000)), labels = c(0.1, 1, 10, 100, 1000)) +
    xlab("Survival in understorey") +
    theme_bw() +
    facet_grid(region ~ .) +
    theme(legend.position = "none")
  
  if(recruitment.measure == "stan_yearly_recruits_per_mean_adult_ba_trans"){
    hypervolume.results$mcp_plots_3d$survival_recruitment = 
      hypervolume.results$mcp_plots_3d$survival_recruitment +
      ylab("Recruitment per unit of reproductive basal area")}
  if(recruitment.measure == "stan_yearly_recruits_per_capita_trans"){
    hypervolume.results$mcp_plots_3d$survival_recruitment = 
      hypervolume.results$mcp_plots_3d$survival_recruitment +
      ylab("Recruitment per capita")}
  
  # legend
  data.to.plot  = rbindlist(hypervolume.results$mcps_to_plot_3d$growth_survival, idcol = "CTFS_plot")
  data.to.plot = merge(data.to.plot, CTFS.plots, by = "CTFS_plot")
  data.to.plot$CTFS_plot = factor(data.to.plot$CTFS_plot, levels = CTFS.plots$CTFS_plot)
  
  legend.plot =
    ggplot(data = data.to.plot) +
    geom_polygon(aes(x = survival_bottom , y = growth_top, fill = CTFS_plot, color = CTFS_plot), size = 1.5, alpha = 0.01) +
    scale_fill_manual(name = "Forest", values = unique(data.to.plot$color)) + 
    scale_color_manual(name = "Forest", values = unique(data.to.plot$color))  +
    xlab("Annual survival in understorey") +
    ylab(bquote('Annual growth in canopy (mm dbh year'^-1*')')) +
    theme_bw() +
    facet_grid(region ~ .) 
  
  ggsave(plot = legend.plot, 
         filename = "results/per plot_PPA/hypervolumes/growth_surv_rec_ba_legend.svg", 
         height = 6, width = 6)
  
  ggsave(plot = plot_grid(plotlist = hypervolume.results$mcp_plots_3d, ncol = 3, align = "hv"), 
         filename = "results/per plot_PPA/hypervolumes/growth_surv_rec_ba_hypervolume.svg", 
         height = 5, width = 7.5)
  
  #return(hypervolume.results)
}

###################################################
calculate.hypervolumes = function(all.stan.estimates,
                                  CTFS.plots,
                                  recruitment.measure){
  
  results = list("summary" =  data.frame("CTFS_plot" = NA,
                                         "hypervolume_3d_vol" = NA)[0,],
                 "hypervolumes_3d" = list(),
                 "random_points" = list(),
                 "centroids" = list(),
                 "data" = list())
  
  # transform values for hypervolumes
  backtransform.recruitment.ba = c()
  backtransform.recruitment.capita = c()
  backtransform.survival = c()
  backtransform.growth = c()
  
  # make all recruits with NaN or -Inf to zero
  all.stan.estimates$recruitment$stan_yearly_recruits_per_mean_adult_ba_trans[!is.finite(all.stan.estimates$recruitment$stan_yearly_recruits_per_mean_adult_ba_trans)] = 0
  all.stan.estimates$recruitment$stan_yearly_recruits_per_capita_trans[!is.finite(all.stan.estimates$recruitment$stan_yearly_recruits_per_capita_trans)] = 0
  
  backtransform.recruitment.ba[1] = mean(all.stan.estimates$recruitment$stan_yearly_recruits_per_mean_adult_ba_trans)
  all.stan.estimates$recruitment$stan_yearly_recruits_per_mean_adult_ba_trans = all.stan.estimates$recruitment$stan_yearly_recruits_per_mean_adult_ba_trans - backtransform.recruitment.ba[1]
  backtransform.recruitment.ba[2] = sd(all.stan.estimates$recruitment$stan_yearly_recruits_per_mean_adult_ba_trans)
  all.stan.estimates$recruitment$stan_yearly_recruits_per_mean_adult_ba_trans = all.stan.estimates$recruitment$stan_yearly_recruits_per_mean_adult_ba_trans / backtransform.recruitment.ba[2]
  
  backtransform.recruitment.capita[1] = mean(all.stan.estimates$recruitment$stan_yearly_recruits_per_capita_trans)
  all.stan.estimates$recruitment$stan_yearly_recruits_per_capita_trans = all.stan.estimates$recruitment$stan_yearly_recruits_per_capita_trans - backtransform.recruitment.capita[1]
  backtransform.recruitment.capita[2] = sd(all.stan.estimates$recruitment$stan_yearly_recruits_per_capita_trans)
  all.stan.estimates$recruitment$stan_yearly_recruits_per_capita_trans = all.stan.estimates$recruitment$stan_yearly_recruits_per_capita_trans / backtransform.recruitment.capita[2]
  
  backtransform.survival[1] = mean(all.stan.estimates$survival$stan_mean_surv_rate)
  all.stan.estimates$survival$mean_survival_per_year_scaled  =  all.stan.estimates$survival$stan_mean_surv_rate - backtransform.survival[1]
  backtransform.survival[2] = sd(all.stan.estimates$survival$mean_survival_per_year_scaled)
  all.stan.estimates$survival$mean_survival_per_year_scaled = all.stan.estimates$survival$mean_survival_per_year_scaled / backtransform.survival[2]
  
  backtransform.growth[1] = mean(all.stan.estimates$growth$stan_mean_growth_rate)
  all.stan.estimates$growth$mean_growth_per_year_scaled = all.stan.estimates$growth$stan_mean_growth_rate - backtransform.growth[1]
  backtransform.growth[2] = sd(all.stan.estimates$growth$mean_growth_per_year_scaled)
  all.stan.estimates$growth$mean_growth_per_year_scaled = all.stan.estimates$growth$mean_growth_per_year_scaled / backtransform.growth[2]
  
  for(i in 1:length(CTFS.plots$CTFS_plot)){
    
    plot.temp = CTFS.plots$CTFS_plot[i]
    
    results$hypervolumes_3d[[plot.temp]] = list()
    results$random_points[[plot.temp]] = list()
    results$data[[plot.temp]] = list()
    
    if(recruitment.measure == "stan_yearly_recruits_per_mean_adult_ba_trans"){
      recruitment.temp = all.stan.estimates$recruitment[CTFS_plot == plot.temp,
                                                        .(sp, recruitment = stan_yearly_recruits_per_mean_adult_ba_trans)]}
    
    if(recruitment.measure == "stan_yearly_recruits_per_capita_trans"){
        recruitment.temp = all.stan.estimates$recruitment[CTFS_plot == plot.temp,
                                                     .(sp, recruitment = stan_yearly_recruits_per_capita_trans)]}
      
    survival.temp = all.stan.estimates$survival[CTFS_plot == plot.temp,.(sp, crown_layer, mean_survival_per_year_scaled)]
    growth.temp = all.stan.estimates$growth[CTFS_plot == plot.temp,.(sp, crown_layer, mean_growth_per_year_scaled)]
      
    # compile demographic rates (growth in highest and survival in lowest layer)
    understorey.layer = max(growth.temp$crown_layer)-1
    if(lowest.layer == 0){lowest.layer = 1}
    growth.top = growth.temp[crown_layer == 1]
    survival.understorey = survival.temp[crown_layer == lowest.layer]
      
    rates.temp.merged = merge(growth.top[,.(sp, growth_top = mean_growth_per_year_scaled)],
                              survival.understorey[,.(sp, survival_bottom = mean_survival_per_year_scaled)], 
                              by = c("sp"))
    rates.temp.merged = merge(rates.temp.merged,
                              recruitment.temp[,.(sp, recruitment)], 
                              by = c("sp"))
      
    # calculate hypervole
    results$hypervolumes_3d[[plot.temp]] =
      hypervolume_svm(data = rates.temp.merged[,.(growth_top, survival_bottom, recruitment)],
                      name = plot.temp,
                      svm.nu = 0.3,
                      svm.gamma = 0.5,
                      verbose = F)
    
    # kernel.density = estimate_bandwidth(data = rates.temp.merged[,.(growth_top, survival_bottom, recruitment)])
    # kernel.density = kernel.density * 0.5
    
    #plot(results$hypervolumes_3d[[plot.temp]], limits = list(c(-10,20),c(-10,10),c(-10,10)))
    
    # results$hypervolumes_3d[[plot.temp]] =
    #   expectation_convex(input = rates.temp.merged[,.(growth_top, survival_bottom, recruitment)], 
    #                      method = "hitandrun", chunksize = 1000, check.memory = F)
    # 
    # plot(results$hypervolumes_3d[[plot.temp]], limits = list(c(-10,20),c(-10,10),c(-10,10)))
    # 
    # results$hypervolumes_3d[[plot.temp]] =
    #    hypervolume_gaussian(data = rates.temp.merged[,.(growth_top, survival_bottom, recruitment)],
    #                    name = plot.temp,
    #                    verbose = F)
    # 
    # test = hypervolume_threshold(hv = results$hypervolumes_3d[[plot.temp]],
    #                              num.thresholds = 1000,
    #                              quantile.requested =  0.8)
    # 
    # plot(results$hypervolumes_3d[[plot.temp]], limits = list(c(-10,20),c(-10,10),c(-10,10)))
    # 
    # test = hypervolume_threshold(hv = results$hypervolumes_3d[[plot.temp]],
    #                              num.thresholds = 20)
    # test$HypervolumesThresholded[[1]]
    # plot(hypervolume_threshold(results$hypervolumes_3d[[plot.temp]], plot=FALSE, verbose=FALSE,
    #                            quantile.requested=0.5))
    #                            quantile.requested.type="probability")[[1]],
    #      limits = list(c(-10,20),c(-10,10),c(-10,10)))
    # 
    results$summary = rbind(results$summary,
                            data.frame("CTFS_plot" = plot.temp,
                                       "hypervolume_3d_vol" = results$hypervolumes_3d[[plot.temp]]@Volume))
      
      # backtransform random points for plotting
      random.points.temp = data.table(results$hypervolumes_3d[[plot.temp]]@RandomPoints)
      
      random.points.temp$growth_top = random.points.temp$growth_top * backtransform.growth[2]
      random.points.temp$growth_top = random.points.temp$growth_top + backtransform.growth[1]
      random.points.temp$survival_bottom = random.points.temp$survival_bottom * backtransform.survival[2]
      random.points.temp$survival_bottom = random.points.temp$survival_bottom + backtransform.survival[1]
      
      if(recruitment.measure == "stan_yearly_recruits_per_mean_adult_ba_trans"){
        random.points.temp$recruitment = random.points.temp$recruitment * backtransform.recruitment.ba[2]
        random.points.temp$recruitment = random.points.temp$recruitment + backtransform.recruitment.ba[1]}
      if(recruitment.measure == "stan_yearly_recruits_per_capita_trans"){
        random.points.temp$recruitment = random.points.temp$recruitment * backtransform.recruitment.capita[2]
        random.points.temp$recruitment = random.points.temp$recruitment + backtransform.recruitment.capita[1]}
      
      results$random_points[[plot.temp]] = random.points.temp
      
      # backtransform centroids for plotting
      centroids.temp = get_centroid(results$hypervolumes_3d[[plot.temp]])
      
      centroids.temp = data.frame("growth_top" = centroids.temp[1],
                                  "survival_bottom" = centroids.temp[2],
                                  "recruitment" = centroids.temp[3])
      
      centroids.temp$growth_top = centroids.temp$growth_top * backtransform.growth[2]
      centroids.temp$growth_top = centroids.temp$growth_top + backtransform.growth[1]
      centroids.temp$survival_bottom = centroids.temp$survival_bottom * backtransform.survival[2]
      centroids.temp$survival_bottom = centroids.temp$survival_bottom + backtransform.survival[1]
      
      if(recruitment.measure == "stan_yearly_recruits_per_mean_adult_ba_trans"){
        centroids.temp$recruitment = centroids.temp$recruitment * backtransform.recruitment.ba[2]
        centroids.temp$recruitment = centroids.temp$recruitment + backtransform.recruitment.ba[1]}
      
      if(recruitment.measure == "stan_yearly_recruits_per_capita_trans"){
        centroids.temp$recruitment = centroids.temp$recruitment * backtransform.recruitment.capita[2]
        centroids.temp$recruitment = centroids.temp$recruitment + backtransform.recruitment.capita[1]}
      
      results$centroids[[plot.temp]] = centroids.temp
      
      # back-transform raw data for plotting
      rates.temp.merged$growth_top = rates.temp.merged$growth_top * backtransform.growth[2]
      rates.temp.merged$growth_top = rates.temp.merged$growth_top + backtransform.growth[1]
      rates.temp.merged$survival_bottom = rates.temp.merged$survival_bottom * backtransform.survival[2]
      rates.temp.merged$survival_bottom = rates.temp.merged$survival_bottom + backtransform.survival[1]
      
      if(recruitment.measure == "stan_yearly_recruits_per_mean_adult_ba_trans"){
        rates.temp.merged$recruitment = rates.temp.merged$recruitment * backtransform.recruitment.ba[2]
        rates.temp.merged$recruitment = rates.temp.merged$recruitment + backtransform.recruitment.ba[1]}
      
      if(recruitment.measure == "stan_yearly_recruits_per_capita_trans"){
        rates.temp.merged$recruitment = rates.temp.merged$recruitment * backtransform.recruitment.capita[2]
        rates.temp.merged$recruitment = rates.temp.merged$recruitment + backtransform.recruitment.capita[1]}
      
      results$data[[plot.temp]] = rates.temp.merged
      
      # print progress
      print(paste(c(plot.temp, " done"), collapse = ""))
      }
  return(results)
}

###################################################
calculate.means.and.80perc.for.single.rates = function(all.stan.estimates,
                                                       CTFS.plots,
                                                       CTFS.order,
                                                       output.folder){
  
  results = data.frame("CTFS_plot" = CTFS.plots$CTFS_plot,
                       "color" = CTFS.plots$color[match(CTFS.plots$CTFS_plot, CTFS.plots$CTFS_plot)],
                       "growth_top_mean" = NA, "growth_top_sd" = NA, "growth_top_10" = NA, "growth_top_90" = NA, "growth_top_80range" =NA,
                       "growth_bottom_mean" = NA, "growth_bottom_sd" = NA, "growth_bottom_10" = NA, "growth_bottom_90" = NA, "growth_bottom_80range" =NA,
                       "survival_top_mean" = NA, "survival_top_sd" = NA, "survival_top_10" = NA, "survival_top_90" = NA, "survival_top_80range" =NA,
                       "survival_bottom_mean" = NA, "survival_bottom_sd" = NA, "survival_bottom_10" = NA, "survival_bottom_90" = NA, "survival_bottom_80range" =NA,
                       "log_rec_ba_mean" = NA, "log_rec_ba_sd" = NA, "log_rec_ba_10" = NA, "log_rec_ba_90" = NA, "log_rec_ba_80range" =NA,
                       "log_rec_capita_mean" = NA, "log_rec_capita_sd" = NA, "log_rec_capita_10" = NA, "log_rec_capita_90" = NA, "log_rec_capita_80range" =NA)
  
  for(i in 1:nrow(CTFS.plots)){
    
    plot.temp = CTFS.plots$CTFS_plot[i]
    
    # get bottom and top growth
    growth.temp = all.stan.estimates$growth[,.(CTFS_plot, sp, crown_layer, 
                                               growth = stan_mean_growth_rate,
                                               growth_scaled = scale(stan_mean_growth_rate))]
    growth.top = growth.temp[CTFS_plot == plot.temp & crown_layer == 2]
    growth.bottom = growth.temp[CTFS_plot == plot.temp]
    growth.bottom = growth.bottom[crown_layer == max(crown_layer)-1]
    growth.top$crown_layer = NULL
    growth.bottom$crown_layer = NULL
    
    # get bottom and top survival
    survival.temp = all.stan.estimates$survival[,.(CTFS_plot, sp, crown_layer, 
                                                   survival = stan_mean_surv_rate,
                                                   survival_scaled = scale(stan_mean_surv_rate))]
    survival.top = survival.temp[CTFS_plot == plot.temp & crown_layer == 2]
    survival.bottom = survival.temp[CTFS_plot == plot.temp]
    survival.bottom = survival.bottom[crown_layer == max(crown_layer)-1]
    survival.top$crown_layer = NULL
    survival.bottom$crown_layer = NULL
    
    # get recruitment 
    rec.temp.ba = all.stan.estimates$recruitment[,.(CTFS_plot, sp, 
                                                    rec_ba_log = stan_yearly_recruits_per_mean_adult_ba_trans,
                                                    rec_ba_log_scaled = stan_yearly_recruits_per_mean_adult_ba_trans)]
    rec.temp.ba = rec.temp.ba[CTFS_plot == plot.temp]
    
    rec.temp.capita = all.stan.estimates$recruitment[,.(CTFS_plot, sp, 
                                                        rec_capita_log = stan_yearly_recruits_per_capita_trans,
                                                        rec_capita_log_scaled = stan_yearly_recruits_per_capita_trans)]
    rec.temp.capita = rec.temp.capita[CTFS_plot == plot.temp]
    
    results$growth_top_mean[i] = mean(growth.top$growth)
    results$growth_top_sd[i] = sd(growth.top$growth)
    results$growth_top_10[i] = quantile(growth.top$growth, probs = 0.1)
    results$growth_top_90[i] = quantile(growth.top$growth, probs = 0.9)
    results$growth_top_80range[i] = results$growth_top_90[i] - results$growth_top_10[i]
    
    results$growth_bottom_mean[i] = mean(growth.bottom$growth)
    results$growth_bottom_sd[i] = sd(growth.bottom$growth)
    results$growth_bottom_10[i] = quantile(growth.bottom$growth, probs = 0.1)
    results$growth_bottom_90[i] = quantile(growth.bottom$growth, probs = 0.9)
    results$growth_bottom_80range[i] = results$growth_bottom_90[i] - results$growth_bottom_10[i]
    
    results$survival_top_mean[i] = mean(survival.top$survival)
    results$survival_top_sd[i] = sd(survival.top$survival)
    results$survival_top_10[i] = quantile(survival.top$survival, probs = 0.1)
    results$survival_top_90[i] = quantile(survival.top$survival, probs = 0.9)
    results$survival_top_80range[i] = results$survival_top_90[i] - results$survival_top_10[i]
    
    results$survival_bottom_mean[i] = mean(survival.bottom$survival)
    results$survival_bottom_sd[i] = sd(survival.bottom$survival)
    results$survival_bottom_10[i] = quantile(survival.bottom$survival, probs = 0.1)
    results$survival_bottom_90[i] = quantile(survival.bottom$survival, probs = 0.9)
    results$survival_bottom_80range[i] = results$survival_bottom_90[i] - results$survival_bottom_10[i]
    
    results$log_rec_ba_mean[i] = mean(rec.temp.ba$rec_ba_log)
    results$log_rec_ba_sd[i] = sd(rec.temp.ba$rec_ba_log)
    results$log_rec_ba_10[i] = quantile(rec.temp.ba$rec_ba_log, probs = 0.1)
    results$log_rec_ba_90[i] = quantile(rec.temp.ba$rec_ba_log, probs = 0.9)
    results$log_rec_ba_80range[i] = results$log_rec_ba_90[i] - results$log_rec_ba_10[i]
    
    results$log_rec_capita_mean[i] = mean(rec.temp.capita$rec_capita_log)
    results$log_rec_capita_sd[i] = sd(rec.temp.capita$rec_capita_log)
    results$log_rec_capita_10[i] = quantile(rec.temp.capita$rec_capita_log, probs = 0.1)
    results$log_rec_capita_90[i] = quantile(rec.temp.capita$rec_capita_log, probs = 0.9)
    results$log_rec_capita_80range[i] = results$log_rec_capita_90[i] - results$log_rec_capita_10[i]}
  
  results$CTFS_plot = as.character(results$CTFS_plot)
  results$color = as.character(results$color)
  results = results[match(CTFS.order, results$CTFS_plot),]
  results$CTFS_plot = factor(results$CTFS_plot, levels = results$CTFS_plot)
  
  plot.growth.top = ggplot(data = results) +
    geom_pointrange(aes(x = CTFS_plot, y = growth_top_mean, ymin = growth_top_10, ymax = growth_top_90, color = CTFS_plot), size = 1) +
    scale_color_manual(name = "Forest", values = as.character(results$color)) +
    xlab("Forest") + ylab("Annual increase in dbh (mm)") +
    coord_cartesian(ylim = c(0,5)) +
    theme_bw() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Growth in canopy") 
  
  plot.growth.bottom = ggplot(data = results) +
    geom_pointrange(aes(x = CTFS_plot, y = growth_bottom_mean, ymin = growth_bottom_10, ymax = growth_bottom_90, color = CTFS_plot), size = 1) +
    scale_color_manual(name = "Forest", values = as.character(results$color)) +
    xlab("Forest") + ylab("Annual increase in dbh (mm)") +
    ylim(c(0,5)) +
    theme_bw() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Growth in understorey")
  
  plot.survival.top = ggplot(data = results) +
    geom_pointrange(aes(x = CTFS_plot, y = survival_top_mean, ymin = survival_top_10, ymax = survival_top_90, color = CTFS_plot), size = 1) +
    scale_color_manual(name = "Forest", values = as.character(results$color)) +
    xlab("Forest") + ylab("Annual survival") +
    ylim(c(0.88,1)) +  
    theme_bw() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Survival in canopy")
  
  plot.survival.bottom = ggplot(data = results) +
    geom_pointrange(aes(x = CTFS_plot, y = survival_bottom_mean, ymin = survival_bottom_10, ymax = survival_bottom_90, color = CTFS_plot), size = 1) +
    scale_color_manual(name = "Forest", values = as.character(results$color)) +
    xlab("Forest") + ylab("Annual survival") +
    ylim(c(0.88,1)) + 
    theme_bw() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Survival in understorey")
  
  plot.rec.ba = ggplot(data = results) +
    geom_pointrange(aes(x = CTFS_plot, y = log_rec_ba_mean, ymin = log_rec_ba_10, ymax = log_rec_ba_90, color = CTFS_plot), size = 1) +
    scale_color_manual(name = "Forest", values = as.character(results$color)) +
    xlab("Forest") + ylab("Log recruitment per BA") +
    theme_bw() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Recruitment per adult BA")
  
  plot.rec.capita = ggplot(data = results) +
    geom_pointrange(aes(x = CTFS_plot, y = log_rec_capita_mean, ymin = log_rec_capita_10, ymax = log_rec_capita_90, color = CTFS_plot), size = 1) +
    scale_color_manual(name = "Forest", values = as.character(results$color)) +
    xlab("Forest") + ylab("Log recruitment per capita") +
    theme_bw() + theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Recruitment per capita")
  
  plotlist = list(plot.growth.top, plot.growth.bottom, plot.survival.top, plot.survival.bottom, plot.rec.ba, plot.rec.capita)
  plot.grid = plot_grid(plotlist = plotlist, ncol = 2)
  ggsave(plot = plot.grid, filename = "results/per plot_PPA/means_and_sds/means_sd_top_bottom_layer.svg",
         height = 16, width = 12)
  
  return(results)}

###################################################
plot.stan.estimates.against.data = function(CTFS.plots,
                                            all.stan.estimates,
                                            output.folder){
  for(i in 1:nrow(CTFS.plots)){
    
    plot.temp = CTFS.plots$CTFS_plot[i]
    
    # load data
    growth.data.temp = readRDS(paste(c(output.folder, "/", plot.temp, "/", plot.temp,"_all_growth_for_supplement.rds"), collapse = ""))
    survival.data.temp = readRDS(paste(c(output.folder, "/", plot.temp, "/", plot.temp,"_all_survival_for_supplement.rds"), collapse = ""))
    recruitment.data.temp = readRDS(paste(c(output.folder, "/" ,plot.temp, "/", plot.temp,"_recruitment_unbalanced.rds"), collapse = ""))
    
    growth.data.temp = rbindlist(growth.data.temp, idcol = "census", use.names=TRUE, fill = T)
    survival.data.temp = rbindlist(survival.data.temp, idcol = "census", use.names=TRUE, fill = T)
    
    growth.data.temp = growth.data.temp[,.(growth_mean = mean(dinc_per_year),
                                           growth_n = length(dinc_per_year)),
                                        by = .(sp, crown_layer)]
    growth.data.temp$growth_n[growth.data.temp$growth_n > 200] = 200
    survival.data.temp = survival.data.temp[,.(survival_mean = mean(alive),
                                               survival_n = length(alive)),
                                            by = .(sp, crown_layer)]
    survival.data.temp$survival_annual =1 - ((1- survival.data.temp$survival_mean) /5)
    survival.data.temp$survival_n[survival.data.temp$survival_n > 1000] = 1000
    recruitment.data.temp = recruitment.data.temp[,.(rec_mean = mean(recruits),
                                                     rec_n = length(recruits)),
                                                  by = sp]
    
    # merge with stan estimates
    growth.to.plot = merge(growth.data.temp, all.stan.estimates$growth[CTFS_plot == plot.temp],
                           by = c("sp","crown_layer"))
    survival.to.plot = merge(survival.data.temp, all.stan.estimates$survival[CTFS_plot == plot.temp],
                             by = c("sp","crown_layer"))
    recruitment.to.plot = merge(recruitment.data.temp, all.stan.estimates$recruitment[CTFS_plot == plot.temp],
                                by = c("sp"))
    
    # plots
    plot.growth = ggplot(data = growth.to.plot) +
      geom_point(aes(x = growth_mean, y = stan_mean_growth_rate, size = growth_n, color = stan_weight_growth_rate), alpha = 0.5, pch = 16) +
      geom_abline(aes(intercept = 0, slope = 1)) + 
      #geom_smooth(aes(x = growth_mean, y = stan_mean_growth_rate), method = "lm") +
      scale_color_gradient(low = "blue", high = "orange") +
      facet_wrap(. ~ crown_layer, scales = "free") +
      theme_bw() +
      ylab("stan_mean_growth_rate") + xlab("data growth_mean")
    print(paste(c(plot.temp, " cor growth: ", cor(growth.to.plot$growth_mean, growth.to.plot$stan_mean_growth_rate)), collapse = ""))
    
    plot.survival = ggplot(data = survival.to.plot) +
      geom_point(aes(x = survival_annual, y = stan_mean_surv_rate, size = survival_n, color = stan_weight_surv_rate), alpha = 0.5) +
      facet_wrap(. ~ crown_layer, scales = "free") +
      scale_color_gradient(low = "blue", high = "orange") +
      theme_bw() +
      ylab("stan_mean_survival_rate") + xlab("data survival_mean")
    print(paste(c(plot.temp, " cor survival: ", cor(survival.to.plot$survival_mean, survival.to.plot$stan_mean_surv_rate)), collapse = ""))
    cor(survival.to.plot$survival_mean, survival.to.plot$stan_mean_surv_rate)
    
    plot.recruitment = ggplot(data = recruitment.to.plot) +
      geom_point(aes(x = rec_mean, y = stan_mean_nr_recruits, alpha = rec_n, size = rec_n)) +
      geom_abline(aes(intercept = 0, slope = 1)) +
      theme_bw()
    print(paste(c(plot.temp, " cor rec ba: ", cor(recruitment.to.plot$rec_mean, recruitment.to.plot$stan_mean_nr_recruits)), collapse = ""))
    
    ggsave(paste(c(output.folder,"/",plot.temp,"/",plot.temp, "_stan_vs_data_growth.png"), collapse = ""),
           plot.growth, height = 5, width = 12)
    ggsave(paste(c(output.folder,"/",plot.temp,"/",plot.temp, "_stan_vs_data_survival.png"), collapse = ""),
           plot.survival, height = 5, width = 12)
    ggsave(paste(c(output.folder,"/",plot.temp,"/",plot.temp, "_stan_vs_data_recruitment.png"), collapse = ""),
           plot.recruitment, height = 5, width = 12)
    
  }
  
}

###################################################################
extract.the.number.of.individuals.and.species = function(CTFS.plots,
                                                         folder.of.data.to.compile){
  
  results = list(summary = data.frame(CTFS_plot = CTFS.plots$CTFS_plot,
                                      nr_sp = NA, nr_ind = NA),
                 sp_list = data.frame(CTFS_plot = NA, sp = NA, latin = NA)[0,])
  
  all.sp.tables = list()

  # bci
  load("data/bci/species list/bci.spptable.RDATA")
  all.sp.tables[["bci"]] = data.frame("sp" = tolower(bci.spptable$sp),
                                      "latin" = tolower(bci.spptable$Latin))
  
  # fushan
  fushan.spptable = fread("data/fushan/species list/splist_all.txt")
  all.sp.tables[["fushan"]] = data.frame("sp" = tolower(fushan.spptable$sp),
                                         "latin" = tolower(fushan.spptable$fullname))
  
  # hkk
  load("data/hkk/species list/hkk.spptable.RDATA")
  all.sp.tables[["hkk"]] = data.frame("sp" = tolower(hkk.spptable$sp),
                                      "latin" = tolower(hkk.spptable$Latin))
  
  # ituri_edoro
  load("data/ituri_edoro/species list/ituri.spptable.RDATA")
  all.sp.tables[["ituri_edoro"]] = data.frame("sp" = tolower(ituri.spptable$sp),
                                              "latin" = tolower(ituri.spptable$Latin))
  all.sp.tables[["ituri_lenda"]] = data.frame("sp" = tolower(ituri.spptable$sp),
                                              "latin" = tolower(ituri.spptable$Latin))
  
  # korup
  load("data/korup/species list/korup.spptable.RDATA")
  all.sp.tables[["korup"]] = data.frame("sp" = tolower(korup.spptable$sp),
                                        "latin" = tolower(korup.spptable$Latin))
  
  # lambir
  lambir.spptable = fread("data/lambir/species list/Lambir_species_list.txt")
  all.sp.tables[["lambir"]] = data.frame("sp" = tolower(lambir.spptable$Mnemonic),
                                        "latin" = tolower(paste(lambir.spptable$Genus, lambir.spptable$Species, sep = " ")))
  
  # luquillo
  luquillo.spptable = fread("data/luquillo/species list/Luquillo_species_list.txt")
  all.sp.tables[["luquillo"]] = data.frame("sp" = tolower(luquillo.spptable$Mnemonic),
                                           "latin" = tolower(paste(luquillo.spptable$Genus, luquillo.spptable$Species, sep = " ")))
  
  # nanjenshan
  nanjenshan.spptable = fread("data/nanjenshan/species list/forestgeo webpage and abbreviations merged.txt")
  all.sp.tables[["nanjenshan"]] = data.frame("sp" = tolower(nanjenshan.spptable$sp),
                                             "latin" = tolower(paste(nanjenshan.spptable$GENUS, nanjenshan.spptable$SPECIES, sep = " ")))
  
  # palanan
  palanan.spptable = fread("data/palanan/species list/Palanan species list.txt")
  all.sp.tables[["palanan"]] = data.frame("sp" = tolower(palanan.spptable$Mnemonic),
                                          "latin" = tolower(paste(palanan.spptable$Genus, palanan.spptable$Species, sep = " ")))
  
  # pasoh
  load("data/pasoh/species list/pasoh.spptable.RDATA")
  all.sp.tables[["pasoh"]] = data.frame("sp" = tolower(pasoh.spptable$sp),
                                        "latin" = tolower(pasoh.spptable$Latin))
  
  # sherman
  sherman.spptable = fread("data/sherman/species list/shermansp.txt")
  all.sp.tables[["sherman"]] = data.frame("sp" = tolower(sherman.spptable$spcode),
                                        "latin" = tolower(paste(sherman.spptable$genus, sherman.spptable$species, sep = " ")))
  
  # sinharaja
  sinharaja.spptable = fread("data/sinharaja/species list/species_list_sinharaja.txt")
  all.sp.tables[["sinharaja"]] = data.frame("sp" = tolower(sinharaja.spptable$Mnemonic),
                                            "latin" = tolower(paste(sinharaja.spptable$Genus, sinharaja.spptable$Species, sep = " ")))
  
  # yasuni
  load("data/yasuni/species list/splist95.rdata")
  load("data/yasuni/species list/splist2007.rdata")
  load("data/yasuni/species list/yasuni.spptable.rdata")

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
  yasuni.spptable = data.table(species.table)

  all.sp.tables[["yasuni"]] = data.frame("sp" = tolower(yasuni.spptable$sp),
                                         "latin" = tolower(paste(yasuni.spptable$genus, yasuni.spptable$specie, sep = " ")))
  
  # make one big table
  all.sp.dt = rbindlist(all.sp.tables, idcol = "CTFS_plot")
  all.sp.stan.dt = all.sp.dt[0,]
  
  # only keep those sp with stan estimates
  for(i in 1:nrow(CTFS.plots)){
    plot.temp = CTFS.plots$CTFS_plot[i]
    survival.data.temp = data.table(readRDS(paste(c(folder.of.data.to.compile,"/",plot.temp,"/",plot.temp, "_all_survival_for_supplement.RDS"), collapse = ""))[[1]])
    survival.stan.temp = data.table(readRDS(paste(c(folder.of.data.to.compile,"/",plot.temp,"/",plot.temp, "_stan_survival.RDS"), collapse = "")))
    
    survival.data.temp = survival.data.temp[sp %in% unique(survival.stan.temp$sp)]
    all.sp.dt.temp = all.sp.dt[CTFS_plot == plot.temp & sp %in% survival.stan.temp$sp]
    results$summary$CTFS_plot[i] = plot.temp
    results$summary$nr_sp[i] = length(unique(survival.stan.temp$sp))
    results$summary$nr_ind[i] = nrow(survival.data.temp)
    
    results$sp_list = rbind(results$sp_list, all.sp.dt.temp)}
  
  # remove duplicated names in the sp_list
  results$sp_list$latin_short = sub("^(\\S*\\s+\\S+).*", "\\1", results$sp_list$latin)
  results$sp_list = results$sp_list[!(duplicated(results$sp_list$latin_short)),]
  
  return(results)
}


