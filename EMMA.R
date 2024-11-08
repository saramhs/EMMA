#setup
#Modern Applied Statistics with S
pkgs = c("dplyr", "ggplot2", "MASS") 
install.packages(pkgs)
inst = lapply(pkgs, library, character.only = TRUE) 

#################################################################################################################
################################################## import data ##################################################
#################################################################################################################

getwd() 
dir() 
setwd("C:/Users/mohan197/EMMA_Marcell/Data/Runoff/")

runoff<-read.csv("2024.02.27_ROChemistry2009-2011.csv")

setwd("C:/Users/mohan197/EMMA_Marcell/Codes/")
source("data.r")


setwd("C:/Users/mohan197/EMMA_Marcell/Data/Snow_Seasons_Mariel/")
snow_timing<-read.csv("snow_seasons.csv")

############################################Functions#############################################################
##################################################################################################################

####################################### lowercase the column names ########################################

lowercase<-function(df){
  colnames(df)<-tolower(colnames(df))
  return(df)
}

###################################### runoff date column has a different name  ##########################

change_column_name <- function(df, old_colname, new_colname) {
  colnames(df)[which(names(df) == old_colname)] <- new_colname
  return(df)
}

############################ make sure df's date column is of type "date" and is of the same format "year month day" ###########################

#date_column_chr<-function (df){
#  df$date <- as.Date(strptime(df$date, format = "%m/%d/%Y %H:%M"))
 # return(df)
#}

date_column_chr<-function (df){
  df$datetime <- as.POSIXct(strptime(df$datetime, format = "%m/%d/%Y %H:%M")) 
  return(df)
}
date_column_chr_nohours<-function (df){
  df$date <- as.Date(strptime(df$date, format = "%m/%d/%Y"))
  return(df)
}


date_column<-function (df){
  df$date <- as.Date(df$date, format = "%m/%d/%Y %H:%M")
  return(df)
}

#df$date<-as.Date(df$date,format="%d/%m/%Y %H:%M")

##################################### filter for the shared time #######################################

filter_date <- function(df, start_date, end_date) {
  filtered_df <- df %>%
    filter(as.Date(date, format = "%Y-%m-%d") >= as.Date(start_date) &
             as.Date(date, format = "%Y-%m-%d") <= as.Date(end_date))
  
  return(filtered_df)
}

##################################### separate date and time ##########################################
date_time_separated<-function(df){
  df$time<-format(as.POSIXct(df$datetime), format = "%H:%M:%S")
  df$date<-as.Date(df$datetime, format = "%m/%d/%Y")
  return(df)
}

#################################### Define seasons ###################################################

#month, month_name,month_range, season_name
add_month_season <- function(df) {
  df$date_copied<-df$date
  df$date<-as.Date(df$date,format="%Y-%m-%d") #this is only for Contributions #"%m-%d-%Y") THIS IS FOR ALL OTHER DATA
  df$date<-as.Date(df$date,format="%Y-%m-%d")
  #create month column
  df$month <- lubridate::month(df$date)
  #create month name
  df$month_name <- case_when(
    df$month == 1 ~ "January",
    df$month == 2 ~ "February",
    df$month == 3 ~ "March",
    df$month == 4 ~ "April",
    df$month == 5 ~ "May",
    df$month == 6 ~ "June",
    df$month == 7 ~ "July",
    df$month == 8 ~ "August",
    df$month == 9 ~ "September",
    df$month == 10 ~ "October",
    df$month == 11 ~ "November",
    df$month == 12 ~ "December",
    TRUE ~ NA_character_  # Handle any other cases (optional)
  )
  # Create month range
 # df$month_range <- case_when(
 #   df$month %in% 7:10 ~ "Jul-Oct",
 #   df$month %in% c(11:12, 1:3) ~ "Nov-Mar",
 #   df$month %in% 4:6 ~ "Apr-June",
 #   TRUE ~ "Other"
 # )
  
  # Create season_name
 # df$season_name <- case_when(
  #  df$month_range %in% "Jul-Oct" ~ "Growing",
   # df$month_range %in% "Apr-June" ~ "Melt",
  #  df$month_range %in% "Nov-Mar" ~ "Dormant",
  #  TRUE ~ NA_character_
#  )
  return(df)
}
#the time and number of abrupt changes within time series


####################################################### daily average #############################################
daily_avg<-function(df){
  df%>%
    mutate(year=year(date),month=month(date),day=day(date))%>%
    group_by(year,month,day)%>%
    summarise(across(where(is.numeric), ~mean(., na.rm=TRUE)))%>%
    mutate(date=as.Date(paste(year,month,day,sep='-')))
}


################################################### What's the use? ###############################################

#when combining two dataset that have data for the same date
avg_same_date<-function(df){ 
  result<-df%>%
    group_by(date) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE))
  return(result)
}

avg_same_date <- function(df) { 
  result <- df %>%
    group_by(date) %>%
    summarise(across(where(is.numeric), ~ mean(., na.rm = TRUE)))
  
  print(result)
  
  return(result)
}

#unique(surf_rf_s2n$date)

#check if same date exist among all observations?!
check_same_date<-function(stream,event_precip,bog_pore,lagg_pore,surf_rf_s2n,surf_rf_s2s, sub_rf_s2n, sub_rf_s2s){
  # Extracting the dates from the summaries
  dates_stream <- stream$date
  dates_event_precip <- event_precip$date
  dates_bog_pore_week <-bog_pore$date
  dates_lagg_pore_week <- lagg_pore$date
  dates_surf_rf_s2n <- surf_rf_s2n$date
  dates_surf_rf_s2s <- surf_rf_s2s$date
  dates_sub_rf_s2n <- sub_rf_s2n$date
  dates_sub_rf_s2s <- sub_rf_s2s$date
}

#################################################### Ph ######################################################
transform_ph<-function(df){
  #the negative logarithm (base 10) of the concentration of hydrogen ions in a solution
  #convert pH back to concentration, you would use the inverse of the logarithmic function,which is the exponentiation function.
  df<-mutate(df,h_ion = 10^(-ph))
  return(df)
}

################################################## select & order columns ###########################
select_and_order_columns <- function(df) {
  selected_df <- df %>%
    #select(spcond, cl, so4, ca, k, mg, na, al, fe, mn, si, sr,nh4, srp,tn,tp,npoc,tc.ic,doc,br,thgf,mehgf,pb,o18,d,tempc,h_ion)
    #remove nh4 tc.ic doc br thgf mehgf pb o18 d
    #select(spcond, cl, so4, ca, k, mg, na, al, fe, mn, si, sr, srp,tn,tp,npoc,tempc,h_ion)
    #select(cl, so4, ca, k, mg, na, al, fe, mn, si, sr, srp,tn,tp,npoc,tempc,h_ion,doc)
    #select(ca, mg,sr, srp,npoc,tempc,doc)
    #select(ca, mg,sr, srp)
    select(cl,so4,ca,k,mg,na,al,fe,mn,si,sr,srp,tn,tp,h_ion)
  return(selected_df)
}

rm_columns<-function(df){
  #stream_col<-c("lab_id","peatland","name","date","month","month_name","month_range","season_name","temp_c","stage") #summary(stream_data)
  #bog_lagg_col<-c("lab_id","piezometer","date","month","month_name","month_range","season_name") #summary(bog_pore_week)
  #runoff_col<-c("lab_id","name","type","date","month","month_name","month_range","season_name") #summary(surf_rf)
  #precip_col<-c("lab_id","location","date","month","month_name","month_range","season_name") #summary(event_precip)
  col_rm<-c("lab_id","peatland","piezometer","name","date","type","location","month","month_name","month_range","season_name","temp_c","stage","thgu","mehgu","d202hg","mif204hg","mif201","hg","mif200hg","mif199hg","br","npoc",'tc.ic','uva360s',"bdoc","uva360",'mehgf','thgf','uva254',"mif201hg","feii","feiii","fe3","fe2","uva360f","no3","nh4","tempc","runoffvolume_l","ph")
  selected_df<-df[, !(names(df) %in% col_rm)] #,"date"
  #stream_col) |!(names(df) %in%bog_lagg_col) |!(names(df) %in%runoff_col) |!(names(df) %in%precip_col)
}

####################################################### residual plot #############################################
#Rbias and RRMSE 
corrected_function <- function(stflow_df, residual, solute) {
  rbias <- (sum(residual[[solute]], na.rm = TRUE)) / (mean(stflow_df[[solute]], na.rm = TRUE))
  rrmse <- sqrt(sum(residual[[solute]]^2, na.rm = TRUE)) / (nrow(residual) * mean(stflow_df[[solute]], na.rm = TRUE))
  return(list(rbias = rbias, rrmse = rrmse))
}
#metrics <- corrected_function(stream_20092011_slct, residuals, "spcond")
#rbias <- metrics$rbias
#rrmse <- metrics$rrmse

#plot residual
plot_residual <- function(df_slct, residuals, solute, rbias, rrmse) {
  par(mar = c(4, 4, 2, 2))
  plot(df_slct[[solute]], residuals[[solute]], 
       xlab = solute, 
       ylab = "Residuals")
  abline(h = 0, col = "red", lty = 2)
  
  # Calculate max and min values of x and y coordinates
  max_x <- max(df_slct[[solute]])
  min_x <- min(df_slct[[solute]])
  max_y <- max(residuals[[solute]])
  min_y <- min(residuals[[solute]])
  
  # Add RRMSE and relative bias as text annotations
  text(x = 0.5, y = 0, 
       labels = paste("RRMSE:", round(rrmse, 2), "\nRelative Bias:", round(rbias, 2)),
       pos = 4,font = 2,cex = 1, family = "Times New Roman")  # pos = 4 places text to the right of the coordinates
}


#standardize
standardize_columns <- function(data) {  
  col_means <- colMeans(data, na.rm = TRUE) #[,4:9]
  col_sds <- apply(data, 2, sd, na.rm = TRUE) #[,4:9]
  standardized_data <- na.omit(scale(data, center = col_means, scale = col_sds)) #[,4:9]
  
  return(standardized_data)
}


#projection
proj_on_pcs<- function(data){
  projected_EM<-data %*% pca_result$rotation #loadings <- pca_result$rotation
  return(projected_EM)
}


#extracting indices of different dimensions for the ones that have low or high slope

#when all dimensions are gathered in one column in the indices dataset and you want to separate rows with different col name
indices_dim <- function(indices_slope, col) {
  # Filter rows based on the current column
  indices <- indices_slope[indices_slope[, "col"] == col, ]
  indices<-as.data.frame(indices)
  # Create a new variable using the column name
  new_var_name <- paste0(deparse(substitute(indices_slope)),"_", col, "d")
  
  # Assign the result to the new variable
  assign(new_var_name, indices, envir = .GlobalEnv)
}


#add zero to data so that all have the same period
add_zero<-function(df){
  start_date<- min(stream_data$date)
  end_date<- max(stream_data$date)
  empty_df <- data.frame(date = seq.Date(from = start_date, to = end_date, by = "day"))
  
  # Merge the empty dataframe with your original dataframe
  merged_df <- merge(df, empty_df, by = "date", all = TRUE)
}



############################################### Data Correction ###############################################
###############################################################################################################
grab_weir<-grab
lagg_pool<-lagg
rm(grab)
rm(lagg)
rm(bog_porewater_month)


dataframes<- list(runoff,event_precip,grab_weir,lagg_pool,bog_pore_week,lagg_pore_week,gw_month,snow_timing)



################################ lowercase ###############################
#auto<-lowercase(auto)
#bog_pore_month<-lowercase(bog_pore_month)

lowercased<-lapply(dataframes, lowercase )

runoff<-as.data.frame(lowercased[1])
event_precip<-as.data.frame(lowercased[2])
grab_weir<-as.data.frame(lowercased[3])
lagg_pool<-as.data.frame(lowercased[4])
bog_pore_week<-as.data.frame(lowercased[5])
lagg_pore_week<-as.data.frame(lowercased[6])
gw_month<-as.data.frame(lowercased[7])
snow_timing<-as.data.frame(lowercased[8])
rm(lowercased)

colnames(runoff)[colnames(runoff) == "tp_"] <- "tp"
colnames(runoff)[colnames(runoff) == "tn_"] <- "tn"
colnames(runoff)[colnames(runoff)=="na."]<-"na"
colnames(runoff)[colnames(runoff)=="date.time"]<-"datetime"

################################ add inverse ph column ###############################
#auto<-transform_ph(auto)
#bog_pore_month<-transform_ph(bog_pore_month)
dataframes<- list(runoff,event_precip,grab_weir,lagg_pool,bog_pore_week,lagg_pore_week,gw_month)

transformed_ph<-lapply(dataframes, transform_ph )


runoff<-as.data.frame(transformed_ph[1])
event_precip<-as.data.frame(transformed_ph[2])
grab_weir<-as.data.frame(transformed_ph[3])
lagg_pool<-as.data.frame(transformed_ph[4])
bog_pore_week<-as.data.frame(transformed_ph[5])
lagg_pore_week<-as.data.frame(transformed_ph[6])
gw_month<-as.data.frame(transformed_ph[7])
rm(transformed_ph)


################################ separate date & time columns ###############################
#bog_pore_month<-date_column(bog_pore_month)
runoff<-date_column_chr(runoff) 

dataframes<- list(runoff,event_precip,grab_weir,lagg_pool,bog_pore_week,lagg_pore_week,gw_month)

datetime_separate<-lapply(dataframes, date_time_separated)

runoff<-as.data.frame(datetime_separate[1])
event_precip<-as.data.frame(datetime_separate[2])
grab_weir<-as.data.frame(datetime_separate[3])
lagg_pool<-as.data.frame(datetime_separate[4])
bog_pore_week<-as.data.frame(datetime_separate[5])
lagg_pore_week<-as.data.frame(datetime_separate[6])
gw_month<-as.data.frame(datetime_separate[7])
rm(datetime_separate)

#separate runoff
sub_rf<-runoff[runoff$name=="S2N SUB" | runoff$name=="S2S SUB",]
surf_rf<-runoff[runoff$name=="S2N SURF" | runoff$name=="S2S SURF",] 

############################################## season, month and month range #####################################
runoff<-add_month_season(runoff)
event_precip<-add_month_season(event_precip)
bog_pore_week<-add_month_season(bog_pore_week)
lagg_pore_week<-add_month_season(lagg_pore_week)
#surf_rf_s2n<-add_month_season(surf_rf_s2n)
#surf_rf_s2s<-add_month_season(surf_rf_s2s)
#sub_rf_s2n<-add_month_season(sub_rf_s2n)
#sub_rf_s2s<-add_month_season(sub_rf_s2s)
surf_rf<-add_month_season(surf_rf)
sub_rf<-add_month_season(sub_rf)
grab_weir<-add_month_season(grab_weir)
lagg_pool<-add_month_season(lagg_pool)
gw_month<-add_month_season(gw_month)
#bog_pore_month<-add_month_season(bog_pore_month)

snow_timing_2009_2011<-snow_timing[snow_timing$wyear>=2009 & snow_timing$wyear<=2011,]

grab_weir$name<-"grab_weir"
lagg_pool$name<-"lagg_pool"

#combine streamflow grab and lagg samples
grabweir_laggpool <- bind_rows(grab_weir=grab_weir,lagg_pool=lagg_pool,.id = "name")
grabweir_laggpool<-grabweir_laggpool[order(as.Date(grabweir_laggpool$date, format="%Y-%m-%d")), ] 
#stream_data<-grabweir_laggpool
#1259 (for grab) +734 (for lagg) = 1993 data exist for stream_data
#the grab+lagg data is from 1986/24/03 to 2021/16/11
#rm(grabweir_laggpool)


###### check how they are different (grab_weir & lagg pool) and if lagg pool is similar to lag porewater################


############################################### choose a dataframe for streamwater chemistry ##########################
dataframes<- c("grab_weir","lagg_pool", "grabweir_laggpool")
cat("Available dataframes:\n")
choice_stf<-menu(dataframes, title="choose a dataframe to use as your stream data:")

if (choice_stf>0) {
  stream_data_name<-dataframes[choice_stf]
  stream_data<-get(stream_data_name)
  print(stream_data)
 } else{
    print("not a valid dataframe \n")
  }







#only select the required variables
select_and_order_columns <- function(df) {
  selected_df <-select(df,cl,so4,ca,k,mg,na,al,fe,mn,si,sr,srp,tn,tp,h_ion,doc,o18,d,tempc)
  return(selected_df)
}

#stream_slct<-as.data.frame(select_and_order_columns(stream_data))
stream_slct<-dplyr::select(stream_data,cl,so4,ca,k,mg,na,al,fe,mn,si,sr,srp,tn,tp,h_ion,doc) #stream_data is teh grab+lagg samples
stream_slct_rmNa<-na.omit(stream_slct)
#rm(stream_slct)

data<-stream_slct_rmNa
#rm(stream_slct_rmNa)


#when plotting Missing Map, neede to add zero values to cover the same time period
#stream<-as.data.frame(rm_columns(stream_data))
#precip_add_zero<-as.data.frame(add_zero(event_precip))
#surf_rf_add_zero<-as.data.frame(add_zero(surf_rf))
#sub_rf_add_zero<-as.data.frame(add_zero(sub_rf))
#bog_pore_add_zero<-as.data.frame(add_zero(bog_pore_week))
#lagg_pore_wadd_zero<-as.data.frame(add_zero(lagg_pore_week))


#when plotting Missing Map, neede to drop some columns such as lab_id
#stream_rm<-as.data.frame(rm_columns(stream_data))
#precip_rm<-as.data.frame(rm_columns(precip))
#surf_rf_rm<-as.data.frame(rm_columns(surf_rf))
#sub_rf_rm<-as.data.frame(rm_columns(sub_rf))
#bog_pore_week_rm<-as.data.frame(rm_columns(bog_pore_week))
#lagg_pore_week_rm<-as.data.frame(rm_columns(lagg_pore_week))

#substituting 0.5*detection limit for elements in precipitation
event_precip <- event_precip %>%
  mutate(al = 0.005,
         mn = 0.005,
         si = 0.025,
         sr = 0.005)


print("done with data correction")




########################################### PCA #################################
pca <-princomp(data, cor=TRUE) #this is somehow similar to  prcomp(cleaned_data,center=TRUE, scale. = TRUE). (only the first PC is different)
#print(pca_result2)

#loadings or eigenvectors ; specifically is the eigenvector of the correlation or covariance matrix
PCs<-pca$loading #similar to loadings <- pca_result$rotation (loadings are eigenvectors of the correlation matrix)
#View(PCs)

#variances
summary(pca)
print(summary(pca))

# Extracts Necessary Components
columns<-ncol(data) #columns=14
Comp1<-pca$loading[1:columns]
Comp2<-pca$loading[ (columns+1) : (2*columns) ] #18+1 goes in the second row
Comp3<-pca$loading[ (2*columns+1) : (3*columns) ]
Comp4<-pca$loading[ (3*columns+1) : (4*columns) ]
comp5=PCs[(4*columns+1):(5*columns)]

print("done with pca")

################################################### Projections #####################################

# Produces Standardized Data
stddata<-data
mean<-NULL
sd<-NULL
for (i in names(data)) { #data is in format data.frame
  #returns a subset of your data containing only the first column
  #data[[1]]: gives you the values of the first column directly, without any additional information like row numbers or column names.
  mean[[i]]<-mean(data[[i]],na.rm=TRUE) #set na.rm to TRUE if NA values should be ignored
  sd[[i]]<-sd(data[[i]],na.rm=TRUE) #set na.rm to TRUE if NA values should be ignored
  stddata[[i]]=(data[[i]]-mean[[i]])/sd[[i]] #[[1]] means first column
}
#standardizing data would also deseasonalize the data

#identical(stddata2, stddata)

#stddata <- mutate_all(data, ~ (.-mean(., na.rm = TRUE)) / sd(., na.rm = TRUE))
#mutate_all: to apply a transformation to all columns of a data frame.
# ~ : to define an anonymous function or formula
# . : columns
# -mean(): subtracts the mean

#identical(stddata2, stddata)
#rm(stddata2)

# eigen vectors, of corr matrix, to be specific 
X<-as.matrix(stddata)
V1<-t(as.matrix(Comp1)) #create vectors
V2<-t(as.matrix(data.frame(Comp1,Comp2))) # as.matrix gets rid of the row names; compare data.frame(Comp1,Comp2)[1,1:2] with as.matrix(data.frame(Comp1,Comp2))[1,1:2].   
V3<-t(as.matrix(data.frame(Comp1, Comp2, Comp3)))
V4<-t(as.matrix(data.frame(Comp1, Comp2, Comp3,Comp4)))
V5<-t(as.matrix(data.frame(Comp1, Comp2, Comp3,Comp4,comp5)))

# Projects Data 
#I have wrongly done the projection with standardized_tracers %*% loadings_4dim
X1<-X %*% (t(V1) %*% (ginv(V1 %*% t(V1)) %*% V1)) #here, creates a matrix
X2<-X %*% (t(V2) %*% (ginv(V2 %*% t(V2)) %*% V2))
X3<-X %*% (t(V3) %*% (ginv(V3 %*% t(V3)) %*% V3))
X4<-X %*% (t(V4) %*% (ginv(V4 %*% t(V4)) %*% V4))
X5<-X %*% (t(V5) %*% (ginv(V5 %*% t(V5)) %*% V5))

X1<-data.frame(X1)
X2<-data.frame(X2)
X3<-data.frame(X3)
X4<-data.frame(X4)
X5<-data.frame(X5)

colnames(X1)<-names(data)
colnames(X2)<-names(data)
colnames(X3)<-names(data)
colnames(X4)<-names(data)
colnames(X5)<-names(data)

# Destandardizes Data
projection1<-data
projection2<-data
projection3<-data
projection4<-data
projection5<-data

#X1, is the projection of the original data on the first eigenvector of the corr matrix, so has 15 variables
#X2, is the projection of the original data on the first two eigenvectors of the corr matrix, again has 15 variables
for (i in names(data)) {
  projection1[[i]]<- (X1[[i]]*sd[[i]])+mean[[i]]
}
for (i in names(data)) {
  projection2[[i]]<- (X2[[i]]*sd[[i]])+mean[[i]]
}
for (i in names(data)) {
  projection3[[i]]<- (X3[[i]]*sd[[i]])+mean[[i]]
}
for (i in names(data)) {
  projection4[[i]]<- (X4[[i]]*sd[[i]])+mean[[i]]
}
for (i in names(data)) {
  projection5[[i]]<- (X5[[i]]*sd[[i]])+mean[[i]]
}

# Creates Residuals
Residual1<- projection1-data
Residual2<- projection2-data
Residual3<- projection3-data
Residual4<- projection4-data
Residual5<- projection5-data 


# Function to extract the overall ANOVA p-value out of a linear model object
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F) #p for probability; cdf; cumulative distribution function (CDF) of the F-distribution
  attributes(p) <- NULL
  return(p)
}

pvalues<-data.frame(matrix(NA, nrow=columns, ncol=5)) #creates a data frame to hold the p-values
RRMSE<-data.frame(matrix(NA, nrow=columns, ncol=5)) #creates a data frame to hold the RRMSE
rsq <- data.frame(matrix(NA, nrow=columns, ncol=5))
slope<-data.frame(matrix(NA,nrow=columns,ncol=5))
#columns: # of constituents


#Computes the p-values and fills the data frame
n<-1
for (i in names(data)) {
  fit<-lm(Residual1[[i]]~data[[i]]) # an object of class "lm"
  slope[n,1]<-coef(fit)[ "data[[i]]" ] # or coefficients(fit) ; coef() is a function which derives a model object's coefficients
  #permutation test p-values vs. the usual F-test p-values
  pvalues[n,1]<-lmp(fit) #Fitting and testing linear models with permutation tests. #tests the created model by running over random set of 
  #A permutation test5 is used to determine the statistical significance of a model by computing a test statistic on the dataset and then for many random permutations of that data.
  RRMSE[n,1] <- (sqrt(sum((fit[5]-data[i])^2))/(length(data[i])*mean[[i]])) #Relative root mean square error
  #  RRMSE[n, 1] <- (sqrt(sum((fit[[5]] - data[[i]])^2)) / (length(data[[i]]) * mean(data[[i]])))
  #fit[[5]] gives you the fitted values 
  rsq[n,1] <- summary(fit)["r.squared"]
  n <- n+1
}


n<-1
for (i in names(data)) {
  fit<-lm(Residual2[[i]]~data[[i]])
  slope[n,2]<-coef(fit)[ "data[[i]]" ]
  pvalues[n,2]<-lmp(fit)
  RRMSE[n,2] <- (sqrt(sum((fit[5]-data[i])^2))/(length(data[i])*mean[[i]]))
  # RRMSE[n, 2] <- (sqrt(sum((fit[[5]] - data[[i]])^2)) / (length(data[[i]]) * mean(data[[i]])))
  rsq[n,2] <- summary(fit)["r.squared"]
  n <- n+1
}

n<-1
for (i in names(data)) {
  fit<-lm(Residual3[[i]]~data[[i]])
  slope[n,3]<-coef(fit)[ "data[[i]]" ]
  pvalues[n,3]<-lmp(fit)
  RRMSE[n,3] <- (sqrt(sum((fit[5]-data[i])^2))/(length(data[i])*mean[[i]]))
  #RRMSE[n, 3] <- (sqrt(sum((fit[[5]] - data[[i]])^2)) / (length(data[[i]]) * mean(data[[i]])))
  rsq[n,3] <- summary(fit)["r.squared"]
  n <- n+1
}

n <- 1
for (i in names(data)) {
  fit <- lm(Residual4[[i]] ~ data[[i]])
  slope[n,4]<-coef(fit)[ "data[[i]]" ]
  pvalues[n, 4] <- lmp(fit)
  RRMSE[n,4] <- (sqrt(sum((fit[5]-data[i])^2))/(length(data[i])*mean[[i]]))
  #RRMSE[n, 4] <- (sqrt(sum((fit[[5]] - data[[i]])^2)) / (length(data[[i]]) * mean(data[[i]])))
  rsq[n, 4] <- summary(fit)$r.squared
  
  n <- n + 1
}

n <- 1
for (i in names(data)) {
  fit <- lm(Residual5[[i]] ~ data[[i]])
  slope[n,5]<-coef(fit)[ "data[[i]]" ]
  pvalues[n, 5] <- lmp(fit)
  RRMSE[n,5] <- (sqrt(sum((fit[5]-data[i])^2))/(length(data[i])*mean[[i]]))
  #RRMSE[n, 5] <- (sqrt(sum((fit[[5]] - data[[i]])^2)) / (length(data[[i]]) * mean(data[[i]])))
  rsq[n, 5] <- summary(fit)$r.squared
  
  n <- n + 1
}

# Labels the Rows and Columns of the P-values
colnames(pvalues)=c("1D", "2D", "3D","4D","5D")
rownames(pvalues)=names(data)

colnames(RRMSE)=c("1D", "2D", "3D","4D","5D")
rownames(RRMSE)=names(data)

colnames(rsq)=c("1D", "2D", "3D","4D","5D")
rownames(rsq)=names(data)

colnames(slope)=c("1D", "2D", "3D","4D","5D")
rownames(slope)=names(data)

print("done with projections") # also residuals


print("done with identifying conservative tracers")

# so far, using streamflow data with a group of constituents, we formed PCs and then for the first two 
#or three PCs, we projected the streamflow with that group to these selected PCs, then looked at each 
#constituent's residuals to decide on conservative ones. From now on, we choose conservative tracers and
#form the PCs based on these conservative tracers. Then, all the streamflow and endmember samples
#will be projected on these new PCs.




######################################## ######################################## ######################################## 
######################################## now identifying EMs (distance & mixing diagram) #######################################
######################################## ######################################## ######################################## 



######################## Projection on the PCs formed only by conservative tracers and
########################  not the previously selected group of constituents
########### Note: #stream_data include grab+lagg samples ;  data<-stream_slct_rmNa
############ nrow(stream_data) : 1993;  nrow(grab): 1259 ;  nrow(lagg): 734

#stream data comes from lagg and grab samples, so we need to identify which samples come from which
# location and whether their chemistry are different
#also, porewater samples come from differnt piezometers and each woudl have their own location

#select conservative tracers in streamflow and endmembers
select_tracers_doc <- function(df) {
  selected_df <- df %>%
    dplyr::select("ca","mg","al","mn","si","sr","doc","date")
  return(selected_df)
}
select_tracers_for_upland <- function(df) {
  selected_df <- df %>%
    dplyr::select("ca","mg","al","mn","si","sr","doc","date","name")
  return(selected_df)
}
select_tracers_toc <- function(df) {
  selected_df <- df %>%
    dplyr::select("ca","mg","al","mn","si","sr","toc","date")
  return(selected_df)
}
select_tracers_toc_gw <- function(df) {
  selected_df <- df %>%
    dplyr::select("ca","mg","al","mn","si","sr","toc","date","name")
  return(selected_df)
}

select_tracers_withdate <- function(df) {
  selected_df <- df %>%
    dplyr::select("ca","mg","al","mn","si","sr","doc","date","name")
  return(selected_df)
}

select_tracers_withpiezometer <- function(df) {
  selected_df <- df %>%
    dplyr::select("ca","mg","al","mn","si","sr","toc","date","piezometer")
  return(selected_df)
}


surf_slct<-select_tracers_for_upland(surf_rf)%>%mutate(group="surf")%>%na.omit()
sub_slct<-select_tracers_for_upland(sub_rf)%>%mutate(group="sub")%>%na.omit()
bog_slct<-select_tracers_withpiezometer(bog_pore_week)%>%mutate(group="bog")%>%na.omit()
lagg_slct<-select_tracers_withpiezometer(lagg_pore_week)%>%mutate(group="lagg")%>%na.omit()
precip_slct<-select_tracers_toc(event_precip)%>%mutate(group="precip")%>%na.omit()
stream_slct<-select_tracers_withdate(stream_data)%>%mutate(year=year(date))%>%na.omit()
gw_slct<-select_tracers_toc_gw(gw_month)%>%mutate(group="gw")%>%na.omit()

colnames(lagg_slct)[colnames(lagg_slct) == "toc"] <- "doc"
colnames(bog_slct)[colnames(bog_slct) == "toc"] <- "doc"
colnames(`precip_slct`)[colnames(precip_slct) == "toc"] <- "doc"
colnames(gw_slct)[colnames(gw_slct) == "toc"] <- "doc"

## I will look at the EMs' distance after looking at the chemistry of endmembers and exploring why they show this chemical behavior

###################################### Exploring source waters chemistry #####################################

####################################### (1) Mixing diagrams with biplots ######################################
####################################### (2) Time Series of constituents' concentrations in endmembers ######################################
####################################### (3) c-q plots for subsurface-soil moisture ######################################


library("dplyr")
library("lubridate")

#data
#stream_slct<-dplyr::select(stream_data,date,name,ca,mg,mn,al,si,sr,doc)
stream_tracers<-stream_slct
  #na.omit(stream_slct)%>%mutate(year=year(date))
data_mixing_space<-stream_tracers[,-c(8,9,10)]

#gw starts from 2007 but when applying na.omit it starts from 2013 and is not applicable for our year of desired (2009-2011)

#colnames(bog_pore_month)[colnames(bog_pore_month) == "toc"] <- "doc"
bog_month_depth0<-bog_pore_month%>% filter(depth == 0)
bog_month_depth30<-bog_pore_month%>% filter(depth == 30)
bog_month_depth50<-bog_pore_month%>% filter(depth == 50)
bog_month_depth100<-bog_pore_month%>% filter(depth == 100)
bog_month_depth150<-bog_pore_month%>% filter(depth == 150)
bog_month_depth200<-bog_pore_month%>% filter(depth == 200)


selecting<-function(df){
  df<-dplyr::select(df,date,name,piezometer,ca,mg,mn,al,si,sr,doc)
  return(df)
}

bog_month_depth0_slct<-selecting(bog_month_depth0)%>%mutate(group="bog0")%>%na.omit()

bog_month_depth30_slct<-selecting(bog_month_depth30)%>%mutate(group="bog30")%>%na.omit()

bog_month_depth50_slct<-selecting(bog_month_depth50)%>%mutate(group="bog50")%>%na.omit()

bog_month_depth100_slct<-selecting(bog_month_depth100)%>%mutate(group="bog100")%>%na.omit()

bog_month_depth150_slct<-selecting(bog_month_depth150)%>%mutate(group="bog150")%>%na.omit()

bog_month_depth200_slct<-selecting(bog_month_depth200)%>%mutate(group="bog200")%>%na.omit()

############################## explore each dataset based on their selected tracers and group ##################
library(dplyr)
stream_tracers_summary<-stream_tracers[,-c(8,10,11)]%>%
  group_by(name) %>%
  summarise(across(everything(), list(mean = ~mean(.), sd = ~sd(.), median = ~median(.)), .names = "{.col}_{.fn}"))

print(stream_tracers_summary)
stream_tracers_summary<-as.data.frame(stream_tracers_summary)
write.csv((stream_tracers_summary), "stream_tracers_summary.csv", row.names = FALSE)

#################################### PROJECTION ####################################
####################################  for mixing diagram   ####################################


mixing_biplot<-function(yr)
{
  #yr<-2013
  stream_tracers_yr<-stream_tracers%>% filter(year(date) == yr) 
  lagg_yr<-lagg%>% filter(year(date) == yr) 
  bog_yr<-bog%>% filter(year(date) == yr) 
  surf_yr<-surf%>% filter(year(date) == yr) 
  sub_yr<-sub%>% filter(year(date) == yr) 
  precip_yr<-precip%>% filter(year(date) == yr) 
  gw_yr<-gw%>% filter(year(date) == yr) 
  #deeper bog starts from the year 2014 and hence not useful the years 2009-2011
  bog_month_depth0_yr<-bog_month_depth0_slct%>% filter(year(date) == yr) 
  bog_month_depth30_yr<-bog_month_depth30_slct%>% filter(year(date) == yr) 
  bog_month_depth50_yr<-bog_month_depth50_slct%>% filter(year(date) == yr) 
  bog_month_depth100_yr<-bog_month_depth100_slct%>% filter(year(date) == yr) 
  bog_month_depth150_yr<-bog_month_depth150_slct%>% filter(year(date) == yr) 
  bog_month_depth200_yr<-bog_month_depth200_slct%>% filter(year(date) == yr)
  
  data_mixing_space<-stream_tracers_yr[-c(8:11)]
  #pca
  pca_mixing <-princomp(data_mixing_space, cor=TRUE) #eigenvalue decomposition #using correlation matrix does mean that we are using the standardized version of data
  
  
  
  # Extracts Necessary Components
  columns<-ncol(data_mixing_space) #columns=14
  Comp1<-pca_mixing$loading[1:columns]
  Comp2<-pca_mixing$loading[ (columns+1) : (2*columns) ] #18+1 goes in the second row
  dim<-2 #2 dimensions
  comps<-pca_mixing$loading[1:(columns*dim)]
  V<-as.matrix(data.frame(split(comps, 1:columns)))
  V_named<-V
  colnames(V_named)<-c("Ca","Mg","Mn","Al","Si","Sr","DOC")
  
  #From the file  #timeseries_lower right of mixing diagram
  # Produces Standardized Data
  lagg_rm<-lagg_yr[,-c(8:11)]
  bog_rm<-bog_yr[,-c(8:11)]
  surf_rm<-surf_yr[,-c(8:11)]
  sub_rm<-sub_yr[,-c(8:11)]
  precip_rm<-precip_yr[,-c(8:10)]
  gw_rm<-gw_yr[,-c(8:11)]
  bog0_rm<-bog_month_depth0_yr[,-c(1:3,11,12)]
  bog30_rm<-bog_month_depth30_yr[,-c(1:3,11,12)]
  bog50_rm<-bog_month_depth50_yr[,-c(1:3,11,12)]
  bog100_rm<-bog_month_depth100_yr[,-c(1:3,11,12)]
  bog150_rm<-bog_month_depth150_yr[,-c(1:3,11,12)]
  bog200_rm<-bog_month_depth200_yr[,-c(1:3,11,12)]
  
  
  stddata<-data_mixing_space
  mean<-NULL
  sd<-NULL
  for (i in names(data_mixing_space)) {
    mean[[i]]<-mean(data[[i]]) #set na.rm to TRUE if NA values should be ignored
    sd[[i]]<-sd(data[[i]]) #set na.rm to TRUE if NA values should be ignored
    stddata[[i]]=(data_mixing_space[[i]]-mean[[i]])/sd[[i]]
  }
  
  stdlagg<-lagg_rm
  for (i in names(data_mixing_space)) {
    stdlagg[[i]]=(lagg_rm[[i]]-mean[[i]])/sd[[i]]
  }
  stdbog<-bog_rm
  for (i in names(data_mixing_space)) {
    stdbog[[i]]=(bog_rm[[i]]-mean[[i]])/sd[[i]]
  }
  stdsurf<-surf_rm
  for (i in names(data_mixing_space)) {
    stdsurf[[i]]=(surf_rm[[i]]-mean[[i]])/sd[[i]]
  }
  stdsub<-sub_rm
  for (i in names(data_mixing_space)) {
    stdsub[[i]]=(sub_rm[[i]]-mean[[i]])/sd[[i]]
  }
  stdprecip<-precip_rm
  for (i in names(data_mixing_space)) {
    stdprecip[[i]]=(precip_rm[[i]]-mean[[i]])/sd[[i]]
  }
  stdgw<-gw_rm
  for (i in names(data_mixing_space)) {
    stdgw[[i]]=(gw_rm[[i]]-mean[[i]])/sd[[i]]
  }
  stdbog_month0<-bog0_rm
  for (i in names(data_mixing_space)) {
    stdbog_month0[[i]]=(bog0_rm[[i]]-mean[[i]])/sd[[i]]
  }
  stdbog_month30<-bog30_rm
  for (i in names(data_mixing_space)) {
    stdbog_month30[[i]]=(bog30_rm[[i]]-mean[[i]])/sd[[i]]
  }
  stdbog_month50<-bog50_rm
  for (i in names(data_mixing_space)) {
    stdbog_month50[[i]]=(bog50_rm[[i]]-mean[[i]])/sd[[i]]
  }
  stdbog_month100<-bog100_rm
  for (i in names(data_mixing_space)) {
    stdbog_month100[[i]]=(bog100_rm[[i]]-mean[[i]])/sd[[i]]
  }
  stdbog_month150<-bog150_rm
  for (i in names(data_mixing_space)) {
    stdbog_month150[[i]]=(bog150_rm[[i]]-mean[[i]])/sd[[i]]
  }
  stdbog_month200<-bog200_rm
  for (i in names(data_mixing_space)) {
    stdbog_month200[[i]]=(bog200_rm[[i]]-mean[[i]])/sd[[i]]
  }
  
  
  stddata_mtx<-as.matrix(stddata)
  stdlagg_mtx<-as.matrix(stdlagg)
  stdbog_mtx<-as.matrix(stdbog)
  stdsurf_mtx<-as.matrix(stdsurf)
  stdsub_mtx<-as.matrix(stdsub)
  stdprecip_mtx<-as.matrix(stdprecip)
  stdgw_mtx<-as.matrix(stdgw)
  stdbog_month0_mtx<-as.matrix(stdbog_month0)
  stdbog_month30_mtx<-as.matrix(stdbog_month30)
  stdbog_month50_mtx<-as.matrix(stdbog_month50)
  stdbog_month100_mtx<-as.matrix(stdbog_month100)
  stdbog_month150_mtx<-as.matrix(stdbog_month150)
  stdbog_month200_mtx<-as.matrix(stdbog_month200)
  
  
  # Projects Data onto Components
  stream_projection<-data.frame(stddata_mtx %*% t(V))
  
  #projection for each endmember 
  lagg_projection<-data.frame(stdlagg_mtx %*% t(V)) #lagg
  bog_projection<-data.frame(stdbog_mtx %*% t(V))  #bog
  surf_projection<-data.frame(stdsurf_mtx %*% t(V)) #surf
  sub_projection<-data.frame(stdsub_mtx %*% t(V))   #sub
  precip_projection<-data.frame(stdprecip_mtx %*% t(V))   #precip
  gw_projection<-data.frame(stdgw_mtx %*% t(V))   #gw
  
  bog_month0_projection<-data.frame(stdbog_month0_mtx %*% t(V))  
  bog_month30_projection<-data.frame(stdbog_month30_mtx %*% t(V))  
  bog_month50_projection<-data.frame(stdbog_month50_mtx %*% t(V))  
  bog_month100_projection<-data.frame(stdbog_month100_mtx %*% t(V))  
  bog_month150_projection<-data.frame(stdbog_month150_mtx %*% t(V))  
  bog_month200_projection<-data.frame(stdbog_month200_mtx %*% t(V))  
  
  
  #sub_projection_big<-which(sub_projection$X1 > 20)
  #sub_projection_rmdbig<-sub_projection %>% filter(row_number() != sub_projection_big)
  
  
  #lagg_projection_big<-which(lagg_projection$X1 > 15)
  #lagg_projection_rmdbig<-lagg_projection %>% filter(row_number() != lagg_projection_big)
  
  #In the mixing space, use the V_named to draw the arrows,
  #each column will be one arrow with the first row (coordiante along pc1) being the x of the endpoint and the second row being the y of the endpoint
  stream_projection_season<-stream_projection%>%mutate(date=stream_tracers_yr$date)%>%mutate(season=stream_tracers_yr$season)%>%mutate(name=stream_tracers_yr$name)
  lagg_projection_season<-lagg_projection%>%mutate(date=lagg_yr$date)%>%mutate(season=lagg_yr$season)%>%mutate(piezometer=lagg_yr$piezometer)
  bog_projection_season<-bog_projection%>%mutate(date=bog_yr$date)%>%mutate(season=bog_yr$season)%>%mutate(piezometer=bog_yr$piezometer)
  surf_projection_season<-surf_projection%>%mutate(date=surf_yr$date)%>%mutate(season=surf_yr$season)%>%mutate(name=surf_yr$name)
  sub_projection_season<-sub_projection%>%mutate(date=sub_yr$date)%>%mutate(season=sub_yr$season)%>%mutate(name=sub_yr$name)
  precip_projection_season<-precip_projection%>%mutate(date=precip_yr$date)%>%mutate(season=precip_yr$season)
  
  precip_projection_season$location<-"precip"
  
  
  
  arrows_df <- data.frame(
    variable = rep(colnames(V_named), each = 1),
    PC1 = as.vector(V_named[1, ]),
    PC2 = as.vector(V_named[2, ])
  )
  
  
  lagg_projection_season<-lagg_projection_season %>%
    rename(location = piezometer)
  
  bog_projection_season<-bog_projection_season %>%
    rename(location = piezometer)
  
  surf_projection_season<-surf_projection_season %>%
    rename(location = name)
  sub_projection_season<-sub_projection_season %>%
    rename(location = name)
  
  stream_projection_season<-stream_projection_season %>%
    rename(location = name)
  ########################################## MIXING DIAGRAM WITH BIPLOT ######################################
  ##########################################                            ##########################################
  
  #pch_shape <- c("melt" = 18, "growing" = 17, "dormant" = 8)
  #grablagg_size<-c("grab"=12, "lagg"=2)
  shape_stream<-c("grab_SF"=8, "lagg_SF"=14)
  #season_colors <- c("melt" = alpha("blue",0.5), "growing" =alpha("#01bd5f",0.5), "dormant" = alpha("darkred",0.5))
  #rm(season_colors)
  #install.packages("ggrepel")
  library(ggrepel)
  
  library(ggplot2)
  x_limits <- c(-10, 10)
  y_limits <- c(-10, 10)
  
  p <- ggplot(data = stream_projection_season, aes(x = X1, y = X2)) +
    geom_point(aes(shape=location),#shape = season, #,size=factor(name). #color = season,shape=location)
               size = 6, alpha = 0.8,color="purple") +facet_wrap(~location)+
    ##scale_shape_manual(values = pch_shape) +
    scale_shape_manual(values = shape_stream) +
    #scale_color_manual(values = season_colors) + 
    ##scale_size_manual(values=grablagg_size)+
    labs(x = "PC 1 (75.3%)", y = "PC 2 (10.6%)")+
    scale_x_continuous(limits = x_limits,
                       breaks = seq(-10, 10, by = 5) # Explicitly setting the x-axis ticks
    ) +
    scale_y_continuous(limits = y_limits,
                       breaks =seq(-10, 10, by = 5)  # Explicitly setting the y-axis ticks
    ) +
    theme(text = element_text(family = "Times", size = 14), 
          axis.title = element_text(size = 14), 
          axis.text = element_text(size = 14),  
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14),#panel.background = element_blank(),
          panel.grid.major = element_line(color = "grey90"),
          panel.grid.minor = element_line(color = "grey90"),
          panel.background = element_rect(fill = "white", color = NA),
          panel.border = element_blank())+
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black")
  
  lagg_projection_season$group <- "Lagg"
  bog_projection_season$group <- "Bog"
  #surf_projection_season$group <- "Surf_rf"
  #sub_projection_season$group <- "Sub_rf"
  precip_projection_season$group <- "Precip"
  #bog_month0_projection_season$group <- "bog0"
  #bog_month30_projection_season$group <- "bog30"
  #bog_month50_projection_season$group <- "bog50"
  #bog_month100_projection_season$group <- "bog100"
  #bog_month150_projection_season$group <- "bog150"
  #bog_month200_projection_season$group <- "bog200"
  
  all_projections <- rbind(
    transform(lagg_projection_season[,-c(3)], shape = "Lagg",color="Lagg"), #, color = season
    transform(bog_projection_season[,-c(3)], shape = "Bog",color="Bog"),
    #transform(surf_projection_season[,-c(3)], shape = "Surf_rf",color="Surf_rf"),#, color = season
    #transform(sub_projection_season[,-c(3)], shape = "Sub_rf",color="Sub_rf"),#, color = season
    transform(precip_projection_season[,-3], shape = "Precip",color="Precip")
    #transform(bog_month0_projection, shape = "bog0", color = "bog0"),
    #  transform(bog_month30_projection[,-3], shape = "bog30", color = "bog30"),
    #  transform(bog_month50_projection[,-3], shape = "bog50", color = "bog50"),
    # transform(bog_month100_projection, shape = "bog100", color = "bog100"),
    # transform(bog_month150_projection[,-3], shape = "bog150", color = "bog150")
    # transform(bog_month200_projection, shape = "bog200", color = "bog200")
    
  )
  
  # Add EM points
  p <- p +
    geom_point(data = all_projections, aes(x = X1, y = X2, shape = shape,color=group)#, color = season
               ,alpha=0.8, size = 5)#+facet_wrap(~season)
  guides(shape = guide_legend(override.aes = list(size = 14)))
  # Adding shapes and colors for the new points
  additional_shapes <- c("Lagg" = 16, "Bog" = 18, "Surf_rf" = 1, "Sub_rf" = 18, "Precip" = 20)  #,"bog0"=16,"bog30"=16,"bog50"=16,"bog100"=16,"bog150"=16,"bog200"=16)
  additional_colors <- c("Lagg" = alpha("#6b7701",0.9), "Bog" = alpha("#dfd901",0.8), "Surf_rf" = alpha("cornflowerblue",0.7) , "Sub_rf" = alpha("chocolate4",0.7) , "Precip" = "skyblue")#,"bog0"=alpha("#fee39b",0.8),"bog30"=alpha("lightsalmon3",0.7),"bog50"=alpha("brown",0.7),"bog100"="#e4a801","bog150"=alpha("darkgoldenrod4",0.8),"bog200"=alpha("salmon4",0.7))
  
  #"Surf_rf" = 1, "Sub_rf" = 18 "Lagg" = 15,
  #"#548401"
  p <- p + 
    scale_size_manual(values=grablagg_size)+
    scale_shape_manual(values = c(shape_stream, additional_shapes)) +
    scale_color_manual(values = c("purple",additional_colors)) +#season_colors, 
    #scale_color_manual(values = season_colors)+
    guides(shape = guide_legend("Legend"), color = guide_legend("Legend"),size=guide_legend("Legend"))+ggtitle(yr)
  
  
  p <- p +
    geom_segment(data = arrows_df, aes(x = 0, y = 0, xend = PC1 * 10, yend = PC2 * 10),
                 arrow = arrow(length = unit(0.2, "cm") , type="closed"),
                 color = "black")+
    geom_text_repel(data = arrows_df, aes(x = PC1 * 10, y = PC2 * 10, label = variable),
                    family = "Times", size = 14 / .pt, vjust = 0.5, hjust = -0.75, max.overlaps = Inf)   # Adjust font and size
  # geom_text (data = arrows_df, aes(x = PC1 * 6, y = PC2 * 6, label = variable),
  #              family = "Times", size = 10 / .pt, vjust = -0.5, hjust = -0.5)   # Adjust font and size
  
  #p
  # return(p)
  
  return(p)
}

#different years 

yr <- 2009
mixing_biplot(yr)




############################################# preparing the data for the Time series ##########################################
####################################### snow depth data from 2008 ####################################### 

getwd()
setwd("/Users/safl/Reference_folder/EMMA/data/")
#install.packages("readxl")
library("readxl")

snowdepth_GR<-read_excel("snowdepth_grandRapid2008onward.xls")
snowdepth_GR<-as.data.frame(snowdepth_GR)
snowdepth_GR<-lowercase(snowdepth_GR)
snowdepth_GR$date<-as.Date(snowdepth_GR$date)

snowdepth_GR_nm<-snowdepth_GR%>%mutate_at(vars(`minimum temperature degrees (f)`, 
                                               `precipitation (inches)`, 
                                               `snow (inches)`, 
                                               `snow depth (inches)`), 
                                          as.numeric)
#I will only use this:snow depth (inches)`

snowdepth_GR_nm_20092011<-snowdepth_GR_nm%>%filter(year(date)>=2009&year(date)<=2011)
#info:
#precipitation is: rainfall plus the water equivalent found in snowfall
#1 inch of rain is equivalent to 10 inches of snowfall.
# S2S, a north-facing hillslope that is south of the S2 bog.
# S2N, a south-facing hillslope that is north of the S2 bog

################################### 10 minute soil temperature and moisture ############################
setwd("/Users/safl/Reference_folder/EMMA/data/aditional downloaded data/soil moisture 10 min/")

soilTemp_10min<-read.csv("MEF_S2_soil_temp_10min.csv")
soilMoist_10min<-read.csv("MEF_S2_soil_moisture_10min.csv")
#str(soilTemp_10min)
#str(soilMoist_10min)

soilTemp_10min<-lowercase(soilTemp_10min)
soilMoist_10min<-lowercase(soilMoist_10min)

soilTemp_10min <- soilTemp_10min %>% rename(date = timestamp)
soilMoist_10min <- soilMoist_10min %>% rename(date = timestamp)


date_column_chr_posix<-function (df){
  df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S")
  df$date <- format(df$date, "%Y-%d-%m")
  df$date <- as.Date(df$date)
  return(df)
}

soilTemp_10min<-date_column_chr_posix(soilTemp_10min)
soilMoist_10min<-date_column_chr_posix(soilMoist_10min)

library("lubridate")
soilMoist_10min_20092011<-soilMoist_10min%>%filter(year(date)>=2009&year(date)<=2011)
soilTemp_10min_20092011<-soilTemp_10min%>%filter(year(date)>=2009&year(date)<=2011)


soilMoist_daily <- soilMoist_10min_20092011 %>%
  group_by(date) %>%
  summarize(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

soilTemp_daily <- soilTemp_10min_20092011 %>%
  group_by(date) %>%
  summarize(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

soilTemp_daily<-as.data.frame(soilTemp_daily)
soilMoist_daily<-as.data.frame(soilMoist_daily)

#combining upland soil data
soil_data <- merge(soilMoist_daily,soilTemp_daily, by = "date")

# soil data and snow data
soil_snow_data <- merge(soil_data, snowdepth_GR_nm_20092011, by = "date")

#sm_snow_data <- merge(soilMoist_daily, snowdepth_GR_nm_20092011, by = "date")
soil_snow_data_2009<-soil_snow_data%>%filter((year(date))==2009)
soil_snow_data_2010<-soil_snow_data%>%filter((year(date))==2010)
soil_snow_data_2011<-soil_snow_data%>%filter((year(date))==2011)

library(scales)
scale<-function(df){
  numeric_columns <- df %>%
    select_if(is.numeric)%>%
    select_if(~ !all(is.na(.)))
  return(numeric_columns)}

soil_snow_data_2009_scaled<-as.data.frame(lapply(scale(soil_snow_data_2009), rescale, to = c(0, 1)))%>%mutate(date=soil_snow_data_2009$date)
soil_snow_data_2010_scaled<-as.data.frame(lapply(scale(soil_snow_data_2010), rescale, to = c(0, 1)))%>%mutate(date=soil_snow_data_2010$date)
soil_snow_data_2011_scaled<-as.data.frame(lapply(scale(soil_snow_data_2011), rescale, to = c(0, 1)))%>%mutate(date=soil_snow_data_2011$date)

library("lubridate")

yr<-2009
stream_tracers_yr<-stream_tracers%>% filter(year(date) == yr) %>%mutate(group=1)%>%mutate(DoY=yday(date))
lagg_yr<-lagg%>% filter(year(date) == yr) %>%mutate(group=2)%>%mutate(DoY=yday(date))
bog_yr<-bog%>% filter(year(date) == yr) %>%mutate(group=3)%>%mutate(DoY=yday(date))
surf_yr<-surf%>% filter(year(date) == yr) %>%mutate(group=4)%>%mutate(DoY=yday(date))
sub_yr<-sub%>% filter(year(date) == yr) %>%mutate(group=5)%>%mutate(DoY=yday(date))
precip_yr<-precip%>% filter(year(date) == yr) %>%mutate(group=6)%>%mutate(DoY=yday(date))
gw_yr<-gw%>% filter(year(date) == yr) %>%mutate(group=7)%>%mutate(DoY=yday(date))

df_binded<-bind_rows(stream_tracers_yr,lagg_yr,surf_yr,sub_yr,precip_yr,.id = "group")

#deeper bog starts from the year 2014 and hence not useful the years 2009-2011
bog_month_depth0_yr<-bog_month_depth0_slct%>% filter(year(date) == yr) 
bog_month_depth30_yr<-bog_month_depth30_slct%>% filter(year(date) == yr) 
bog_month_depth50_yr<-bog_month_depth50_slct%>% filter(year(date) == yr) 
bog_month_depth100_yr<-bog_month_depth100_slct%>% filter(year(date) == yr) 
bog_month_depth150_yr<-bog_month_depth150_slct%>% filter(year(date) == yr) 
bog_month_depth200_yr<-bog_month_depth200_slct%>% filter(year(date) == yr)



############################################# Data availability ############################
library(ggplot2)

ggplot(df_binded, aes(x=DoY, y=si, fill=group, color=group)) +
  geom_point(shape=21, size=3, show.legend=FALSE) + 
  geom_point(aes(y=-as.numeric(group) - 0.5), shape="|", size=8, show.legend=TRUE) +
  geom_hline(yintercept=-1) +
  scale_fill_manual(
    name="source water", 
    labels=c("stream", "lagg", "surface-RF", "subsurface-RF", "precipitation"),
    values=c("blue", "#6b7701", "cornflowerblue", "chocolate4", "skyblue")
  ) +
  scale_color_manual(
    name="source water", 
    labels=c("stream", "lagg", "surface-RF", "subsurface-RF", "precipitation"),
    values=c("blue", "#6b7701", "cornflowerblue", "chocolate4", "skyblue")
  ) +
  scale_y_continuous(breaks=c(0:10, 1)) + 
  ggtitle("2009") + 
  theme_classic(base_size=16) + 
  theme(
    plot.title=element_text(family="Times"),
    panel.background=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank()
  ) +
  guides(color=guide_legend(override.aes=list(fill=NA)))





#rug_p<-ggplot(stream_tracers_yr, aes(date,mg))+geom_jitter()+theme_classic()
#rug_p<-rug_p+geom_rug(stream_tracers_yr, mapping=aes(date,mg), outside=TRUE,sides="bl", alpha=1/2, position="jitter",length=unit(0.05,"npc"))+
#scale_x_date(date_breaks="months",date_labels="%b-%y")+
#ggtitle("Stream, 2009")+theme(plot.title=element_text(family = "Times"))+
#coord_cartesian(clip="off")+theme(plot.margin=margin(1,1,1,1,"cm"))


#geom_jitter(stream_tracers_yr, mapping =aes(date, mg),color = "blue") +  #point1
#geom_rug(data = stream_tracers_yr, mapping = aes(date, mg), outside = TRUE, sides = "bl", alpha = 1/2, position = position_nudge(x = -6), length = unit(0.05, "npc"), color = "blue",show.legend=TRUE) +

#geom_point(stream_tracers_yr,mapping=aes(y = -as.numeric(group) - 2.5, color = "group"), 
#          shape = "|", size = 8)+

#geom_jitter(data = lagg_yr, mapping = aes(date, mg), color = "red") +  #point2
#geom_rug(data = lagg_yr, mapping = aes(date, mg),outside = TRUE,sides = "bl", alpha = 1/2, position = position_nudge(x = -6), color = "red", show.legend=TRUE) +
#geom_jitter(data = surf_yr, mapping = aes(date, mg), color = "green") +  #point2
#geom_rug(data = surf_yr, mapping = aes(date, mg),outside = TRUE,sides = "bl", alpha = 1/2, position = position_nudge(x = -6), color = "green", show.legend=TRUE) +



############################################## Time sereis #################################################
# Time series of different elements in different source waters

#preparing data for time series; scaling for comparison
sub_yr_scaled<-as.data.frame(lapply(scale(sub_yr), rescale, to = c(0, 1)))%>%mutate(date=sub_yr$date)








ggplot(stream_tracers_yr, aes(date, mg)) + stat_bin_2d(bins = 40, colour = "white")+theme_classic()+ scale_x_continuous(limits = c(0, 1))

library(ggplot2)
library(grid)

# Adding offset variables
stream_tracers_yr$offset <- 0
lagg$offset <- 1

# Base plot with jitter
rug_p <- ggplot(stream_tracers_yr, aes(date, mg)) +
  geom_jitter(color = "blue") +  # Color for stream_tracers_yr
  geom_jitter(data = lagg, mapping = aes(date, mg), color = "red") +  # Add lagg data with different color
  theme_classic() +
  scale_x_date(date_breaks = "months", date_labels = "%b-%y") +
  ggtitle("Stream, 2009") +
  theme(plot.title = element_text(family = "Times")) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"))

# Adding the rugs with offset
rug_p <- rug_p +
  geom_rug(data = stream_tracers_yr, mapping = aes(date, mg + offset), outside = TRUE, sides = "b", alpha = 1/2, length = unit(0.05, "npc"), color = "blue") +
  geom_rug(data = lagg, mapping = aes(date, mg + offset + 1), outside = TRUE, sides = "b", alpha = 1/2, length = unit(0.05, "npc"), color = "red")

# Print the plot
print(rug_p)








library(tidyverse)

Ratio <- max(c(soil_snow_data_2009$s2s_lo_dp.x, soil_snow_data_2009$s2s_lo_dp.y),na.rm = TRUE) / max(soil_snow_data_2009$s2s_lo_dp.x,na.rm = TRUE)
SMMax <- max(soil_snow_data_2009$s2s_lo_dp.x,na.rm = TRUE)
BottomOffset <- 0.05

#si, ca, sr, mg,mn,al,doc
p <- ggplot(soil_snow_data_2009, aes(x = date)) +
  geom_line(aes(y = s2s_lo_sh.y, color = "SoilTemp")) +
  geom_bar(aes(y = `snow depth (inches)`, fill = "Snowdepth"),color =alpha("skyblue",0.4), stat = "identity", position = "identity") +
  geom_rect(aes(xmin = as.Date(date) - 0.1,
                xmax = as.Date(date) + 0.1,
                ymin = (BottomOffset + SMMax - s2s_lo_sh.x) * Ratio,
                ymax = (BottomOffset + SMMax) * Ratio,
                fill = "SVWC"),color=alpha("blue", 0.5)) + 
  geom_hline(yintercept = (BottomOffset + SMMax) * Ratio, color = "blue") +
  geom_hline(yintercept = 0, color = "black") +
  geom_point(data = sub_yr , aes(y =si, color="Si"),  size = 2) + #aes(y = element, color="element"),  size = 2) +  # Adding geom_point for si
  
  labs(title = "Si in subsurface (2009); S2S-lower position- depth", x = "Date", y = "", color = "Variable") +
  scale_y_continuous(name = expression(" "), sec.axis = sec_axis(~ BottomOffset + SMMax  - . / Ratio, name = "SVWC (-)"), expand = c(0, 0)) +
  scale_color_manual(name = " ", values = c("SoilTemp" = "red", "Si" ="black")) +
  scale_fill_manual(name = " ", values = c(" ","Snowdepth" = alpha("skyblue", 0.4),"SVWC" = alpha("blue", 0.5))) +
  theme_classic() +
  theme(axis.line.y.right = element_line(color = "blue"), 
        axis.ticks.y.right = element_line(color = "blue"),
        axis.text.y.right = element_text(color = "blue"),
        axis.title.y.right = element_text(color = "blue"),
        axis.line.y.left = element_line(color = "black"), 
        axis.ticks.y.left = element_line(color = "black"),
        axis.text.y.left = element_text(color = "black"),
        axis.title.y.left = element_text(color = "black"),
        legend.position = "bottom")

print(p)





################################### distance ###############################################

surf_slct_mean <- apply(surf_slct, 2, mean, na.rm = TRUE)
sub_slct_mean <- apply(sub_slct, 2, mean, na.rm = TRUE)
bog_slct_mean <- apply(bog_slct, 2, mean, na.rm = TRUE) #2 means applying the function to each column of the matrix
lagg_slct_mean <- apply(lagg_slct, 2, mean, na.rm = TRUE)
precip_slct_mean <- apply(precip_slct, 2, mean, na.rm = TRUE)

# Compute the standard deviation for each column of the ENDMEMBERS
surf_slct_std <- apply(surf_slct, 2, sd, na.rm = TRUE)
sub_slct_std <- apply(sub_slct, 2, sd, na.rm = TRUE)
bog_slct_std <- apply(bog_slct, 2, sd, na.rm = TRUE)
lagg_slct_std <- apply(lagg_slct, 2, sd, na.rm = TRUE)
precip_slct_std <- apply(precip_slct, 2, sd, na.rm = TRUE)

surf_slct_mean_df <- as.data.frame(t(surf_slct_mean))
sub_slct_mean_df <- as.data.frame(t(sub_slct_mean))
bog_slct_mean_df<-as.data.frame(t(bog_slct_mean))
lagg_slct_mean_df<-as.data.frame(t(lagg_slct_mean))
precip_slct_mean_df<-as.data.frame(t(precip_slct_mean))

surf_slct_std_df <- as.data.frame(t(surf_slct_std))
sub_slct_std_df <- as.data.frame(t(sub_slct_std))
bog_slct_std_df<-as.data.frame(t(bog_slct_std))
lagg_slct_std_df<-as.data.frame(t(lagg_slct_std))
precip_slct_std_df<-as.data.frame(t(precip_slct_std))

combined_df <- rbind(surf_slct_mean_df, sub_slct_mean_df,bog_slct_mean_df,lagg_slct_mean_df,precip_slct_mean_df)

end_std_combined<-rbind(surf_slct_std_df, sub_slct_std_df,bog_slct_std_df,lagg_slct_std_df,precip_slct_std_df)

# Set column names

colnames(combined_df) <- c("ca","mg","al","mn","si","sr","doc")
colnames(end_std_combined) <- c("ca","mg","al","mn","si","sr","doc")

#  colnames(combined_df) <- c("ca","mg","al","mn","si","sr","tp","srp")
# colnames(end_std_combined) <- c("ca","mg","al","mn","si","sr","tp","srp")

# Add row names
rownames(combined_df) <- c("surf_rf", "sub_rf","bog_porewatre","lagg_porewater", "precip")
rownames(end_std_combined) <- c("surf_rf", "sub_rf","bog_porewatre","lagg_porewater","precip")

# Print the combined data frame
#print(combined_df)

ends<-combined_df
ends_std<-end_std_combined
data_with_date<-na.omit(stream_slct)
#  data<-data_with_date[,-9]
data<-data_with_date[,-8]
data_with_season<-add_month_season(data_with_date)


dim<-2

# Runs PCA
#if(nrow(data)>2){
PCA<-princomp(data, cor=TRUE)

# Extracts Necessary Components
columns<-ncol(data)
comps<-PCA$loading[1:(columns*dim)]
V<-as.matrix(data.frame(split(comps, 1:columns)))


# Produces Standardized Data
stddata<-data
mean<-NULL
sd<-NULL
for (i in names(data)) {
  mean[[i]]<-mean(data[[i]]) #set na.rm to TRUE if NA values should be ignored
  sd[[i]]<-sd(data[[i]]) #set na.rm to TRUE if NA values should be ignored
  stddata[[i]]=(data[[i]]-mean[[i]])/sd[[i]]
}

stdends<-ends
for (i in names(data)) {
  stdends[[i]]=(ends[[i]]-mean[[i]])/sd[[i]]
}

stdends_std<-ends_std
for (i in names(data)) {
  stdends_std[[i]]=(ends_std[[i]]-mean[[i]])/sd[[i]]
}

# Projects Data
b<-as.matrix(stdends)
bproj<-data.frame(b %*% (t(V) %*% (ginv(V %*% t(V)) %*% V)))

b_std<-as.matrix(stdends_std)
bproj_std<-data.frame(b_std %*% (t(V) %*% (ginv(V %*% t(V)) %*% V)))

# Destandardizes Data
endproj<-ends
for (i in 1:columns) {
  endproj[[i]]<- (bproj[[i]]*sd[[i]])+mean[[i]]
}

endproj_std<-ends_std
for (i in 1:columns) {
  endproj_std[[i]]<- (bproj_std[[i]]*sd[[i]])+mean[[i]] #standard deviation of ems is projected : endproj_std
}

# Calculates Distance
d<-abs(ends-endproj)
distance<-d

# Formats distances into percents
for (i in 1:columns) {
  distance[[i]]<-sprintf("%1.2f%%",d[[i]]/ends[[i]]*100) #f%%: a floating number with percentage sign
}

#setwd("/Users/safl/Reference_folder/EMMA/results") 

# Writes Distance to a CSV file
#write.csv(distance, file = "Percent_Distances.csv")

#####

############################################### PCA interpretation #############################################

#pca
pca_mixing <-princomp(data_mixing_space, cor=TRUE) #eigenvalue decomposition #using correlation matrix does mean that we are using the standardized version of data

PCs<-pca_mixing$loading #relation between constituents and PCs
PCs

#variances
summary(pca_mixing)
print(summary(pca_mixing))
#scree plot
#install.packages("factoextra")
library("factoextra")
fviz_eig(pca_mixing, addlabels = TRUE, hjust = -0.3, lim = c(0, 100), barfill="white",barcolor ="darkblue"
         ,linecolor ="red")+theme_classic()+labs(title = "Scree plot",
                                                 x = "Principal Components", y = "% of variances")
#variables
var<-get_pca_var(pca_mixing)
head(var$coord)#coordinates
head(var$cos2)#quality
head(var$contrib)#contributions
head(var$cor)#this is the same as coordinates
#correlations
fviz_pca_var(pca_mixing, col.var = "black")
fviz_pca_var(pca_mixing, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
) #colored by quality of representation

fviz_pca_var(pca_mixing, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)#colored by contributions

#contributions
corrplot(var$contrib, is.corr=FALSE)
fviz_contrib(pca_mixing, choice = "var", axes = 1, top = 10)
fviz_contrib(pca_mixing, choice = "var", axes = 2, top = 10)
fviz_contrib(pca_mixing, choice = "var", axes = 1:2, top = 10)

#quality
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)
fviz_cos2(pca_mixing, choice = "var", axes = 1:2)

#dimension description
library("FactoMineR")
pca_mixing2<- PCA(data_mixing_space, graph = FALSE)

pc_desc <- dimdesc(pca_mixing2, axes = c(1,2), proba = 0.05)
pc_desc
pc_desc$Dim.1
pc_desc$Dim.2



#groupings in the plot of PCs
#add year to the streamflow
stream_tracers<-stream_tracers%>%mutate(year=year(date))%>%mutate(season=
                                                                    case_when(month(date)%in%c(4:6)~"melt",
                                                                              month(date)%in%c(7:10)~"growing",
                                                                              month(date)%in%c(11,12,1,2,3)~"dormant"))



stream_tracers_2008<-stream_tracers%>%filter(year==2008)%>%na.omit()
pca_2008<-princomp(stream_tracers_2008[,-c(1,8,9)], cor=TRUE) 

#variables
var_2008<-get_pca_var(pca_2008)
corrplot(var_2008$contrib, is.corr=FALSE)

fviz_pca_ind(pca_2008,col.ind=stream_tracers_2008$season,palette = c("#00AFBB", "#E7B800","#FC4E07"),addEllipses = TRUE)
#,addEllipses = TRUE)
#palette = c("#00AFBB", "#E7B800","#FC4E07")

library(dplyr)
library(ggplot2)
library(factoextra)



yr_mixing<-function(df,yr){
  stream_tracers_yr<- df %>% filter(year == yr) %>% na.omit()
  
  # Perform PCA on the filtered data
  pca_yr <- princomp(stream_tracers_yr[,-c(1,8,9)], cor = TRUE)
  
  # Plot the PCA results
  #fviz_pca_ind(pca_yr, axes=c(1,2),
  #             col.ind = stream_tracers_yr$season, 
  #             palette = c("#00AFBB", "#E7B800", "#FC4E07"),addEllipses = TRUE) +#,ellipse.type = "confidence" or"convex"
  #  ggtitle(paste("PCA ", yr))
  
  ## Plot the biplot 
  fviz_pca_biplot(pca_yr, 
                  col.ind = stream_tracers_yr$season,palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
                  label = "var",addEllipses = TRUE, #label = "all", ,ellipse.type = "confidence" 
                  col.var = "black", repel = TRUE,
                  legend.title = "seasons")+ #xlab = "PC1", ylab = "PC2",
    ggtitle(yr)
  
}

yr <- 2020
yr_mixing(stream_tracers,yr)



#the whole years
fviz_pca_biplot(pca_mixing, 
                col.ind = stream_tracers_season$season,palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
                label = "var",addEllipses = TRUE, #label = "all", ,ellipse.type = "confidence" 
                col.var = "black", repel = TRUE,
                legend.title = "seasons")






















#add season after these codes:
stream_tracers<-na.omit(stream_slct)
data_mixing_space<-stream_tracers[,-c(8,9)]


stream_tracers<-stream_tracers%>%mutate(year=year(date))%>%mutate(season=
                                                                    case_when(month(date)%in%c(4:6)~"melt",
                                                                              month(date)%in%c(7:10)~"growing",
                                                                              month(date)%in%c(11,12,1,2,3)~"dormant"))
#From the file  #timeseries_lower right of mixing diagram
lagg<-lagg_slct%>%mutate(group="lagg")%>%na.omit()%>%mutate(season=
                                                              case_when(month(date)%in%c(4:6)~"melt",
                                                                        month(date)%in%c(7:10)~"growing",
                                                                        month(date)%in%c(11,12,1,2,3)~"dormant"))

bog<-bog_slct%>%mutate(group="bog")%>%na.omit()%>%mutate(season=
                                                           case_when(month(date)%in%c(4:6)~"melt",
                                                                     month(date)%in%c(7:10)~"growing",
                                                                     month(date)%in%c(11,12,1,2,3)~"dormant"))

surf<-surf_slct%>%mutate(group="surf")%>%na.omit()%>%mutate(season=
                                                              case_when(month(date)%in%c(4:6)~"melt",
                                                                        month(date)%in%c(7:10)~"growing",
                                                                        month(date)%in%c(11,12,1,2,3)~"dormant"))

sub<-sub_slct%>%mutate(group="sub")%>%na.omit()%>%mutate(season=
                                                           case_when(month(date)%in%c(4:6)~"melt",
                                                                     month(date)%in%c(7:10)~"growing",
                                                                     month(date)%in%c(11,12,1,2,3)~"dormant"))

precip<-precip_slct%>%mutate(group="precip")%>%na.omit()%>%mutate(season=
                                                                    case_when(month(date)%in%c(4:6)~"melt",
                                                                              month(date)%in%c(7:10)~"growing",
                                                                              month(date)%in%c(11,12,1,2,3)~"dormant"))

gw<-gw_slct%>%mutate(group="gw")%>%na.omit()%>%mutate(season=
                                                        case_when(month(date)%in%c(4:6)~"melt",
                                                                  month(date)%in%c(7:10)~"growing",
                                                                  month(date)%in%c(11,12,1,2,3)~"dormant"))



#after inverse ph column:

#runoff's date column has a different name
runoff <- change_column_name(runoff, "date.time", "datetime")
runoff <- change_column_name(runoff, "datetime", "date")
#auto <- change_column_name(auto, "datetime", "date") #sub-daily
grab <- change_column_name(grab, "datetime", "date") #weekly
lagg <- change_column_name(lagg, "datetime", "date") #weekly
bog_pore_week <- change_column_name(bog_pore_week, "datetime", "date") #porewater
lagg_pore_week <- change_column_name(lagg_pore_week, "datetime", "date") #porewater
event_precip <- change_column_name(event_precip, "datetime", "date") #sporadic
gw_month <- change_column_name(gw_month, "datetime", "date")
#bog_pore_month <- change_column_name(bog_pore_month, "datetime", "date")




#after defining selecting function:
bog_month_depth0_slct<-selecting(bog_month_depth0)%>%mutate(group="bog0")%>%na.omit()%>%mutate(season=
                                                                                                 case_when(month(date)%in%c(4:6)~"melt",
                                                                                                           month(date)%in%c(7:10)~"growing",
                                                                                                           month(date)%in%c(11,12,1,2,3)~"dormant"))

bog_month_depth30_slct<-selecting(bog_month_depth30)%>%mutate(group="bog30")%>%na.omit()%>%mutate(season=
                                                                                                    case_when(month(date)%in%c(4:6)~"melt",
                                                                                                              month(date)%in%c(7:10)~"growing",
                                                                                                              month(date)%in%c(11,12,1,2,3)~"dormant"))

bog_month_depth50_slct<-selecting(bog_month_depth50)%>%mutate(group="bog50")%>%na.omit()%>%mutate(season=
                                                                                                    case_when(month(date)%in%c(4:6)~"melt",
                                                                                                              month(date)%in%c(7:10)~"growing",
                                                                                                              month(date)%in%c(11,12,1,2,3)~"dormant"))

bog_month_depth100_slct<-selecting(bog_month_depth100)%>%mutate(group="bog100")%>%na.omit()%>%mutate(season=
                                                                                                       case_when(month(date)%in%c(4:6)~"melt",
                                                                                                                 month(date)%in%c(7:10)~"growing",
                                                                                                                 month(date)%in%c(11,12,1,2,3)~"dormant"))

bog_month_depth150_slct<-selecting(bog_month_depth150)%>%mutate(group="bog150")%>%na.omit()%>%mutate(season=
                                                                                                       case_when(month(date)%in%c(4:6)~"melt",
                                                                                                                 month(date)%in%c(7:10)~"growing",
                                                                                                                 month(date)%in%c(11,12,1,2,3)~"dormant"))

bog_month_depth200_slct<-selecting(bog_month_depth200)%>%mutate(group="bog200")%>%na.omit()%>%mutate(season=
                                                                                                       case_when(month(date)%in%c(4:6)~"melt",
                                                                                                                 month(date)%in%c(7:10)~"growing",
                                                                                                                 month(date)%in%c(11,12,1,2,3)~"dormant"))






#make sure date is date
runoff<-date_column_chr(runoff)
#auto<-date_column(auto)
grab<-date_column(grab)
lagg<-date_column(lagg)
bog_pore_week<-date_column(bog_pore_week)
lagg_pore_week<-date_column(lagg_pore_week)
event_precip<-date_column(event_precip)
gw_month<-date_column(gw_month)
#bog_pore_month<-date_column(bog_pore_month)
































































































































































































































































# Defie your desired time period


#EMMA analysis will have three parts; 1) Conservative tracers, using residual plots 2) mixing diagrams 3) distance of endmembers
# 4) contributions 5) correction of contributions 6)simulating streamwater chemistry 7) 





