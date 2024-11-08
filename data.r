
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/1179/1/826aa93418f6a48dd8cbb831cc7bd02b" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


auto <-read.csv(infile1,header=F 
                ,skip=1
                ,sep=","  
                , col.names=c(
                  "LAB_ID",     
                  "Peatland",     
                  "NAME",     
                  "DateTime",     
                  "PH",     
                  "SPCOND",     
                  "CL",     
                  "SO4",     
                  "CA",     
                  "K",     
                  "MG",     
                  "Na",     
                  "AL",     
                  "FE",     
                  "MN",     
                  "SI",     
                  "SR",     
                  "SRP",     
                  "TN",     
                  "TP",     
                  "NPOC",     
                  "TC.IC",     
                  "O18",     
                  "D"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(auto$LAB_ID)=="factor") auto$LAB_ID <-as.numeric(levels(auto$LAB_ID))[as.integer(auto$LAB_ID) ]               
if (class(auto$LAB_ID)=="character") auto$LAB_ID <-as.numeric(auto$LAB_ID)
if (class(auto$Peatland)!="factor") auto$Peatland<- as.factor(auto$Peatland)
if (class(auto$NAME)!="factor") auto$NAME<- as.factor(auto$NAME)                                   
# attempting to convert auto$DateTime dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp1DateTime<-as.POSIXct(auto$DateTime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1DateTime) == length(tmp1DateTime[!is.na(tmp1DateTime)])){auto$DateTime <- tmp1DateTime } else {print("Date conversion failed for auto$DateTime. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1DateTime) 
if (class(auto$PH)=="factor") auto$PH <-as.numeric(levels(auto$PH))[as.integer(auto$PH) ]               
if (class(auto$PH)=="character") auto$PH <-as.numeric(auto$PH)
if (class(auto$SPCOND)=="factor") auto$SPCOND <-as.numeric(levels(auto$SPCOND))[as.integer(auto$SPCOND) ]               
if (class(auto$SPCOND)=="character") auto$SPCOND <-as.numeric(auto$SPCOND)
if (class(auto$CL)=="factor") auto$CL <-as.numeric(levels(auto$CL))[as.integer(auto$CL) ]               
if (class(auto$CL)=="character") auto$CL <-as.numeric(auto$CL)
if (class(auto$SO4)=="factor") auto$SO4 <-as.numeric(levels(auto$SO4))[as.integer(auto$SO4) ]               
if (class(auto$SO4)=="character") auto$SO4 <-as.numeric(auto$SO4)
if (class(auto$CA)=="factor") auto$CA <-as.numeric(levels(auto$CA))[as.integer(auto$CA) ]               
if (class(auto$CA)=="character") auto$CA <-as.numeric(auto$CA)
if (class(auto$K)=="factor") auto$K <-as.numeric(levels(auto$K))[as.integer(auto$K) ]               
if (class(auto$K)=="character") auto$K <-as.numeric(auto$K)
if (class(auto$MG)=="factor") auto$MG <-as.numeric(levels(auto$MG))[as.integer(auto$MG) ]               
if (class(auto$MG)=="character") auto$MG <-as.numeric(auto$MG)
if (class(auto$Na)=="factor") auto$Na <-as.numeric(levels(auto$Na))[as.integer(auto$Na) ]               
if (class(auto$Na)=="character") auto$Na <-as.numeric(auto$Na)
if (class(auto$AL)=="factor") auto$AL <-as.numeric(levels(auto$AL))[as.integer(auto$AL) ]               
if (class(auto$AL)=="character") auto$AL <-as.numeric(auto$AL)
if (class(auto$FE)=="factor") auto$FE <-as.numeric(levels(auto$FE))[as.integer(auto$FE) ]               
if (class(auto$FE)=="character") auto$FE <-as.numeric(auto$FE)
if (class(auto$MN)=="factor") auto$MN <-as.numeric(levels(auto$MN))[as.integer(auto$MN) ]               
if (class(auto$MN)=="character") auto$MN <-as.numeric(auto$MN)
if (class(auto$SI)=="factor") auto$SI <-as.numeric(levels(auto$SI))[as.integer(auto$SI) ]               
if (class(auto$SI)=="character") auto$SI <-as.numeric(auto$SI)
if (class(auto$SR)=="factor") auto$SR <-as.numeric(levels(auto$SR))[as.integer(auto$SR) ]               
if (class(auto$SR)=="character") auto$SR <-as.numeric(auto$SR)
if (class(auto$SRP)=="factor") auto$SRP <-as.numeric(levels(auto$SRP))[as.integer(auto$SRP) ]               
if (class(auto$SRP)=="character") auto$SRP <-as.numeric(auto$SRP)
if (class(auto$TN)=="factor") auto$TN <-as.numeric(levels(auto$TN))[as.integer(auto$TN) ]               
if (class(auto$TN)=="character") auto$TN <-as.numeric(auto$TN)
if (class(auto$TP)=="factor") auto$TP <-as.numeric(levels(auto$TP))[as.integer(auto$TP) ]               
if (class(auto$TP)=="character") auto$TP <-as.numeric(auto$TP)
if (class(auto$NPOC)=="factor") auto$NPOC <-as.numeric(levels(auto$NPOC))[as.integer(auto$NPOC) ]               
if (class(auto$NPOC)=="character") auto$NPOC <-as.numeric(auto$NPOC)
if (class(auto$TC.IC)=="factor") auto$TC.IC <-as.numeric(levels(auto$TC.IC))[as.integer(auto$TC.IC) ]               
if (class(auto$TC.IC)=="character") auto$TC.IC <-as.numeric(auto$TC.IC)
if (class(auto$O18)=="factor") auto$O18 <-as.numeric(levels(auto$O18))[as.integer(auto$O18) ]               
if (class(auto$O18)=="character") auto$O18 <-as.numeric(auto$O18)
if (class(auto$D)=="factor") auto$D <-as.numeric(levels(auto$D))[as.integer(auto$D) ]               
if (class(auto$D)=="character") auto$D <-as.numeric(auto$D)

# Convert Missing Values to NA for non-dates

auto$PH <- ifelse((trimws(as.character(auto$PH))==trimws("NA")),NA,auto$PH)               
suppressWarnings(auto$PH <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(auto$PH))==as.character(as.numeric("NA"))),NA,auto$PH))
auto$SPCOND <- ifelse((trimws(as.character(auto$SPCOND))==trimws("NA")),NA,auto$SPCOND)               
suppressWarnings(auto$SPCOND <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(auto$SPCOND))==as.character(as.numeric("NA"))),NA,auto$SPCOND))
auto$CL <- ifelse((trimws(as.character(auto$CL))==trimws("NA")),NA,auto$CL)               
suppressWarnings(auto$CL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(auto$CL))==as.character(as.numeric("NA"))),NA,auto$CL))
auto$SO4 <- ifelse((trimws(as.character(auto$SO4))==trimws("NA")),NA,auto$SO4)               
suppressWarnings(auto$SO4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(auto$SO4))==as.character(as.numeric("NA"))),NA,auto$SO4))
auto$CA <- ifelse((trimws(as.character(auto$CA))==trimws("NA")),NA,auto$CA)               
suppressWarnings(auto$CA <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(auto$CA))==as.character(as.numeric("NA"))),NA,auto$CA))
auto$K <- ifelse((trimws(as.character(auto$K))==trimws("NA")),NA,auto$K)               
suppressWarnings(auto$K <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(auto$K))==as.character(as.numeric("NA"))),NA,auto$K))
auto$MG <- ifelse((trimws(as.character(auto$MG))==trimws("NA")),NA,auto$MG)               
suppressWarnings(auto$MG <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(auto$MG))==as.character(as.numeric("NA"))),NA,auto$MG))
auto$Na <- ifelse((trimws(as.character(auto$Na))==trimws("NA")),NA,auto$Na)               
suppressWarnings(auto$Na <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(auto$Na))==as.character(as.numeric("NA"))),NA,auto$Na))
auto$AL <- ifelse((trimws(as.character(auto$AL))==trimws("NA")),NA,auto$AL)               
suppressWarnings(auto$AL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(auto$AL))==as.character(as.numeric("NA"))),NA,auto$AL))
auto$FE <- ifelse((trimws(as.character(auto$FE))==trimws("NA")),NA,auto$FE)               
suppressWarnings(auto$FE <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(auto$FE))==as.character(as.numeric("NA"))),NA,auto$FE))
auto$MN <- ifelse((trimws(as.character(auto$MN))==trimws("NA")),NA,auto$MN)               
suppressWarnings(auto$MN <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(auto$MN))==as.character(as.numeric("NA"))),NA,auto$MN))
auto$SI <- ifelse((trimws(as.character(auto$SI))==trimws("NA")),NA,auto$SI)               
suppressWarnings(auto$SI <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(auto$SI))==as.character(as.numeric("NA"))),NA,auto$SI))
auto$SR <- ifelse((trimws(as.character(auto$SR))==trimws("NA")),NA,auto$SR)               
suppressWarnings(auto$SR <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(auto$SR))==as.character(as.numeric("NA"))),NA,auto$SR))
auto$SRP <- ifelse((trimws(as.character(auto$SRP))==trimws("NA")),NA,auto$SRP)               
suppressWarnings(auto$SRP <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(auto$SRP))==as.character(as.numeric("NA"))),NA,auto$SRP))
auto$TN <- ifelse((trimws(as.character(auto$TN))==trimws("NA")),NA,auto$TN)               
suppressWarnings(auto$TN <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(auto$TN))==as.character(as.numeric("NA"))),NA,auto$TN))
auto$TP <- ifelse((trimws(as.character(auto$TP))==trimws("NA")),NA,auto$TP)               
suppressWarnings(auto$TP <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(auto$TP))==as.character(as.numeric("NA"))),NA,auto$TP))
auto$NPOC <- ifelse((trimws(as.character(auto$NPOC))==trimws("NA")),NA,auto$NPOC)               
suppressWarnings(auto$NPOC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(auto$NPOC))==as.character(as.numeric("NA"))),NA,auto$NPOC))
auto$TC.IC <- ifelse((trimws(as.character(auto$TC.IC))==trimws("NA")),NA,auto$TC.IC)               
suppressWarnings(auto$TC.IC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(auto$TC.IC))==as.character(as.numeric("NA"))),NA,auto$TC.IC))
auto$O18 <- ifelse((trimws(as.character(auto$O18))==trimws("NA")),NA,auto$O18)               
suppressWarnings(auto$O18 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(auto$O18))==as.character(as.numeric("NA"))),NA,auto$O18))
auto$D <- ifelse((trimws(as.character(auto$D))==trimws("NA")),NA,auto$D)               
suppressWarnings(auto$D <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(auto$D))==as.character(as.numeric("NA"))),NA,auto$D))


# Here is the structure of the input data frame:
str(auto)                            
attach(auto)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(LAB_ID)
summary(Peatland)
summary(NAME)
summary(DateTime)
summary(PH)
summary(SPCOND)
summary(CL)
summary(SO4)
summary(CA)
summary(K)
summary(MG)
summary(Na)
summary(AL)
summary(FE)
summary(MN)
summary(SI)
summary(SR)
summary(SRP)
summary(TN)
summary(TP)
summary(NPOC)
summary(TC.IC)
summary(O18)
summary(D) 
# Get more details on character variables

summary(as.factor(auto$Peatland)) 
summary(as.factor(auto$NAME))
detach(auto)               


inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/1179/1/f3127bddb8dcd2f6844e913319693d3c" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


lagg <-read.csv(infile2,header=F 
                ,skip=1
                ,sep=","  
                , col.names=c(
                  "LAB_ID",     
                  "Peatland",     
                  "NAME",     
                  "DateTime",     
                  "PH",     
                  "SPCOND",     
                  "CL",     
                  "SO4",     
                  "CA",     
                  "K",     
                  "MG",     
                  "Na",     
                  "AL",     
                  "FE",     
                  "MN",     
                  "SI",     
                  "SR",     
                  "NH4",     
                  "NO3",     
                  "SRP",     
                  "TN",     
                  "TP",     
                  "NPOC",     
                  "TC.IC",     
                  "DOC",     
                  "UVA254",     
                  "UVA360",     
                  "THGF",     
                  "MEHGF",     
                  "PB",     
                  "O18",     
                  "D",     
                  "FEII",     
                  "FEIII",     
                  "STAGE",     
                  "q",     
                  "TEMPC"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(lagg$LAB_ID)=="factor") lagg$LAB_ID <-as.numeric(levels(lagg$LAB_ID))[as.integer(lagg$LAB_ID) ]               
if (class(lagg$LAB_ID)=="character") lagg$LAB_ID <-as.numeric(lagg$LAB_ID)
if (class(lagg$Peatland)!="factor") lagg$Peatland<- as.factor(lagg$Peatland)
if (class(lagg$NAME)!="factor") lagg$NAME<- as.factor(lagg$NAME)                                   
# attempting to convert lagg$DateTime dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp2DateTime<-as.POSIXct(lagg$DateTime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp2DateTime) == length(tmp2DateTime[!is.na(tmp2DateTime)])){lagg$DateTime <- tmp2DateTime } else {print("Date conversion failed for lagg$DateTime. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp2DateTime) 
if (class(lagg$PH)=="factor") lagg$PH <-as.numeric(levels(lagg$PH))[as.integer(lagg$PH) ]               
if (class(lagg$PH)=="character") lagg$PH <-as.numeric(lagg$PH)
if (class(lagg$SPCOND)=="factor") lagg$SPCOND <-as.numeric(levels(lagg$SPCOND))[as.integer(lagg$SPCOND) ]               
if (class(lagg$SPCOND)=="character") lagg$SPCOND <-as.numeric(lagg$SPCOND)
if (class(lagg$CL)=="factor") lagg$CL <-as.numeric(levels(lagg$CL))[as.integer(lagg$CL) ]               
if (class(lagg$CL)=="character") lagg$CL <-as.numeric(lagg$CL)
if (class(lagg$SO4)=="factor") lagg$SO4 <-as.numeric(levels(lagg$SO4))[as.integer(lagg$SO4) ]               
if (class(lagg$SO4)=="character") lagg$SO4 <-as.numeric(lagg$SO4)
if (class(lagg$CA)=="factor") lagg$CA <-as.numeric(levels(lagg$CA))[as.integer(lagg$CA) ]               
if (class(lagg$CA)=="character") lagg$CA <-as.numeric(lagg$CA)
if (class(lagg$K)=="factor") lagg$K <-as.numeric(levels(lagg$K))[as.integer(lagg$K) ]               
if (class(lagg$K)=="character") lagg$K <-as.numeric(lagg$K)
if (class(lagg$MG)=="factor") lagg$MG <-as.numeric(levels(lagg$MG))[as.integer(lagg$MG) ]               
if (class(lagg$MG)=="character") lagg$MG <-as.numeric(lagg$MG)
if (class(lagg$Na)=="factor") lagg$Na <-as.numeric(levels(lagg$Na))[as.integer(lagg$Na) ]               
if (class(lagg$Na)=="character") lagg$Na <-as.numeric(lagg$Na)
if (class(lagg$AL)=="factor") lagg$AL <-as.numeric(levels(lagg$AL))[as.integer(lagg$AL) ]               
if (class(lagg$AL)=="character") lagg$AL <-as.numeric(lagg$AL)
if (class(lagg$FE)=="factor") lagg$FE <-as.numeric(levels(lagg$FE))[as.integer(lagg$FE) ]               
if (class(lagg$FE)=="character") lagg$FE <-as.numeric(lagg$FE)
if (class(lagg$MN)=="factor") lagg$MN <-as.numeric(levels(lagg$MN))[as.integer(lagg$MN) ]               
if (class(lagg$MN)=="character") lagg$MN <-as.numeric(lagg$MN)
if (class(lagg$SI)=="factor") lagg$SI <-as.numeric(levels(lagg$SI))[as.integer(lagg$SI) ]               
if (class(lagg$SI)=="character") lagg$SI <-as.numeric(lagg$SI)
if (class(lagg$SR)=="factor") lagg$SR <-as.numeric(levels(lagg$SR))[as.integer(lagg$SR) ]               
if (class(lagg$SR)=="character") lagg$SR <-as.numeric(lagg$SR)
if (class(lagg$NH4)=="factor") lagg$NH4 <-as.numeric(levels(lagg$NH4))[as.integer(lagg$NH4) ]               
if (class(lagg$NH4)=="character") lagg$NH4 <-as.numeric(lagg$NH4)
if (class(lagg$NO3)=="factor") lagg$NO3 <-as.numeric(levels(lagg$NO3))[as.integer(lagg$NO3) ]               
if (class(lagg$NO3)=="character") lagg$NO3 <-as.numeric(lagg$NO3)
if (class(lagg$SRP)=="factor") lagg$SRP <-as.numeric(levels(lagg$SRP))[as.integer(lagg$SRP) ]               
if (class(lagg$SRP)=="character") lagg$SRP <-as.numeric(lagg$SRP)
if (class(lagg$TN)=="factor") lagg$TN <-as.numeric(levels(lagg$TN))[as.integer(lagg$TN) ]               
if (class(lagg$TN)=="character") lagg$TN <-as.numeric(lagg$TN)
if (class(lagg$TP)=="factor") lagg$TP <-as.numeric(levels(lagg$TP))[as.integer(lagg$TP) ]               
if (class(lagg$TP)=="character") lagg$TP <-as.numeric(lagg$TP)
if (class(lagg$NPOC)=="factor") lagg$NPOC <-as.numeric(levels(lagg$NPOC))[as.integer(lagg$NPOC) ]               
if (class(lagg$NPOC)=="character") lagg$NPOC <-as.numeric(lagg$NPOC)
if (class(lagg$TC.IC)=="factor") lagg$TC.IC <-as.numeric(levels(lagg$TC.IC))[as.integer(lagg$TC.IC) ]               
if (class(lagg$TC.IC)=="character") lagg$TC.IC <-as.numeric(lagg$TC.IC)
if (class(lagg$DOC)=="factor") lagg$DOC <-as.numeric(levels(lagg$DOC))[as.integer(lagg$DOC) ]               
if (class(lagg$DOC)=="character") lagg$DOC <-as.numeric(lagg$DOC)
if (class(lagg$UVA254)=="factor") lagg$UVA254 <-as.numeric(levels(lagg$UVA254))[as.integer(lagg$UVA254) ]               
if (class(lagg$UVA254)=="character") lagg$UVA254 <-as.numeric(lagg$UVA254)
if (class(lagg$UVA360)=="factor") lagg$UVA360 <-as.numeric(levels(lagg$UVA360))[as.integer(lagg$UVA360) ]               
if (class(lagg$UVA360)=="character") lagg$UVA360 <-as.numeric(lagg$UVA360)
if (class(lagg$THGF)=="factor") lagg$THGF <-as.numeric(levels(lagg$THGF))[as.integer(lagg$THGF) ]               
if (class(lagg$THGF)=="character") lagg$THGF <-as.numeric(lagg$THGF)
if (class(lagg$MEHGF)=="factor") lagg$MEHGF <-as.numeric(levels(lagg$MEHGF))[as.integer(lagg$MEHGF) ]               
if (class(lagg$MEHGF)=="character") lagg$MEHGF <-as.numeric(lagg$MEHGF)
if (class(lagg$PB)=="factor") lagg$PB <-as.numeric(levels(lagg$PB))[as.integer(lagg$PB) ]               
if (class(lagg$PB)=="character") lagg$PB <-as.numeric(lagg$PB)
if (class(lagg$O18)=="factor") lagg$O18 <-as.numeric(levels(lagg$O18))[as.integer(lagg$O18) ]               
if (class(lagg$O18)=="character") lagg$O18 <-as.numeric(lagg$O18)
if (class(lagg$D)=="factor") lagg$D <-as.numeric(levels(lagg$D))[as.integer(lagg$D) ]               
if (class(lagg$D)=="character") lagg$D <-as.numeric(lagg$D)
if (class(lagg$FEII)=="factor") lagg$FEII <-as.numeric(levels(lagg$FEII))[as.integer(lagg$FEII) ]               
if (class(lagg$FEII)=="character") lagg$FEII <-as.numeric(lagg$FEII)
if (class(lagg$FEIII)=="factor") lagg$FEIII <-as.numeric(levels(lagg$FEIII))[as.integer(lagg$FEIII) ]               
if (class(lagg$FEIII)=="character") lagg$FEIII <-as.numeric(lagg$FEIII)
if (class(lagg$STAGE)=="factor") lagg$STAGE <-as.numeric(levels(lagg$STAGE))[as.integer(lagg$STAGE) ]               
if (class(lagg$STAGE)=="character") lagg$STAGE <-as.numeric(lagg$STAGE)
if (class(lagg$q)=="factor") lagg$q <-as.numeric(levels(lagg$q))[as.integer(lagg$q) ]               
if (class(lagg$q)=="character") lagg$q <-as.numeric(lagg$q)
if (class(lagg$TEMPC)=="factor") lagg$TEMPC <-as.numeric(levels(lagg$TEMPC))[as.integer(lagg$TEMPC) ]               
if (class(lagg$TEMPC)=="character") lagg$TEMPC <-as.numeric(lagg$TEMPC)

# Convert Missing Values to NA for non-dates

lagg$PH <- ifelse((trimws(as.character(lagg$PH))==trimws("NA")),NA,lagg$PH)               
suppressWarnings(lagg$PH <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$PH))==as.character(as.numeric("NA"))),NA,lagg$PH))
lagg$SPCOND <- ifelse((trimws(as.character(lagg$SPCOND))==trimws("NA")),NA,lagg$SPCOND)               
suppressWarnings(lagg$SPCOND <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$SPCOND))==as.character(as.numeric("NA"))),NA,lagg$SPCOND))
lagg$CL <- ifelse((trimws(as.character(lagg$CL))==trimws("NA")),NA,lagg$CL)               
suppressWarnings(lagg$CL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$CL))==as.character(as.numeric("NA"))),NA,lagg$CL))
lagg$SO4 <- ifelse((trimws(as.character(lagg$SO4))==trimws("NA")),NA,lagg$SO4)               
suppressWarnings(lagg$SO4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$SO4))==as.character(as.numeric("NA"))),NA,lagg$SO4))
lagg$CA <- ifelse((trimws(as.character(lagg$CA))==trimws("NA")),NA,lagg$CA)               
suppressWarnings(lagg$CA <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$CA))==as.character(as.numeric("NA"))),NA,lagg$CA))
lagg$K <- ifelse((trimws(as.character(lagg$K))==trimws("NA")),NA,lagg$K)               
suppressWarnings(lagg$K <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$K))==as.character(as.numeric("NA"))),NA,lagg$K))
lagg$MG <- ifelse((trimws(as.character(lagg$MG))==trimws("NA")),NA,lagg$MG)               
suppressWarnings(lagg$MG <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$MG))==as.character(as.numeric("NA"))),NA,lagg$MG))
lagg$Na <- ifelse((trimws(as.character(lagg$Na))==trimws("NA")),NA,lagg$Na)               
suppressWarnings(lagg$Na <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$Na))==as.character(as.numeric("NA"))),NA,lagg$Na))
lagg$AL <- ifelse((trimws(as.character(lagg$AL))==trimws("NA")),NA,lagg$AL)               
suppressWarnings(lagg$AL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$AL))==as.character(as.numeric("NA"))),NA,lagg$AL))
lagg$FE <- ifelse((trimws(as.character(lagg$FE))==trimws("NA")),NA,lagg$FE)               
suppressWarnings(lagg$FE <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$FE))==as.character(as.numeric("NA"))),NA,lagg$FE))
lagg$MN <- ifelse((trimws(as.character(lagg$MN))==trimws("NA")),NA,lagg$MN)               
suppressWarnings(lagg$MN <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$MN))==as.character(as.numeric("NA"))),NA,lagg$MN))
lagg$SI <- ifelse((trimws(as.character(lagg$SI))==trimws("NA")),NA,lagg$SI)               
suppressWarnings(lagg$SI <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$SI))==as.character(as.numeric("NA"))),NA,lagg$SI))
lagg$SR <- ifelse((trimws(as.character(lagg$SR))==trimws("NA")),NA,lagg$SR)               
suppressWarnings(lagg$SR <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$SR))==as.character(as.numeric("NA"))),NA,lagg$SR))
lagg$NH4 <- ifelse((trimws(as.character(lagg$NH4))==trimws("NA")),NA,lagg$NH4)               
suppressWarnings(lagg$NH4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$NH4))==as.character(as.numeric("NA"))),NA,lagg$NH4))
lagg$NO3 <- ifelse((trimws(as.character(lagg$NO3))==trimws("NA")),NA,lagg$NO3)               
suppressWarnings(lagg$NO3 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$NO3))==as.character(as.numeric("NA"))),NA,lagg$NO3))
lagg$SRP <- ifelse((trimws(as.character(lagg$SRP))==trimws("NA")),NA,lagg$SRP)               
suppressWarnings(lagg$SRP <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$SRP))==as.character(as.numeric("NA"))),NA,lagg$SRP))
lagg$TN <- ifelse((trimws(as.character(lagg$TN))==trimws("NA")),NA,lagg$TN)               
suppressWarnings(lagg$TN <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$TN))==as.character(as.numeric("NA"))),NA,lagg$TN))
lagg$TP <- ifelse((trimws(as.character(lagg$TP))==trimws("NA")),NA,lagg$TP)               
suppressWarnings(lagg$TP <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$TP))==as.character(as.numeric("NA"))),NA,lagg$TP))
lagg$NPOC <- ifelse((trimws(as.character(lagg$NPOC))==trimws("NA")),NA,lagg$NPOC)               
suppressWarnings(lagg$NPOC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$NPOC))==as.character(as.numeric("NA"))),NA,lagg$NPOC))
lagg$TC.IC <- ifelse((trimws(as.character(lagg$TC.IC))==trimws("NA")),NA,lagg$TC.IC)               
suppressWarnings(lagg$TC.IC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$TC.IC))==as.character(as.numeric("NA"))),NA,lagg$TC.IC))
lagg$DOC <- ifelse((trimws(as.character(lagg$DOC))==trimws("NA")),NA,lagg$DOC)               
suppressWarnings(lagg$DOC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$DOC))==as.character(as.numeric("NA"))),NA,lagg$DOC))
lagg$UVA254 <- ifelse((trimws(as.character(lagg$UVA254))==trimws("NA")),NA,lagg$UVA254)               
suppressWarnings(lagg$UVA254 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$UVA254))==as.character(as.numeric("NA"))),NA,lagg$UVA254))
lagg$UVA360 <- ifelse((trimws(as.character(lagg$UVA360))==trimws("NA")),NA,lagg$UVA360)               
suppressWarnings(lagg$UVA360 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$UVA360))==as.character(as.numeric("NA"))),NA,lagg$UVA360))
lagg$THGF <- ifelse((trimws(as.character(lagg$THGF))==trimws("NA")),NA,lagg$THGF)               
suppressWarnings(lagg$THGF <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$THGF))==as.character(as.numeric("NA"))),NA,lagg$THGF))
lagg$MEHGF <- ifelse((trimws(as.character(lagg$MEHGF))==trimws("NA")),NA,lagg$MEHGF)               
suppressWarnings(lagg$MEHGF <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$MEHGF))==as.character(as.numeric("NA"))),NA,lagg$MEHGF))
lagg$PB <- ifelse((trimws(as.character(lagg$PB))==trimws("NA")),NA,lagg$PB)               
suppressWarnings(lagg$PB <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$PB))==as.character(as.numeric("NA"))),NA,lagg$PB))
lagg$O18 <- ifelse((trimws(as.character(lagg$O18))==trimws("NA")),NA,lagg$O18)               
suppressWarnings(lagg$O18 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$O18))==as.character(as.numeric("NA"))),NA,lagg$O18))
lagg$D <- ifelse((trimws(as.character(lagg$D))==trimws("NA")),NA,lagg$D)               
suppressWarnings(lagg$D <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$D))==as.character(as.numeric("NA"))),NA,lagg$D))
lagg$FEII <- ifelse((trimws(as.character(lagg$FEII))==trimws("NA")),NA,lagg$FEII)               
suppressWarnings(lagg$FEII <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$FEII))==as.character(as.numeric("NA"))),NA,lagg$FEII))
lagg$FEIII <- ifelse((trimws(as.character(lagg$FEIII))==trimws("NA")),NA,lagg$FEIII)               
suppressWarnings(lagg$FEIII <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$FEIII))==as.character(as.numeric("NA"))),NA,lagg$FEIII))
lagg$STAGE <- ifelse((trimws(as.character(lagg$STAGE))==trimws("NA")),NA,lagg$STAGE)               
suppressWarnings(lagg$STAGE <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$STAGE))==as.character(as.numeric("NA"))),NA,lagg$STAGE))
lagg$q <- ifelse((trimws(as.character(lagg$q))==trimws("NA")),NA,lagg$q)               
suppressWarnings(lagg$q <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$q))==as.character(as.numeric("NA"))),NA,lagg$q))
lagg$TEMPC <- ifelse((trimws(as.character(lagg$TEMPC))==trimws("NA")),NA,lagg$TEMPC)               
suppressWarnings(lagg$TEMPC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(lagg$TEMPC))==as.character(as.numeric("NA"))),NA,lagg$TEMPC))


# Here is the structure of the input data frame:
str(lagg)                            
attach(lagg)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(LAB_ID)
summary(Peatland)
summary(NAME)
summary(DateTime)
summary(PH)
summary(SPCOND)
summary(CL)
summary(SO4)
summary(CA)
summary(K)
summary(MG)
summary(Na)
summary(AL)
summary(FE)
summary(MN)
summary(SI)
summary(SR)
summary(NH4)
summary(NO3)
summary(SRP)
summary(TN)
summary(TP)
summary(NPOC)
summary(TC.IC)
summary(DOC)
summary(UVA254)
summary(UVA360)
summary(THGF)
summary(MEHGF)
summary(PB)
summary(O18)
summary(D)
summary(FEII)
summary(FEIII)
summary(STAGE)
summary(q)
summary(TEMPC) 
# Get more details on character variables

summary(as.factor(lagg$Peatland)) 
summary(as.factor(lagg$NAME))
detach(lagg)               


inUrl3  <- "https://pasta.lternet.edu/package/data/eml/edi/1179/1/769ea4fc5a257d29f0c8b494121a792c" 
infile3 <- tempfile()
try(download.file(inUrl3,infile3,method="curl"))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")


grab <-read.csv(infile3,header=F 
                ,skip=1
                ,sep=","  
                , col.names=c(
                  "LAB_ID",     
                  "Peatland",     
                  "NAME",     
                  "DateTime",     
                  "PH",     
                  "SPCOND",     
                  "CL",     
                  "SO4",     
                  "CA",     
                  "K",     
                  "MG",     
                  "Na",     
                  "AL",     
                  "FE",     
                  "MN",     
                  "SI",     
                  "SR",     
                  "NH4",     
                  "NO3",     
                  "SRP",     
                  "TN",     
                  "TP",     
                  "NPOC",     
                  "TC.IC",     
                  "DOC",     
                  "UVA254",     
                  "UVA360",     
                  "UVA360S",     
                  "BDOC",     
                  "BR",     
                  "THGF",     
                  "MEHGF",     
                  "THGU",     
                  "MEHGU",     
                  "d202Hg",     
                  "MIF204Hg",     
                  "MIF201Hg",     
                  "MIF200Hg",     
                  "MIF199Hg",     
                  "PB",     
                  "O18",     
                  "D",     
                  "FEII",     
                  "FEIII",     
                  "STAGE",     
                  "q",     
                  "TEMPC"    ), check.names=TRUE)

unlink(infile3)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(grab$LAB_ID)=="factor") grab$LAB_ID <-as.numeric(levels(grab$LAB_ID))[as.integer(grab$LAB_ID) ]               
if (class(grab$LAB_ID)=="character") grab$LAB_ID <-as.numeric(grab$LAB_ID)
if (class(grab$Peatland)!="factor") grab$Peatland<- as.factor(grab$Peatland)
if (class(grab$NAME)!="factor") grab$NAME<- as.factor(grab$NAME)                                   
# attempting to convert grab$DateTime dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp3DateTime<-as.POSIXct(grab$DateTime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp3DateTime) == length(tmp3DateTime[!is.na(tmp3DateTime)])){grab$DateTime <- tmp3DateTime } else {print("Date conversion failed for grab$DateTime. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp3DateTime) 
if (class(grab$PH)=="factor") grab$PH <-as.numeric(levels(grab$PH))[as.integer(grab$PH) ]               
if (class(grab$PH)=="character") grab$PH <-as.numeric(grab$PH)
if (class(grab$SPCOND)=="factor") grab$SPCOND <-as.numeric(levels(grab$SPCOND))[as.integer(grab$SPCOND) ]               
if (class(grab$SPCOND)=="character") grab$SPCOND <-as.numeric(grab$SPCOND)
if (class(grab$CL)=="factor") grab$CL <-as.numeric(levels(grab$CL))[as.integer(grab$CL) ]               
if (class(grab$CL)=="character") grab$CL <-as.numeric(grab$CL)
if (class(grab$SO4)=="factor") grab$SO4 <-as.numeric(levels(grab$SO4))[as.integer(grab$SO4) ]               
if (class(grab$SO4)=="character") grab$SO4 <-as.numeric(grab$SO4)
if (class(grab$CA)=="factor") grab$CA <-as.numeric(levels(grab$CA))[as.integer(grab$CA) ]               
if (class(grab$CA)=="character") grab$CA <-as.numeric(grab$CA)
if (class(grab$K)=="factor") grab$K <-as.numeric(levels(grab$K))[as.integer(grab$K) ]               
if (class(grab$K)=="character") grab$K <-as.numeric(grab$K)
if (class(grab$MG)=="factor") grab$MG <-as.numeric(levels(grab$MG))[as.integer(grab$MG) ]               
if (class(grab$MG)=="character") grab$MG <-as.numeric(grab$MG)
if (class(grab$Na)=="factor") grab$Na <-as.numeric(levels(grab$Na))[as.integer(grab$Na) ]               
if (class(grab$Na)=="character") grab$Na <-as.numeric(grab$Na)
if (class(grab$AL)=="factor") grab$AL <-as.numeric(levels(grab$AL))[as.integer(grab$AL) ]               
if (class(grab$AL)=="character") grab$AL <-as.numeric(grab$AL)
if (class(grab$FE)=="factor") grab$FE <-as.numeric(levels(grab$FE))[as.integer(grab$FE) ]               
if (class(grab$FE)=="character") grab$FE <-as.numeric(grab$FE)
if (class(grab$MN)=="factor") grab$MN <-as.numeric(levels(grab$MN))[as.integer(grab$MN) ]               
if (class(grab$MN)=="character") grab$MN <-as.numeric(grab$MN)
if (class(grab$SI)=="factor") grab$SI <-as.numeric(levels(grab$SI))[as.integer(grab$SI) ]               
if (class(grab$SI)=="character") grab$SI <-as.numeric(grab$SI)
if (class(grab$SR)=="factor") grab$SR <-as.numeric(levels(grab$SR))[as.integer(grab$SR) ]               
if (class(grab$SR)=="character") grab$SR <-as.numeric(grab$SR)
if (class(grab$NH4)=="factor") grab$NH4 <-as.numeric(levels(grab$NH4))[as.integer(grab$NH4) ]               
if (class(grab$NH4)=="character") grab$NH4 <-as.numeric(grab$NH4)
if (class(grab$NO3)=="factor") grab$NO3 <-as.numeric(levels(grab$NO3))[as.integer(grab$NO3) ]               
if (class(grab$NO3)=="character") grab$NO3 <-as.numeric(grab$NO3)
if (class(grab$SRP)=="factor") grab$SRP <-as.numeric(levels(grab$SRP))[as.integer(grab$SRP) ]               
if (class(grab$SRP)=="character") grab$SRP <-as.numeric(grab$SRP)
if (class(grab$TN)=="factor") grab$TN <-as.numeric(levels(grab$TN))[as.integer(grab$TN) ]               
if (class(grab$TN)=="character") grab$TN <-as.numeric(grab$TN)
if (class(grab$TP)=="factor") grab$TP <-as.numeric(levels(grab$TP))[as.integer(grab$TP) ]               
if (class(grab$TP)=="character") grab$TP <-as.numeric(grab$TP)
if (class(grab$NPOC)=="factor") grab$NPOC <-as.numeric(levels(grab$NPOC))[as.integer(grab$NPOC) ]               
if (class(grab$NPOC)=="character") grab$NPOC <-as.numeric(grab$NPOC)
if (class(grab$TC.IC)=="factor") grab$TC.IC <-as.numeric(levels(grab$TC.IC))[as.integer(grab$TC.IC) ]               
if (class(grab$TC.IC)=="character") grab$TC.IC <-as.numeric(grab$TC.IC)
if (class(grab$DOC)=="factor") grab$DOC <-as.numeric(levels(grab$DOC))[as.integer(grab$DOC) ]               
if (class(grab$DOC)=="character") grab$DOC <-as.numeric(grab$DOC)
if (class(grab$UVA254)=="factor") grab$UVA254 <-as.numeric(levels(grab$UVA254))[as.integer(grab$UVA254) ]               
if (class(grab$UVA254)=="character") grab$UVA254 <-as.numeric(grab$UVA254)
if (class(grab$UVA360)=="factor") grab$UVA360 <-as.numeric(levels(grab$UVA360))[as.integer(grab$UVA360) ]               
if (class(grab$UVA360)=="character") grab$UVA360 <-as.numeric(grab$UVA360)
if (class(grab$UVA360S)=="factor") grab$UVA360S <-as.numeric(levels(grab$UVA360S))[as.integer(grab$UVA360S) ]               
if (class(grab$UVA360S)=="character") grab$UVA360S <-as.numeric(grab$UVA360S)
if (class(grab$BDOC)=="factor") grab$BDOC <-as.numeric(levels(grab$BDOC))[as.integer(grab$BDOC) ]               
if (class(grab$BDOC)=="character") grab$BDOC <-as.numeric(grab$BDOC)
if (class(grab$BR)=="factor") grab$BR <-as.numeric(levels(grab$BR))[as.integer(grab$BR) ]               
if (class(grab$BR)=="character") grab$BR <-as.numeric(grab$BR)
if (class(grab$THGF)=="factor") grab$THGF <-as.numeric(levels(grab$THGF))[as.integer(grab$THGF) ]               
if (class(grab$THGF)=="character") grab$THGF <-as.numeric(grab$THGF)
if (class(grab$MEHGF)=="factor") grab$MEHGF <-as.numeric(levels(grab$MEHGF))[as.integer(grab$MEHGF) ]               
if (class(grab$MEHGF)=="character") grab$MEHGF <-as.numeric(grab$MEHGF)
if (class(grab$THGU)=="factor") grab$THGU <-as.numeric(levels(grab$THGU))[as.integer(grab$THGU) ]               
if (class(grab$THGU)=="character") grab$THGU <-as.numeric(grab$THGU)
if (class(grab$MEHGU)=="factor") grab$MEHGU <-as.numeric(levels(grab$MEHGU))[as.integer(grab$MEHGU) ]               
if (class(grab$MEHGU)=="character") grab$MEHGU <-as.numeric(grab$MEHGU)
if (class(grab$d202Hg)=="factor") grab$d202Hg <-as.numeric(levels(grab$d202Hg))[as.integer(grab$d202Hg) ]               
if (class(grab$d202Hg)=="character") grab$d202Hg <-as.numeric(grab$d202Hg)
if (class(grab$MIF204Hg)=="factor") grab$MIF204Hg <-as.numeric(levels(grab$MIF204Hg))[as.integer(grab$MIF204Hg) ]               
if (class(grab$MIF204Hg)=="character") grab$MIF204Hg <-as.numeric(grab$MIF204Hg)
if (class(grab$MIF201Hg)=="factor") grab$MIF201Hg <-as.numeric(levels(grab$MIF201Hg))[as.integer(grab$MIF201Hg) ]               
if (class(grab$MIF201Hg)=="character") grab$MIF201Hg <-as.numeric(grab$MIF201Hg)
if (class(grab$MIF200Hg)=="factor") grab$MIF200Hg <-as.numeric(levels(grab$MIF200Hg))[as.integer(grab$MIF200Hg) ]               
if (class(grab$MIF200Hg)=="character") grab$MIF200Hg <-as.numeric(grab$MIF200Hg)
if (class(grab$MIF199Hg)=="factor") grab$MIF199Hg <-as.numeric(levels(grab$MIF199Hg))[as.integer(grab$MIF199Hg) ]               
if (class(grab$MIF199Hg)=="character") grab$MIF199Hg <-as.numeric(grab$MIF199Hg)
if (class(grab$PB)=="factor") grab$PB <-as.numeric(levels(grab$PB))[as.integer(grab$PB) ]               
if (class(grab$PB)=="character") grab$PB <-as.numeric(grab$PB)
if (class(grab$O18)=="factor") grab$O18 <-as.numeric(levels(grab$O18))[as.integer(grab$O18) ]               
if (class(grab$O18)=="character") grab$O18 <-as.numeric(grab$O18)
if (class(grab$D)=="factor") grab$D <-as.numeric(levels(grab$D))[as.integer(grab$D) ]               
if (class(grab$D)=="character") grab$D <-as.numeric(grab$D)
if (class(grab$FEII)=="factor") grab$FEII <-as.numeric(levels(grab$FEII))[as.integer(grab$FEII) ]               
if (class(grab$FEII)=="character") grab$FEII <-as.numeric(grab$FEII)
if (class(grab$FEIII)=="factor") grab$FEIII <-as.numeric(levels(grab$FEIII))[as.integer(grab$FEIII) ]               
if (class(grab$FEIII)=="character") grab$FEIII <-as.numeric(grab$FEIII)
if (class(grab$STAGE)=="factor") grab$STAGE <-as.numeric(levels(grab$STAGE))[as.integer(grab$STAGE) ]               
if (class(grab$STAGE)=="character") grab$STAGE <-as.numeric(grab$STAGE)
if (class(grab$q)=="factor") grab$q <-as.numeric(levels(grab$q))[as.integer(grab$q) ]               
if (class(grab$q)=="character") grab$q <-as.numeric(grab$q)
if (class(grab$TEMPC)=="factor") grab$TEMPC <-as.numeric(levels(grab$TEMPC))[as.integer(grab$TEMPC) ]               
if (class(grab$TEMPC)=="character") grab$TEMPC <-as.numeric(grab$TEMPC)

# Convert Missing Values to NA for non-dates

grab$PH <- ifelse((trimws(as.character(grab$PH))==trimws("NA")),NA,grab$PH)               
suppressWarnings(grab$PH <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$PH))==as.character(as.numeric("NA"))),NA,grab$PH))
grab$SPCOND <- ifelse((trimws(as.character(grab$SPCOND))==trimws("NA")),NA,grab$SPCOND)               
suppressWarnings(grab$SPCOND <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$SPCOND))==as.character(as.numeric("NA"))),NA,grab$SPCOND))
grab$CL <- ifelse((trimws(as.character(grab$CL))==trimws("NA")),NA,grab$CL)               
suppressWarnings(grab$CL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$CL))==as.character(as.numeric("NA"))),NA,grab$CL))
grab$SO4 <- ifelse((trimws(as.character(grab$SO4))==trimws("NA")),NA,grab$SO4)               
suppressWarnings(grab$SO4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$SO4))==as.character(as.numeric("NA"))),NA,grab$SO4))
grab$CA <- ifelse((trimws(as.character(grab$CA))==trimws("NA")),NA,grab$CA)               
suppressWarnings(grab$CA <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$CA))==as.character(as.numeric("NA"))),NA,grab$CA))
grab$K <- ifelse((trimws(as.character(grab$K))==trimws("NA")),NA,grab$K)               
suppressWarnings(grab$K <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$K))==as.character(as.numeric("NA"))),NA,grab$K))
grab$MG <- ifelse((trimws(as.character(grab$MG))==trimws("NA")),NA,grab$MG)               
suppressWarnings(grab$MG <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$MG))==as.character(as.numeric("NA"))),NA,grab$MG))
grab$Na <- ifelse((trimws(as.character(grab$Na))==trimws("NA")),NA,grab$Na)               
suppressWarnings(grab$Na <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$Na))==as.character(as.numeric("NA"))),NA,grab$Na))
grab$AL <- ifelse((trimws(as.character(grab$AL))==trimws("NA")),NA,grab$AL)               
suppressWarnings(grab$AL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$AL))==as.character(as.numeric("NA"))),NA,grab$AL))
grab$FE <- ifelse((trimws(as.character(grab$FE))==trimws("NA")),NA,grab$FE)               
suppressWarnings(grab$FE <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$FE))==as.character(as.numeric("NA"))),NA,grab$FE))
grab$MN <- ifelse((trimws(as.character(grab$MN))==trimws("NA")),NA,grab$MN)               
suppressWarnings(grab$MN <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$MN))==as.character(as.numeric("NA"))),NA,grab$MN))
grab$SI <- ifelse((trimws(as.character(grab$SI))==trimws("NA")),NA,grab$SI)               
suppressWarnings(grab$SI <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$SI))==as.character(as.numeric("NA"))),NA,grab$SI))
grab$SR <- ifelse((trimws(as.character(grab$SR))==trimws("NA")),NA,grab$SR)               
suppressWarnings(grab$SR <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$SR))==as.character(as.numeric("NA"))),NA,grab$SR))
grab$NH4 <- ifelse((trimws(as.character(grab$NH4))==trimws("NA")),NA,grab$NH4)               
suppressWarnings(grab$NH4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$NH4))==as.character(as.numeric("NA"))),NA,grab$NH4))
grab$NO3 <- ifelse((trimws(as.character(grab$NO3))==trimws("NA")),NA,grab$NO3)               
suppressWarnings(grab$NO3 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$NO3))==as.character(as.numeric("NA"))),NA,grab$NO3))
grab$SRP <- ifelse((trimws(as.character(grab$SRP))==trimws("NA")),NA,grab$SRP)               
suppressWarnings(grab$SRP <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$SRP))==as.character(as.numeric("NA"))),NA,grab$SRP))
grab$TN <- ifelse((trimws(as.character(grab$TN))==trimws("NA")),NA,grab$TN)               
suppressWarnings(grab$TN <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$TN))==as.character(as.numeric("NA"))),NA,grab$TN))
grab$TP <- ifelse((trimws(as.character(grab$TP))==trimws("NA")),NA,grab$TP)               
suppressWarnings(grab$TP <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$TP))==as.character(as.numeric("NA"))),NA,grab$TP))
grab$NPOC <- ifelse((trimws(as.character(grab$NPOC))==trimws("NA")),NA,grab$NPOC)               
suppressWarnings(grab$NPOC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$NPOC))==as.character(as.numeric("NA"))),NA,grab$NPOC))
grab$TC.IC <- ifelse((trimws(as.character(grab$TC.IC))==trimws("NA")),NA,grab$TC.IC)               
suppressWarnings(grab$TC.IC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$TC.IC))==as.character(as.numeric("NA"))),NA,grab$TC.IC))
grab$DOC <- ifelse((trimws(as.character(grab$DOC))==trimws("NA")),NA,grab$DOC)               
suppressWarnings(grab$DOC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$DOC))==as.character(as.numeric("NA"))),NA,grab$DOC))
grab$UVA254 <- ifelse((trimws(as.character(grab$UVA254))==trimws("NA")),NA,grab$UVA254)               
suppressWarnings(grab$UVA254 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$UVA254))==as.character(as.numeric("NA"))),NA,grab$UVA254))
grab$UVA360 <- ifelse((trimws(as.character(grab$UVA360))==trimws("NA")),NA,grab$UVA360)               
suppressWarnings(grab$UVA360 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$UVA360))==as.character(as.numeric("NA"))),NA,grab$UVA360))
grab$UVA360S <- ifelse((trimws(as.character(grab$UVA360S))==trimws("NA")),NA,grab$UVA360S)               
suppressWarnings(grab$UVA360S <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$UVA360S))==as.character(as.numeric("NA"))),NA,grab$UVA360S))
grab$BDOC <- ifelse((trimws(as.character(grab$BDOC))==trimws("NA")),NA,grab$BDOC)               
suppressWarnings(grab$BDOC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$BDOC))==as.character(as.numeric("NA"))),NA,grab$BDOC))
grab$BR <- ifelse((trimws(as.character(grab$BR))==trimws("NA")),NA,grab$BR)               
suppressWarnings(grab$BR <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$BR))==as.character(as.numeric("NA"))),NA,grab$BR))
grab$THGF <- ifelse((trimws(as.character(grab$THGF))==trimws("NA")),NA,grab$THGF)               
suppressWarnings(grab$THGF <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$THGF))==as.character(as.numeric("NA"))),NA,grab$THGF))
grab$MEHGF <- ifelse((trimws(as.character(grab$MEHGF))==trimws("NA")),NA,grab$MEHGF)               
suppressWarnings(grab$MEHGF <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$MEHGF))==as.character(as.numeric("NA"))),NA,grab$MEHGF))
grab$THGU <- ifelse((trimws(as.character(grab$THGU))==trimws("NA")),NA,grab$THGU)               
suppressWarnings(grab$THGU <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$THGU))==as.character(as.numeric("NA"))),NA,grab$THGU))
grab$MEHGU <- ifelse((trimws(as.character(grab$MEHGU))==trimws("NA")),NA,grab$MEHGU)               
suppressWarnings(grab$MEHGU <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$MEHGU))==as.character(as.numeric("NA"))),NA,grab$MEHGU))
grab$d202Hg <- ifelse((trimws(as.character(grab$d202Hg))==trimws("NA")),NA,grab$d202Hg)               
suppressWarnings(grab$d202Hg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$d202Hg))==as.character(as.numeric("NA"))),NA,grab$d202Hg))
grab$MIF204Hg <- ifelse((trimws(as.character(grab$MIF204Hg))==trimws("NA")),NA,grab$MIF204Hg)               
suppressWarnings(grab$MIF204Hg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$MIF204Hg))==as.character(as.numeric("NA"))),NA,grab$MIF204Hg))
grab$MIF201Hg <- ifelse((trimws(as.character(grab$MIF201Hg))==trimws("NA")),NA,grab$MIF201Hg)               
suppressWarnings(grab$MIF201Hg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$MIF201Hg))==as.character(as.numeric("NA"))),NA,grab$MIF201Hg))
grab$MIF200Hg <- ifelse((trimws(as.character(grab$MIF200Hg))==trimws("NA")),NA,grab$MIF200Hg)               
suppressWarnings(grab$MIF200Hg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$MIF200Hg))==as.character(as.numeric("NA"))),NA,grab$MIF200Hg))
grab$MIF199Hg <- ifelse((trimws(as.character(grab$MIF199Hg))==trimws("NA")),NA,grab$MIF199Hg)               
suppressWarnings(grab$MIF199Hg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$MIF199Hg))==as.character(as.numeric("NA"))),NA,grab$MIF199Hg))
grab$PB <- ifelse((trimws(as.character(grab$PB))==trimws("NA")),NA,grab$PB)               
suppressWarnings(grab$PB <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$PB))==as.character(as.numeric("NA"))),NA,grab$PB))
grab$O18 <- ifelse((trimws(as.character(grab$O18))==trimws("NA")),NA,grab$O18)               
suppressWarnings(grab$O18 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$O18))==as.character(as.numeric("NA"))),NA,grab$O18))
grab$D <- ifelse((trimws(as.character(grab$D))==trimws("NA")),NA,grab$D)               
suppressWarnings(grab$D <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$D))==as.character(as.numeric("NA"))),NA,grab$D))
grab$FEII <- ifelse((trimws(as.character(grab$FEII))==trimws("NA")),NA,grab$FEII)               
suppressWarnings(grab$FEII <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$FEII))==as.character(as.numeric("NA"))),NA,grab$FEII))
grab$FEIII <- ifelse((trimws(as.character(grab$FEIII))==trimws("NA")),NA,grab$FEIII)               
suppressWarnings(grab$FEIII <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$FEIII))==as.character(as.numeric("NA"))),NA,grab$FEIII))
grab$STAGE <- ifelse((trimws(as.character(grab$STAGE))==trimws("NA")),NA,grab$STAGE)               
suppressWarnings(grab$STAGE <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$STAGE))==as.character(as.numeric("NA"))),NA,grab$STAGE))
grab$q <- ifelse((trimws(as.character(grab$q))==trimws("NA")),NA,grab$q)               
suppressWarnings(grab$q <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$q))==as.character(as.numeric("NA"))),NA,grab$q))
grab$TEMPC <- ifelse((trimws(as.character(grab$TEMPC))==trimws("NA")),NA,grab$TEMPC)               
suppressWarnings(grab$TEMPC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(grab$TEMPC))==as.character(as.numeric("NA"))),NA,grab$TEMPC))


# Here is the structure of the input data frame:
str(grab)                            
attach(grab)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(LAB_ID)
summary(Peatland)
summary(NAME)
summary(DateTime)
summary(PH)
summary(SPCOND)
summary(CL)
summary(SO4)
summary(CA)
summary(K)
summary(MG)
summary(Na)
summary(AL)
summary(FE)
summary(MN)
summary(SI)
summary(SR)
summary(NH4)
summary(NO3)
summary(SRP)
summary(TN)
summary(TP)
summary(NPOC)
summary(TC.IC)
summary(DOC)
summary(UVA254)
summary(UVA360)
summary(UVA360S)
summary(BDOC)
summary(BR)
summary(THGF)
summary(MEHGF)
summary(THGU)
summary(MEHGU)
summary(d202Hg)
summary(MIF204Hg)
summary(MIF201Hg)
summary(MIF200Hg)
summary(MIF199Hg)
summary(PB)
summary(O18)
summary(D)
summary(FEII)
summary(FEIII)
summary(STAGE)
summary(q)
summary(TEMPC) 
# Get more details on character variables

summary(as.factor(grab$Peatland)) 
summary(as.factor(grab$NAME))
detach(grab)     

#bog+lagg porewater
# Package ID: edi.712.2 Cataloging System:https://pasta.edirepository.org.
# Data set title: Marcell Experimental Forest porewater chemistry at the S2 catchment, 2009 - ongoing.
# Data set creator:  Stephen Sebestyen - USDA Forest Service, Northern Research Station 
# Data set creator:  Nina Lany - USDA Forest Service, Northern Research Station 
# Data set creator:  John Larson - USDA Forest Service, Northern Research Station 
# Data set creator:  Nathan Aspelin - USDA Forest Service, Northern Research Station 
# Data set creator:  Keith Oleheiser - XCEL Engineering 
# Data set creator:  Doris Nelson - USDA Forest Service, Northern Research Station 
# Data set creator:  Steven Hall - Iowa State University 
# Data set creator:  Holly Curtinrich - Iowa State University 
# Contact:    -  Data Manager, Marcell Experimental Forest  - nina.lany@usda.gov
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/712/2/6108c9dac7d837a4955f233eb4467ff2" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


bog_pore_month <-read.csv(infile1,header=F 
                          ,skip=1
                          ,sep=","  
                          , col.names=c(
                            "LAB_ID",     
                            "NAME",     
                            "PIEZOMETER",     
                            "DEPTH",     
                            "DateTime",     
                            "PH",     
                            "SPCOND",     
                            "CL",     
                            "SO4",     
                            "CA",     
                            "K",     
                            "MG",     
                            "Na",     
                            "AL",     
                            "FE",     
                            "MN",     
                            "SI",     
                            "SR",     
                            "NH4",     
                            "NO3",     
                            "SRP",     
                            "TN",     
                            "TP",     
                            "TOC"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(bog_pore_month$LAB_ID)!="factor") bog_pore_month$LAB_ID<- as.factor(bog_pore_month$LAB_ID)
if (class(bog_pore_month$NAME)!="factor") bog_pore_month$NAME<- as.factor(bog_pore_month$NAME)
if (class(bog_pore_month$PIEZOMETER)!="factor") bog_pore_month$PIEZOMETER<- as.factor(bog_pore_month$PIEZOMETER)
if (class(bog_pore_month$DEPTH)=="factor") bog_pore_month$DEPTH <-as.numeric(levels(bog_pore_month$DEPTH))[as.integer(bog_pore_month$DEPTH) ]               
if (class(bog_pore_month$DEPTH)=="character") bog_pore_month$DEPTH <-as.numeric(bog_pore_month$DEPTH)                                   
# attempting to convert bog_pore_month$DateTime dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp1DateTime<-as.POSIXct(bog_pore_month$DateTime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1DateTime) == length(tmp1DateTime[!is.na(tmp1DateTime)])){bog_pore_month$DateTime <- tmp1DateTime } else {print("Date conversion failed for bog_pore_month$DateTime. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1DateTime) 
if (class(bog_pore_month$PH)=="factor") bog_pore_month$PH <-as.numeric(levels(bog_pore_month$PH))[as.integer(bog_pore_month$PH) ]               
if (class(bog_pore_month$PH)=="character") bog_pore_month$PH <-as.numeric(bog_pore_month$PH)
if (class(bog_pore_month$SPCOND)=="factor") bog_pore_month$SPCOND <-as.numeric(levels(bog_pore_month$SPCOND))[as.integer(bog_pore_month$SPCOND) ]               
if (class(bog_pore_month$SPCOND)=="character") bog_pore_month$SPCOND <-as.numeric(bog_pore_month$SPCOND)
if (class(bog_pore_month$CL)=="factor") bog_pore_month$CL <-as.numeric(levels(bog_pore_month$CL))[as.integer(bog_pore_month$CL) ]               
if (class(bog_pore_month$CL)=="character") bog_pore_month$CL <-as.numeric(bog_pore_month$CL)
if (class(bog_pore_month$SO4)=="factor") bog_pore_month$SO4 <-as.numeric(levels(bog_pore_month$SO4))[as.integer(bog_pore_month$SO4) ]               
if (class(bog_pore_month$SO4)=="character") bog_pore_month$SO4 <-as.numeric(bog_pore_month$SO4)
if (class(bog_pore_month$CA)=="factor") bog_pore_month$CA <-as.numeric(levels(bog_pore_month$CA))[as.integer(bog_pore_month$CA) ]               
if (class(bog_pore_month$CA)=="character") bog_pore_month$CA <-as.numeric(bog_pore_month$CA)
if (class(bog_pore_month$K)=="factor") bog_pore_month$K <-as.numeric(levels(bog_pore_month$K))[as.integer(bog_pore_month$K) ]               
if (class(bog_pore_month$K)=="character") bog_pore_month$K <-as.numeric(bog_pore_month$K)
if (class(bog_pore_month$MG)=="factor") bog_pore_month$MG <-as.numeric(levels(bog_pore_month$MG))[as.integer(bog_pore_month$MG) ]               
if (class(bog_pore_month$MG)=="character") bog_pore_month$MG <-as.numeric(bog_pore_month$MG)
if (class(bog_pore_month$Na)=="factor") bog_pore_month$Na <-as.numeric(levels(bog_pore_month$Na))[as.integer(bog_pore_month$Na) ]               
if (class(bog_pore_month$Na)=="character") bog_pore_month$Na <-as.numeric(bog_pore_month$Na)
if (class(bog_pore_month$AL)=="factor") bog_pore_month$AL <-as.numeric(levels(bog_pore_month$AL))[as.integer(bog_pore_month$AL) ]               
if (class(bog_pore_month$AL)=="character") bog_pore_month$AL <-as.numeric(bog_pore_month$AL)
if (class(bog_pore_month$FE)=="factor") bog_pore_month$FE <-as.numeric(levels(bog_pore_month$FE))[as.integer(bog_pore_month$FE) ]               
if (class(bog_pore_month$FE)=="character") bog_pore_month$FE <-as.numeric(bog_pore_month$FE)
if (class(bog_pore_month$MN)=="factor") bog_pore_month$MN <-as.numeric(levels(bog_pore_month$MN))[as.integer(bog_pore_month$MN) ]               
if (class(bog_pore_month$MN)=="character") bog_pore_month$MN <-as.numeric(bog_pore_month$MN)
if (class(bog_pore_month$SI)=="factor") bog_pore_month$SI <-as.numeric(levels(bog_pore_month$SI))[as.integer(bog_pore_month$SI) ]               
if (class(bog_pore_month$SI)=="character") bog_pore_month$SI <-as.numeric(bog_pore_month$SI)
if (class(bog_pore_month$SR)=="factor") bog_pore_month$SR <-as.numeric(levels(bog_pore_month$SR))[as.integer(bog_pore_month$SR) ]               
if (class(bog_pore_month$SR)=="character") bog_pore_month$SR <-as.numeric(bog_pore_month$SR)
if (class(bog_pore_month$NH4)=="factor") bog_pore_month$NH4 <-as.numeric(levels(bog_pore_month$NH4))[as.integer(bog_pore_month$NH4) ]               
if (class(bog_pore_month$NH4)=="character") bog_pore_month$NH4 <-as.numeric(bog_pore_month$NH4)
if (class(bog_pore_month$NO3)=="factor") bog_pore_month$NO3 <-as.numeric(levels(bog_pore_month$NO3))[as.integer(bog_pore_month$NO3) ]               
if (class(bog_pore_month$NO3)=="character") bog_pore_month$NO3 <-as.numeric(bog_pore_month$NO3)
if (class(bog_pore_month$SRP)=="factor") bog_pore_month$SRP <-as.numeric(levels(bog_pore_month$SRP))[as.integer(bog_pore_month$SRP) ]               
if (class(bog_pore_month$SRP)=="character") bog_pore_month$SRP <-as.numeric(bog_pore_month$SRP)
if (class(bog_pore_month$TN)=="factor") bog_pore_month$TN <-as.numeric(levels(bog_pore_month$TN))[as.integer(bog_pore_month$TN) ]               
if (class(bog_pore_month$TN)=="character") bog_pore_month$TN <-as.numeric(bog_pore_month$TN)
if (class(bog_pore_month$TP)=="factor") bog_pore_month$TP <-as.numeric(levels(bog_pore_month$TP))[as.integer(bog_pore_month$TP) ]               
if (class(bog_pore_month$TP)=="character") bog_pore_month$TP <-as.numeric(bog_pore_month$TP)
if (class(bog_pore_month$TOC)=="factor") bog_pore_month$TOC <-as.numeric(levels(bog_pore_month$TOC))[as.integer(bog_pore_month$TOC) ]               
if (class(bog_pore_month$TOC)=="character") bog_pore_month$TOC <-as.numeric(bog_pore_month$TOC)

# Convert Missing Values to NA for non-dates

bog_pore_month$PH <- ifelse((trimws(as.character(bog_pore_month$PH))==trimws("-9999")),NA,bog_pore_month$PH)               
suppressWarnings(bog_pore_month$PH <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_month$PH))==as.character(as.numeric("-9999"))),NA,bog_pore_month$PH))
bog_pore_month$SPCOND <- ifelse((trimws(as.character(bog_pore_month$SPCOND))==trimws("-9999")),NA,bog_pore_month$SPCOND)               
suppressWarnings(bog_pore_month$SPCOND <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_month$SPCOND))==as.character(as.numeric("-9999"))),NA,bog_pore_month$SPCOND))
bog_pore_month$CL <- ifelse((trimws(as.character(bog_pore_month$CL))==trimws("-9999")),NA,bog_pore_month$CL)               
suppressWarnings(bog_pore_month$CL <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_month$CL))==as.character(as.numeric("-9999"))),NA,bog_pore_month$CL))
bog_pore_month$SO4 <- ifelse((trimws(as.character(bog_pore_month$SO4))==trimws("-9999")),NA,bog_pore_month$SO4)               
suppressWarnings(bog_pore_month$SO4 <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_month$SO4))==as.character(as.numeric("-9999"))),NA,bog_pore_month$SO4))
bog_pore_month$CA <- ifelse((trimws(as.character(bog_pore_month$CA))==trimws("-9999")),NA,bog_pore_month$CA)               
suppressWarnings(bog_pore_month$CA <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_month$CA))==as.character(as.numeric("-9999"))),NA,bog_pore_month$CA))
bog_pore_month$K <- ifelse((trimws(as.character(bog_pore_month$K))==trimws("-9999")),NA,bog_pore_month$K)               
suppressWarnings(bog_pore_month$K <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_month$K))==as.character(as.numeric("-9999"))),NA,bog_pore_month$K))
bog_pore_month$MG <- ifelse((trimws(as.character(bog_pore_month$MG))==trimws("-9999")),NA,bog_pore_month$MG)               
suppressWarnings(bog_pore_month$MG <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_month$MG))==as.character(as.numeric("-9999"))),NA,bog_pore_month$MG))
bog_pore_month$Na <- ifelse((trimws(as.character(bog_pore_month$Na))==trimws("-9999")),NA,bog_pore_month$Na)               
suppressWarnings(bog_pore_month$Na <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_month$Na))==as.character(as.numeric("-9999"))),NA,bog_pore_month$Na))
bog_pore_month$AL <- ifelse((trimws(as.character(bog_pore_month$AL))==trimws("-9999")),NA,bog_pore_month$AL)               
suppressWarnings(bog_pore_month$AL <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_month$AL))==as.character(as.numeric("-9999"))),NA,bog_pore_month$AL))
bog_pore_month$FE <- ifelse((trimws(as.character(bog_pore_month$FE))==trimws("-9999")),NA,bog_pore_month$FE)               
suppressWarnings(bog_pore_month$FE <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_month$FE))==as.character(as.numeric("-9999"))),NA,bog_pore_month$FE))
bog_pore_month$MN <- ifelse((trimws(as.character(bog_pore_month$MN))==trimws("-9999")),NA,bog_pore_month$MN)               
suppressWarnings(bog_pore_month$MN <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_month$MN))==as.character(as.numeric("-9999"))),NA,bog_pore_month$MN))
bog_pore_month$SI <- ifelse((trimws(as.character(bog_pore_month$SI))==trimws("-9999")),NA,bog_pore_month$SI)               
suppressWarnings(bog_pore_month$SI <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_month$SI))==as.character(as.numeric("-9999"))),NA,bog_pore_month$SI))
bog_pore_month$SR <- ifelse((trimws(as.character(bog_pore_month$SR))==trimws("-9999")),NA,bog_pore_month$SR)               
suppressWarnings(bog_pore_month$SR <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_month$SR))==as.character(as.numeric("-9999"))),NA,bog_pore_month$SR))
bog_pore_month$NH4 <- ifelse((trimws(as.character(bog_pore_month$NH4))==trimws("-9999")),NA,bog_pore_month$NH4)               
suppressWarnings(bog_pore_month$NH4 <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_month$NH4))==as.character(as.numeric("-9999"))),NA,bog_pore_month$NH4))
bog_pore_month$NO3 <- ifelse((trimws(as.character(bog_pore_month$NO3))==trimws("-9999")),NA,bog_pore_month$NO3)               
suppressWarnings(bog_pore_month$NO3 <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_month$NO3))==as.character(as.numeric("-9999"))),NA,bog_pore_month$NO3))
bog_pore_month$SRP <- ifelse((trimws(as.character(bog_pore_month$SRP))==trimws("-9999")),NA,bog_pore_month$SRP)               
suppressWarnings(bog_pore_month$SRP <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_month$SRP))==as.character(as.numeric("-9999"))),NA,bog_pore_month$SRP))
bog_pore_month$TN <- ifelse((trimws(as.character(bog_pore_month$TN))==trimws("-9999")),NA,bog_pore_month$TN)               
suppressWarnings(bog_pore_month$TN <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_month$TN))==as.character(as.numeric("-9999"))),NA,bog_pore_month$TN))
bog_pore_month$TP <- ifelse((trimws(as.character(bog_pore_month$TP))==trimws("-9999")),NA,bog_pore_month$TP)               
suppressWarnings(bog_pore_month$TP <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_month$TP))==as.character(as.numeric("-9999"))),NA,bog_pore_month$TP))
bog_pore_month$TOC <- ifelse((trimws(as.character(bog_pore_month$TOC))==trimws("-9999")),NA,bog_pore_month$TOC)               
suppressWarnings(bog_pore_month$TOC <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_month$TOC))==as.character(as.numeric("-9999"))),NA,bog_pore_month$TOC))


# Here is the structure of the input data frame:
str(bog_pore_month)                            
attach(bog_pore_month)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(LAB_ID)
summary(NAME)
summary(PIEZOMETER)
summary(DEPTH)
summary(DateTime)
summary(PH)
summary(SPCOND)
summary(CL)
summary(SO4)
summary(CA)
summary(K)
summary(MG)
summary(Na)
summary(AL)
summary(FE)
summary(MN)
summary(SI)
summary(SR)
summary(NH4)
summary(NO3)
summary(SRP)
summary(TN)
summary(TP)
summary(TOC) 
# Get more details on character variables

summary(as.factor(bog_pore_month$LAB_ID)) 
summary(as.factor(bog_pore_month$NAME)) 
summary(as.factor(bog_pore_month$PIEZOMETER))
detach(bog_pore_month)               


inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/712/2/0c95f59906b6d579b9304a8b474617c6" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


bog_pore_synop <-read.csv(infile2,header=F 
                          ,skip=1
                          ,sep=","  
                          , col.names=c(
                            "LAB_ID",     
                            "PIEZOMETER",     
                            "DateTime",     
                            "PH",     
                            "SPCOND",     
                            "CL",     
                            "SO4",     
                            "CA",     
                            "K",     
                            "MG",     
                            "Na",     
                            "AL",     
                            "FE",     
                            "MN",     
                            "SI",     
                            "SR",     
                            "SRP",     
                            "TN",     
                            "TP",     
                            "TOC",     
                            "O18",     
                            "D"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(bog_pore_synop$LAB_ID)!="factor") bog_pore_synop$LAB_ID<- as.factor(bog_pore_synop$LAB_ID)
if (class(bog_pore_synop$PIEZOMETER)!="factor") bog_pore_synop$PIEZOMETER<- as.factor(bog_pore_synop$PIEZOMETER)                                   
# attempting to convert bog_pore_synop$DateTime dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp2DateTime<-as.POSIXct(bog_pore_synop$DateTime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp2DateTime) == length(tmp2DateTime[!is.na(tmp2DateTime)])){bog_pore_synop$DateTime <- tmp2DateTime } else {print("Date conversion failed for bog_pore_synop$DateTime. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp2DateTime) 
if (class(bog_pore_synop$PH)=="factor") bog_pore_synop$PH <-as.numeric(levels(bog_pore_synop$PH))[as.integer(bog_pore_synop$PH) ]               
if (class(bog_pore_synop$PH)=="character") bog_pore_synop$PH <-as.numeric(bog_pore_synop$PH)
if (class(bog_pore_synop$SPCOND)=="factor") bog_pore_synop$SPCOND <-as.numeric(levels(bog_pore_synop$SPCOND))[as.integer(bog_pore_synop$SPCOND) ]               
if (class(bog_pore_synop$SPCOND)=="character") bog_pore_synop$SPCOND <-as.numeric(bog_pore_synop$SPCOND)
if (class(bog_pore_synop$CL)=="factor") bog_pore_synop$CL <-as.numeric(levels(bog_pore_synop$CL))[as.integer(bog_pore_synop$CL) ]               
if (class(bog_pore_synop$CL)=="character") bog_pore_synop$CL <-as.numeric(bog_pore_synop$CL)
if (class(bog_pore_synop$SO4)=="factor") bog_pore_synop$SO4 <-as.numeric(levels(bog_pore_synop$SO4))[as.integer(bog_pore_synop$SO4) ]               
if (class(bog_pore_synop$SO4)=="character") bog_pore_synop$SO4 <-as.numeric(bog_pore_synop$SO4)
if (class(bog_pore_synop$CA)=="factor") bog_pore_synop$CA <-as.numeric(levels(bog_pore_synop$CA))[as.integer(bog_pore_synop$CA) ]               
if (class(bog_pore_synop$CA)=="character") bog_pore_synop$CA <-as.numeric(bog_pore_synop$CA)
if (class(bog_pore_synop$K)=="factor") bog_pore_synop$K <-as.numeric(levels(bog_pore_synop$K))[as.integer(bog_pore_synop$K) ]               
if (class(bog_pore_synop$K)=="character") bog_pore_synop$K <-as.numeric(bog_pore_synop$K)
if (class(bog_pore_synop$MG)=="factor") bog_pore_synop$MG <-as.numeric(levels(bog_pore_synop$MG))[as.integer(bog_pore_synop$MG) ]               
if (class(bog_pore_synop$MG)=="character") bog_pore_synop$MG <-as.numeric(bog_pore_synop$MG)
if (class(bog_pore_synop$Na)=="factor") bog_pore_synop$Na <-as.numeric(levels(bog_pore_synop$Na))[as.integer(bog_pore_synop$Na) ]               
if (class(bog_pore_synop$Na)=="character") bog_pore_synop$Na <-as.numeric(bog_pore_synop$Na)
if (class(bog_pore_synop$AL)=="factor") bog_pore_synop$AL <-as.numeric(levels(bog_pore_synop$AL))[as.integer(bog_pore_synop$AL) ]               
if (class(bog_pore_synop$AL)=="character") bog_pore_synop$AL <-as.numeric(bog_pore_synop$AL)
if (class(bog_pore_synop$FE)=="factor") bog_pore_synop$FE <-as.numeric(levels(bog_pore_synop$FE))[as.integer(bog_pore_synop$FE) ]               
if (class(bog_pore_synop$FE)=="character") bog_pore_synop$FE <-as.numeric(bog_pore_synop$FE)
if (class(bog_pore_synop$MN)=="factor") bog_pore_synop$MN <-as.numeric(levels(bog_pore_synop$MN))[as.integer(bog_pore_synop$MN) ]               
if (class(bog_pore_synop$MN)=="character") bog_pore_synop$MN <-as.numeric(bog_pore_synop$MN)
if (class(bog_pore_synop$SI)=="factor") bog_pore_synop$SI <-as.numeric(levels(bog_pore_synop$SI))[as.integer(bog_pore_synop$SI) ]               
if (class(bog_pore_synop$SI)=="character") bog_pore_synop$SI <-as.numeric(bog_pore_synop$SI)
if (class(bog_pore_synop$SR)=="factor") bog_pore_synop$SR <-as.numeric(levels(bog_pore_synop$SR))[as.integer(bog_pore_synop$SR) ]               
if (class(bog_pore_synop$SR)=="character") bog_pore_synop$SR <-as.numeric(bog_pore_synop$SR)
if (class(bog_pore_synop$SRP)=="factor") bog_pore_synop$SRP <-as.numeric(levels(bog_pore_synop$SRP))[as.integer(bog_pore_synop$SRP) ]               
if (class(bog_pore_synop$SRP)=="character") bog_pore_synop$SRP <-as.numeric(bog_pore_synop$SRP)
if (class(bog_pore_synop$TN)=="factor") bog_pore_synop$TN <-as.numeric(levels(bog_pore_synop$TN))[as.integer(bog_pore_synop$TN) ]               
if (class(bog_pore_synop$TN)=="character") bog_pore_synop$TN <-as.numeric(bog_pore_synop$TN)
if (class(bog_pore_synop$TP)=="factor") bog_pore_synop$TP <-as.numeric(levels(bog_pore_synop$TP))[as.integer(bog_pore_synop$TP) ]               
if (class(bog_pore_synop$TP)=="character") bog_pore_synop$TP <-as.numeric(bog_pore_synop$TP)
if (class(bog_pore_synop$TOC)=="factor") bog_pore_synop$TOC <-as.numeric(levels(bog_pore_synop$TOC))[as.integer(bog_pore_synop$TOC) ]               
if (class(bog_pore_synop$TOC)=="character") bog_pore_synop$TOC <-as.numeric(bog_pore_synop$TOC)
if (class(bog_pore_synop$O18)=="factor") bog_pore_synop$O18 <-as.numeric(levels(bog_pore_synop$O18))[as.integer(bog_pore_synop$O18) ]               
if (class(bog_pore_synop$O18)=="character") bog_pore_synop$O18 <-as.numeric(bog_pore_synop$O18)
if (class(bog_pore_synop$D)=="factor") bog_pore_synop$D <-as.numeric(levels(bog_pore_synop$D))[as.integer(bog_pore_synop$D) ]               
if (class(bog_pore_synop$D)=="character") bog_pore_synop$D <-as.numeric(bog_pore_synop$D)

# Convert Missing Values to NA for non-dates

bog_pore_synop$PH <- ifelse((trimws(as.character(bog_pore_synop$PH))==trimws("-9999")),NA,bog_pore_synop$PH)               
suppressWarnings(bog_pore_synop$PH <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_synop$PH))==as.character(as.numeric("-9999"))),NA,bog_pore_synop$PH))
bog_pore_synop$SPCOND <- ifelse((trimws(as.character(bog_pore_synop$SPCOND))==trimws("-9999")),NA,bog_pore_synop$SPCOND)               
suppressWarnings(bog_pore_synop$SPCOND <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_synop$SPCOND))==as.character(as.numeric("-9999"))),NA,bog_pore_synop$SPCOND))
bog_pore_synop$CL <- ifelse((trimws(as.character(bog_pore_synop$CL))==trimws("-9999")),NA,bog_pore_synop$CL)               
suppressWarnings(bog_pore_synop$CL <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_synop$CL))==as.character(as.numeric("-9999"))),NA,bog_pore_synop$CL))
bog_pore_synop$SO4 <- ifelse((trimws(as.character(bog_pore_synop$SO4))==trimws("-9999")),NA,bog_pore_synop$SO4)               
suppressWarnings(bog_pore_synop$SO4 <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_synop$SO4))==as.character(as.numeric("-9999"))),NA,bog_pore_synop$SO4))
bog_pore_synop$CA <- ifelse((trimws(as.character(bog_pore_synop$CA))==trimws("-9999")),NA,bog_pore_synop$CA)               
suppressWarnings(bog_pore_synop$CA <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_synop$CA))==as.character(as.numeric("-9999"))),NA,bog_pore_synop$CA))
bog_pore_synop$K <- ifelse((trimws(as.character(bog_pore_synop$K))==trimws("-9999")),NA,bog_pore_synop$K)               
suppressWarnings(bog_pore_synop$K <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_synop$K))==as.character(as.numeric("-9999"))),NA,bog_pore_synop$K))
bog_pore_synop$MG <- ifelse((trimws(as.character(bog_pore_synop$MG))==trimws("-9999")),NA,bog_pore_synop$MG)               
suppressWarnings(bog_pore_synop$MG <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_synop$MG))==as.character(as.numeric("-9999"))),NA,bog_pore_synop$MG))
bog_pore_synop$Na <- ifelse((trimws(as.character(bog_pore_synop$Na))==trimws("-9999")),NA,bog_pore_synop$Na)               
suppressWarnings(bog_pore_synop$Na <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_synop$Na))==as.character(as.numeric("-9999"))),NA,bog_pore_synop$Na))
bog_pore_synop$AL <- ifelse((trimws(as.character(bog_pore_synop$AL))==trimws("-9999")),NA,bog_pore_synop$AL)               
suppressWarnings(bog_pore_synop$AL <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_synop$AL))==as.character(as.numeric("-9999"))),NA,bog_pore_synop$AL))
bog_pore_synop$FE <- ifelse((trimws(as.character(bog_pore_synop$FE))==trimws("-9999")),NA,bog_pore_synop$FE)               
suppressWarnings(bog_pore_synop$FE <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_synop$FE))==as.character(as.numeric("-9999"))),NA,bog_pore_synop$FE))
bog_pore_synop$MN <- ifelse((trimws(as.character(bog_pore_synop$MN))==trimws("-9999")),NA,bog_pore_synop$MN)               
suppressWarnings(bog_pore_synop$MN <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_synop$MN))==as.character(as.numeric("-9999"))),NA,bog_pore_synop$MN))
bog_pore_synop$SI <- ifelse((trimws(as.character(bog_pore_synop$SI))==trimws("-9999")),NA,bog_pore_synop$SI)               
suppressWarnings(bog_pore_synop$SI <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_synop$SI))==as.character(as.numeric("-9999"))),NA,bog_pore_synop$SI))
bog_pore_synop$SR <- ifelse((trimws(as.character(bog_pore_synop$SR))==trimws("-9999")),NA,bog_pore_synop$SR)               
suppressWarnings(bog_pore_synop$SR <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_synop$SR))==as.character(as.numeric("-9999"))),NA,bog_pore_synop$SR))
bog_pore_synop$SRP <- ifelse((trimws(as.character(bog_pore_synop$SRP))==trimws("-9999")),NA,bog_pore_synop$SRP)               
suppressWarnings(bog_pore_synop$SRP <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_synop$SRP))==as.character(as.numeric("-9999"))),NA,bog_pore_synop$SRP))
bog_pore_synop$TN <- ifelse((trimws(as.character(bog_pore_synop$TN))==trimws("-9999")),NA,bog_pore_synop$TN)               
suppressWarnings(bog_pore_synop$TN <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_synop$TN))==as.character(as.numeric("-9999"))),NA,bog_pore_synop$TN))
bog_pore_synop$TP <- ifelse((trimws(as.character(bog_pore_synop$TP))==trimws("-9999")),NA,bog_pore_synop$TP)               
suppressWarnings(bog_pore_synop$TP <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_synop$TP))==as.character(as.numeric("-9999"))),NA,bog_pore_synop$TP))
bog_pore_synop$TOC <- ifelse((trimws(as.character(bog_pore_synop$TOC))==trimws("-9999")),NA,bog_pore_synop$TOC)               
suppressWarnings(bog_pore_synop$TOC <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_synop$TOC))==as.character(as.numeric("-9999"))),NA,bog_pore_synop$TOC))
bog_pore_synop$O18 <- ifelse((trimws(as.character(bog_pore_synop$O18))==trimws("-9999")),NA,bog_pore_synop$O18)               
suppressWarnings(bog_pore_synop$O18 <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_synop$O18))==as.character(as.numeric("-9999"))),NA,bog_pore_synop$O18))
bog_pore_synop$D <- ifelse((trimws(as.character(bog_pore_synop$D))==trimws("-9999")),NA,bog_pore_synop$D)               
suppressWarnings(bog_pore_synop$D <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_synop$D))==as.character(as.numeric("-9999"))),NA,bog_pore_synop$D))


# Here is the structure of the input data frame:
str(bog_pore_synop)                            
attach(bog_pore_synop)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(LAB_ID)
summary(PIEZOMETER)
summary(DateTime)
summary(PH)
summary(SPCOND)
summary(CL)
summary(SO4)
summary(CA)
summary(K)
summary(MG)
summary(Na)
summary(AL)
summary(FE)
summary(MN)
summary(SI)
summary(SR)
summary(SRP)
summary(TN)
summary(TP)
summary(TOC)
summary(O18)
summary(D) 
# Get more details on character variables

summary(as.factor(bog_pore_synop$LAB_ID)) 
summary(as.factor(bog_pore_synop$PIEZOMETER))
detach(bog_pore_synop)               


inUrl3  <- "https://pasta.lternet.edu/package/data/eml/edi/712/2/8f2373caa4839f0922378c24e00e3906" 
infile3 <- tempfile()
try(download.file(inUrl3,infile3,method="curl"))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")


bog_pore_week <-read.csv(infile3,header=F 
                         ,skip=1
                         ,sep=","  
                         , col.names=c(
                           "LAB_ID",     
                           "PIEZOMETER",     
                           "DateTime",     
                           "PH",     
                           "SPCOND",     
                           "CL",     
                           "SO4",     
                           "CA",     
                           "K",     
                           "MG",     
                           "Na",     
                           "AL",     
                           "FE",     
                           "MN",     
                           "SI",     
                           "SR",     
                           "NH4",     
                           "NO3",     
                           "SRP",     
                           "TN",     
                           "TP",     
                           "TOC",     
                           "O18",     
                           "D",     
                           "FEII",     
                           "FEIII"    ), check.names=TRUE)

unlink(infile3)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(bog_pore_week$LAB_ID)!="factor") bog_pore_week$LAB_ID<- as.factor(bog_pore_week$LAB_ID)
if (class(bog_pore_week$PIEZOMETER)!="factor") bog_pore_week$PIEZOMETER<- as.factor(bog_pore_week$PIEZOMETER)                                   
# attempting to convert bog_pore_week$DateTime dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp3DateTime<-as.POSIXct(bog_pore_week$DateTime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp3DateTime) == length(tmp3DateTime[!is.na(tmp3DateTime)])){bog_pore_week$DateTime <- tmp3DateTime } else {print("Date conversion failed for bog_pore_week$DateTime. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp3DateTime) 
if (class(bog_pore_week$PH)=="factor") bog_pore_week$PH <-as.numeric(levels(bog_pore_week$PH))[as.integer(bog_pore_week$PH) ]               
if (class(bog_pore_week$PH)=="character") bog_pore_week$PH <-as.numeric(bog_pore_week$PH)
if (class(bog_pore_week$SPCOND)=="factor") bog_pore_week$SPCOND <-as.numeric(levels(bog_pore_week$SPCOND))[as.integer(bog_pore_week$SPCOND) ]               
if (class(bog_pore_week$SPCOND)=="character") bog_pore_week$SPCOND <-as.numeric(bog_pore_week$SPCOND)
if (class(bog_pore_week$CL)=="factor") bog_pore_week$CL <-as.numeric(levels(bog_pore_week$CL))[as.integer(bog_pore_week$CL) ]               
if (class(bog_pore_week$CL)=="character") bog_pore_week$CL <-as.numeric(bog_pore_week$CL)
if (class(bog_pore_week$SO4)=="factor") bog_pore_week$SO4 <-as.numeric(levels(bog_pore_week$SO4))[as.integer(bog_pore_week$SO4) ]               
if (class(bog_pore_week$SO4)=="character") bog_pore_week$SO4 <-as.numeric(bog_pore_week$SO4)
if (class(bog_pore_week$CA)=="factor") bog_pore_week$CA <-as.numeric(levels(bog_pore_week$CA))[as.integer(bog_pore_week$CA) ]               
if (class(bog_pore_week$CA)=="character") bog_pore_week$CA <-as.numeric(bog_pore_week$CA)
if (class(bog_pore_week$K)=="factor") bog_pore_week$K <-as.numeric(levels(bog_pore_week$K))[as.integer(bog_pore_week$K) ]               
if (class(bog_pore_week$K)=="character") bog_pore_week$K <-as.numeric(bog_pore_week$K)
if (class(bog_pore_week$MG)=="factor") bog_pore_week$MG <-as.numeric(levels(bog_pore_week$MG))[as.integer(bog_pore_week$MG) ]               
if (class(bog_pore_week$MG)=="character") bog_pore_week$MG <-as.numeric(bog_pore_week$MG)
if (class(bog_pore_week$Na)=="factor") bog_pore_week$Na <-as.numeric(levels(bog_pore_week$Na))[as.integer(bog_pore_week$Na) ]               
if (class(bog_pore_week$Na)=="character") bog_pore_week$Na <-as.numeric(bog_pore_week$Na)
if (class(bog_pore_week$AL)=="factor") bog_pore_week$AL <-as.numeric(levels(bog_pore_week$AL))[as.integer(bog_pore_week$AL) ]               
if (class(bog_pore_week$AL)=="character") bog_pore_week$AL <-as.numeric(bog_pore_week$AL)
if (class(bog_pore_week$FE)=="factor") bog_pore_week$FE <-as.numeric(levels(bog_pore_week$FE))[as.integer(bog_pore_week$FE) ]               
if (class(bog_pore_week$FE)=="character") bog_pore_week$FE <-as.numeric(bog_pore_week$FE)
if (class(bog_pore_week$MN)=="factor") bog_pore_week$MN <-as.numeric(levels(bog_pore_week$MN))[as.integer(bog_pore_week$MN) ]               
if (class(bog_pore_week$MN)=="character") bog_pore_week$MN <-as.numeric(bog_pore_week$MN)
if (class(bog_pore_week$SI)=="factor") bog_pore_week$SI <-as.numeric(levels(bog_pore_week$SI))[as.integer(bog_pore_week$SI) ]               
if (class(bog_pore_week$SI)=="character") bog_pore_week$SI <-as.numeric(bog_pore_week$SI)
if (class(bog_pore_week$SR)=="factor") bog_pore_week$SR <-as.numeric(levels(bog_pore_week$SR))[as.integer(bog_pore_week$SR) ]               
if (class(bog_pore_week$SR)=="character") bog_pore_week$SR <-as.numeric(bog_pore_week$SR)
if (class(bog_pore_week$NH4)=="factor") bog_pore_week$NH4 <-as.numeric(levels(bog_pore_week$NH4))[as.integer(bog_pore_week$NH4) ]               
if (class(bog_pore_week$NH4)=="character") bog_pore_week$NH4 <-as.numeric(bog_pore_week$NH4)
if (class(bog_pore_week$NO3)=="factor") bog_pore_week$NO3 <-as.numeric(levels(bog_pore_week$NO3))[as.integer(bog_pore_week$NO3) ]               
if (class(bog_pore_week$NO3)=="character") bog_pore_week$NO3 <-as.numeric(bog_pore_week$NO3)
if (class(bog_pore_week$SRP)=="factor") bog_pore_week$SRP <-as.numeric(levels(bog_pore_week$SRP))[as.integer(bog_pore_week$SRP) ]               
if (class(bog_pore_week$SRP)=="character") bog_pore_week$SRP <-as.numeric(bog_pore_week$SRP)
if (class(bog_pore_week$TN)=="factor") bog_pore_week$TN <-as.numeric(levels(bog_pore_week$TN))[as.integer(bog_pore_week$TN) ]               
if (class(bog_pore_week$TN)=="character") bog_pore_week$TN <-as.numeric(bog_pore_week$TN)
if (class(bog_pore_week$TP)=="factor") bog_pore_week$TP <-as.numeric(levels(bog_pore_week$TP))[as.integer(bog_pore_week$TP) ]               
if (class(bog_pore_week$TP)=="character") bog_pore_week$TP <-as.numeric(bog_pore_week$TP)
if (class(bog_pore_week$TOC)=="factor") bog_pore_week$TOC <-as.numeric(levels(bog_pore_week$TOC))[as.integer(bog_pore_week$TOC) ]               
if (class(bog_pore_week$TOC)=="character") bog_pore_week$TOC <-as.numeric(bog_pore_week$TOC)
if (class(bog_pore_week$O18)=="factor") bog_pore_week$O18 <-as.numeric(levels(bog_pore_week$O18))[as.integer(bog_pore_week$O18) ]               
if (class(bog_pore_week$O18)=="character") bog_pore_week$O18 <-as.numeric(bog_pore_week$O18)
if (class(bog_pore_week$D)=="factor") bog_pore_week$D <-as.numeric(levels(bog_pore_week$D))[as.integer(bog_pore_week$D) ]               
if (class(bog_pore_week$D)=="character") bog_pore_week$D <-as.numeric(bog_pore_week$D)
if (class(bog_pore_week$FEII)=="factor") bog_pore_week$FEII <-as.numeric(levels(bog_pore_week$FEII))[as.integer(bog_pore_week$FEII) ]               
if (class(bog_pore_week$FEII)=="character") bog_pore_week$FEII <-as.numeric(bog_pore_week$FEII)
if (class(bog_pore_week$FEIII)=="factor") bog_pore_week$FEIII <-as.numeric(levels(bog_pore_week$FEIII))[as.integer(bog_pore_week$FEIII) ]               
if (class(bog_pore_week$FEIII)=="character") bog_pore_week$FEIII <-as.numeric(bog_pore_week$FEIII)

# Convert Missing Values to NA for non-dates

bog_pore_week$PH <- ifelse((trimws(as.character(bog_pore_week$PH))==trimws("-9999")),NA,bog_pore_week$PH)               
suppressWarnings(bog_pore_week$PH <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_week$PH))==as.character(as.numeric("-9999"))),NA,bog_pore_week$PH))
bog_pore_week$SPCOND <- ifelse((trimws(as.character(bog_pore_week$SPCOND))==trimws("-9999")),NA,bog_pore_week$SPCOND)               
suppressWarnings(bog_pore_week$SPCOND <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_week$SPCOND))==as.character(as.numeric("-9999"))),NA,bog_pore_week$SPCOND))
bog_pore_week$CL <- ifelse((trimws(as.character(bog_pore_week$CL))==trimws("-9999")),NA,bog_pore_week$CL)               
suppressWarnings(bog_pore_week$CL <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_week$CL))==as.character(as.numeric("-9999"))),NA,bog_pore_week$CL))
bog_pore_week$SO4 <- ifelse((trimws(as.character(bog_pore_week$SO4))==trimws("-9999")),NA,bog_pore_week$SO4)               
suppressWarnings(bog_pore_week$SO4 <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_week$SO4))==as.character(as.numeric("-9999"))),NA,bog_pore_week$SO4))
bog_pore_week$CA <- ifelse((trimws(as.character(bog_pore_week$CA))==trimws("-9999")),NA,bog_pore_week$CA)               
suppressWarnings(bog_pore_week$CA <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_week$CA))==as.character(as.numeric("-9999"))),NA,bog_pore_week$CA))
bog_pore_week$K <- ifelse((trimws(as.character(bog_pore_week$K))==trimws("-9999")),NA,bog_pore_week$K)               
suppressWarnings(bog_pore_week$K <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_week$K))==as.character(as.numeric("-9999"))),NA,bog_pore_week$K))
bog_pore_week$MG <- ifelse((trimws(as.character(bog_pore_week$MG))==trimws("-9999")),NA,bog_pore_week$MG)               
suppressWarnings(bog_pore_week$MG <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_week$MG))==as.character(as.numeric("-9999"))),NA,bog_pore_week$MG))
bog_pore_week$Na <- ifelse((trimws(as.character(bog_pore_week$Na))==trimws("-9999")),NA,bog_pore_week$Na)               
suppressWarnings(bog_pore_week$Na <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_week$Na))==as.character(as.numeric("-9999"))),NA,bog_pore_week$Na))
bog_pore_week$AL <- ifelse((trimws(as.character(bog_pore_week$AL))==trimws("-9999")),NA,bog_pore_week$AL)               
suppressWarnings(bog_pore_week$AL <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_week$AL))==as.character(as.numeric("-9999"))),NA,bog_pore_week$AL))
bog_pore_week$FE <- ifelse((trimws(as.character(bog_pore_week$FE))==trimws("-9999")),NA,bog_pore_week$FE)               
suppressWarnings(bog_pore_week$FE <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_week$FE))==as.character(as.numeric("-9999"))),NA,bog_pore_week$FE))
bog_pore_week$MN <- ifelse((trimws(as.character(bog_pore_week$MN))==trimws("-9999")),NA,bog_pore_week$MN)               
suppressWarnings(bog_pore_week$MN <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_week$MN))==as.character(as.numeric("-9999"))),NA,bog_pore_week$MN))
bog_pore_week$SI <- ifelse((trimws(as.character(bog_pore_week$SI))==trimws("-9999")),NA,bog_pore_week$SI)               
suppressWarnings(bog_pore_week$SI <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_week$SI))==as.character(as.numeric("-9999"))),NA,bog_pore_week$SI))
bog_pore_week$SR <- ifelse((trimws(as.character(bog_pore_week$SR))==trimws("-9999")),NA,bog_pore_week$SR)               
suppressWarnings(bog_pore_week$SR <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_week$SR))==as.character(as.numeric("-9999"))),NA,bog_pore_week$SR))
bog_pore_week$NH4 <- ifelse((trimws(as.character(bog_pore_week$NH4))==trimws("-9999")),NA,bog_pore_week$NH4)               
suppressWarnings(bog_pore_week$NH4 <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_week$NH4))==as.character(as.numeric("-9999"))),NA,bog_pore_week$NH4))
bog_pore_week$NO3 <- ifelse((trimws(as.character(bog_pore_week$NO3))==trimws("-9999")),NA,bog_pore_week$NO3)               
suppressWarnings(bog_pore_week$NO3 <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_week$NO3))==as.character(as.numeric("-9999"))),NA,bog_pore_week$NO3))
bog_pore_week$SRP <- ifelse((trimws(as.character(bog_pore_week$SRP))==trimws("-9999")),NA,bog_pore_week$SRP)               
suppressWarnings(bog_pore_week$SRP <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_week$SRP))==as.character(as.numeric("-9999"))),NA,bog_pore_week$SRP))
bog_pore_week$TN <- ifelse((trimws(as.character(bog_pore_week$TN))==trimws("-9999")),NA,bog_pore_week$TN)               
suppressWarnings(bog_pore_week$TN <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_week$TN))==as.character(as.numeric("-9999"))),NA,bog_pore_week$TN))
bog_pore_week$TP <- ifelse((trimws(as.character(bog_pore_week$TP))==trimws("-9999")),NA,bog_pore_week$TP)               
suppressWarnings(bog_pore_week$TP <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_week$TP))==as.character(as.numeric("-9999"))),NA,bog_pore_week$TP))
bog_pore_week$TOC <- ifelse((trimws(as.character(bog_pore_week$TOC))==trimws("-9999")),NA,bog_pore_week$TOC)               
suppressWarnings(bog_pore_week$TOC <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_week$TOC))==as.character(as.numeric("-9999"))),NA,bog_pore_week$TOC))
bog_pore_week$O18 <- ifelse((trimws(as.character(bog_pore_week$O18))==trimws("-9999")),NA,bog_pore_week$O18)               
suppressWarnings(bog_pore_week$O18 <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_week$O18))==as.character(as.numeric("-9999"))),NA,bog_pore_week$O18))
bog_pore_week$D <- ifelse((trimws(as.character(bog_pore_week$D))==trimws("-9999")),NA,bog_pore_week$D)               
suppressWarnings(bog_pore_week$D <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_week$D))==as.character(as.numeric("-9999"))),NA,bog_pore_week$D))
bog_pore_week$FEII <- ifelse((trimws(as.character(bog_pore_week$FEII))==trimws("-9999")),NA,bog_pore_week$FEII)               
suppressWarnings(bog_pore_week$FEII <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_week$FEII))==as.character(as.numeric("-9999"))),NA,bog_pore_week$FEII))
bog_pore_week$FEIII <- ifelse((trimws(as.character(bog_pore_week$FEIII))==trimws("-9999")),NA,bog_pore_week$FEIII)               
suppressWarnings(bog_pore_week$FEIII <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(bog_pore_week$FEIII))==as.character(as.numeric("-9999"))),NA,bog_pore_week$FEIII))


# Here is the structure of the input data frame:
str(bog_pore_week)                            
attach(bog_pore_week)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(LAB_ID)
summary(PIEZOMETER)
summary(DateTime)
summary(PH)
summary(SPCOND)
summary(CL)
summary(SO4)
summary(CA)
summary(K)
summary(MG)
summary(Na)
summary(AL)
summary(FE)
summary(MN)
summary(SI)
summary(SR)
summary(NH4)
summary(NO3)
summary(SRP)
summary(TN)
summary(TP)
summary(TOC)
summary(O18)
summary(D)
summary(FEII)
summary(FEIII) 
# Get more details on character variables

summary(as.factor(bog_pore_week$LAB_ID)) 
summary(as.factor(bog_pore_week$PIEZOMETER))
detach(bog_pore_week)               


inUrl4  <- "https://pasta.lternet.edu/package/data/eml/edi/712/2/a2540ed6f622b41a308f14cd94c3cc32" 
infile4 <- tempfile()
try(download.file(inUrl4,infile4,method="curl"))
if (is.na(file.size(infile4))) download.file(inUrl4,infile4,method="auto")


lagg_pore_synop <-read.csv(infile4,header=F 
                           ,skip=1
                           ,sep=","  
                           , col.names=c(
                             "LAB_ID",     
                             "PIEZOMETER",     
                             "DateTime",     
                             "PH",     
                             "SPCOND",     
                             "CL",     
                             "SO4",     
                             "CA",     
                             "K",     
                             "MG",     
                             "Na",     
                             "AL",     
                             "FE",     
                             "MN",     
                             "SI",     
                             "SR",     
                             "SRP",     
                             "TN",     
                             "TP",     
                             "TOC",     
                             "O18",     
                             "D"    ), check.names=TRUE)

unlink(infile4)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(lagg_pore_synop$LAB_ID)!="factor") lagg_pore_synop$LAB_ID<- as.factor(lagg_pore_synop$LAB_ID)
if (class(lagg_pore_synop$PIEZOMETER)!="factor") lagg_pore_synop$PIEZOMETER<- as.factor(lagg_pore_synop$PIEZOMETER)                                   
# attempting to convert lagg_pore_synop$DateTime dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp4DateTime<-as.POSIXct(lagg_pore_synop$DateTime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp4DateTime) == length(tmp4DateTime[!is.na(tmp4DateTime)])){lagg_pore_synop$DateTime <- tmp4DateTime } else {print("Date conversion failed for lagg_pore_synop$DateTime. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp4DateTime) 
if (class(lagg_pore_synop$PH)=="factor") lagg_pore_synop$PH <-as.numeric(levels(lagg_pore_synop$PH))[as.integer(lagg_pore_synop$PH) ]               
if (class(lagg_pore_synop$PH)=="character") lagg_pore_synop$PH <-as.numeric(lagg_pore_synop$PH)
if (class(lagg_pore_synop$SPCOND)=="factor") lagg_pore_synop$SPCOND <-as.numeric(levels(lagg_pore_synop$SPCOND))[as.integer(lagg_pore_synop$SPCOND) ]               
if (class(lagg_pore_synop$SPCOND)=="character") lagg_pore_synop$SPCOND <-as.numeric(lagg_pore_synop$SPCOND)
if (class(lagg_pore_synop$CL)=="factor") lagg_pore_synop$CL <-as.numeric(levels(lagg_pore_synop$CL))[as.integer(lagg_pore_synop$CL) ]               
if (class(lagg_pore_synop$CL)=="character") lagg_pore_synop$CL <-as.numeric(lagg_pore_synop$CL)
if (class(lagg_pore_synop$SO4)=="factor") lagg_pore_synop$SO4 <-as.numeric(levels(lagg_pore_synop$SO4))[as.integer(lagg_pore_synop$SO4) ]               
if (class(lagg_pore_synop$SO4)=="character") lagg_pore_synop$SO4 <-as.numeric(lagg_pore_synop$SO4)
if (class(lagg_pore_synop$CA)=="factor") lagg_pore_synop$CA <-as.numeric(levels(lagg_pore_synop$CA))[as.integer(lagg_pore_synop$CA) ]               
if (class(lagg_pore_synop$CA)=="character") lagg_pore_synop$CA <-as.numeric(lagg_pore_synop$CA)
if (class(lagg_pore_synop$K)=="factor") lagg_pore_synop$K <-as.numeric(levels(lagg_pore_synop$K))[as.integer(lagg_pore_synop$K) ]               
if (class(lagg_pore_synop$K)=="character") lagg_pore_synop$K <-as.numeric(lagg_pore_synop$K)
if (class(lagg_pore_synop$MG)=="factor") lagg_pore_synop$MG <-as.numeric(levels(lagg_pore_synop$MG))[as.integer(lagg_pore_synop$MG) ]               
if (class(lagg_pore_synop$MG)=="character") lagg_pore_synop$MG <-as.numeric(lagg_pore_synop$MG)
if (class(lagg_pore_synop$Na)=="factor") lagg_pore_synop$Na <-as.numeric(levels(lagg_pore_synop$Na))[as.integer(lagg_pore_synop$Na) ]               
if (class(lagg_pore_synop$Na)=="character") lagg_pore_synop$Na <-as.numeric(lagg_pore_synop$Na)
if (class(lagg_pore_synop$AL)=="factor") lagg_pore_synop$AL <-as.numeric(levels(lagg_pore_synop$AL))[as.integer(lagg_pore_synop$AL) ]               
if (class(lagg_pore_synop$AL)=="character") lagg_pore_synop$AL <-as.numeric(lagg_pore_synop$AL)
if (class(lagg_pore_synop$FE)=="factor") lagg_pore_synop$FE <-as.numeric(levels(lagg_pore_synop$FE))[as.integer(lagg_pore_synop$FE) ]               
if (class(lagg_pore_synop$FE)=="character") lagg_pore_synop$FE <-as.numeric(lagg_pore_synop$FE)
if (class(lagg_pore_synop$MN)=="factor") lagg_pore_synop$MN <-as.numeric(levels(lagg_pore_synop$MN))[as.integer(lagg_pore_synop$MN) ]               
if (class(lagg_pore_synop$MN)=="character") lagg_pore_synop$MN <-as.numeric(lagg_pore_synop$MN)
if (class(lagg_pore_synop$SI)=="factor") lagg_pore_synop$SI <-as.numeric(levels(lagg_pore_synop$SI))[as.integer(lagg_pore_synop$SI) ]               
if (class(lagg_pore_synop$SI)=="character") lagg_pore_synop$SI <-as.numeric(lagg_pore_synop$SI)
if (class(lagg_pore_synop$SR)=="factor") lagg_pore_synop$SR <-as.numeric(levels(lagg_pore_synop$SR))[as.integer(lagg_pore_synop$SR) ]               
if (class(lagg_pore_synop$SR)=="character") lagg_pore_synop$SR <-as.numeric(lagg_pore_synop$SR)
if (class(lagg_pore_synop$SRP)=="factor") lagg_pore_synop$SRP <-as.numeric(levels(lagg_pore_synop$SRP))[as.integer(lagg_pore_synop$SRP) ]               
if (class(lagg_pore_synop$SRP)=="character") lagg_pore_synop$SRP <-as.numeric(lagg_pore_synop$SRP)
if (class(lagg_pore_synop$TN)=="factor") lagg_pore_synop$TN <-as.numeric(levels(lagg_pore_synop$TN))[as.integer(lagg_pore_synop$TN) ]               
if (class(lagg_pore_synop$TN)=="character") lagg_pore_synop$TN <-as.numeric(lagg_pore_synop$TN)
if (class(lagg_pore_synop$TP)=="factor") lagg_pore_synop$TP <-as.numeric(levels(lagg_pore_synop$TP))[as.integer(lagg_pore_synop$TP) ]               
if (class(lagg_pore_synop$TP)=="character") lagg_pore_synop$TP <-as.numeric(lagg_pore_synop$TP)
if (class(lagg_pore_synop$TOC)=="factor") lagg_pore_synop$TOC <-as.numeric(levels(lagg_pore_synop$TOC))[as.integer(lagg_pore_synop$TOC) ]               
if (class(lagg_pore_synop$TOC)=="character") lagg_pore_synop$TOC <-as.numeric(lagg_pore_synop$TOC)
if (class(lagg_pore_synop$O18)=="factor") lagg_pore_synop$O18 <-as.numeric(levels(lagg_pore_synop$O18))[as.integer(lagg_pore_synop$O18) ]               
if (class(lagg_pore_synop$O18)=="character") lagg_pore_synop$O18 <-as.numeric(lagg_pore_synop$O18)
if (class(lagg_pore_synop$D)=="factor") lagg_pore_synop$D <-as.numeric(levels(lagg_pore_synop$D))[as.integer(lagg_pore_synop$D) ]               
if (class(lagg_pore_synop$D)=="character") lagg_pore_synop$D <-as.numeric(lagg_pore_synop$D)

# Convert Missing Values to NA for non-dates

lagg_pore_synop$PH <- ifelse((trimws(as.character(lagg_pore_synop$PH))==trimws("-9999")),NA,lagg_pore_synop$PH)               
suppressWarnings(lagg_pore_synop$PH <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_synop$PH))==as.character(as.numeric("-9999"))),NA,lagg_pore_synop$PH))
lagg_pore_synop$SPCOND <- ifelse((trimws(as.character(lagg_pore_synop$SPCOND))==trimws("-9999")),NA,lagg_pore_synop$SPCOND)               
suppressWarnings(lagg_pore_synop$SPCOND <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_synop$SPCOND))==as.character(as.numeric("-9999"))),NA,lagg_pore_synop$SPCOND))
lagg_pore_synop$CL <- ifelse((trimws(as.character(lagg_pore_synop$CL))==trimws("-9999")),NA,lagg_pore_synop$CL)               
suppressWarnings(lagg_pore_synop$CL <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_synop$CL))==as.character(as.numeric("-9999"))),NA,lagg_pore_synop$CL))
lagg_pore_synop$SO4 <- ifelse((trimws(as.character(lagg_pore_synop$SO4))==trimws("-9999")),NA,lagg_pore_synop$SO4)               
suppressWarnings(lagg_pore_synop$SO4 <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_synop$SO4))==as.character(as.numeric("-9999"))),NA,lagg_pore_synop$SO4))
lagg_pore_synop$CA <- ifelse((trimws(as.character(lagg_pore_synop$CA))==trimws("-9999")),NA,lagg_pore_synop$CA)               
suppressWarnings(lagg_pore_synop$CA <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_synop$CA))==as.character(as.numeric("-9999"))),NA,lagg_pore_synop$CA))
lagg_pore_synop$K <- ifelse((trimws(as.character(lagg_pore_synop$K))==trimws("-9999")),NA,lagg_pore_synop$K)               
suppressWarnings(lagg_pore_synop$K <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_synop$K))==as.character(as.numeric("-9999"))),NA,lagg_pore_synop$K))
lagg_pore_synop$MG <- ifelse((trimws(as.character(lagg_pore_synop$MG))==trimws("-9999")),NA,lagg_pore_synop$MG)               
suppressWarnings(lagg_pore_synop$MG <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_synop$MG))==as.character(as.numeric("-9999"))),NA,lagg_pore_synop$MG))
lagg_pore_synop$Na <- ifelse((trimws(as.character(lagg_pore_synop$Na))==trimws("-9999")),NA,lagg_pore_synop$Na)               
suppressWarnings(lagg_pore_synop$Na <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_synop$Na))==as.character(as.numeric("-9999"))),NA,lagg_pore_synop$Na))
lagg_pore_synop$AL <- ifelse((trimws(as.character(lagg_pore_synop$AL))==trimws("-9999")),NA,lagg_pore_synop$AL)               
suppressWarnings(lagg_pore_synop$AL <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_synop$AL))==as.character(as.numeric("-9999"))),NA,lagg_pore_synop$AL))
lagg_pore_synop$FE <- ifelse((trimws(as.character(lagg_pore_synop$FE))==trimws("-9999")),NA,lagg_pore_synop$FE)               
suppressWarnings(lagg_pore_synop$FE <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_synop$FE))==as.character(as.numeric("-9999"))),NA,lagg_pore_synop$FE))
lagg_pore_synop$MN <- ifelse((trimws(as.character(lagg_pore_synop$MN))==trimws("-9999")),NA,lagg_pore_synop$MN)               
suppressWarnings(lagg_pore_synop$MN <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_synop$MN))==as.character(as.numeric("-9999"))),NA,lagg_pore_synop$MN))
lagg_pore_synop$SI <- ifelse((trimws(as.character(lagg_pore_synop$SI))==trimws("-9999")),NA,lagg_pore_synop$SI)               
suppressWarnings(lagg_pore_synop$SI <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_synop$SI))==as.character(as.numeric("-9999"))),NA,lagg_pore_synop$SI))
lagg_pore_synop$SR <- ifelse((trimws(as.character(lagg_pore_synop$SR))==trimws("-9999")),NA,lagg_pore_synop$SR)               
suppressWarnings(lagg_pore_synop$SR <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_synop$SR))==as.character(as.numeric("-9999"))),NA,lagg_pore_synop$SR))
lagg_pore_synop$SRP <- ifelse((trimws(as.character(lagg_pore_synop$SRP))==trimws("-9999")),NA,lagg_pore_synop$SRP)               
suppressWarnings(lagg_pore_synop$SRP <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_synop$SRP))==as.character(as.numeric("-9999"))),NA,lagg_pore_synop$SRP))
lagg_pore_synop$TN <- ifelse((trimws(as.character(lagg_pore_synop$TN))==trimws("-9999")),NA,lagg_pore_synop$TN)               
suppressWarnings(lagg_pore_synop$TN <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_synop$TN))==as.character(as.numeric("-9999"))),NA,lagg_pore_synop$TN))
lagg_pore_synop$TP <- ifelse((trimws(as.character(lagg_pore_synop$TP))==trimws("-9999")),NA,lagg_pore_synop$TP)               
suppressWarnings(lagg_pore_synop$TP <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_synop$TP))==as.character(as.numeric("-9999"))),NA,lagg_pore_synop$TP))
lagg_pore_synop$TOC <- ifelse((trimws(as.character(lagg_pore_synop$TOC))==trimws("-9999")),NA,lagg_pore_synop$TOC)               
suppressWarnings(lagg_pore_synop$TOC <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_synop$TOC))==as.character(as.numeric("-9999"))),NA,lagg_pore_synop$TOC))
lagg_pore_synop$O18 <- ifelse((trimws(as.character(lagg_pore_synop$O18))==trimws("-9999")),NA,lagg_pore_synop$O18)               
suppressWarnings(lagg_pore_synop$O18 <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_synop$O18))==as.character(as.numeric("-9999"))),NA,lagg_pore_synop$O18))
lagg_pore_synop$D <- ifelse((trimws(as.character(lagg_pore_synop$D))==trimws("-9999")),NA,lagg_pore_synop$D)               
suppressWarnings(lagg_pore_synop$D <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_synop$D))==as.character(as.numeric("-9999"))),NA,lagg_pore_synop$D))


# Here is the structure of the input data frame:
str(lagg_pore_synop)                            
attach(lagg_pore_synop)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(LAB_ID)
summary(PIEZOMETER)
summary(DateTime)
summary(PH)
summary(SPCOND)
summary(CL)
summary(SO4)
summary(CA)
summary(K)
summary(MG)
summary(Na)
summary(AL)
summary(FE)
summary(MN)
summary(SI)
summary(SR)
summary(SRP)
summary(TN)
summary(TP)
summary(TOC)
summary(O18)
summary(D) 
# Get more details on character variables

summary(as.factor(lagg_pore_synop$LAB_ID)) 
summary(as.factor(lagg_pore_synop$PIEZOMETER))
detach(lagg_pore_synop)               


inUrl5  <- "https://pasta.lternet.edu/package/data/eml/edi/712/2/c53ed302cae125ef38f4e0e71adf9b29" 
infile5 <- tempfile()
try(download.file(inUrl5,infile5,method="curl"))
if (is.na(file.size(infile5))) download.file(inUrl5,infile5,method="auto")


lagg_pore_week <-read.csv(infile5,header=F 
                          ,skip=1
                          ,sep=","  
                          , col.names=c(
                            "LAB_ID",     
                            "PIEZOMETER",     
                            "DateTime",     
                            "PH",     
                            "SPCOND",     
                            "CL",     
                            "SO4",     
                            "CA",     
                            "K",     
                            "MG",     
                            "Na",     
                            "AL",     
                            "FE",     
                            "MN",     
                            "SI",     
                            "SR",     
                            "NH4",     
                            "NO3",     
                            "SRP",     
                            "TN",     
                            "TP",     
                            "TOC",     
                            "O18",     
                            "D",     
                            "FEII",     
                            "FEIII"    ), check.names=TRUE)

unlink(infile5)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(lagg_pore_week$LAB_ID)!="factor") lagg_pore_week$LAB_ID<- as.factor(lagg_pore_week$LAB_ID)
if (class(lagg_pore_week$PIEZOMETER)!="factor") lagg_pore_week$PIEZOMETER<- as.factor(lagg_pore_week$PIEZOMETER)                                   
# attempting to convert lagg_pore_week$DateTime dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp5DateTime<-as.POSIXct(lagg_pore_week$DateTime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp5DateTime) == length(tmp5DateTime[!is.na(tmp5DateTime)])){lagg_pore_week$DateTime <- tmp5DateTime } else {print("Date conversion failed for lagg_pore_week$DateTime. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp5DateTime) 
if (class(lagg_pore_week$PH)=="factor") lagg_pore_week$PH <-as.numeric(levels(lagg_pore_week$PH))[as.integer(lagg_pore_week$PH) ]               
if (class(lagg_pore_week$PH)=="character") lagg_pore_week$PH <-as.numeric(lagg_pore_week$PH)
if (class(lagg_pore_week$SPCOND)=="factor") lagg_pore_week$SPCOND <-as.numeric(levels(lagg_pore_week$SPCOND))[as.integer(lagg_pore_week$SPCOND) ]               
if (class(lagg_pore_week$SPCOND)=="character") lagg_pore_week$SPCOND <-as.numeric(lagg_pore_week$SPCOND)
if (class(lagg_pore_week$CL)=="factor") lagg_pore_week$CL <-as.numeric(levels(lagg_pore_week$CL))[as.integer(lagg_pore_week$CL) ]               
if (class(lagg_pore_week$CL)=="character") lagg_pore_week$CL <-as.numeric(lagg_pore_week$CL)
if (class(lagg_pore_week$SO4)=="factor") lagg_pore_week$SO4 <-as.numeric(levels(lagg_pore_week$SO4))[as.integer(lagg_pore_week$SO4) ]               
if (class(lagg_pore_week$SO4)=="character") lagg_pore_week$SO4 <-as.numeric(lagg_pore_week$SO4)
if (class(lagg_pore_week$CA)=="factor") lagg_pore_week$CA <-as.numeric(levels(lagg_pore_week$CA))[as.integer(lagg_pore_week$CA) ]               
if (class(lagg_pore_week$CA)=="character") lagg_pore_week$CA <-as.numeric(lagg_pore_week$CA)
if (class(lagg_pore_week$K)=="factor") lagg_pore_week$K <-as.numeric(levels(lagg_pore_week$K))[as.integer(lagg_pore_week$K) ]               
if (class(lagg_pore_week$K)=="character") lagg_pore_week$K <-as.numeric(lagg_pore_week$K)
if (class(lagg_pore_week$MG)=="factor") lagg_pore_week$MG <-as.numeric(levels(lagg_pore_week$MG))[as.integer(lagg_pore_week$MG) ]               
if (class(lagg_pore_week$MG)=="character") lagg_pore_week$MG <-as.numeric(lagg_pore_week$MG)
if (class(lagg_pore_week$Na)=="factor") lagg_pore_week$Na <-as.numeric(levels(lagg_pore_week$Na))[as.integer(lagg_pore_week$Na) ]               
if (class(lagg_pore_week$Na)=="character") lagg_pore_week$Na <-as.numeric(lagg_pore_week$Na)
if (class(lagg_pore_week$AL)=="factor") lagg_pore_week$AL <-as.numeric(levels(lagg_pore_week$AL))[as.integer(lagg_pore_week$AL) ]               
if (class(lagg_pore_week$AL)=="character") lagg_pore_week$AL <-as.numeric(lagg_pore_week$AL)
if (class(lagg_pore_week$FE)=="factor") lagg_pore_week$FE <-as.numeric(levels(lagg_pore_week$FE))[as.integer(lagg_pore_week$FE) ]               
if (class(lagg_pore_week$FE)=="character") lagg_pore_week$FE <-as.numeric(lagg_pore_week$FE)
if (class(lagg_pore_week$MN)=="factor") lagg_pore_week$MN <-as.numeric(levels(lagg_pore_week$MN))[as.integer(lagg_pore_week$MN) ]               
if (class(lagg_pore_week$MN)=="character") lagg_pore_week$MN <-as.numeric(lagg_pore_week$MN)
if (class(lagg_pore_week$SI)=="factor") lagg_pore_week$SI <-as.numeric(levels(lagg_pore_week$SI))[as.integer(lagg_pore_week$SI) ]               
if (class(lagg_pore_week$SI)=="character") lagg_pore_week$SI <-as.numeric(lagg_pore_week$SI)
if (class(lagg_pore_week$SR)=="factor") lagg_pore_week$SR <-as.numeric(levels(lagg_pore_week$SR))[as.integer(lagg_pore_week$SR) ]               
if (class(lagg_pore_week$SR)=="character") lagg_pore_week$SR <-as.numeric(lagg_pore_week$SR)
if (class(lagg_pore_week$NH4)=="factor") lagg_pore_week$NH4 <-as.numeric(levels(lagg_pore_week$NH4))[as.integer(lagg_pore_week$NH4) ]               
if (class(lagg_pore_week$NH4)=="character") lagg_pore_week$NH4 <-as.numeric(lagg_pore_week$NH4)
if (class(lagg_pore_week$NO3)=="factor") lagg_pore_week$NO3 <-as.numeric(levels(lagg_pore_week$NO3))[as.integer(lagg_pore_week$NO3) ]               
if (class(lagg_pore_week$NO3)=="character") lagg_pore_week$NO3 <-as.numeric(lagg_pore_week$NO3)
if (class(lagg_pore_week$SRP)=="factor") lagg_pore_week$SRP <-as.numeric(levels(lagg_pore_week$SRP))[as.integer(lagg_pore_week$SRP) ]               
if (class(lagg_pore_week$SRP)=="character") lagg_pore_week$SRP <-as.numeric(lagg_pore_week$SRP)
if (class(lagg_pore_week$TN)=="factor") lagg_pore_week$TN <-as.numeric(levels(lagg_pore_week$TN))[as.integer(lagg_pore_week$TN) ]               
if (class(lagg_pore_week$TN)=="character") lagg_pore_week$TN <-as.numeric(lagg_pore_week$TN)
if (class(lagg_pore_week$TP)=="factor") lagg_pore_week$TP <-as.numeric(levels(lagg_pore_week$TP))[as.integer(lagg_pore_week$TP) ]               
if (class(lagg_pore_week$TP)=="character") lagg_pore_week$TP <-as.numeric(lagg_pore_week$TP)
if (class(lagg_pore_week$TOC)=="factor") lagg_pore_week$TOC <-as.numeric(levels(lagg_pore_week$TOC))[as.integer(lagg_pore_week$TOC) ]               
if (class(lagg_pore_week$TOC)=="character") lagg_pore_week$TOC <-as.numeric(lagg_pore_week$TOC)
if (class(lagg_pore_week$O18)=="factor") lagg_pore_week$O18 <-as.numeric(levels(lagg_pore_week$O18))[as.integer(lagg_pore_week$O18) ]               
if (class(lagg_pore_week$O18)=="character") lagg_pore_week$O18 <-as.numeric(lagg_pore_week$O18)
if (class(lagg_pore_week$D)=="factor") lagg_pore_week$D <-as.numeric(levels(lagg_pore_week$D))[as.integer(lagg_pore_week$D) ]               
if (class(lagg_pore_week$D)=="character") lagg_pore_week$D <-as.numeric(lagg_pore_week$D)
if (class(lagg_pore_week$FEII)=="factor") lagg_pore_week$FEII <-as.numeric(levels(lagg_pore_week$FEII))[as.integer(lagg_pore_week$FEII) ]               
if (class(lagg_pore_week$FEII)=="character") lagg_pore_week$FEII <-as.numeric(lagg_pore_week$FEII)
if (class(lagg_pore_week$FEIII)=="factor") lagg_pore_week$FEIII <-as.numeric(levels(lagg_pore_week$FEIII))[as.integer(lagg_pore_week$FEIII) ]               
if (class(lagg_pore_week$FEIII)=="character") lagg_pore_week$FEIII <-as.numeric(lagg_pore_week$FEIII)

# Convert Missing Values to NA for non-dates

lagg_pore_week$PH <- ifelse((trimws(as.character(lagg_pore_week$PH))==trimws("-9999")),NA,lagg_pore_week$PH)               
suppressWarnings(lagg_pore_week$PH <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_week$PH))==as.character(as.numeric("-9999"))),NA,lagg_pore_week$PH))
lagg_pore_week$SPCOND <- ifelse((trimws(as.character(lagg_pore_week$SPCOND))==trimws("-9999")),NA,lagg_pore_week$SPCOND)               
suppressWarnings(lagg_pore_week$SPCOND <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_week$SPCOND))==as.character(as.numeric("-9999"))),NA,lagg_pore_week$SPCOND))
lagg_pore_week$CL <- ifelse((trimws(as.character(lagg_pore_week$CL))==trimws("-9999")),NA,lagg_pore_week$CL)               
suppressWarnings(lagg_pore_week$CL <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_week$CL))==as.character(as.numeric("-9999"))),NA,lagg_pore_week$CL))
lagg_pore_week$SO4 <- ifelse((trimws(as.character(lagg_pore_week$SO4))==trimws("-9999")),NA,lagg_pore_week$SO4)               
suppressWarnings(lagg_pore_week$SO4 <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_week$SO4))==as.character(as.numeric("-9999"))),NA,lagg_pore_week$SO4))
lagg_pore_week$CA <- ifelse((trimws(as.character(lagg_pore_week$CA))==trimws("-9999")),NA,lagg_pore_week$CA)               
suppressWarnings(lagg_pore_week$CA <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_week$CA))==as.character(as.numeric("-9999"))),NA,lagg_pore_week$CA))
lagg_pore_week$K <- ifelse((trimws(as.character(lagg_pore_week$K))==trimws("-9999")),NA,lagg_pore_week$K)               
suppressWarnings(lagg_pore_week$K <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_week$K))==as.character(as.numeric("-9999"))),NA,lagg_pore_week$K))
lagg_pore_week$MG <- ifelse((trimws(as.character(lagg_pore_week$MG))==trimws("-9999")),NA,lagg_pore_week$MG)               
suppressWarnings(lagg_pore_week$MG <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_week$MG))==as.character(as.numeric("-9999"))),NA,lagg_pore_week$MG))
lagg_pore_week$Na <- ifelse((trimws(as.character(lagg_pore_week$Na))==trimws("-9999")),NA,lagg_pore_week$Na)               
suppressWarnings(lagg_pore_week$Na <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_week$Na))==as.character(as.numeric("-9999"))),NA,lagg_pore_week$Na))
lagg_pore_week$AL <- ifelse((trimws(as.character(lagg_pore_week$AL))==trimws("-9999")),NA,lagg_pore_week$AL)               
suppressWarnings(lagg_pore_week$AL <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_week$AL))==as.character(as.numeric("-9999"))),NA,lagg_pore_week$AL))
lagg_pore_week$FE <- ifelse((trimws(as.character(lagg_pore_week$FE))==trimws("-9999")),NA,lagg_pore_week$FE)               
suppressWarnings(lagg_pore_week$FE <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_week$FE))==as.character(as.numeric("-9999"))),NA,lagg_pore_week$FE))
lagg_pore_week$MN <- ifelse((trimws(as.character(lagg_pore_week$MN))==trimws("-9999")),NA,lagg_pore_week$MN)               
suppressWarnings(lagg_pore_week$MN <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_week$MN))==as.character(as.numeric("-9999"))),NA,lagg_pore_week$MN))
lagg_pore_week$SI <- ifelse((trimws(as.character(lagg_pore_week$SI))==trimws("-9999")),NA,lagg_pore_week$SI)               
suppressWarnings(lagg_pore_week$SI <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_week$SI))==as.character(as.numeric("-9999"))),NA,lagg_pore_week$SI))
lagg_pore_week$SR <- ifelse((trimws(as.character(lagg_pore_week$SR))==trimws("-9999")),NA,lagg_pore_week$SR)               
suppressWarnings(lagg_pore_week$SR <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_week$SR))==as.character(as.numeric("-9999"))),NA,lagg_pore_week$SR))
lagg_pore_week$NH4 <- ifelse((trimws(as.character(lagg_pore_week$NH4))==trimws("-9999")),NA,lagg_pore_week$NH4)               
suppressWarnings(lagg_pore_week$NH4 <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_week$NH4))==as.character(as.numeric("-9999"))),NA,lagg_pore_week$NH4))
lagg_pore_week$NO3 <- ifelse((trimws(as.character(lagg_pore_week$NO3))==trimws("-9999")),NA,lagg_pore_week$NO3)               
suppressWarnings(lagg_pore_week$NO3 <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_week$NO3))==as.character(as.numeric("-9999"))),NA,lagg_pore_week$NO3))
lagg_pore_week$SRP <- ifelse((trimws(as.character(lagg_pore_week$SRP))==trimws("-9999")),NA,lagg_pore_week$SRP)               
suppressWarnings(lagg_pore_week$SRP <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_week$SRP))==as.character(as.numeric("-9999"))),NA,lagg_pore_week$SRP))
lagg_pore_week$TN <- ifelse((trimws(as.character(lagg_pore_week$TN))==trimws("-9999")),NA,lagg_pore_week$TN)               
suppressWarnings(lagg_pore_week$TN <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_week$TN))==as.character(as.numeric("-9999"))),NA,lagg_pore_week$TN))
lagg_pore_week$TP <- ifelse((trimws(as.character(lagg_pore_week$TP))==trimws("-9999")),NA,lagg_pore_week$TP)               
suppressWarnings(lagg_pore_week$TP <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_week$TP))==as.character(as.numeric("-9999"))),NA,lagg_pore_week$TP))
lagg_pore_week$TOC <- ifelse((trimws(as.character(lagg_pore_week$TOC))==trimws("-9999")),NA,lagg_pore_week$TOC)               
suppressWarnings(lagg_pore_week$TOC <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_week$TOC))==as.character(as.numeric("-9999"))),NA,lagg_pore_week$TOC))
lagg_pore_week$O18 <- ifelse((trimws(as.character(lagg_pore_week$O18))==trimws("-9999")),NA,lagg_pore_week$O18)               
suppressWarnings(lagg_pore_week$O18 <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_week$O18))==as.character(as.numeric("-9999"))),NA,lagg_pore_week$O18))
lagg_pore_week$D <- ifelse((trimws(as.character(lagg_pore_week$D))==trimws("-9999")),NA,lagg_pore_week$D)               
suppressWarnings(lagg_pore_week$D <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_week$D))==as.character(as.numeric("-9999"))),NA,lagg_pore_week$D))
lagg_pore_week$FEII <- ifelse((trimws(as.character(lagg_pore_week$FEII))==trimws("-9999")),NA,lagg_pore_week$FEII)               
suppressWarnings(lagg_pore_week$FEII <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_week$FEII))==as.character(as.numeric("-9999"))),NA,lagg_pore_week$FEII))
lagg_pore_week$FEIII <- ifelse((trimws(as.character(lagg_pore_week$FEIII))==trimws("-9999")),NA,lagg_pore_week$FEIII)               
suppressWarnings(lagg_pore_week$FEIII <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(lagg_pore_week$FEIII))==as.character(as.numeric("-9999"))),NA,lagg_pore_week$FEIII))


# Here is the structure of the input data frame:
str(lagg_pore_week)                            
attach(lagg_pore_week)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(LAB_ID)
summary(PIEZOMETER)
summary(DateTime)
summary(PH)
summary(SPCOND)
summary(CL)
summary(SO4)
summary(CA)
summary(K)
summary(MG)
summary(Na)
summary(AL)
summary(FE)
summary(MN)
summary(SI)
summary(SR)
summary(NH4)
summary(NO3)
summary(SRP)
summary(TN)
summary(TP)
summary(TOC)
summary(O18)
summary(D)
summary(FEII)
summary(FEIII) 
# Get more details on character variables

summary(as.factor(lagg_pore_week$LAB_ID)) 
summary(as.factor(lagg_pore_week$PIEZOMETER))
detach(lagg_pore_week)               


inUrl6  <- "https://pasta.lternet.edu/package/data/eml/edi/712/2/90fe9126cc7c9922d5a4f1b771cc8e0c" 
infile6 <- tempfile()
try(download.file(inUrl6,infile6,method="curl"))
if (is.na(file.size(infile6))) download.file(inUrl6,infile6,method="auto")

#precipitation

# Package ID: edi.609.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Marcell Experimental Forest event based precipitation chemistry, 2008 - ongoing.
# Data set creator:  Stephen Sebestyen - USDA Forest Service, Northern Research Station 
# Data set creator:  Keith Oleheiser - XCEL Engineering 
# Data set creator:  John Larson - USDA Forest Service, Northern Research Station 
# Data set creator:  Nathan Aspelin - USDA Forest Service, Northern Research Station 
# Data set creator:  Jonathan Stelling - University of Minnesota 
# Data set creator:  Natalie Griffiths - Oak Ridge National Laboratory 
# Data set creator:  Nina Lany - USDA Forest Service, Northern Research Station 
# Contact:    -  Data Manager, Marcell Experimental Forest  - nina.lany@usda.gov
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/609/1/759dac4390593331bf1335a94b15af60" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


event_precip <-read.csv(infile1,header=F 
                        ,skip=1
                        ,sep=","  
                        , col.names=c(
                          "LAB_ID",     
                          "Location",     
                          "DateTime",     
                          "pH",     
                          "SpCond",     
                          "Cl",     
                          "SO4",     
                          "Ca",     
                          "Mg",     
                          "NH4",     
                          "NO3",     
                          "SRP",     
                          "TN",     
                          "TOC",     
                          "d18O",     
                          "d2H"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(event_precip$LAB_ID)!="factor") event_precip$LAB_ID<- as.factor(event_precip$LAB_ID)
if (class(event_precip$Location)!="factor") event_precip$Location<- as.factor(event_precip$Location)                                   
# attempting to convert event_precip$DateTime dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp1DateTime<-as.POSIXct(event_precip$DateTime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1DateTime) == length(tmp1DateTime[!is.na(tmp1DateTime)])){event_precip$DateTime <- tmp1DateTime } else {print("Date conversion failed for event_precip$DateTime. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1DateTime) 
if (class(event_precip$pH)=="factor") event_precip$pH <-as.numeric(levels(event_precip$pH))[as.integer(event_precip$pH) ]               
if (class(event_precip$pH)=="character") event_precip$pH <-as.numeric(event_precip$pH)
if (class(event_precip$SpCond)=="factor") event_precip$SpCond <-as.numeric(levels(event_precip$SpCond))[as.integer(event_precip$SpCond) ]               
if (class(event_precip$SpCond)=="character") event_precip$SpCond <-as.numeric(event_precip$SpCond)
if (class(event_precip$Cl)=="factor") event_precip$Cl <-as.numeric(levels(event_precip$Cl))[as.integer(event_precip$Cl) ]               
if (class(event_precip$Cl)=="character") event_precip$Cl <-as.numeric(event_precip$Cl)
if (class(event_precip$SO4)=="factor") event_precip$SO4 <-as.numeric(levels(event_precip$SO4))[as.integer(event_precip$SO4) ]               
if (class(event_precip$SO4)=="character") event_precip$SO4 <-as.numeric(event_precip$SO4)
if (class(event_precip$Ca)=="factor") event_precip$Ca <-as.numeric(levels(event_precip$Ca))[as.integer(event_precip$Ca) ]               
if (class(event_precip$Ca)=="character") event_precip$Ca <-as.numeric(event_precip$Ca)
if (class(event_precip$Mg)=="factor") event_precip$Mg <-as.numeric(levels(event_precip$Mg))[as.integer(event_precip$Mg) ]               
if (class(event_precip$Mg)=="character") event_precip$Mg <-as.numeric(event_precip$Mg)
if (class(event_precip$NH4)=="factor") event_precip$NH4 <-as.numeric(levels(event_precip$NH4))[as.integer(event_precip$NH4) ]               
if (class(event_precip$NH4)=="character") event_precip$NH4 <-as.numeric(event_precip$NH4)
if (class(event_precip$NO3)=="factor") event_precip$NO3 <-as.numeric(levels(event_precip$NO3))[as.integer(event_precip$NO3) ]               
if (class(event_precip$NO3)=="character") event_precip$NO3 <-as.numeric(event_precip$NO3)
if (class(event_precip$SRP)=="factor") event_precip$SRP <-as.numeric(levels(event_precip$SRP))[as.integer(event_precip$SRP) ]               
if (class(event_precip$SRP)=="character") event_precip$SRP <-as.numeric(event_precip$SRP)
if (class(event_precip$TN)=="factor") event_precip$TN <-as.numeric(levels(event_precip$TN))[as.integer(event_precip$TN) ]               
if (class(event_precip$TN)=="character") event_precip$TN <-as.numeric(event_precip$TN)
if (class(event_precip$TOC)=="factor") event_precip$TOC <-as.numeric(levels(event_precip$TOC))[as.integer(event_precip$TOC) ]               
if (class(event_precip$TOC)=="character") event_precip$TOC <-as.numeric(event_precip$TOC)
if (class(event_precip$d18O)=="factor") event_precip$d18O <-as.numeric(levels(event_precip$d18O))[as.integer(event_precip$d18O) ]               
if (class(event_precip$d18O)=="character") event_precip$d18O <-as.numeric(event_precip$d18O)
if (class(event_precip$d2H)=="factor") event_precip$d2H <-as.numeric(levels(event_precip$d2H))[as.integer(event_precip$d2H) ]               
if (class(event_precip$d2H)=="character") event_precip$d2H <-as.numeric(event_precip$d2H)

# Convert Missing Values to NA for non-dates

event_precip$pH <- ifelse((trimws(as.character(event_precip$pH))==trimws("-9999")),NA,event_precip$pH)               
suppressWarnings(event_precip$pH <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(event_precip$pH))==as.character(as.numeric("-9999"))),NA,event_precip$pH))
event_precip$SpCond <- ifelse((trimws(as.character(event_precip$SpCond))==trimws("-9999")),NA,event_precip$SpCond)               
suppressWarnings(event_precip$SpCond <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(event_precip$SpCond))==as.character(as.numeric("-9999"))),NA,event_precip$SpCond))
event_precip$Cl <- ifelse((trimws(as.character(event_precip$Cl))==trimws("-9999")),NA,event_precip$Cl)               
suppressWarnings(event_precip$Cl <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(event_precip$Cl))==as.character(as.numeric("-9999"))),NA,event_precip$Cl))
event_precip$SO4 <- ifelse((trimws(as.character(event_precip$SO4))==trimws("-9999")),NA,event_precip$SO4)               
suppressWarnings(event_precip$SO4 <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(event_precip$SO4))==as.character(as.numeric("-9999"))),NA,event_precip$SO4))
event_precip$Ca <- ifelse((trimws(as.character(event_precip$Ca))==trimws("-9999")),NA,event_precip$Ca)               
suppressWarnings(event_precip$Ca <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(event_precip$Ca))==as.character(as.numeric("-9999"))),NA,event_precip$Ca))
event_precip$Mg <- ifelse((trimws(as.character(event_precip$Mg))==trimws("-9999")),NA,event_precip$Mg)               
suppressWarnings(event_precip$Mg <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(event_precip$Mg))==as.character(as.numeric("-9999"))),NA,event_precip$Mg))
event_precip$NH4 <- ifelse((trimws(as.character(event_precip$NH4))==trimws("-9999")),NA,event_precip$NH4)               
suppressWarnings(event_precip$NH4 <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(event_precip$NH4))==as.character(as.numeric("-9999"))),NA,event_precip$NH4))
event_precip$NO3 <- ifelse((trimws(as.character(event_precip$NO3))==trimws("-9999")),NA,event_precip$NO3)               
suppressWarnings(event_precip$NO3 <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(event_precip$NO3))==as.character(as.numeric("-9999"))),NA,event_precip$NO3))
event_precip$SRP <- ifelse((trimws(as.character(event_precip$SRP))==trimws("-9999")),NA,event_precip$SRP)               
suppressWarnings(event_precip$SRP <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(event_precip$SRP))==as.character(as.numeric("-9999"))),NA,event_precip$SRP))
event_precip$TN <- ifelse((trimws(as.character(event_precip$TN))==trimws("-9999")),NA,event_precip$TN)               
suppressWarnings(event_precip$TN <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(event_precip$TN))==as.character(as.numeric("-9999"))),NA,event_precip$TN))
event_precip$TOC <- ifelse((trimws(as.character(event_precip$TOC))==trimws("-9999")),NA,event_precip$TOC)               
suppressWarnings(event_precip$TOC <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(event_precip$TOC))==as.character(as.numeric("-9999"))),NA,event_precip$TOC))
event_precip$d18O <- ifelse((trimws(as.character(event_precip$d18O))==trimws("-9999")),NA,event_precip$d18O)               
suppressWarnings(event_precip$d18O <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(event_precip$d18O))==as.character(as.numeric("-9999"))),NA,event_precip$d18O))
event_precip$d2H <- ifelse((trimws(as.character(event_precip$d2H))==trimws("-9999")),NA,event_precip$d2H)               
suppressWarnings(event_precip$d2H <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(event_precip$d2H))==as.character(as.numeric("-9999"))),NA,event_precip$d2H))


# Here is the structure of the input data frame:
str(event_precip)                            
attach(event_precip)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(LAB_ID)
summary(Location)
summary(DateTime)
summary(pH)
summary(SpCond)
summary(Cl)
summary(SO4)
summary(Ca)
summary(Mg)
summary(NH4)
summary(NO3)
summary(SRP)
summary(TN)
summary(TOC)
summary(d18O)
summary(d2H) 
# Get more details on character variables

summary(as.factor(event_precip$LAB_ID)) 
summary(as.factor(event_precip$Location))
detach(event_precip)             


############################################################## ground water############################################################## 
############################################################## ############################################################## 
# Package ID: edi.711.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Marcell Experimental Forest monthly aquifer groundwater chemistry at the S2 catchment, 2007 - ongoing.
# Data set creator:  Stephen Sebestyen - USDA Forest Service, Northern Research Station 
# Data set creator:  Richard Kyllander - USDA Forest Service, Northern Research Station 
# Data set creator:  John Larson - USDA Forest Service, Northern Research Station 
# Data set creator:  Nathan Aspelin - USDA Forest Service, Northern Research Station 
# Data set creator:  Keith Oleheiser - XCEL Engineering 
# Data set creator:  Doris Nelson - USDA Forest Service, Northern Research Station 
# Data set creator:  Jonathan Stelling - University of Minnesota 
# Data set creator:  Nina Lany - USDA Forest Service, Northern Research Station 
# Data set creator:  Randall Kolka - USDA Forest Service, Northern Research Station 
# Contact:    -  Data Manager, Marcell Experimental Forest  - nina.lany@usda.gov
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/711/1/646af91a1a2a2a503af868ed50b4bdd8" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


gw_month <-read.csv(infile1,header=F 
                    ,skip=1
                    ,sep=","  
                    , col.names=c(
                      "LAB_ID",     
                      "NAME",     
                      "DateTime",     
                      "PH",     
                      "SPCOND",     
                      "CL",     
                      "SO4",     
                      "CA",     
                      "K",     
                      "MG",     
                      "Na",     
                      "AL",     
                      "MN",     
                      "SI",     
                      "SR",     
                      "NH4",     
                      "NO3",     
                      "SRP",     
                      "TN",     
                      "TP",     
                      "TOC",     
                      "O18",     
                      "D"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(gw_month$LAB_ID)!="factor") gw_month$LAB_ID<- as.factor(gw_month$LAB_ID)
if (class(gw_month$NAME)!="factor") gw_month$NAME<- as.factor(gw_month$NAME)                                   
# attempting to convert gw_month$DateTime dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp1DateTime<-as.POSIXct(gw_month$DateTime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1DateTime) == length(tmp1DateTime[!is.na(tmp1DateTime)])){gw_month$DateTime <- tmp1DateTime } else {print("Date conversion failed for gw_month$DateTime. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1DateTime) 
if (class(gw_month$PH)=="factor") gw_month$PH <-as.numeric(levels(gw_month$PH))[as.integer(gw_month$PH) ]               
if (class(gw_month$PH)=="character") gw_month$PH <-as.numeric(gw_month$PH)
if (class(gw_month$SPCOND)=="factor") gw_month$SPCOND <-as.numeric(levels(gw_month$SPCOND))[as.integer(gw_month$SPCOND) ]               
if (class(gw_month$SPCOND)=="character") gw_month$SPCOND <-as.numeric(gw_month$SPCOND)
if (class(gw_month$CL)=="factor") gw_month$CL <-as.numeric(levels(gw_month$CL))[as.integer(gw_month$CL) ]               
if (class(gw_month$CL)=="character") gw_month$CL <-as.numeric(gw_month$CL)
if (class(gw_month$SO4)=="factor") gw_month$SO4 <-as.numeric(levels(gw_month$SO4))[as.integer(gw_month$SO4) ]               
if (class(gw_month$SO4)=="character") gw_month$SO4 <-as.numeric(gw_month$SO4)
if (class(gw_month$CA)=="factor") gw_month$CA <-as.numeric(levels(gw_month$CA))[as.integer(gw_month$CA) ]               
if (class(gw_month$CA)=="character") gw_month$CA <-as.numeric(gw_month$CA)
if (class(gw_month$K)=="factor") gw_month$K <-as.numeric(levels(gw_month$K))[as.integer(gw_month$K) ]               
if (class(gw_month$K)=="character") gw_month$K <-as.numeric(gw_month$K)
if (class(gw_month$MG)=="factor") gw_month$MG <-as.numeric(levels(gw_month$MG))[as.integer(gw_month$MG) ]               
if (class(gw_month$MG)=="character") gw_month$MG <-as.numeric(gw_month$MG)
if (class(gw_month$Na)=="factor") gw_month$Na <-as.numeric(levels(gw_month$Na))[as.integer(gw_month$Na) ]               
if (class(gw_month$Na)=="character") gw_month$Na <-as.numeric(gw_month$Na)
if (class(gw_month$AL)=="factor") gw_month$AL <-as.numeric(levels(gw_month$AL))[as.integer(gw_month$AL) ]               
if (class(gw_month$AL)=="character") gw_month$AL <-as.numeric(gw_month$AL)
if (class(gw_month$MN)=="factor") gw_month$MN <-as.numeric(levels(gw_month$MN))[as.integer(gw_month$MN) ]               
if (class(gw_month$MN)=="character") gw_month$MN <-as.numeric(gw_month$MN)
if (class(gw_month$SI)=="factor") gw_month$SI <-as.numeric(levels(gw_month$SI))[as.integer(gw_month$SI) ]               
if (class(gw_month$SI)=="character") gw_month$SI <-as.numeric(gw_month$SI)
if (class(gw_month$SR)=="factor") gw_month$SR <-as.numeric(levels(gw_month$SR))[as.integer(gw_month$SR) ]               
if (class(gw_month$SR)=="character") gw_month$SR <-as.numeric(gw_month$SR)
if (class(gw_month$NH4)=="factor") gw_month$NH4 <-as.numeric(levels(gw_month$NH4))[as.integer(gw_month$NH4) ]               
if (class(gw_month$NH4)=="character") gw_month$NH4 <-as.numeric(gw_month$NH4)
if (class(gw_month$NO3)=="factor") gw_month$NO3 <-as.numeric(levels(gw_month$NO3))[as.integer(gw_month$NO3) ]               
if (class(gw_month$NO3)=="character") gw_month$NO3 <-as.numeric(gw_month$NO3)
if (class(gw_month$SRP)=="factor") gw_month$SRP <-as.numeric(levels(gw_month$SRP))[as.integer(gw_month$SRP) ]               
if (class(gw_month$SRP)=="character") gw_month$SRP <-as.numeric(gw_month$SRP)
if (class(gw_month$TN)=="factor") gw_month$TN <-as.numeric(levels(gw_month$TN))[as.integer(gw_month$TN) ]               
if (class(gw_month$TN)=="character") gw_month$TN <-as.numeric(gw_month$TN)
if (class(gw_month$TP)=="factor") gw_month$TP <-as.numeric(levels(gw_month$TP))[as.integer(gw_month$TP) ]               
if (class(gw_month$TP)=="character") gw_month$TP <-as.numeric(gw_month$TP)
if (class(gw_month$TOC)=="factor") gw_month$TOC <-as.numeric(levels(gw_month$TOC))[as.integer(gw_month$TOC) ]               
if (class(gw_month$TOC)=="character") gw_month$TOC <-as.numeric(gw_month$TOC)
if (class(gw_month$O18)=="factor") gw_month$O18 <-as.numeric(levels(gw_month$O18))[as.integer(gw_month$O18) ]               
if (class(gw_month$O18)=="character") gw_month$O18 <-as.numeric(gw_month$O18)
if (class(gw_month$D)=="factor") gw_month$D <-as.numeric(levels(gw_month$D))[as.integer(gw_month$D) ]               
if (class(gw_month$D)=="character") gw_month$D <-as.numeric(gw_month$D)

# Convert Missing Values to NA for non-dates

gw_month$PH <- ifelse((trimws(as.character(gw_month$PH))==trimws("-9999")),NA,gw_month$PH)               
suppressWarnings(gw_month$PH <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(gw_month$PH))==as.character(as.numeric("-9999"))),NA,gw_month$PH))
gw_month$SPCOND <- ifelse((trimws(as.character(gw_month$SPCOND))==trimws("-9999")),NA,gw_month$SPCOND)               
suppressWarnings(gw_month$SPCOND <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(gw_month$SPCOND))==as.character(as.numeric("-9999"))),NA,gw_month$SPCOND))
gw_month$CL <- ifelse((trimws(as.character(gw_month$CL))==trimws("-9999")),NA,gw_month$CL)               
suppressWarnings(gw_month$CL <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(gw_month$CL))==as.character(as.numeric("-9999"))),NA,gw_month$CL))
gw_month$SO4 <- ifelse((trimws(as.character(gw_month$SO4))==trimws("-9999")),NA,gw_month$SO4)               
suppressWarnings(gw_month$SO4 <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(gw_month$SO4))==as.character(as.numeric("-9999"))),NA,gw_month$SO4))
gw_month$CA <- ifelse((trimws(as.character(gw_month$CA))==trimws("-9999")),NA,gw_month$CA)               
suppressWarnings(gw_month$CA <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(gw_month$CA))==as.character(as.numeric("-9999"))),NA,gw_month$CA))
gw_month$K <- ifelse((trimws(as.character(gw_month$K))==trimws("-9999")),NA,gw_month$K)               
suppressWarnings(gw_month$K <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(gw_month$K))==as.character(as.numeric("-9999"))),NA,gw_month$K))
gw_month$MG <- ifelse((trimws(as.character(gw_month$MG))==trimws("-9999")),NA,gw_month$MG)               
suppressWarnings(gw_month$MG <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(gw_month$MG))==as.character(as.numeric("-9999"))),NA,gw_month$MG))
gw_month$Na <- ifelse((trimws(as.character(gw_month$Na))==trimws("-9999")),NA,gw_month$Na)               
suppressWarnings(gw_month$Na <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(gw_month$Na))==as.character(as.numeric("-9999"))),NA,gw_month$Na))
gw_month$AL <- ifelse((trimws(as.character(gw_month$AL))==trimws("-9999")),NA,gw_month$AL)               
suppressWarnings(gw_month$AL <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(gw_month$AL))==as.character(as.numeric("-9999"))),NA,gw_month$AL))
gw_month$MN <- ifelse((trimws(as.character(gw_month$MN))==trimws("-9999")),NA,gw_month$MN)               
suppressWarnings(gw_month$MN <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(gw_month$MN))==as.character(as.numeric("-9999"))),NA,gw_month$MN))
gw_month$SI <- ifelse((trimws(as.character(gw_month$SI))==trimws("-9999")),NA,gw_month$SI)               
suppressWarnings(gw_month$SI <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(gw_month$SI))==as.character(as.numeric("-9999"))),NA,gw_month$SI))
gw_month$SR <- ifelse((trimws(as.character(gw_month$SR))==trimws("-9999")),NA,gw_month$SR)               
suppressWarnings(gw_month$SR <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(gw_month$SR))==as.character(as.numeric("-9999"))),NA,gw_month$SR))
gw_month$NH4 <- ifelse((trimws(as.character(gw_month$NH4))==trimws("-9999")),NA,gw_month$NH4)               
suppressWarnings(gw_month$NH4 <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(gw_month$NH4))==as.character(as.numeric("-9999"))),NA,gw_month$NH4))
gw_month$NO3 <- ifelse((trimws(as.character(gw_month$NO3))==trimws("-9999")),NA,gw_month$NO3)               
suppressWarnings(gw_month$NO3 <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(gw_month$NO3))==as.character(as.numeric("-9999"))),NA,gw_month$NO3))
gw_month$SRP <- ifelse((trimws(as.character(gw_month$SRP))==trimws("-9999")),NA,gw_month$SRP)               
suppressWarnings(gw_month$SRP <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(gw_month$SRP))==as.character(as.numeric("-9999"))),NA,gw_month$SRP))
gw_month$TN <- ifelse((trimws(as.character(gw_month$TN))==trimws("-9999")),NA,gw_month$TN)               
suppressWarnings(gw_month$TN <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(gw_month$TN))==as.character(as.numeric("-9999"))),NA,gw_month$TN))
gw_month$TP <- ifelse((trimws(as.character(gw_month$TP))==trimws("-9999")),NA,gw_month$TP)               
suppressWarnings(gw_month$TP <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(gw_month$TP))==as.character(as.numeric("-9999"))),NA,gw_month$TP))
gw_month$TOC <- ifelse((trimws(as.character(gw_month$TOC))==trimws("-9999")),NA,gw_month$TOC)               
suppressWarnings(gw_month$TOC <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(gw_month$TOC))==as.character(as.numeric("-9999"))),NA,gw_month$TOC))
gw_month$O18 <- ifelse((trimws(as.character(gw_month$O18))==trimws("-9999")),NA,gw_month$O18)               
suppressWarnings(gw_month$O18 <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(gw_month$O18))==as.character(as.numeric("-9999"))),NA,gw_month$O18))
gw_month$D <- ifelse((trimws(as.character(gw_month$D))==trimws("-9999")),NA,gw_month$D)               
suppressWarnings(gw_month$D <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(gw_month$D))==as.character(as.numeric("-9999"))),NA,gw_month$D))


# Here is the structure of the input data frame:
str(gw_month)                            
attach(gw_month)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(LAB_ID)
summary(NAME)
summary(DateTime)
summary(PH)
summary(SPCOND)
summary(CL)
summary(SO4)
summary(CA)
summary(K)
summary(MG)
summary(Na)
summary(AL)
summary(MN)
summary(SI)
summary(SR)
summary(NH4)
summary(NO3)
summary(SRP)
summary(TN)
summary(TP)
summary(TOC)
summary(O18)
summary(D) 
# Get more details on character variables

summary(as.factor(gw_month$LAB_ID)) 
summary(as.factor(gw_month$NAME))
detach(gw_month)  



##########################################################################
# Package ID: edi.712.2 Cataloging System:https://pasta.edirepository.org.
# Data set title: Marcell Experimental Forest porewater chemistry at the S2 catchment, 2009 - ongoing.
# Data set creator:  Stephen Sebestyen - USDA Forest Service, Northern Research Station 
# Data set creator:  Nina Lany - USDA Forest Service, Northern Research Station 
# Data set creator:  John Larson - USDA Forest Service, Northern Research Station 
# Data set creator:  Nathan Aspelin - USDA Forest Service, Northern Research Station 
# Data set creator:  Keith Oleheiser - XCEL Engineering 
# Data set creator:  Doris Nelson - USDA Forest Service, Northern Research Station 
# Data set creator:  Steven Hall - Iowa State University 
# Data set creator:  Holly Curtinrich - Iowa State University 
# Contact:    -  Data Manager, Marcell Experimental Forest  - nina.lany@usda.gov
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/712/2/6108c9dac7d837a4955f233eb4467ff2" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "LAB_ID",     
                 "NAME",     
                 "PIEZOMETER",     
                 "DEPTH",     
                 "DateTime",     
                 "PH",     
                 "SPCOND",     
                 "CL",     
                 "SO4",     
                 "CA",     
                 "K",     
                 "MG",     
                 "Na",     
                 "AL",     
                 "FE",     
                 "MN",     
                 "SI",     
                 "SR",     
                 "NH4",     
                 "NO3",     
                 "SRP",     
                 "TN",     
                 "TP",     
                 "TOC"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$LAB_ID)!="factor") dt1$LAB_ID<- as.factor(dt1$LAB_ID)
if (class(dt1$NAME)!="factor") dt1$NAME<- as.factor(dt1$NAME)
if (class(dt1$PIEZOMETER)!="factor") dt1$PIEZOMETER<- as.factor(dt1$PIEZOMETER)
if (class(dt1$DEPTH)=="factor") dt1$DEPTH <-as.numeric(levels(dt1$DEPTH))[as.integer(dt1$DEPTH) ]               
if (class(dt1$DEPTH)=="character") dt1$DEPTH <-as.numeric(dt1$DEPTH)                                   
# attempting to convert dt1$DateTime dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp1DateTime<-as.POSIXct(dt1$DateTime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1DateTime) == length(tmp1DateTime[!is.na(tmp1DateTime)])){dt1$DateTime <- tmp1DateTime } else {print("Date conversion failed for dt1$DateTime. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1DateTime) 
if (class(dt1$PH)=="factor") dt1$PH <-as.numeric(levels(dt1$PH))[as.integer(dt1$PH) ]               
if (class(dt1$PH)=="character") dt1$PH <-as.numeric(dt1$PH)
if (class(dt1$SPCOND)=="factor") dt1$SPCOND <-as.numeric(levels(dt1$SPCOND))[as.integer(dt1$SPCOND) ]               
if (class(dt1$SPCOND)=="character") dt1$SPCOND <-as.numeric(dt1$SPCOND)
if (class(dt1$CL)=="factor") dt1$CL <-as.numeric(levels(dt1$CL))[as.integer(dt1$CL) ]               
if (class(dt1$CL)=="character") dt1$CL <-as.numeric(dt1$CL)
if (class(dt1$SO4)=="factor") dt1$SO4 <-as.numeric(levels(dt1$SO4))[as.integer(dt1$SO4) ]               
if (class(dt1$SO4)=="character") dt1$SO4 <-as.numeric(dt1$SO4)
if (class(dt1$CA)=="factor") dt1$CA <-as.numeric(levels(dt1$CA))[as.integer(dt1$CA) ]               
if (class(dt1$CA)=="character") dt1$CA <-as.numeric(dt1$CA)
if (class(dt1$K)=="factor") dt1$K <-as.numeric(levels(dt1$K))[as.integer(dt1$K) ]               
if (class(dt1$K)=="character") dt1$K <-as.numeric(dt1$K)
if (class(dt1$MG)=="factor") dt1$MG <-as.numeric(levels(dt1$MG))[as.integer(dt1$MG) ]               
if (class(dt1$MG)=="character") dt1$MG <-as.numeric(dt1$MG)
if (class(dt1$Na)=="factor") dt1$Na <-as.numeric(levels(dt1$Na))[as.integer(dt1$Na) ]               
if (class(dt1$Na)=="character") dt1$Na <-as.numeric(dt1$Na)
if (class(dt1$AL)=="factor") dt1$AL <-as.numeric(levels(dt1$AL))[as.integer(dt1$AL) ]               
if (class(dt1$AL)=="character") dt1$AL <-as.numeric(dt1$AL)
if (class(dt1$FE)=="factor") dt1$FE <-as.numeric(levels(dt1$FE))[as.integer(dt1$FE) ]               
if (class(dt1$FE)=="character") dt1$FE <-as.numeric(dt1$FE)
if (class(dt1$MN)=="factor") dt1$MN <-as.numeric(levels(dt1$MN))[as.integer(dt1$MN) ]               
if (class(dt1$MN)=="character") dt1$MN <-as.numeric(dt1$MN)
if (class(dt1$SI)=="factor") dt1$SI <-as.numeric(levels(dt1$SI))[as.integer(dt1$SI) ]               
if (class(dt1$SI)=="character") dt1$SI <-as.numeric(dt1$SI)
if (class(dt1$SR)=="factor") dt1$SR <-as.numeric(levels(dt1$SR))[as.integer(dt1$SR) ]               
if (class(dt1$SR)=="character") dt1$SR <-as.numeric(dt1$SR)
if (class(dt1$NH4)=="factor") dt1$NH4 <-as.numeric(levels(dt1$NH4))[as.integer(dt1$NH4) ]               
if (class(dt1$NH4)=="character") dt1$NH4 <-as.numeric(dt1$NH4)
if (class(dt1$NO3)=="factor") dt1$NO3 <-as.numeric(levels(dt1$NO3))[as.integer(dt1$NO3) ]               
if (class(dt1$NO3)=="character") dt1$NO3 <-as.numeric(dt1$NO3)
if (class(dt1$SRP)=="factor") dt1$SRP <-as.numeric(levels(dt1$SRP))[as.integer(dt1$SRP) ]               
if (class(dt1$SRP)=="character") dt1$SRP <-as.numeric(dt1$SRP)
if (class(dt1$TN)=="factor") dt1$TN <-as.numeric(levels(dt1$TN))[as.integer(dt1$TN) ]               
if (class(dt1$TN)=="character") dt1$TN <-as.numeric(dt1$TN)
if (class(dt1$TP)=="factor") dt1$TP <-as.numeric(levels(dt1$TP))[as.integer(dt1$TP) ]               
if (class(dt1$TP)=="character") dt1$TP <-as.numeric(dt1$TP)
if (class(dt1$TOC)=="factor") dt1$TOC <-as.numeric(levels(dt1$TOC))[as.integer(dt1$TOC) ]               
if (class(dt1$TOC)=="character") dt1$TOC <-as.numeric(dt1$TOC)

# Convert Missing Values to NA for non-dates

dt1$PH <- ifelse((trimws(as.character(dt1$PH))==trimws("-9999")),NA,dt1$PH)               
suppressWarnings(dt1$PH <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$PH))==as.character(as.numeric("-9999"))),NA,dt1$PH))
dt1$SPCOND <- ifelse((trimws(as.character(dt1$SPCOND))==trimws("-9999")),NA,dt1$SPCOND)               
suppressWarnings(dt1$SPCOND <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$SPCOND))==as.character(as.numeric("-9999"))),NA,dt1$SPCOND))
dt1$CL <- ifelse((trimws(as.character(dt1$CL))==trimws("-9999")),NA,dt1$CL)               
suppressWarnings(dt1$CL <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$CL))==as.character(as.numeric("-9999"))),NA,dt1$CL))
dt1$SO4 <- ifelse((trimws(as.character(dt1$SO4))==trimws("-9999")),NA,dt1$SO4)               
suppressWarnings(dt1$SO4 <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$SO4))==as.character(as.numeric("-9999"))),NA,dt1$SO4))
dt1$CA <- ifelse((trimws(as.character(dt1$CA))==trimws("-9999")),NA,dt1$CA)               
suppressWarnings(dt1$CA <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$CA))==as.character(as.numeric("-9999"))),NA,dt1$CA))
dt1$K <- ifelse((trimws(as.character(dt1$K))==trimws("-9999")),NA,dt1$K)               
suppressWarnings(dt1$K <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$K))==as.character(as.numeric("-9999"))),NA,dt1$K))
dt1$MG <- ifelse((trimws(as.character(dt1$MG))==trimws("-9999")),NA,dt1$MG)               
suppressWarnings(dt1$MG <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$MG))==as.character(as.numeric("-9999"))),NA,dt1$MG))
dt1$Na <- ifelse((trimws(as.character(dt1$Na))==trimws("-9999")),NA,dt1$Na)               
suppressWarnings(dt1$Na <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$Na))==as.character(as.numeric("-9999"))),NA,dt1$Na))
dt1$AL <- ifelse((trimws(as.character(dt1$AL))==trimws("-9999")),NA,dt1$AL)               
suppressWarnings(dt1$AL <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$AL))==as.character(as.numeric("-9999"))),NA,dt1$AL))
dt1$FE <- ifelse((trimws(as.character(dt1$FE))==trimws("-9999")),NA,dt1$FE)               
suppressWarnings(dt1$FE <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$FE))==as.character(as.numeric("-9999"))),NA,dt1$FE))
dt1$MN <- ifelse((trimws(as.character(dt1$MN))==trimws("-9999")),NA,dt1$MN)               
suppressWarnings(dt1$MN <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$MN))==as.character(as.numeric("-9999"))),NA,dt1$MN))
dt1$SI <- ifelse((trimws(as.character(dt1$SI))==trimws("-9999")),NA,dt1$SI)               
suppressWarnings(dt1$SI <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$SI))==as.character(as.numeric("-9999"))),NA,dt1$SI))
dt1$SR <- ifelse((trimws(as.character(dt1$SR))==trimws("-9999")),NA,dt1$SR)               
suppressWarnings(dt1$SR <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$SR))==as.character(as.numeric("-9999"))),NA,dt1$SR))
dt1$NH4 <- ifelse((trimws(as.character(dt1$NH4))==trimws("-9999")),NA,dt1$NH4)               
suppressWarnings(dt1$NH4 <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$NH4))==as.character(as.numeric("-9999"))),NA,dt1$NH4))
dt1$NO3 <- ifelse((trimws(as.character(dt1$NO3))==trimws("-9999")),NA,dt1$NO3)               
suppressWarnings(dt1$NO3 <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$NO3))==as.character(as.numeric("-9999"))),NA,dt1$NO3))
dt1$SRP <- ifelse((trimws(as.character(dt1$SRP))==trimws("-9999")),NA,dt1$SRP)               
suppressWarnings(dt1$SRP <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$SRP))==as.character(as.numeric("-9999"))),NA,dt1$SRP))
dt1$TN <- ifelse((trimws(as.character(dt1$TN))==trimws("-9999")),NA,dt1$TN)               
suppressWarnings(dt1$TN <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$TN))==as.character(as.numeric("-9999"))),NA,dt1$TN))
dt1$TP <- ifelse((trimws(as.character(dt1$TP))==trimws("-9999")),NA,dt1$TP)               
suppressWarnings(dt1$TP <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$TP))==as.character(as.numeric("-9999"))),NA,dt1$TP))
dt1$TOC <- ifelse((trimws(as.character(dt1$TOC))==trimws("-9999")),NA,dt1$TOC)               
suppressWarnings(dt1$TOC <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$TOC))==as.character(as.numeric("-9999"))),NA,dt1$TOC))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(LAB_ID)
summary(NAME)
summary(PIEZOMETER)
summary(DEPTH)
summary(DateTime)
summary(PH)
summary(SPCOND)
summary(CL)
summary(SO4)
summary(CA)
summary(K)
summary(MG)
summary(Na)
summary(AL)
summary(FE)
summary(MN)
summary(SI)
summary(SR)
summary(NH4)
summary(NO3)
summary(SRP)
summary(TN)
summary(TP)
summary(TOC) 
# Get more details on character variables

summary(as.factor(dt1$LAB_ID)) 
summary(as.factor(dt1$NAME)) 
summary(as.factor(dt1$PIEZOMETER))
detach(dt1)               
bog_porewater_month<-dt1
rm(dt1)
##################################################################
rm(bog_pore_month)
rm(bog_pore_synop)
rm(lagg_pore_synop)
rm(auto)