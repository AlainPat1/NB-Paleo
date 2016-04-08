# From Alain to Moumita 2015-11-10

# Alain's load parameters from Win8 office machine
epath = file.path("C:/Users/alain/Documents")

# Alain's load parameters from Kubuntu home machine
epath =file.path("/media/alain/2d6cfd0e-c558-4a1f-bf21-e9e9ec13b2a7/home/alain/UMCS_bak/Documents")

# Check path:
epath

# Load Maltampec file (first file sent)
M=read.delim2(paste(epath,"/SUPERVISION/Moumita_Karmakar_2015/Maltampec_pigs.txt",sep=""),header=TRUE)
names(M)



# Concentrations in file "Maltampec_pigs.txt" are in ug/mL
# Notice that depth 7 cm has been analysed twice.

# Get molar mass of pigments (g/mol)

epath = file.path("C:/Users/Moumita")
epath
M=read.delim2(paste(epath,"/Post Doc at Shipgaan/First set data/Maltampec_pigs.txt",sep=""),header=TRUE)
names(M)

FucoMM=658.91

# Compute values in nmol/g OM

M$Fuco2=(M$Fuco/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/FucoMM)
summary(M$Fuco2)
M$Fuco2
edit(M)

# 2015-11-22 ######################################
# Load "all pigments files" sent 2015-11-
allpigs=read.delim(paste(epath,"/RECHERCHE_Labos_GIZC/_Analyses_redaction/Moumita/Pigments_analyses_to_Moumita.txt",sep=""),header=TRUE)
dim(allpigs) # 212 rows x 27 columns

# Compute Fuco2 concentration in nmol/g for all records:
FucoMM=658.91
allpigs$Fuco2=(allpigs$Fuco/allpigs$Fraction_injected) / (allpigs$Sediment_dry_mass_g * allpigs$Organic_content_pct) * (1000/FucoMM)
summary(allpigs$Fuco2)

# Identify negative values by sorting Fuco2
allpigs[order(allpigs$Fuco2),c("Station","Fuco","Sediment_dry_mass_g", "Fuco2")]

# Let's make all negative values (probably due to calibration detection limit) equal to zero
allpigs$Fuco2[allpigs$Fuco2<0]=0

# Check result of precedent line of code
allpigs[order(allpigs$Fuco2),c("Station","DateCoreSampled","Fuco", "Fuco2")]

# Check distribution with the "stem" function
stem(allpigs$Fuco2)

# List values of Sediment dry mass used for extraction
allpigs$Sediment_dry_mass_g

# List values Sediment dry mass values that are Not Available
subset(allpigs, is.na(Sediment_dry_mass_g),select=c(Station, MedianDepth_cm, Sediment_dry_mass_g))

# Get the mean value of Sediment dry mass used for extraction
mean(allpigs$Sediment_dry_mass_g, na.rm=T) # Should be 0.47... g

# Replace missing values by the mean
allpigs$Sediment_dry_mass_g[is.na(allpigs$Sediment_dry_mass_g)]=mean(allpigs$Sediment_dry_mass_g,na.rm=T)

# Check that replacement is OK
allpigs$Sediment_dry_mass_g

# Check why we still have NAs for Fuco2
subset(allpigs,is.na(Fuco2),select=c("Station", "MedianDepth_cm", "Sediment_dry_mass_g", "Organic_content_pct",
                                     "Fraction_injected", "Fuco","Fuco2"))

# There are NAs for "Organic_content_pct".
# Get a representative estimate of Organic content for those records with missing values (Tabusintac aval).
subset(allpigs, Station=="Tabusintac_AVAL", select=c(Station, MedianDepth_cm, Organic_content_pct))

# Get the mean Organic content of depths 83, 85, 87 cm (corresponding to rows 208, 209, 210):
allpigs[208:210,"Organic_content_pct"]
mean(allpigs[208:210,"Organic_content_pct"]) # 10.2%

# Attribute mean to NAs
allpigs[,"Organic_content_pct"]
allpigs$Organic_content_pct[is.na(allpigs$Organic_content_pct)]=mean(allpigs[208:210,"Organic_content_pct"])
allpigs[,"Organic_content_pct"]

# Once all NAs are "fixed", recompute Fuco2:
allpigs$Fuco2=(allpigs$Fuco/allpigs$Fraction_injected) / (allpigs$Sediment_dry_mass_g * allpigs$Organic_content_pct) * (1000/FucoMM)

# Save data as csv, if you want
write.csv(allpigs,"allpigscsv.csv")

# If you want to save file elsewhere:
# write.csv(allpigs,"C:/path/to/directory/allpigscsv.csv")

# Save data as RData, if you want...
save(allpigs,file="allpigs.RData")
# Notice how RData files are smaller than equivalent csv files...

# Load a RData file
load("allpigs.RData")
# ... or if needed:
# load("C:/path/to/directory/allpigs.RData")



# Get the levels of factor "Station"
levels(allpigs$Station)

# Create a subset for "Caraquet AVAL
CarUppigs=subset(allpigs,Station=="Caraquet AVAL")
dim(CarUppigs) # 14 x 28
summary(CarUppigs$Fuco2)
summary(CarUppigs$MedianDepth_cm)

# Generate a plot
plot(CarUppigs$MedianDepth_cm~CarUppigs$Fuco2,ylim=rev(c(range(CarUppigs$MedianDepth_cm))),xlim=range(CarUppigs$Fuco2), type="b")
names(allpigs)



TableName[100,"Sediment_dry_mass_g"]=0.3

names(allpigs)
allpigs$Station


plot(M$Median_depth_cm~ M$Fuco2, type="b", ylim=rev(c(0,50)), xlab="Pigment concentration (nmol/g OM)")


PeriMM=630.82
M$Peri2=(M$Peri/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/PeriMM)
summary(M$Peri2)
M$Peri2


AphaMM=

M$Apha2=(M$Apha/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/AphaMM)
summary(M$Apha2)
M$Apha2


MyxoMM=731.01
M$Myxo2=(M$Myxo/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/MyxoMM)
summary(M$Myxo2)
M$Myxo2


AlloxMM=564.84
M$Allox2=(M$Allox/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/AlloxMM)
summary(M$Allox2)
M$Allox2


DiatoxMM=566.86
M$Diatox2=(M$Diatox/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/DiatoxMM)
summary(M$Diatox2)
M$Diatox2


LutZeaMM=568.87
M$LutZea2=(M$LutZea/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/LutZeaMM)
summary(M$LutZea2)
M$LutZea2


CanthMM=564.82
M$Canth2=(M$Canth/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/CanthMM)
summary(M$Canth2)
M$Canth2


ChlbMM=907.49
M$Chlb2=(M$Chlb/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/ChlbMM)
summary(M$Chlb2)
M$Chlb2


EchiMM=550.86
M$Echi2=(M$Echi/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/EchiMM)
summary(M$Echi2)
M$Echi2



ChlaMM=893.51
M$Chla2=(M$Chla/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/ChlaMM)
summary(M$Chla2)
M$Chla2


alphacarotMM=536.87
M$alphacarot2=(M$alphacarot/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/alphacarotMM)
summary(M$alphacarot2)
M$alphacarot2


Beta_caroMM=536.89
M$Beta_caro2=(M$Beta_caro/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/Beta_caroMM)
summary(M$Beta_caro2)
M$Beta_caro2


PheoMM=871.19
M$Pheo2=(M$Pheo/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/PheoMM)
summary(M$Pheo2)
M$Pheo2



png("MaltampecPigments.png")
plot(M$Median_depth_cm~ M$Fuco2, type="b", ylim=rev(c(0,50)), xlim=c(0,400),
     xlab="Pigment concentration (nmol/g OM)", pch="f", col=1)
points(M$Median_depth_cm~ M$Allox2, type="b", ylim=rev(c(0,50)), pch="a", col=2)
points(M$Median_depth_cm~ M$Diatox2, type="b", ylim=rev(c(0,50)), pch="d", col=3)
points(M$Median_depth_cm~ M$LutZea2, type="b", ylim=rev(c(0,50)), pch="l", col=4)
points(M$Median_depth_cm~ M$Canth2, type="b", ylim=rev(c(0,50)), pch="c", col=5)
points(M$Median_depth_cm~ M$Chlb2, type="b", ylim=rev(c(0,50)), pch="b", col=6)
points(M$Median_depth_cm~ M$Echi2, type="b", ylim=rev(c(0,50)), pch="e", col=7)
points(M$Median_depth_cm~ M$Chla2, type="b", ylim=rev(c(0,50)), pch="*", col=8)
points(M$Median_depth_cm~ M$Beta_caro2, type="b", ylim=rev(c(0,50)), pch="B", col=10)
title(main="Maltampec pigments as of 2015-11-17")
dev.off()

help(write.csv)

write.table(x, file = "", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"))

dim(TableName) # to get the dimensions of the data frame (number of rows x columns).
names(TableName) # to get column hadings
TableName[0,] # does the same as “names” and reads as “list zero rows and all colums”
TableName[1:5, c(1,5,9)] # list rows 1 to 5 and columns 1, 5, 9
ls() # to see what “objects” are loaded in memory.

save(M,file="M.RData") # to save table “M” as an R data file
save(M,file=”/path/to/directory/M.RData”) # if you want to save or load elsewhere
load("M.RData") # to load the data file

dim(M)
Msub1=subset(M,select=c(Fuco2, Myxo2, Allox2, Diatox2, Chlb2)) # to select some columns
dim(Msub1)
Msub2=subset(M, Median_depth_cm > 10)  # to select some rows
dim(Msub2)
Msub3=subset(M, Sample=="Malt. 2010 11cm" | Sample=="Malt. 2010 13cm",
    select=(Fuco2:Chla2)) # to select some rows and some columns
dim(Msub3)

Msub4=subset(M,!is.na(Fuco2),select=(Fuco2:Chla2)) # to remove rows that have missing Fuco2 data
cor(Msub4, use="pairwise.complete.obs",method="pearson") # to get a correlation marix

stem(TableName$Variable) # to generate a stem plot and check for the presence of outliers
TableName[order(TableName$SortVariable1, TableName$SortVariable2, decreasing=F),c(“Station”,”Date”,”Variable1”)]
# To get the Station and date info of each Variable1 records, sorted by Variable1, then Variable2


Friday 20th Nov
epath = file.path("C:/Users/Moumita")
epath
M=read.delim2(paste(epath,"/Post Doc at Shipgaan/All pigments/Pigments_analyses_to_Moumita.txt",sep=""),header=TRUE)
names(M)

TableName[100,"Sediment_dry_mass_g"]=0.3

Pigment_analyses_to_Moumita.txt[115,"Sediment_dry_mass_g"]=0.326




