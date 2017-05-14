###################################################################################
# ARM/CDMA PROGRAM TECHNICAL REVIEW
# Reviewed by:  David Dornisch
# Date: 7/21/16
###################################################################################
# DM#1286481
# Job Code: 100329
# Job Name: FOIA Litigation Costs
# Program Name: FOIA Litigation Cases with decisions rendered cleaning and removing duplicates
# Prepared by: Andrew Banister
# Last updated: 7/12/16
#
#
# Purpose:
#   To obtain a table of duplicates and the number of original cases.
#   Duplicate cases are not identical across the row, but they should have
#   identical case numbers and districts.
#
# Input file: 09-14 FOIA Cases.csv
#
# Output file: duplicate cases.csv
###################################################################################

cases<-read.csv("09-14 FOIA Cases.csv", as.is=TRUE, strip.white=TRUE)

# Translate incorrectly labeled case numbers recorded as the excel autocorrect 'date' format
# (ex: 09/1/1959) into the real case number equivalent (ex: 09-1959)
# The /1/ is replaced with a -. Also, replace case numbers that have -C- or CR to a -
cases[,"Case.Number"] <- gsub("/1/|-C-|CR","-",cases[,"Case.Number"])

##################################################################
#### Cleanup Districts to similar format
##################################################################

districtlookups=read.csv("District Lookups.csv",as.is= TRUE, header = TRUE)
library(DataCombine)
cases2 <- FindReplace(data = cases, Var = "Court", replaceData = districtlookups,
                      from = "Lookup", "Court", exact = TRUE)

##################################################################
### Manually fix incorrect data. (used case dockets from foiaproject.org)
##################################################################

#No Case Number
#https://www.cadc.uscourts.gov/internet/opinions.nsf/AEB7E0F7C6B73AA985257D900053E7DB/$file/13-5137-1522260.pdf
cases2[360,"Case.Number"] = "13-5137"
#Judge Initials from Case number RJL
cases2[1687,"Case.Number"] = "09-01020"
#MONAGHAN, AIDAN V. DOJ; FBI Civil Action No. 09-005608 (D. D.C.)
#http://foiaproject.org/case_detail/?case_no=2:2009cv02199&distcode=NV&style=foia&title=on
cases2[1793,"Case.Number"] = "09-2199"
#TCHEFUNCTA CLUB ESTATES INC. V. U.S. ARMY CORPS OF ENGINEERS Civil Action No. 10-016371 (E.D. L.A.)
#http://foiaproject.org/case_detail/?case_no=2:2010cv01637&distcode=LAE&style=foia&title=on
cases2[1462,"Case.Number"] = "10-01637"

##################################################################
#### Separate cases with  multiple case numbers into separate columns
##################################################################

cases3<-cases2
cases3[,"Case.Number"] <-gsub(";|and|&", ",",cases3[,"Case.Number"])
library(tidyr)
cases4 <- separate(cases3, Case.Number, c("c1","c2","c3","c4","c5"), ",",
                   remove=TRUE, convert=FALSE, extra="warn", fill="right")

# Create a separate data fram with just the Case numbers
c1c5 <- cases4[c("c1","c2","c3","c4","c5")]

# There are 3 DC cases with two case numbers and the second case number is in a different format:
# These case numbers are not duplicates so I removed them and kept their state case numbers.
# Row 1359 635 F. 3d 1160
c1c5[1359,"c2"] = ""
# Row 1443 642 F. 3d 1161
c1c5[1443,"c2"] = ""
# Row 1478 662 F.3d 1240
c1c5[1478,"c2"] = ""


# Trim White Space and Check that number of characters are all less than or equal to 8
c1c5 <- apply(c1c5,2,function(a){trimws(a, which ="both")})
trimcheck <- apply(c1c5,2,function(a){nchar(a,type = "chars", allowNA = FALSE, keepNA = TRUE)})
all(trimcheck<=8, na.rm = TRUE)

##################################################################
### Change all Case numbers into format XX-XXXXX
##################################################################

# Separate case numbers into year and number matrix. Make them numeric for easier data transformation.
c1c5.year <- gsub("-.*","",c1c5)
c1c5.year <- apply(c1c5.year,2,as.numeric)
c1c5.num <- gsub(".*-","",c1c5)
c1c5.num <- apply(c1c5.num,2,as.numeric)


# Use formatC to add leading zeros ex. 7 to 07 and 559 to 00559
# then remove NAs that are introduced
c1c5.zero <- formatC(c1c5.num, width = 5, format = "d", flag = "0")
c1c5.zero[c1c5.zero=="   NA"] <- ""
c1c5.year <- formatC(c1c5.year, width = 2, format = "d", flag = "0")
c1c5.year[c1c5.year=="NA"] <- ""


# Combine years with the new adjusted leading zero case numbers
c1c5.final <- c1c5
for (j in 1:5){
  c1c5.final[,j]=paste(c1c5.year[,j],c1c5.zero[,j],sep="-")
}
c1c5.final[c1c5.final=="-"] <- ""

# Check number of characters are all equal to 8
charcheck <- apply(c1c5.final,2,function(a){nchar(a,type = "chars", allowNA = FALSE, keepNA = TRUE)})
all((charcheck==8|charcheck==0),na.rm = TRUE)

##################################################################
### Move duplicate cases with equal case numbers into separate table
##################################################################

cases5 <- cases4
cases5[2:6] <- c1c5.final

# Create a single vector containting all case numbers including the cases with multiple case numbers.
# This takes the previous 5 columns and makes one list for easier data manipulation.
# Also remove the blank or "" values that were introduced from the empty columns.
all.case.numbers <- unlist(list(c1c5.final))
all.case.numbers <- all.case.numbers[!is.na(all.case.numbers)]
all.case.numbers <- all.case.numbers[all.case.numbers != ""]

# Create a vector of all case numbers that are duplicates
unique.duplicates <- unique(all.case.numbers[duplicated(all.case.numbers)])
logic.original.dup<-c1c5.final==0
# Create a subset of the table that has the 5 columns of multiple case numbers.
# This uses the duplicate case numbers previously identified.
# x %in% y finds all y contained in x
for (c in 1:5){
  logic.original.dup[,c] <- c1c5.final[,c] %in% unique.duplicates
}
# Using the logic vector above, select the entrie row for just those cases with duplicate case numbers
duplicate.cases <- cases5[rowSums(logic.original.dup) > 0L,]
row.names(duplicate.cases) <- 1:nrow(duplicate.cases)

# Create a table from duplicate table for cases with different districts (Court)
# and another tabel from duplicates with the same districts (Court)
dif.district <-T
for (q in 1:length(unique.duplicates)){
  same.case <-  duplicate.cases[rowSums(duplicate.cases[,2:6] == unique.duplicates[q]) > 0L,]
  if (all(same.case[1,"Court"]==same.case[,"Court"]) || all(same.case[1,"Case.Name"]==same.case[,"Case.Name"])){
    dif.district[as.numeric(row.names(same.case))]=FALSE
  } 
  if (!all(same.case[1,"Court"]==same.case[,"Court"]) & !all(same.case[1,"Case.Name"]==same.case[,"Case.Name"])){
    dif.district[as.numeric(row.names(same.case))]=TRUE
  } 
}
dif.district.cases <- duplicate.cases[dif.district,]
dif.district.cases <- dif.district.cases[order(dif.district.cases[,"c1"]),]
same.district.cases <- duplicate.cases[!dif.district,]
same.district.cases <- same.district.cases[order(same.district.cases[,"c1"]),]

# check the two tables come distinctly from the duplicate table
nrow(duplicate.cases)==nrow(dif.district.cases)+nrow(same.district.cases)

# Examine the different districts table and manually identify duplicate cases
all.duplicate.cases <- dif.district.cases[c("1","171","179","274","315","220","42","119","224","249",
                                            "250","13","132","64","163","240","333","147","328"),]
dif.district.cases2 <- dif.district.cases[!rownames(dif.district.cases)%in%rownames(all.duplicate.cases),]
all.duplicate.cases[nrow(all.duplicate.cases)+1:nrow(same.district.cases),] <- same.district.cases
all.duplicate.cases <- all.duplicate.cases[order(all.duplicate.cases[,"c1"]),]

# check again the updated two tables come distinctly from the duplicate table
nrow(duplicate.cases)==nrow(dif.district.cases2)+nrow(all.duplicate.cases)

##################################################################
### create table with duplicate cases from earlier year removed and count all cases remaining
##################################################################

all.duplicate.case.nums<-unique(unlist(list(all.duplicate.cases[,2:6])))
all.duplicate.case.nums<-all.duplicate.case.nums[all.duplicate.case.nums!=""]
all.duplicate.cases2<-all.duplicate.cases
all.duplicate.cases2[,"ID"]<-NA

# add IDs to group similar cases across multiple case numbers
for (n in 1:length(all.duplicate.case.nums)){
  logic.dup.group<-rowSums(all.duplicate.cases2[,2:6] == all.duplicate.case.nums[n]) > 0L
  if(all(is.na(all.duplicate.cases2[logic.dup.group,"ID"]))){
    all.duplicate.cases2[logic.dup.group,"ID"] <-  all.duplicate.case.nums[n]
  } 
  else all.duplicate.cases2[logic.dup.group,"ID"] <-  all.duplicate.cases2[logic.dup.group,"ID"][!is.na(unique(all.duplicate.cases2[logic.dup.group,"ID"]))]
}

# only keep most recent year
all.duplicate.case.IDs<-unique(all.duplicate.cases2[,"ID"])
original.dup<-1
for (n in 1:length(all.duplicate.case.IDs)){
  cases.group<-all.duplicate.cases2[all.duplicate.case.IDs[n]==all.duplicate.cases2[,"ID"],]
  original.dup[n]<-row.names(cases.group)[which.max(cases.group[,"Year"])]
}
original.dup.cases<-all.duplicate.cases2[original.dup,]
all.duplicate.cases2[original.dup,"Unique"]<-"x"

# Verify that the new IDs are accurate by checking frequencies.
# Do not want the new ID to only appear once.
library(plyr)
freq.ID<-count(all.duplicate.cases2,'ID')
freq1.ID<-freq.ID[freq.ID[,"freq"]==1,]

# Get final Number of Cases with duplicates removed
nrow(cases5)-(nrow(all.duplicate.cases2)-nrow(original.dup.cases))


# save final product as csv file
write.csv(all.duplicate.cases2,"duplicate cases.csv", na = "", row.names = FALSE)