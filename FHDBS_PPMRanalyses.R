library(tidyverse)

#data source: https://www.fisheries.noaa.gov/inport/item/36902

fhpd<-read.csv("fhpd.csv")
fhpy<-read.csv("fhpy.csv")
fhpyl<-read.csv("fhpyl.csv")
fhspecies<-read.csv("fhsvspecies_list.csv")

pdlen=merge(fhpyl[,c(2,4:7,10:12)], fhpd[,c(2,6:9,23)], by=c("CRUISE6","STATION","SVSPP", "PDSEX","PDID"), all.x=T)


#merge species names

fhspecies<-read.csv("svspecies.csv")
unique(fhspecies$SCINAME)
pdpylen<-merge(fhspecies[,c(1:3)],pdlen,by=c("SVSPP"))

#"AMERICAN PLAICE", "COD(ATLANTIC)", "HERRING(ATLANTIC)","ATLANTIC HALIBUT","REDFISH UNSEPARATED", "TURBOT, GREENLAND HALIBUT", "HADDOCK","SANDLANCES", "SILVER HAKE", "SPINY DOGFISH", "WHITE HAKE", "WITCH FLOUNDER", "YELLOWTAIL FLOUNDER".
scinames<-c("HIPPOGLOSSOIDES PLATESSOIDES",
            "GADUS MORHUA",
            "CLUPEA HARENGUS",
            "HIPPOGLOSSUS HIPPOGLOSSUS",
            "SEBASTES FASCIATUS",
            "REINHARDTIUS HIPPOGLOSSOIDES",
            "MELANOGRAMMUS AEGLEFINUS",
            "AMMODYTES AMERICANUS",
            "MERLUCCIUS BILINEARIS",
            "SQUALUS ACANTHIAS",
            "UROPHYCIS TENUIS",
            "GLYPTOCEPHALUS CYNOGLOSSUS",
            "MYZOPSETTA FERRUGINEA"
            )


sub_pdpylen<-rbind(filter(pdpylen,SCINAME==scinames),
                   filter(pdpylen,grepl('AMMODYTES', SCINAME, ignore.case = TRUE)),
                   filter(pdpylen,grepl('SEBASTES', SCINAME, ignore.case = TRUE)),
                   filter(pdpylen,grepl('LIMANDA', SCINAME, ignore.case = TRUE))
                   )
unique(sub_pdpylen$SCINAME)


#convert prey length to cm
sub_pdpylen$PYLEN <-sub_pdpylen$PYLEN/100

#save subset file
write.csv(sub_pdpylen,"predpreylengths.csv")

#convert lengths to weights

# calculate PPMR for each obs

# check plot

ggplot(sub_pdpylen,aes(log10(PDLEN),log10(PYLEN)))+
  facet_wrap(~SCINAME) +
  geom_point() +
  # stat_summary(fun.data=mean_cl_normal) +
  geom_smooth(method = lm, se = F) +
  theme_minimal()
  
# make a histogram of PPMR by species

