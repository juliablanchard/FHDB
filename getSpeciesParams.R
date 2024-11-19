library(rfishbase)
library(tidyverse)

# list of species names in mizer model
species<-c("Hippoglossoides platessoides",
            "Gadus morhua",
            "Clupea harengus",
            "Hippoglossus hippoglossus",
            "Sebastes fasciatus",
            "Reinhardtius hippoglossoides",
            "Melanogrammus aeglefinus",
            "Ammodytes americanus",
            "Merluccius bilinearis",
            "Squalus acanthias",
            "Urophycis tenuis",
            "Glyptocephalus cynoglossus",
            "Myzopsetta ferruginea")
            
# had to check different names for yellowtail flounder as not one of the below
            #"Pleuronectes ferruginea"
            #"Limanda ferruginea")

#rfishbase::common_to_sci("Yellowtail flounder")

# get max length and convert to max weight (see: https://mizer.course.nov22.sizespectrum.org/build/find-species-parameters.html)

length_weight <- rfishbase::estimate(species) |>
  select("Species", "a", "b")
max_length <- rfishbase::species(species) |>
  select(Species, Length)
max_weight <- full_join(max_length, length_weight) |>
  mutate(w_max = a * Length ^ b)
max_weight


# get maturation length and age, select nearest locality

maturity_tbl <- rfishbase::maturity(species) |>
  select(Species, l_mat = Lm, age_mat = tm, Locality) |>
  na.omit()
maturity<-maturity_tbl |>
  group_by(Species)|>
  summarise(l_mat= median(l_mat),age_mat=median(age_mat)) |> 
 # could get range for these params, to help with param uncertainty  
 # summarise(l_mat_med= median(l_mat),age_mat_med=median(age_mat),l_mat_min= min(l_mat),l_mat_max= max(l_mat),age_mat_min=min(age_mat),age_mat_max=max(age_mat)) %>% 
  ungroup()

#add maturity information to our species parameter data frame and also add a w_mat column.

sp <- max_weight |>
  left_join(maturity, by = c("Species" = "Species")) |>
  mutate(w_mat = a * l_mat ^ b)

# missing maturity for sand lance
ammodytes <- rfishbase::maturity("Ammodytes americanus") |>
  select(Species, l_mat = Lm, age_mat = tm,Locality) 
ammodytes<-median(ammodytes$l_mat)

# convert to weight
sp[1,"l_mat"]<-ammodytes
sp[1,"w_mat"]<-sp[1,"a"] * sp[1,"l_mat"] ^ sp[1,"b"]


# get preferred PPMR, beta and sigma

ppmr<-read.csv("predpreylengths.csv")


sp$SCINAME<-c("AMMODYTES DUBIUS",
           "CLUPEA HARENGUS",
           "GADUS MORHUA",
           "GLYPTOCEPHALUS CYNOGLOSSUS",
           "HIPPOGLOSSOIDES PLATESSOIDES",
           "HIPPOGLOSSUS HIPPOGLOSSUS",
           "MELANOGRAMMUS AEGLEFINUS",
           "MERLUCCIUS BILINEARIS",
           "LIMANDA FERRUGINEA",
           "REINHARDTIUS HIPPOGLOSSOIDES",
           "SEBASTES FASCIATUS",
           "SQUALUS ACANTHIAS",
           "UROPHYCIS TENUIS")

ppmr<-merge(ppmr,sp[,c("a","b")])

ppmr<-ppmr |>
  mutate(prey_wght=0.01*PYLEN^3,pred_wght=a*PDLEN^b) |>
  mutate(ppmr=pred_wght/prey_wght)


ppmr<-ppmr|>
  filter(PDLEN<900) |>
  filter(PYNAM!="EMPTY")

# Check histograms of pred lengths
p1 <- ppmr %>%
  ggplot(aes(x=PDLEN)) +
  geom_histogram(color="#e9ecef", alpha=0.6, position = 'identity') +
  facet_wrap(~SCINAME,scales = "free")+
  theme_minimal() 

# Check histograms of ppmr on log-scale
p2 <- ppmr %>%
  ggplot(aes(x=log10(ppmr))) +
  geom_histogram(color="#e9ecef", alpha=0.6, position = 'identity') +
  facet_wrap(~SCINAME,scales = "free")+
  theme_minimal() 

# Check log10 pred vs log10 prey size plot

p3<-ggplot(ppmr,aes(log10(pred_wght),log10(prey_wght)))+
  facet_wrap(~SCINAME) +
  geom_point() +
  # stat_summary(fun.data=mean_cl_normal) +
  geom_smooth(method = lm, se = F) +
  theme_minimal()


betasig<-ppmr|>
  group_by(SCINAME)|>
  summarise(beta=median(ppmr,na.rm=T),sigma=sd(log10(ppmr),na.rm=T),min_ppmr=min(ppmr,na.rm=T),max_ppmr=max(ppmr,na.rm=T)) |>
  ungroup()


#Hartvig et al. 2011 ; preferred (roughly) should be realised ppmr divided by 2

#betasig$beta_adj<-betasig$beta/(exp((2-2*(2/3)+0.8)/betasig$sigma^2))
betasig$beta_adj<-betasig$beta/2
betasig$min_ppmr_adj<-betasig$min_ppmr/2

# I think min_ppmr_adj or min_ppmr probably best reflection of "preferred" ppmr; else could use a box predation kernel

sp<-merge(sp,betasig)

# w_min - assume 0.001 teleosts, spiny dogfish 26 cm l_min (fishbase, reproduction)

sp$w_min<-0.001
sp[12,"w_min"]<-sp[12,"a"]*26^sp[12,"b"]

# save file

write.csv(sp, "ESS_species_params.csv")
