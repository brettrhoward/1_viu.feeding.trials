rm(list=ls())
setwd("../1_viu.feeding.trials")
# setwd("~/Academic Projects/CFRA 2021 - Mesocosms/1_viu.feeding.trials")

source("../../../../EGC Models CSAS/ggplot-custom-theme.R")

library(plyr)
library(lubridate)
library(fishualize)
library(tidyverse)
library(reshape)


# Load data 
# datafile1 <- "~/Academic Projects/CFRA 2021 - Mesocosms/1_viu.feeding.trials/data/individual_crab_data.csv"
# datafile2 <- "~/Academic Projects/CFRA 2021 - Mesocosms/1_viu.feeding.trials/data/hsc_trials.csv"
# datafile3 <- "~/Academic Projects/CFRA 2021 - Mesocosms/1_viu.feeding.trials/data/hermit_trials.csv"
# datafile4 <- "~/Academic Projects/CFRA 2021 - Mesocosms/1_viu.feeding.trials/data/dire_trials.csv"
# datafile5 <- "~/Academic Projects/CFRA 2021 - Mesocosms/1_viu.feeding.trials/data/littleneck_trials.csv"
# datafile6 <- "~/Academic Projects/CFRA 2021 - Mesocosms/1_viu.feeding.trials/data/mussel_trials.csv"
# datafile7 <- "~/Academic Projects/CFRA 2021 - Mesocosms/1_viu.feeding.trials/data/psc_trials.csv"
# datafile8 <- "~/Academic Projects/CFRA 2021 - Mesocosms/1_viu.feeding.trials/data/varnish_trials.csv"

datafile1 <- "../data/individual_crab_data.csv"
datafile2 <- "../data/hsc_trials.csv"
datafile3 <- "../data/hermit_trials.csv"
datafile4 <- "../data/dire_trials.csv"
datafile5 <- "../data/littleneck_trials.csv"
datafile6 <- "../data/mussel_trials.csv"
datafile7 <- "../data/psc_trials.csv"
datafile8 <- "../data/varnish_trials.csv"

crabs <- read.csv(datafile1, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
hsc <- read.csv(datafile2, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
hermit <- read.csv(datafile3, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
dire <- read.csv(datafile4, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
littleneck <- read.csv(datafile5, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
mussel <- read.csv(datafile6, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
psc <- read.csv(datafile7, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
varnish <- read.csv(datafile8, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)

prey <- bind_rows(hsc, hermit, dire, littleneck, mussel, psc, varnish) %>% left_join(crabs, by = "crab.no") %>% mutate(date.run = dmy(date.run)) %>% select(c(date.run, trial.no, crab.no, spp, spp.size, prey, bin.no, num.fully.eaten, num.attacked, sum.weight.consumed, sex, cw, lc, rc, wgt)) %>% mutate(row.id = row_number()) %>% filter(trial.no != "F-7") %>% filter(!row.id %in% c(279, 334))

prey<-prey %>% mutate(prey_type=case_when(prey %in% c("hairy.shore.crab","purple.shore.crab")~"shore.crab",
                                          prey %in% c("dire.whelk")~"gastropod",
                                          prey %in% c("hermit.crab")~"hermit.crab",
                                          prey %in% c("japanese.littleneck","varnish.clam","mussels")~"bivalves",
                                          TRUE~prey)) 
prey<-prey %>% filter(!is.na(spp.size))

prey<-prey %>% mutate(eaten_yn=case_when(num.fully.eaten>0~1,TRUE~0),attacked_yn=case_when(num.attacked>0~1,TRUE~0)) %>% 
  mutate(desc=case_when(eaten_yn==1&attacked_yn==1~"attacked and eaten",eaten_yn==0&attacked_yn==0~"no activity",eaten_yn==1&attacked_yn==0~"eaten, no extra attack",eaten_yn==0&attacked_yn==1~"attacked not eaten"),
         result_exclusive=case_when(desc%in% c("attacked and eaten","eaten, no extra attack")~"eaten",desc%in% c("attacked, not eaten")~"attack with no eating",TRUE~desc))
table(prey$result_exclusive)

table(prey$eaten_yn,prey$attacked_yn)

# Some crabs were fed that same prey twice in error. Prey order wasn't altered for F6 and F7, so dropped F7 completely. GRA-Sm11 was accidentally moved from Group C to Group D later in experiment and saw the same prey, so removed later trials (by row number). 


# table of crab by prey

a<-ddply(prey, c("crab.no","spp.size","prey_type"),summarize,nCrabs=n_distinct(crab.no))
a<-cast(a,prey_type~spp.size)

# check - how many species was each individual crab offered?
crab.summary.ind<-prey %>% group_by(crab.no, spp, spp.size) %>% 
  summarize(nPreyTypes_offered=n_distinct(prey_type),
            nPreyTypes_eaten=n_distinct(prey_type[result_exclusive=="eaten"]),
            nPreyTypes_attacked=n_distinct(prey_type[result_exclusive=="attacked not eaten"]),
            nPrey_offered=n_distinct(prey), 
            nPrey_eaten=n_distinct(prey[result_exclusive=="eaten"]),
            nPrey_attacked=n_distinct(prey[result_exclusive=="attacked not eaten"]),
            pType_offered_eaten=nPreyTypes_eaten/nPreyTypes_offered)

b<-ddply(crab.summary.ind,c("nPrey_offered","spp.size"),summarize,nCrabs=n_distinct(crab.no))
b<-cast(b,nPrey_offered~spp.size)
b


crab.summary.ind.all.type<-crab.summary.ind %>% filter(nPreyTypes_offered==4)
crab.summary.ind.all.species<-crab.summary.ind %>% filter(nPrey_offered==7)

# Count number of crabs - limited to 4 prey types
# by crab species/size
dt<-data.frame(table(crab.summary.ind.all.type$spp.size))
names(dt)<-c('spp.size',"nCrab_tested")
# by crab species
dt2<-data.frame(table(crab.summary.ind.all.type$spp))
names(dt2)<-c('spp',"nCrab_tested")

# limited to 7 prey species
dt3<-data.frame(table(crab.summary.ind.all.species$spp))
names(dt3)<-c('spp',"nCrab_tested")

# summarize number of crabs eating X number of species (have to fill in missing categories)
#  by prey type with crab size
nCrab_byNPreyType_size<-merge(crab.summary.ind.all.type %>% group_by(spp.size,nPreyTypes_eaten) %>% summarize(nCrab=n_distinct(crab.no)) ,dt, by="spp.size",all.x=T)
nCrab_byNPreyType_size<-rbind.data.frame(nCrab_byNPreyType_size,data.frame(spp.size=c("EGC","GRA-Lg","GRA-Lg","GRA-Sm","GRA-Sm"),nPreyTypes_eaten=c(0,3,4,3,4),nCrab=c(0,0,0,0,0),nCrab_tested=c(0,0,0,0,0)))
nCrab_byNPreyType_size<-nCrab_byNPreyType_size %>% mutate(pCrab_eat=nCrab/nCrab_tested)
nCrab_byNPreyType_size$pCrab_eat[is.na(nCrab_byNPreyType_size$pCrab_eat)]<-0

# by prey species with crab species only
nCrab_byNPrey<-merge(crab.summary.ind %>% filter(nPrey_offered>5) %>% group_by(spp,nPrey_eaten) %>% summarize(nCrab=n_distinct(crab.no)) ,dt3, by="spp",all.x=T)
nCrab_byNPrey<-rbind.data.frame(nCrab_byNPrey,data.frame(spp=c("EGC","EGC","GRA","GRA","GRA","GRA","GRA"),nPrey_eaten=c(0,7,2,4,5,6,7),nCrab=c(0,0,0,0,0,0,0),nCrab_tested=c(0,0,0,0,0,0,0)))
nCrab_byNPrey<-nCrab_byNPrey %>% mutate(pCrab_eat=nCrab/nCrab_tested)
nCrab_byNPrey$pCrab_eat[is.na(nCrab_byNPrey$pCrab_eat)]<-0

#  by prey type with crab species only
nCrab_byNPreyType_group<-merge(crab.summary.ind.all %>% group_by(spp,nPreyTypes_eaten) %>% summarize(nCrab=n_distinct(crab.no)) ,dt2, by="spp",all.x=T)
nCrab_byNPreyType_group<-rbind.data.frame(nCrab_byNPreyType_group,data.frame(spp=c("EGC","GRA"),nPreyTypes_eaten=c(0,4),nCrab=c(0,0),nCrab_tested=c(0,0)))
nCrab_byNPreyType_group<-nCrab_byNPreyType_group %>% mutate(pCrab_eat=nCrab/nCrab_tested)
nCrab_byNPreyType_group$pCrab_eat[is.na(nCrab_byNPreyType_group$pCrab_eat)]<-0



png("../figures/pCrab_byNPreyType_size.png",height=10,width=12, units="cm",res=250)
ggplot(nCrab_byNPreyType_size,aes(x=nPreyTypes_eaten, y=pCrab_eat, fill=spp.size))+
  # geom_density(stat="identity",position="dodge",)+
  stat_smooth(data=nCrab_byNPreyType_size,aes(x=nPreyTypes_eaten,y=pCrab_eat,color=spp.size, fill=spp.size), position="dodge",geom="area",alpha=0.5,color="black")+theme_kg()+
  ylab("Proportion of crabs eating")+xlab("Number of prey types (/4)")+scale_fill_discrete(name="")+theme(legend.position = c(0.85,0.85))+scale_y_continuous(expand=c(0,0),limits=c(0,0.75))+scale_x_continuous(expand=c(0,0))
dev.off()

png("../figures/pCrab_byNPrey_species.png",height=10,width=12, units="cm",res=250)
ggplot(nCrab_byNPrey,aes(x=nPrey_eaten, y=pCrab_eat, fill=spp))+
  # geom_density(stat="identity",position="dodge",)+
  stat_smooth(data=nCrab_byNPrey,aes(x=nPrey_eaten,y=pCrab_eat,color=spp, fill=spp), geom="area",alpha=0.5,color="black")+theme_kg()+
  ylab("Proportion of crabs eating")+xlab("Number of prey species(/7)")+scale_fill_discrete(name="")+theme(legend.position = c(0.85,0.85))+scale_y_continuous(expand=c(0,0),limits=c(0,0.4))+scale_x_continuous(expand=c(0,0))
dev.off()

png("../figures/pCrab_byNPreyType_species.png",height=10,width=12, units="cm",res=250)
ggplot(nCrab_byNPreyType_group,aes(x=nPreyTypes_eaten, y=pCrab_eat, fill=spp))+
  # geom_density(stat="identity",position="dodge",)+
  stat_smooth(data=nCrab_byNPreyType_group,aes(x=nPreyTypes_eaten,y=pCrab_eat,color=spp, fill=spp), position="dodge",geom="area",alpha=0.5,color="black")+theme_kg()+
  ylab("Proportion of crabs eating")+xlab("Number of prey types (/4)")+scale_fill_discrete(name="")+theme(legend.position = c(0.85,0.85))+scale_y_continuous(expand=c(0,0),limits=c(0,0.75))+scale_x_continuous(expand=c(0,0))
dev.off()

#  by prey sp
nCrab_byNPrey<-merge(crab.summary.ind.all %>% group_by(spp.size,nPrey_eaten) %>% summarize(nCrab=n_distinct(crab.no)) ,dt, by="spp.size",all.x=T)
nCrab_byNPrey<-nCrab_byNPrey %>% mutate(pCrab_eat=nCrab/nCrab_tested)
nCrab_byNPrey$pCrab_eat[is.na(nCrab_byNPrey$pCrab_eat)]<-0


ggplot(nCrab_byNPrey,aes(x=nPrey_eaten, y=pCrab_eat, fill=spp.size))+
  # geom_bar(stat="identity",position="dodge")  +
  stat_smooth(data=nCrab_byNPrey,aes(x=nPrey_eaten,y=pCrab_eat,color=spp.size),fill=NA)+theme_kg()+ylim(c(0,1))


crab.summary.ind %>% filter(nPreyTypes_offered==4)%>% ungroup() %>% select(spp.size)  %>% table()

table(crab.summary.ind$spp.size,crab.summary.ind$nPreyTypes_offered)

# count of species eaten by # of crabs
crab.summary.ind

# proportion of crabs of each spp that ate each prey

prey.summary<-prey %>% group_by(prey,prey_type,spp.size) %>% summarize(nCrabs_trial=n_distinct(crab.no),
                                               nCrab_attack=n_distinct(crab.no[result_exclusive=="attacked not eaten"]),
                                               nCrab_eat=n_distinct(crab.no[result_exclusive=="eaten"])) %>% 
  mutate(pCrab_attack=nCrab_attack/nCrabs_trial) %>% 
  mutate(pCrab_eat=nCrab_eat/nCrabs_trial) %>% 
  mutate(label_loc_eat=pCrab_eat+0.02, label_loc_attack=pCrab_eat+pCrab_attack+0.02) 
prey.summary$label_loc_attack[prey.summary$label_loc_attack==prey.summary$label_loc_eat]<-NA

prey.summary2<-prey.summary %>% pivot_longer(cols = c(pCrab_eat,pCrab_attack),values_to="pCrab",names_to="metric")

prey.summary2<-prey.summary2 %>% mutate(prey=case_when(prey=="dire.whelk"~"Whelk",prey=="hairy.shore.crab"~"Shore crab -\nhairy",prey=="purple.shore.crab"~"Shore crab -\npurple",prey=="hermit.crab"~"Hermit crab",prey=="japanese.littleneck"~"Clam -\nJ. littleneck",prey=="varnish.clam"~"Clam -\nvarnish",prey=="mussels"~"Mussel",TRUE~prey))
prey.summary2$prey<-factor(prey.summary2$prey, levels=c("Whelk","Clam -\nJ. littleneck","Clam -\nvarnish","Mussel","Hermit crab","Shore crab -\nhairy","Shore crab -\npurple"))

png("../figures/pCrab_attack_stack1.png",height=10,width=18, units="cm",res=300)
ggplot(prey.summary2, aes(x=prey,y=pCrab, fill=metric,group=metric))+geom_bar(stat="identity", position=position_stack(), color="black")+theme_kg()+scale_y_continuous(expand=c(0,0),limits=c(0,1.01))+theme(axis.text.x = element_text(angle=45,  hjust=1,vjust=1))+facet_grid(.~spp.size)+
  geom_label(data=prey.summary2 %>% filter(metric=="pCrab_eat"),aes(x=prey , y=label_loc_eat, label=paste0(nCrab_eat,"/",nCrabs_trial)), fill=NA, size=2,label.size=NA,position = position_stack())+
  geom_label(data=prey.summary2 %>% filter(metric=="pCrab_attack"),aes(x=prey , y=label_loc_attack, label=paste0(nCrab_attack,"/",nCrabs_trial)), fill=NA, size=2,label.size=NA,position = position_stack())+xlab("")+ylab("Proportion of crabs")+
  scale_fill_manual(values=c("gray","steelblue"), labels=c("Attacked only","Ate"),name="Feeding trial\noutcome")+theme(axis.text = element_text(size=7))
# +
  # theme(legend.position = "none")+ylab("Proportion of crabs attacking")
dev.off()

png("../figures/pCrab_attack_stack2.png",height=9,width=22, units="cm",res=300)
ggplot(prey.summary2, aes(x=spp.size,y=pCrab, alpha=metric,fill=prey_type))+geom_bar(stat="identity", position=position_stack(), color="black")+theme_kg()+scale_y_continuous(expand=c(0,0),limits=c(0,1.01))+theme(axis.text.x = element_text(angle=45,  hjust=1,vjust=1))+facet_grid(.~prey)+
  geom_label(data=prey.summary2 %>% filter(metric=="pCrab_eat"),aes(x=spp.size , y=label_loc_eat, label=paste0(nCrab_eat,"/",nCrabs_trial)), fill=NA, size=2,label.size=NA,position = position_stack(),color="black")+
  geom_label(data=prey.summary2 %>% filter(metric=="pCrab_attack"),aes(x=spp.size , y=label_loc_attack, label=paste0(nCrab_attack,"/",nCrabs_trial)), fill=NA, size=2,label.size=NA,position = position_stack(),color="black")+xlab("")+ylab("Proportion of crabs") +   scale_fill_manual(values=c("gray","steelblue","orange","purple"),labels=c("Bivalves","Gastropod","Hermit crab","Shore crabs"),name="Prey type")+
  scale_alpha_manual(values=c(0.4,1),labels=c("Attacked only","Ate"),name="Feeding trial\noutcome")+theme(axis.text = element_text(size=7))
dev.off()



png("../figures/pCrab_attack_stack.png",height=10,width=18, units="cm",res=300)
ggplot(prey, aes(x=prey,y=pCrab, fill=metric,group=metric))+geom_bar(stat="identity", position=position_stack(), color="black")+theme_kg()+scale_y_continuous(expand=c(0,0),limits=c(0,1.1))+theme(axis.text.x = element_text(angle=45,  hjust=1,vjust=1))+facet_grid(.~spp.size)+
  geom_label(data=prey.summary2 %>% filter(metric=="pCrab_eat"),aes(x=prey , y=pCrab+0.02, label=paste0(nCrab_eat,"/",nCrabs_trial)), fill=NA, size=2,label.size=NA,position = position_stack())+
  geom_label(data=prey.summary2 %>% filter(metric=="pCrab_attack"),aes(x=prey , y=pCrab+0.02, label=paste0(nCrab_attack,"/",nCrabs_trial)), fill=NA, size=2,label.size=NA,position = position_stack())+
  theme(legend.position = "none")+ylab("Proportion of crabs attacking")
dev.off()


# png("../figures/pCrab_attack.png",height=10,width=18, units="cm",res=300)
# ggplot(prey.summary, aes(x=prey,y=pCrab_attack, fill=spp.size))+geom_bar(stat="identity", position=position_dodge(), color="black")+theme_kg()+scale_y_continuous(expand=c(0,0),limits=c(0,1.1))+theme(axis.text.x = element_text(angle=45,  hjust=1,vjust=1))+facet_grid(.~spp.size)+geom_label(aes(x=prey, y=pCrab_attack+0.02, label=paste0(nCrab_attack,"/",nCrabs_trial)), fill=NA, size=2,label.size=NA)+theme(legend.position = "none")+ylab("Proportion of crabs attacking")
# dev.off()
# 
# 
# png("../figures/pCrab_eat.png",height=10,width=18, units="cm",res=300)
# ggplot(prey.summary, aes(x=prey,y=pCrab_eat, fill=spp.size))+geom_bar(stat="identity", position=position_dodge(), color="black")+theme_kg()+scale_y_continuous(expand=c(0,0),limits=c(0,1.1))+theme(axis.text.x = element_text(angle=45,  hjust=1,vjust=1))+facet_grid(.~spp.size)+geom_label(aes(x=prey, y=pCrab_eat+0.02, label=paste0(nCrab_eat,"/",nCrabs_trial)), fill=NA,size=2,label.size=NA)+theme(legend.position = "none")+ylab("Proportion of crabs eating")
# dev.off()

# try limiting to just crabs that got to try all types
prey.summary.all<-prey %>% filter(crab.no %in% crab.summary.ind.all$crab.no) %>% group_by(prey_type,spp.size) %>% summarize(nCrabs_trial=n_distinct(crab.no),
                                                                       nCrab_attack=n_distinct(crab.no[num.fully.eaten>0]),
                                                                       nCrab_eat=n_distinct(crab.no[num.attacked>0])) %>% 
  mutate(pCrab_attack=nCrab_attack/nCrabs_trial) %>% 
  mutate(pCrab_eat=nCrab_eat/nCrabs_trial) %>% 
  mutate(pCrab_eat_attack=nCrab_eat/nCrab_attack)

ggplot(prey.summary.all, aes(x=prey_type,y=pCrab_eat, fill=spp.size))+geom_bar(stat="identity", position=position_dodge(), color="black")+theme_kg()+scale_y_continuous(expand=c(0,0),limits=c(0,1.1))+theme(axis.text.x = element_text(angle=45,  hjust=1,vjust=1))+facet_grid(.~spp.size)+geom_label(aes(x=prey_type, y=pCrab_eat+0.02, label=paste0(nCrab_eat,"/",nCrabs_trial)), fill=NA)

ggplot(prey.summary.all, aes(x=prey_type,y=pCrab_attack, fill=spp.size))+geom_bar(stat="identity", position=position_dodge(), color="black")+theme_kg()+scale_y_continuous(expand=c(0,0),limits=c(0,1.1))+theme(axis.text.x = element_text(angle=45,  hjust=1,vjust=1))+facet_grid(.~spp.size)+geom_label(aes(x=prey_type, y=pCrab_attack+0.02, label=paste0(nCrab_attack,"/",nCrabs_trial)), fill=NA)


prey.summary1<-melt(prey.summary %>% select(prey,spp.size,pCrab_attack),id.vars=c("prey", "spp.size"))
prey.summary1<-cast(prey.summary1, prey~spp.size)

# # Total consumed across all trials
# total.consumed <- prey %>% group_by(spp.size, prey) %>% tally(num.fully.eaten) %>% filter(spp.size != "NA")
# 
# 
# ggplot(filter(total.consumed, prey != "mussels"), aes(fill=spp.size, x = prey, y = n)) +
#   geom_bar(position = "stack", stat = "identity") +
#   scale_fill_fish(option = "Halichoeres_bivittatus", discrete = TRUE, direction = 1) +
#   xlab("Prey species") + ylab("Total consumed across all trials") +
#   scale_y_continuous(expand = c(0,0),
#                      limits = c(0,60)) +
#   theme_classic() +
#   theme(panel.grid.major = element_blank(), legend.position = c(0.90, 0.90), legend.title = element_blank(),
#   axis.text.x = element_text(angle = 45, hjust=1))
# ggsave("figures/totalconsumed_wo_mytilus.tiff", width = 5, height = 6, units = "in")
# 
# 
# total.consumed.w.mytilus <- ggplot(total.consumed, aes(fill=spp.size, x = prey, y = n)) +
#   geom_bar(position = "stack", stat = "identity") +
#   scale_fill_fish(option = "Halichoeres_bivittatus", discrete = TRUE, direction = 1) +
#   xlab("Prey species") + ylab("Total consumed across all trials") +
#   scale_y_continuous(expand = c(0,0),
#                      limits = c(0,800)) +
#   theme_classic() +
#   theme(panel.grid.major = element_blank(), legend.position = c(0.9, 0.9), legend.title = element_blank(),
#         axis.text.x = element_text(angle = 45, hjust=1))
# ggsave("figures/totalconsumed_w_mytilus.tiff", width = 5, height = 6, units = "in")
# 
# # Total consumption in relation to crab size
# size.consumption <- prey %>% group_by(spp.size, crab.no, prey, cw) %>% tally(num.fully.eaten) %>% filter(spp.size != "NA")
# 
# ggplot(size.consumption, aes(colour=spp.size, x = cw, y = n)) +
#   geom_point() +
#   facet_wrap(~prey, ncol = 4, scales = "free") +
#   scale_colour_fish(option = "Halichoeres_bivittatus", discrete = TRUE, direction = 1) +
#   #geom_smooth(method = "lm", se = FALSE) +
#   xlab("Carapace width (mm)") + ylab("Total consumed across all trials") +
#   theme_classic() +
#   theme(panel.grid.major = element_blank(), legend.position = c(0.9, 0.2), legend.title = element_blank())
# ggsave("figures/totalconsumed_crabsize.tiff", width = 7, height = 5, units = "in")

# # Size consumption in relation to crab size
# datafile9 <- "../data/hsc_individual.csv"
# datafile10 <- "../data/littleneck_individual.csv"
# datafile11 <- "../data/psc_individual.csv"
# datafile12 <- "../data/varnish_individual.csv"
# 
# hsc.ind <- read.csv(datafile9, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
# littleneck.ind <- read.csv(datafile10, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
# psc.ind <- read.csv(datafile11, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
# varnish.ind <- read.csv(datafile12, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
# 
# prey.sizes <- bind_rows(hsc.ind, littleneck.ind, psc.ind, varnish.ind) %>% left_join(crabs, by = "crab.no") %>% mutate(date.run = dmy(date.run)) %>% select(c(date.run, trial.no, crab.no, spp, spp.size, prey, weight, eaten, bin.no, sex, cw, lc, rc, wgt)) %>% filter(eaten == 1) %>% mutate(row.id = row_number()) %>% filter(trial.no != "F-7")
# 
# ggplot(prey.sizes, aes(colour=spp.size, x = cw, y = weight)) +
#   geom_point(size = 1) +
#   facet_wrap(~prey, ncol = 4, scales = "free") +
#   scale_colour_fish(option = "Halichoeres_bivittatus", discrete = TRUE, direction = 1) +
#   #geom_smooth(method = "lm", se = FALSE) +
#   xlab("Carapace width (mm)") + ylab("Individual prey weight (g)") +
#   theme_classic() +
#   theme(panel.grid.major = element_blank(), legend.title = element_blank())
# ggsave("figures/ind.weightconsumed_crabsize.tiff", width = 7, height = 2, units = "in")
# 
# ggplot(prey.sizes, aes(colour=spp.size, x = cw, y = weight)) +
#   geom_point(size = 1) +
#   facet_wrap(~prey, ncol = 4, scales = "free") +
#   scale_colour_fish(option = "Halichoeres_bivittatus", discrete = TRUE, direction = 1) +
#   #geom_smooth(method = "lm", se = FALSE) +
#   xlab("Carapace width (mm)") + ylab("Individual prey weight (g)") +
#   theme_classic() +
#   theme(panel.grid.major = element_blank(), legend.title = element_blank())
# ggsave("figures/ind.weightconsumed_crabsize.tiff", width = 7, height = 2, units = "in")
# 
# ## Number of species eaten
# non.eaters <- prey %>% filter(crab.no %in% crab.summary.ind.all$crab.no) %>% group_by(spp.size, crab.no) %>% tally(num.fully.eaten) %>% filter(n == 0) %>% filter(!is.na(spp.size))
# # 
# spp.consumed <- prey %>% filter(crab.no %in% crab.summary.ind.all$crab.no)%>% filter(num.fully.eaten > 0) %>% group_by(spp.size, crab.no) %>% tally()  %>% bind_rows(non.eaters)
# 
# spp.consumed <- prey %>% filter(crab.no %in% crab.summary.ind.all$crab.no) %>% group_by(spp.size) %>%  mutate(nPreyType_eat=n_distinct(prey_type[num.fully.eaten>0]),n=n_distinct())
# 
# ggplot(spp.consumed) + 
#   geom_density(aes(y = ..count.., x = n, color = spp.size, fill = spp.size), alpha = 0.4) +
#   scale_colour_fish(option = "Halichoeres_bivittatus", discrete = TRUE, direction = 1) + 
#   scale_fill_fish(option = "Halichoeres_bivittatus", discrete = TRUE, direction = 1) +
#   xlab("Number of species eaten") + ylab("Count") +
#   scale_y_continuous(expand = c(0,0),
#                      limits = c(0,20)) +
#   scale_x_continuous(expand = c(0,0), 
#                      limits = c(0,7)) +
#   theme_classic() +
#   theme(panel.grid.major = element_blank(), legend.title = element_blank(), legend.position = c(0.85, 0.85)) 
# 
# ggsave("figures/num_species_eaten.tiff", width = 5, height = 5, units = "in")

#### something about magnitude eaten
prey$preyWtbyCrabWt<-prey$sum.weight.consumed/prey$wgt

prey2<-prey %>% filter(!is.na(wgt),!is.na(sum.weight.consumed), result_exclusive=="eaten") 

prey_wt<-prey2 %>% group_by(spp,prey) %>% summarize(mean_preyWtbyCrabWt=mean(preyWtbyCrabWt,na.rm=T),
                                                    sd_preyWtbyCrabWt=sd(preyWtbyCrabWt, na.rm=T),
                                                    n=n_distinct(crab.no))
prey_wt<-rbind(prey_wt, data.frame(spp="GRA",prey="dire.whelk",mean_preyWtbyCrabWt=NA,sd_preyWtbyCrabWt=NA,n=0))

prey_wt<-prey_wt %>% mutate(prey=case_when(prey=="dire.whelk"~"Whelk",prey=="hairy.shore.crab"~"Shore crab -\nhairy",prey=="purple.shore.crab"~"Shore crab -\npurple",prey=="hermit.crab"~"Hermit crab",prey=="japanese.littleneck"~"Clam -\nJ. littleneck",prey=="varnish.clam"~"Clam -\nvarnish",prey=="mussels"~"Mussel",TRUE~prey))
prey_wt$prey<-factor(prey_wt$prey, levels=c("Whelk","Clam -\nJ. littleneck","Clam -\nvarnish","Mussel","Hermit crab","Shore crab -\nhairy","Shore crab -\npurple"))

png("../figures/prey_wt_ratio.png", height=10,width=11, units="cm",res=250)
ggplot(prey_wt,aes(y=mean_preyWtbyCrabWt, x=prey, fill=spp))+geom_point(shape=23, position=position_dodge(width=0.5))+
  geom_errorbar(data=prey_wt,aes(ymin=mean_preyWtbyCrabWt-sd_preyWtbyCrabWt, ymax=mean_preyWtbyCrabWt+sd_preyWtbyCrabWt, color=spp), position=position_dodge(width=0.5), width=0.2)+theme_kg()+scale_y_continuous(expand=c(0,0), name="Weight consumed/weight of crab", limits=c(0,0.4))+xlab("")+scale_fill_discrete(name="")+
  scale_color_discrete(name="")+theme(legend.position = c(0.85,0.85))+
  geom_label(data=prey_wt %>% filter(spp=="EGC"), aes(y=0.015, x=prey, label=paste0("n=",n),color=spp),nudge_x = -0.28, fill=NA, label.size = 0)+
  geom_label(data=prey_wt %>% filter(spp=="GRA"), aes(y=0.015, x=prey, label=paste0("n=",n),color=spp),  nudge_x = 0.2,fill=NA, label.size = 0)
dev.off()
