rm(list=ls())
setwd("~/Academic Projects/CFRA 2021 - Mesocosms/1_viu.feeding.trials")

library(lubridate)
library(fishualize)
library(tidyverse)

# Load data 
datafile1 <- "~/Academic Projects/CFRA 2021 - Mesocosms/1_viu.feeding.trials/data/individual_crab_data.csv"
datafile2 <- "~/Academic Projects/CFRA 2021 - Mesocosms/1_viu.feeding.trials/data/hsc_trials.csv"
datafile3 <- "~/Academic Projects/CFRA 2021 - Mesocosms/1_viu.feeding.trials/data/hermit_trials.csv"
datafile4 <- "~/Academic Projects/CFRA 2021 - Mesocosms/1_viu.feeding.trials/data/dire_trials.csv"
datafile5 <- "~/Academic Projects/CFRA 2021 - Mesocosms/1_viu.feeding.trials/data/littleneck_trials.csv"
datafile6 <- "~/Academic Projects/CFRA 2021 - Mesocosms/1_viu.feeding.trials/data/mussel_trials.csv"
datafile7 <- "~/Academic Projects/CFRA 2021 - Mesocosms/1_viu.feeding.trials/data/psc_trials.csv"
datafile8 <- "~/Academic Projects/CFRA 2021 - Mesocosms/1_viu.feeding.trials/data/varnish_trials.csv"

crabs <- read.csv(datafile1, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
hsc <- read.csv(datafile2, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
hermit <- read.csv(datafile3, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
dire <- read.csv(datafile4, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
littleneck <- read.csv(datafile5, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
mussel <- read.csv(datafile6, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
psc <- read.csv(datafile7, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
varnish <- read.csv(datafile8, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)

prey <- bind_rows(hsc, hermit, dire, littleneck, mussel, psc, varnish) %>% left_join(crabs, by = "crab.no") %>% mutate(date.run = dmy(date.run)) %>% select(c(date.run, trial.no, crab.no, spp, spp.size, prey, bin.no, num.fully.eaten, num.attacked, sum.weight.consumed, sex, cw, lc, rc, wgt)) %>% mutate(row.id = row_number()) %>% filter(trial.no != "F-7") %>% filter(!row.id %in% c(279, 334))

# Some crabs were fed that same prey twice in error. Prey order wasn't altered for F6 and F7, so dropped F7 completely. GRA-Sm11 was accidentally moved from Group C to Group D later in experiment and saw the same prey, so removed later trials (by row number). 

# Total consumed across all trials 
total.consumed <- prey %>% group_by(spp.size, prey) %>% tally(num.fully.eaten) %>% filter(spp.size != "NA")


ggplot(filter(total.consumed, prey != "mussels"), aes(fill=spp.size, x = prey, y = n)) + 
  geom_bar(position = "stack", stat = "identity") + 
  scale_fill_fish(option = "Halichoeres_bivittatus", discrete = TRUE, direction = 1) + 
  xlab("Prey species") + ylab("Total consumed across all trials") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,60)) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), legend.position = c(0.90, 0.90), legend.title = element_blank(), 
  axis.text.x = element_text(angle = 45, hjust=1))
ggsave("figures/totalconsumed_wo_mytilus.tiff", width = 5, height = 6, units = "in")
  

total.consumed.w.mytilus <- ggplot(total.consumed, aes(fill=spp.size, x = prey, y = n)) + 
  geom_bar(position = "stack", stat = "identity") + 
  scale_fill_fish(option = "Halichoeres_bivittatus", discrete = TRUE, direction = 1) + 
  xlab("Prey species") + ylab("Total consumed across all trials") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,800)) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), legend.position = c(0.9, 0.9), legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust=1))
ggsave("figures/totalconsumed_w_mytilus.tiff", width = 5, height = 6, units = "in")

# Total consumption in relation to crab size 
size.consumption <- prey %>% group_by(spp.size, crab.no, prey, cw) %>% tally(num.fully.eaten) %>% filter(spp.size != "NA")

ggplot(size.consumption, aes(colour=spp.size, x = cw, y = n)) + 
  geom_point() + 
  facet_wrap(~prey, ncol = 4, scales = "free") + 
  scale_colour_fish(option = "Halichoeres_bivittatus", discrete = TRUE, direction = 1) + 
  #geom_smooth(method = "lm", se = FALSE) +
  xlab("Carapace width (mm)") + ylab("Total consumed across all trials") +
  theme_classic() +
  theme(panel.grid.major = element_blank(), legend.position = c(0.9, 0.2), legend.title = element_blank()) 
ggsave("figures/totalconsumed_crabsize.tiff", width = 7, height = 5, units = "in")

# Size consumption in relation to crab size 
datafile9 <- "~/Academic Projects/CFRA 2021 - Mesocosms/1_viu.feeding.trials/data/hsc_individual.csv"
datafile10 <- "~/Academic Projects/CFRA 2021 - Mesocosms/1_viu.feeding.trials/data/littleneck_individual.csv"
datafile11 <- "~/Academic Projects/CFRA 2021 - Mesocosms/1_viu.feeding.trials/data/psc_individual.csv"
datafile12 <- "~/Academic Projects/CFRA 2021 - Mesocosms/1_viu.feeding.trials/data/varnish_individual.csv"

hsc.ind <- read.csv(datafile9, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
littleneck.ind <- read.csv(datafile10, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
psc.ind <- read.csv(datafile11, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
varnish.ind <- read.csv(datafile12, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)

prey.sizes <- bind_rows(hsc.ind, littleneck.ind, psc.ind, varnish.ind) %>% left_join(crabs, by = "crab.no") %>% mutate(date.run = dmy(date.run)) %>% select(c(date.run, trial.no, crab.no, spp, spp.size, prey, weight, eaten, bin.no, sex, cw, lc, rc, wgt)) %>% filter(eaten == 1) %>% mutate(row.id = row_number()) %>% filter(trial.no != "F-7") 

ggplot(prey.sizes, aes(colour=spp.size, x = cw, y = weight)) + 
  geom_point(size = 1) + 
  facet_wrap(~prey, ncol = 4, scales = "free") + 
  scale_colour_fish(option = "Halichoeres_bivittatus", discrete = TRUE, direction = 1) + 
  #geom_smooth(method = "lm", se = FALSE) +
  xlab("Carapace width (mm)") + ylab("Individual prey weight (g)") +
  theme_classic() +
  theme(panel.grid.major = element_blank(), legend.title = element_blank()) 
ggsave("figures/ind.weightconsumed_crabsize.tiff", width = 7, height = 2, units = "in")

ggplot(prey.sizes, aes(colour=spp.size, x = cw, y = weight)) + 
  geom_point(size = 1) + 
  facet_wrap(~prey, ncol = 4, scales = "free") + 
  scale_colour_fish(option = "Halichoeres_bivittatus", discrete = TRUE, direction = 1) + 
  #geom_smooth(method = "lm", se = FALSE) +
  xlab("Carapace width (mm)") + ylab("Individual prey weight (g)") +
  theme_classic() +
  theme(panel.grid.major = element_blank(), legend.title = element_blank()) 
ggsave("figures/ind.weightconsumed_crabsize.tiff", width = 7, height = 2, units = "in")

## Number of species eaten
non.eaters <- prey %>% group_by(spp.size, crab.no) %>% tally(num.fully.eaten) %>% filter(n == 0) %>% filter(!is.na(spp.size))
spp.consumed <- prey %>% filter(num.fully.eaten > 0) %>% group_by(spp.size, crab.no) %>% tally() %>% bind_rows(non.eaters)

ggplot(spp.consumed) + 
  geom_density(aes(y = ..count.., x = n, color = spp.size, fill = spp.size), alpha = 0.4) +
  scale_colour_fish(option = "Halichoeres_bivittatus", discrete = TRUE, direction = 1) + 
  scale_fill_fish(option = "Halichoeres_bivittatus", discrete = TRUE, direction = 1) +
  xlab("Number of species eaten") + ylab("Count") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,20)) +
  scale_x_continuous(expand = c(0,0), 
                     limits = c(0,7)) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), legend.title = element_blank(), legend.position = c(0.85, 0.85)) 
ggsave("figures/num_species_eaten.tiff", width = 5, height = 5, units = "in")

