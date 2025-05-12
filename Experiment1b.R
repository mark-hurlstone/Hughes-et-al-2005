# Load dependencies
library("tidyverse")  
library("tidyr")      
library("rstatix") 
source("simple.R")

# Import the data
E1bData <- read_csv("Experiment1b.csv")

# Exclude participant 17
E1bData = E1bData %>%
  filter(Participant !=17)

# Compute proportion of correct responses collapsed across serial positions
# and add to data frame
E1bData$Accuracy <- rowMeans((E1bData[,5:10] == E1bData[,11:16]) * 1)

# Update the data frame (remove input and output items)
E1bData <- as_tibble(cbind(E1bData[,1:4],E1bData[,17])) 

# Convert variables into factors
E1bData$Participant <- factor(E1bData$Participant)
E1bData$State <- factor(E1bData$State, levels = c("SS","CS"))
E1bData$Deviant <- factor(E1bData$Deviant, levels = c("No_Deviant","With_Deviant"))

# Filter the data so it only contains the deviant trials and the immediately
# preceding no-deviant trials
criticalTrials = E1bData %>%
  filter(Trial %in% c(4,5,7,8,17,18,26,27,34,35,40,41))

# Convert from trial-level to aggregate data
E1bDataAgg <- criticalTrials %>% 
  group_by(Participant,State,Deviant) %>%
  get_summary_stats(Accuracy, show = c("mean"))

# Run the ANOVA
aovModel <- anova_test(data = E1bDataAgg, dv = mean, wid = Participant, within = c(State,Deviant), detailed = TRUE, effect.size = "pes")
aovModel$p <- round(aovModel$p, 3)
(aovModel)

# Beak down the state x deviant interaction
#==========================================

# Get ANOVA table
AnovaTable <- get_anova_table(aovModel)

# Calculate cell totals and counts
cellTotals <- E1bDataAgg %>%
  group_by(State,Deviant) %>%
  summarise(sum = sum(mean),n = n())
(cellTotals)

# Create "fixed" and "across" factors
fixed  <- "State"
across <- "Deviant"

# Simple main effects of State at Deviant
smeState <- simple(cellTotals,AnovaTable,fixed,across)
(smeState)

# Create "fixed" and "across" factors
fixed  <- "Deviant"
across <- "State"

# Simple main effects of Deviant at State
smeDeviant <- simple(cellTotals,AnovaTable,fixed,across)
(smeDeviant)

# Get descriptive statistics
descriptives <- E1bDataAgg %>%
  group_by(State,Deviant) %>%
  get_summary_stats(mean, show = c("mean","se"))
(descriptives)

# Plot the data
ggplot(data = descriptives, mapping = aes(x = State, y = mean, fill = Deviant)) +
  geom_col(width = 0.45, position = position_dodge(0.55)) +
  geom_errorbar(mapping = aes(ymin = mean - se, ymax = mean + se),
                size = .5, width = .2, position = position_dodge(.55)) +
  scale_x_discrete(labels = c(expression(paste("Steady-State")), 
                              expression(paste("Changing-State")))) +
  scale_fill_manual(labels = c("No-Deviant", "With-Deviant"), values = c("#355C7D", "#F67280")) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  labs(x = "State", y = "Proportion Correct", color = "Deviant") +
  theme_bw() +
  theme(strip.text            = element_text(size = 14, face = "bold"),
        panel.grid.major      = element_blank(),
        panel.grid.minor      = element_blank(),
        panel.background      = element_blank(),
        panel.border          = element_rect(colour = "black", fill=NA),
        axis.line             = element_blank(),
        axis.title.x          = element_text(size = 14, face = "bold"),
        axis.text.x           = element_text(size = 12, color = "black"),
        axis.text.y           = element_text(size = 12, color = "black"),
        axis.title.y          = element_text(size = 14, face = "bold"),
        legend.title          = element_text(size = 14, face = "bold"),
        legend.background     = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.text           = element_text(size = 12),
        legend.position       = "bottom",
        legend.direction      = "vertical"
  ) 
ggsave("Experiment1b.png", dpi = 300, width = 6, height = 6)
