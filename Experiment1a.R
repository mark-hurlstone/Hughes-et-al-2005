# Load dependencies
library("tidyverse")  
library("tidyr")      
library("rstatix") 
source("simple.R")

# Import the data
E1aData <- read_csv("Experiment1a.csv")

# Compute proportion of correct responses collapsed across serial positions
# and add to data frame
E1aData$Accuracy <- rowMeans((E1aData[,6:13] == E1aData[,14:21]) * 1)

# Update the data frame (remove input and output items)
E1aData <- as_tibble(cbind(E1aData[,1:5],E1aData[,22])) 

# Convert variables into factors
E1aData$Participant <- factor(E1aData$Participant)
E1aData$State <- factor(E1aData$State, levels = c("SS","CS"))
E1aData$Deviant <- factor(E1aData$Deviant, levels = c("No_Deviant","With_Deviant"))
E1aData$Concurrent_Articulation <- factor(E1aData$Concurrent_Articulation, levels = c("No_Concurrent_Articulation","With_Concurrent_Articulation"))

# Convert from trial-level to aggregate data
E1aDataAgg <- E1aData %>%
  group_by(Participant,State,Deviant,Concurrent_Articulation) %>%
  get_summary_stats(Accuracy, show = c("mean"))

# Run the ANOVA
aovModel <- anova_test(data = E1aDataAgg, dv = mean, wid = Participant, within = c(State,Deviant,Concurrent_Articulation), detailed = TRUE, effect.size = "pes")
aovModel$p <- round(aovModel$p, 3)
(aovModel)

# Beak down the state x concurrent articulation interaction
#==========================================================

# Get ANOVA table
AnovaTable <- get_anova_table(aovModel)

# Calculate cell totals and counts
cellTotals1 <- E1aDataAgg %>%
  group_by(State,Concurrent_Articulation) %>%
  summarise(sum = sum(mean),n = n())
(cellTotals1)

# Create "fixed" and "across" factors
fixed  <- "State"
across <- "Concurrent_Articulation"

# Simple main effects of State at Concurrent Articulation
smeState <- simple(cellTotals1,AnovaTable,fixed,across)
(smeState)

# Now, break down the deviant x concurrent articulation interaction
#==================================================================

# Calculate cell totals and counts
cellTotals2 <- E1aDataAgg %>%
  group_by(Deviant,Concurrent_Articulation) %>%
  summarise(sum = sum(mean),n = n())
(cellTotals2)

# Create "fixed" and "across" factors
fixed  <- "Deviant"
across <- "Concurrent_Articulation"

# Simple main effects of Deviant at Concurrent Articulation
smeDeviant <- simple(cellTotals2,AnovaTable,fixed,across)
(smeDeviant)

# Generate a plot of the data
#============================

# Get descriptive statistics
descriptives <- E1aDataAgg %>%
  group_by(State,Deviant,Concurrent_Articulation) %>%
  get_summary_stats(mean, show = c("mean","se"))
(descriptives)

# Change labels for concurrent articulation manipulation levels
levels(descriptives$Concurrent_Articulation) <- c("No-Concurrent Articulation","With-Concurrent Articulation")

# Plot the data
ggplot(data = descriptives, mapping = aes(x = State, y = mean, fill = Deviant)) +
  geom_col(width = 0.45, position = position_dodge(0.55)) +
  geom_errorbar(mapping = aes(ymin = mean - se, ymax = mean + se),
                size = .5, width = .2, position = position_dodge(.55)) +
  scale_x_discrete(labels = c(expression(paste("Steady-State")), 
                              expression(paste("Changing-State")))) +
  scale_fill_manual(labels = c("No-Deviant", "With-Deviant"), values = c("#355C7D", "#F67280")) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  facet_wrap(~Concurrent_Articulation, nrow = 1) +
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
ggsave("Experiment1a.png", dpi=300, width = 8, height = 6)


# # INCORPORATING SERIAL POSITION AS A FACTOR
# 
# # Import data
# E1aData <- read_csv("Experiment1a.csv")
# 
# # Compute correct responses by serial position
# correct <- (E1aData[,6:13] == E1aData[,14:21]) * 1
# colnames(correct) <- c("SP1","SP2","SP3","SP4","SP5","SP6","SP7","SP8")
# 
# # Update the dataframe
# E1aData <- as_tibble(cbind(E1aData[,1:5],correct)) 
# 
# # Convert serial position factor into long format
# E1aDataLng = E1aData %>%
#   gather(Serial_Position,Accuracy,SP1:SP8,factor_key = TRUE)
# 
# # Convert other variables into factors
# E1aDataLng$Participant <- factor(E1aDataLng$Participant)
# E1aDataLng$State <- factor(E1aDataLng$State, levels = c("SS","CS"))
# E1aDataLng$Deviant <- factor(E1aDataLng$Deviant, levels = c("No_Deviant","With_Deviant"))
# E1aDataLng$Concurrent_Articulation <- factor(E1aDataLng$Concurrent_Articulation, levels = c("No_Concurrent_Articulation","With_Concurrent_Articulation"))
# 
# # Convert from trial-level to aggregate data
# E1aDataLng <- E1aDataLng %>%
#   group_by(Participant,State,Deviant,Concurrent_Articulation,Serial_Position) %>%
#   get_summary_stats(Accuracy, show = c("mean"))
# 
# # Run the ANOVA
# aovModel <- anova_test(data = E1aDataLng, dv = mean, wid = Participant, within = c(State,Deviant,Concurrent_Articulation,Serial_Position), detailed = TRUE)
# aovModel$ANOVA$p <- round(aovModel$ANOVA$p, 3)
# (aovModel)
