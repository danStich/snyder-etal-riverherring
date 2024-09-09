# Libraries ----
library(tidyverse)
library(anadrofish)

# Result files ----
# . Read in data if not pre-compiled ----
files_num <- grep('bbh_habitat_baseline', dir("results"))

files <- dir("results")[files_num]

results <- vector(mode='list', length=length(files))

for(i in 1:length(files)){
  # Load file
  load(paste0("results/", files[i]))
  
  # Extract results list from output list
  out <- lapply(result, function(x) x[[c('res')]])
  
  # Collapse as df
  resdf <- do.call(rbind, out)    
  
  # Add to res list
  results[[i]] <- resdf
}
# names(res) <- NULL
pdata <- do.call(rbind, results)

# Create column for all spawners
pdata$spawners = rowSums(pdata[, grep(pattern = "spawners_", x = names(pdata))])

# Coast-wide plots (Figure 6) ----
# Summary data for plotting
plotter <- pdata %>%
  group_by(region, river, scenario) %>%
  summarize(
    pop = mean(spawners),
    lci = quantile(spawners, 0.025),
    uci = quantile(spawners, 0.975),
    samp = length(spawners))

plotter <- plotter %>%
  group_by(scenario) %>%
  summarize(pop = sum(pop),
            lwr = sum(lci),
            upr = sum(uci))

n_pass <- mean(plotter$pop[plotter$scenario == "No passage"]) 
plotter$scenario <- factor(plotter$scenario,
                           levels = c("No passage", "Current", "No dams"),
                           labels = c("No passage", "Current", "No dams"))

# Descriptive statistics
plotter

write.table(plotter, 
            "results/bbh_coastal_pop_ests.csv", 
            sep = ",", quote = FALSE, row.names = FALSE)

# Plot
jpeg("results/Figure6.jpg",
     res = 300, 
     height = 2400,
     width = 3200)
ggplot(plotter, aes(x = scenario, y = pop)) +
  geom_point(size = 6) +
  geom_linerange(aes(xmax = scenario, ymin = lwr, ymax = upr),
                 linewidth = 2) +
  geom_hline(yintercept = n_pass, linetype = 2) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  scale_y_continuous(breaks = seq(0, 5e8, .1e8), labels = seq(0, 500, 10)) +
  xlab("Scenario") +
  ylab("Coastwide blueback herring abundance (millions)") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 24),
    legend.position = "top",
    legend.box = "horizontal",
    legend.margin = margin(unit(.5, units = "npc")),
    axis.title.x = element_text(vjust = -.5, size = 24),
    axis.title.y = element_text(vjust = 2, size = 24),
    strip.background = element_blank())
dev.off()

# Regional plots (Figure 7) ----
# Summary data for plotting

# There are some messed up regions that we still need to sort out...
pdata$region[pdata$river == "Merrimack"] <- "MNE"
pdata$region[pdata$river == "Massachusetts-Rhode Island Coastal."] <- "SNE"
pdata$region[pdata$river == "Connecticut Coastal"] <- "MAT"

plotter <- pdata %>%
  group_by(region, river, scenario) %>%
  summarize(
    pop = mean(spawners),
    lci = quantile(spawners, 0.025),
    uci = quantile(spawners, 0.975),
    samp = length(spawners))

plotter <- plotter %>%
  group_by(region, scenario) %>%
  summarize(pop = sum(pop),
            lwr = sum(lci),
            upr = sum(uci))


plotter$scenario <- factor(plotter$scenario,
                           levels = c("No passage", "Current", "No dams"),
                           labels = c("No passage", "Current", "No dams"))
plotter$region <- factor(plotter$region,
                         levels = c("SAT", "MAT", "SNE", "MNE", "CAN-NNE"),
                         labels = c("SAT", "MAT", "SNE", "MNE", "CAN-NNE"))

write.table(plotter, 
            "results/bbh_regional_pop_ests.csv", 
            sep = ",", quote = FALSE, row.names = FALSE)


jpeg("results/Figure7.jpg",
     res = 300, 
     height = 2400,
     width = 3600)
ggplot(plotter, aes(x = scenario, y = pop,
                    color = region, fill = region)) +
  geom_point(position = position_dodge(0.2),
             size = 6) +
  geom_linerange(aes(xmax = scenario, ymin = lwr, ymax = upr),
                 position = position_dodge(0.2),
                 linewidth = 2) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  scale_y_continuous(breaks = seq(0, 5e8, .1e8), labels = seq(0, 500, 10)) +
  labs(color = "Region", fill = "Region") +
  xlab("Scenario") +
  ylab("Regional blueback herring abundance (millions)") +
  scale_color_grey() +
  scale_fill_grey()+    
  theme_bw() +
  theme(
    title = element_text(size = 24),
    legend.text = element_text(size = 24),
    axis.text = element_text(size = 24),
    legend.position = "top",
    legend.box = "horizontal",
    legend.margin = margin(unit(.5, units = "npc")),
    axis.title.x = element_text(vjust = -.5, size = 24),
    axis.title.y = element_text(vjust = 2, size = 24),
    strip.background = element_blank())
dev.off()

# Regional Habitat calculations (Table 3) ----
# Total habitat for alewife in HUC4_name watersheds
Table3 <- habitat_bbh %>% group_by(POP) %>% 
  summarize(Habitat = sum(Hab_sqkm))

bbh_first_dam <- habitat_bbh %>% 
  group_by(POP, DamOrder) %>% 
  summarize(first_dam = sum(Hab_sqkm)) %>% 
  filter(DamOrder <1)

Table3$first_dam <- bbh_first_dam$first_dam
Table3$difference <- Table3$Habitat - Table3$first_dam
Table3$percent_difference <- round(
  (1 - Table3$first_dam/Table3$Habitat)*100)

write.table(Table3, "results/Table3.csv",
            row.names = FALSE, quote = FALSE, sep = ",")

# River-specific Habitat calculations (Table 4) ----
# Total habitat for alewife in HUC4_name watersheds
Table4 <- habitat_bbh %>% group_by(River_huc) %>% 
  summarize(Habitat = sum(Hab_sqkm))

bbh_first_dam <- habitat_bbh %>% 
  group_by(River_huc, DamOrder) %>% 
  summarize(first_dam = sum(Hab_sqkm)) %>% 
  filter(DamOrder == min(DamOrder))

Table4$first_dam <- bbh_first_dam$first_dam
Table4$difference <- Table4$Habitat - Table4$first_dam
Table4$percent_difference <- round(
  (1 - Table4$first_dam/Table4$Habitat)*100)

write.table(Table4, "results/Table4.csv",
            row.names = FALSE, quote = FALSE, sep = ",")
