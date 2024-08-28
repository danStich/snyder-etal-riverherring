# Libraries ----
library(tidyverse)
library(anadrofish)

# Baseline scenarios ----
# . Read in data if not pre-compiled ----
files_num <- grep('ale_habitat_baseline', dir("results"))

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


# Coast-wide plots (Figure 4) ----
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
            "results/ale_baseline_coastal.csv", 
            sep = ",", quote = FALSE, row.names = FALSE)

# Plot
jpeg("results/ale_baseline_coastal.jpg",
     res = 300, 
     height = 2400,
     width = 3200)
ggplot(plotter, aes(x = scenario, y = pop)) +
  geom_point(size = 6) +
  geom_linerange(aes(xmax = scenario, ymin = lwr, ymax = upr),
                 linewidth = 2) +
  geom_hline(yintercept = n_pass, linetype = 2) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  scale_y_continuous(breaks = seq(0, 5e9, 1e9), labels = seq(0, 5, 1),
                     limits = c(0, 2.5e9)) +
  xlab("Scenario") +
  ylab("Alewife spawning abundance (billions)") +
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

# Regional plots (Figure 5) ----
# Summary data for plotting
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

n_pass <- mean(plotter$pop[plotter$scenario == "No passage"]) 

plotter$scenario <- factor(plotter$scenario,
                           levels = c("No passage", "Current", "No dams"),
                           labels = c("No passage", "Current", "No dams"))
plotter$region <- factor(plotter$region,
                         levels = c("MAT", "SNE", "NNE"),
                         labels = c("MAT", "SNE", "NNE"))

write.table(plotter, 
            "results/ale_baseline_regional.csv", 
            sep = ",", quote = FALSE, row.names = FALSE)


jpeg("results/ale_baseline_regional.jpg",
     res = 300, 
     height = 2400,
     width = 3200)
ggplot(plotter, aes(x = scenario, y = pop,
                    color = region, fill = region)) +
  geom_point(position = position_dodge(0.2), size = 6) +
  geom_linerange(aes(xmax = scenario, ymin = lwr, ymax = upr),
                 position = position_dodge(0.2),
                 linewidth = 2) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  scale_y_continuous(breaks = seq(0, 5e9, .5e9), labels = seq(0, 5, .5)) +
  labs(color = "Region", fill = "Region") +
  xlab("Scenario") +
  ylab("Alewife spawning abundance (billions)") +
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

# Regional Habitat calculations (Table 1) ----
# Total habitat for alewife in HUC4_name watersheds
Table1 <- habitat_ale %>% group_by(POP) %>% 
  summarize(Habitat = sum(Hab_sqkm))

ale_first_dam <- habitat_ale %>% 
  group_by(POP, DamOrder) %>% 
  summarize(first_dam = sum(Hab_sqkm)) %>% 
  filter(DamOrder <1)

Table1$first_dam <- ale_first_dam$first_dam
Table1$difference <- Table1$Habitat - Table1$first_dam
Table1$percent_difference <- round(
  (1 - Table1$first_dam/Table1$Habitat)*100)

write.table(Table1, "results/ale_habitat_regional.csv",
            row.names = FALSE, quote = FALSE, sep = ",")


# River-specific Habitat calculations (Table 2) ----
# Total habitat for alewife in River_huc watersheds
Table2 <- habitat_ale %>% group_by(River_huc) %>% 
  summarize(Habitat = sum(Hab_sqkm))

ale_first_dam <- habitat_ale %>% 
  group_by(River_huc, DamOrder) %>% 
  summarize(first_dam = sum(Hab_sqkm, na.rm = TRUE)) %>% 
  filter(DamOrder == min(DamOrder))

Table2$first_dam <- ale_first_dam$first_dam
Table2$difference <- Table2$Habitat - Table2$first_dam
Table2$percent_difference <- round(
  (1 - Table2$first_dam/Table2$Habitat)*100)

write.table(Table2, "results/ale_habitat_river.csv",
            row.names = FALSE, quote = FALSE, sep = ",")

