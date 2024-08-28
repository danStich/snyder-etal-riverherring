# Libraries ----
library(tidyverse)
library(anadrofish)
library(ggh4x)

# Variable scenarios ----
# . Read in data if not pre-compiled ----
files_num <- grep('bbh_habitat_variable', dir("results"))

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


# Coastwide Plot ----
# Summary data for plotting
plotter <- pdata %>%
  group_by(upstream, downstream, downstream_j, river) %>%
  summarize(
    pop = mean(spawners),
    lci = quantile(spawners, 0.025),
    uci = quantile(spawners, 0.975),
    samp = n())

plotter <- plotter %>%
  group_by(upstream, downstream, downstream_j) %>%
  summarize(pop = sum(pop),
            lwr = sum(lci),
            upr = sum(uci),
            samp = sum(samp))

# Convert grouping vars to character
plotter <- plotter %>%
  mutate(
    downstream = as.character(downstream),
    downstream_j = paste0("DS Juv = ", downstream_j))

baseline <- mean(plotter$pop[plotter$upstream == 0])

# Plotting code
bbh_coastal_variable <- ggplot(plotter, aes(x = upstream, y = pop, color = downstream, fill = downstream)) +
  geom_line() +
  geom_ribbon(
    aes(x = upstream, ymin = lwr, ymax = upr, color = NULL),
    alpha = 0.1) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
  facet_wrap(~downstream_j, nrow = 1) +
  xlab("Upstream passage") +
  ylab("Millions of spawners") +
  labs(color = "DS Adult",
       fill = "DS Adult") +
  scale_y_continuous(breaks = seq(0,20e7, 5e7),
                     labels = format(seq(0, 20, 5), digits=2)) +
  scale_x_continuous(breaks = seq(0, 1, .5)) +
  theme_bw() +
  theme(
    panel.spacing = unit(.02, units = "npc"),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.box = "horizontal",
    legend.margin = margin(unit(.5, units = "npc")),
    axis.text = element_text(color = "black", size = 10),
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 3),
    strip.background = element_blank(),
    strip.text.y = element_text(size = 9, color = "black"),
    strip.text.x = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 10)
  ) +
  # geom_smooth(method = NULL) + # Will remove when there are more data
  geom_line(aes(y = baseline), color = "gray40", lty = 2, lwd = .25)
  
bbh_coastal_variable

jpeg("results/bbh_variable_coastal.jpg",
     res = 300, 
     height = 2400,
     width = 3200)
bbh_coastal_variable
dev.off()

# Regional plots ----
# Summary data for plotting
plotter <- pdata %>%
  group_by(upstream, downstream, downstream_j, river, region) %>%
  summarize(
    pop = mean(spawners),
    lci = quantile(spawners, 0.025),
    uci = quantile(spawners, 0.975),
    samp = n())

plotter <- plotter %>%
  group_by(upstream, downstream, downstream_j, region) %>%
  summarize(pop = sum(pop),
            lwr = sum(lci),
            upr = sum(uci),
            samp = sum(samp))

plotter$region <- factor(plotter$region,
                         levels = c("SAT", "MAT", "SNE", "MNE", "CAN-NNE"))

# Convert grouping vars to character
plotter <- plotter %>%
  mutate(
    downstream = as.character(downstream),
    downstream_j = paste0("DS Juv = ", downstream_j))

baselines <- plotter %>% 
  filter(upstream == 0) %>% 
  group_by(region) %>% 
  summarize(pop = mean(pop))


# Plotting code
bbh_variable_regional <- ggplot(
  plotter, aes(x = upstream, y = pop, color = downstream, fill = downstream)) +
  geom_line() +
  geom_ribbon(
    aes(x = upstream, ymin = lwr, ymax = upr, color = NULL),
    alpha = 0.1) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
  facet_grid(region~downstream_j, scales = "free_y") +
  xlab("Upstream passage") +
  ylab("Millions of spawners") +
  labs(color = "DS Adult",
       fill = "DS Adult") +
  ggh4x::facetted_pos_scales(y = list(
  region == "SAT" ~ scale_y_continuous(breaks = seq(0, 8e7, 2e7),
                                       labels = format(seq(0, 80, 20), digits = 2),
                                       limits = c(0, NA),
                                       expand = c(0, NA)),
  region == "MAT" ~ scale_y_continuous(breaks = seq(0, 8e7, 2e7),
                                       labels = format(seq(0, 80, 20), digits = 2),
                                       limits = c(0, NA),
                                       expand = c(0, NA)),
  region == "SNE" ~ scale_y_continuous(breaks = seq(0, 5e6, 1e6),
                                       labels = format(seq(0, 5, 1), digits = 2),
                                       limits = c(0, NA),
                                       expand = c(0, NA)),
  region == "MNE" ~ scale_y_continuous(breaks = seq(0, 5e6, 1e6),
                                         labels = format(seq(0, 5, 1), digits = 2),
                                         limits = c(0, NA),
                                       expand = c(0, NA)),
  region == "CAN-NNE" ~ scale_y_continuous(breaks = seq(0, 8e7, 2e7),
                                       labels = format(seq(0, 80, 20), digits = 2),
                                       limits = c(0, NA),
                                       expand = c(0, NA)))) +
  scale_x_continuous(breaks = seq(0, 1, .5)) +
  theme_bw() +
  theme(
    panel.spacing = unit(.02, units = "npc"),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.box = "horizontal",
    legend.margin = margin(unit(.5, units = "npc")),
    axis.text = element_text(color = "black", size = 10),
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 3),
    strip.background = element_blank(),
    strip.text.y = element_text(size = 9, color = "black"),
    strip.text.x = element_text(size = 8, color = "black"),
    legend.text = element_text(size = 10)
  ) +
  # geom_smooth(method = NULL) + # Will remove when there are more results
  geom_hline(data = baselines, aes(yintercept = pop), color = "gray40",
             lty = 2, lwd = .25)

bbh_variable_regional

jpeg("results/bbh_variable_regional.jpg",
     res = 300, 
     height = 3200,
     width = 3200)
bbh_variable_regional
dev.off()
