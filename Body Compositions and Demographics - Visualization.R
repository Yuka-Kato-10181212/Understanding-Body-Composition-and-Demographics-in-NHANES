### Visualization

require('haven')
require('srvyr')
require('survey')
require('dplyr')
require('tidyr')
require('apyramid')
require('ggplot2')
require('readr')
require('patchwork')
require('RColorBrewer')
require('viridis')
require("gridExtra")

############################################################################################
## Race/ Ethnicity 
race <- as.data.frame(svytable(~ race1 + phenotype, addf)) %>% 
  mutate(perc = Freq / ave(Freq, phenotype, FUN = sum) * 100,
         perc = round(perc, 1)) %>%
  ungroup() 


ggplot(race, aes(x = "", y = perc, fill = race1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ phenotype) +
  geom_text(aes(label = perc),
            position = position_stack(vjust = 0.47)) +
  theme_void() +
  scale_fill_manual(values = c('lightblue', 'pink', 'lightgreen','yellow'),
                    breaks = c(1, 2, 3, 4),
                    labels = c('Hispanic', 'White', "Black" , "Other")) +
  theme(legend.position = "right") +
  labs(title = "Distribution of  Race/Ethnicity in Each Category(%)",
       fill = "Race/ Ethnicity")


#############################################################################
#BMI Boxplot
#Get BMI boxplot to check if there is outliers 
par(mfrow = c(1, 2))
par(cex.axis = 0.7)
svyboxplot(bmi ~ phenotype, m_addf,
           ylab = "BMI", xlab = 'Phenotype', 
           main = 'BMI Male')


svyboxplot(bmi ~ phenotype, f_addf,
           ylab = "BMI", xlab = 'Phenotype', 
           main = 'BMI Female')

par(mfrow = c(1, 1))
par(cex.axis = 1)

############################################################################
#Weight
weight_mean <-  addf %>% group_by(phenotype) %>%
  summarise(mean = survey_mean(weight, na.rm = TRUE, vartype = "ci"))

weight_mean <- weight_mean[, c(1, 2)]
weight_mean <- weight_mean$mean

#Histogram 
par(mfrow = c(2, 2))

#HA_HM
adhahm_weight <- weight_mean[1]
svyhist(~ weight, adhahm,
        xlim = c(30, 140),
        ylim = c(0, 0.04),
        xlab = "Weight(kg)",
        main = "Distribution of Weight (HA_HM)")
text(x = 50, y = 0.035, labels = paste("Mean:", round(adhahm_weight, 1)))

#HA_LM
adhalm_weight <- weight_mean[2]
svyhist(~ weight, adhalm,
        xlim = c(30, 140),
        ylim = c(0, 0.04),
        xlab = "Weight(kg)",
        main = "Distribution of Weight (HA_LM) ")
text(x = 110, y = 0.03, labels = paste("Mean:", round(adhalm_weight, 1)))



#LA_HM
adlahm_weight <- weight_mean[3]
svyhist(~ weight, adlahm,
        xlim = c(30, 140),
        ylim = c(0, 0.04),
        xlab = "Weight(kg)",
        main = "Distribution of Weight (LA_HM) ")
text(x = 110, y = 0.038, labels = paste("Mean:", round(adlahm_weight, 1)))

#LA_LM
adlalm_weight <- weight_mean[4]
svyhist(~ weight, adlalm,
        xlim = c(30, 140),
        ylim = c(0, 0.04),
        xlab = "Weight(kg)",
        main = "Distribution of Weight (LA_LM) ")
text(x = 100, y = 0.03, labels = paste("Mean:", round(adlalm_weight, 1)))

par(mfrow = c(1, 1))


#######################################################################################
#Density plot of BMI

bmi_mean_m <- get_mean(m_addf, "bmi")
bmi_mean_f <- get_mean(f_addf, "bmi")


bmi_m1 <- get_hist_data(m_hahm, "bmi") 
bmi_f1 <- get_hist_data(f_hahm, "bmi") 

bmi_m2 <- get_hist_data(m_halm, "bmi", breaks = 5) 
bmi_f2 <- get_hist_data(f_halm, "bmi", breaks = 3) 

bmi_m3 <- get_hist_data(m_lahm, "bmi", breaks = 5) 
bmi_f3 <- get_hist_data(f_lahm, "bmi", breaks = 5) 

bmi_m4 <- get_hist_data(m_lalm, "bmi", breaks = 5)
bmi_f4 <- get_hist_data(f_lalm, "bmi", breaks = 5)

bmi_m <- bind_rows(bmi_m1, bmi_m2, bmi_m3, bmi_m4) 
bmi_f <- bind_rows(bmi_f1, bmi_f2, bmi_f3, bmi_f4)


par(mfrow = c(1, 2))

#For Male
plot(bmi_m1$breaks + 2.5, bmi_m1$density, 
     xlab = "BMI", ylab = 'Density', 
     main = "The Distribution of BMI: Male",
     ylim = c(0, 0.2),
     type = 'n')
lines(bmi_m1$breaks + 2.5, bmi_m1$density, col = "blue", lwd = 2)
lines(bmi_m2$breaks + 2.5, bmi_m2$density, col = "red", lwd = 2)
lines(bmi_m3$breaks + 2.5, bmi_m3$density, col = "green", lwd = 2)
lines(bmi_m4$breaks + 2.5, bmi_m4$density, col = "orange", lwd = 2)

#write a legend, also want to add middle point in the x-axis? 
legend('topright',
       legend = c("HA_HM", "HA_LM", "LA_HM", "LA_LM"), 
       col = c("blue", "red", "green", "orange"), 
       lty = 1, lwd = 2)
text(30, 0.2, paste("HA_HM: ", bmi_mean_m[1]), cex = 0.7)
text(30, 0.185, paste("HA_LM: ", bmi_mean_m[2]), cex = 0.7)
text(30, 0.17, paste("LA_HM: ", bmi_mean_m[3]), cex = 0.7)
text(30, 0.155, paste("LA_LM: ", bmi_mean_m[4]), cex = 0.7)


#For Female
plot(bmi_f1$breaks + 2.5, bmi_f1$density, 
     xlab = "BMI", ylab = 'Density', 
     main = "The Distribution of BMI: Female",
     ylim = c(0, 0.2),
     type = 'n')
lines(bmi_f1$breaks + 2.5, bmi_f1$density, col = "blue", lwd = 2)
lines(bmi_f2$breaks + 2.5, bmi_f2$density, col = "red", lwd = 2)
lines(bmi_f3$breaks + 2.5, bmi_f3$density, col = "green", lwd = 2)
lines(bmi_f4$breaks + 2.5, bmi_f4$density, col = "orange", lwd = 2)

#write a legend, also want to add middle point in the x-axis? 
legend('topright',
       legend = c("HA_HM", "HA_LM", "LA_HM", "LA_LM"), 
       col = c("blue", "red", "green", "orange"), 
       lty = 1, lwd = 2)
text(30, 0.2, paste("HA_HM: ", bmi_mean_f[1]), cex = 0.7)
text(30, 0.185, paste("HA_LM: ", bmi_mean_f[2]), cex = 0.7)
text(30, 0.17, paste("LA_HM: ", bmi_mean_f[3]), cex = 0.7)
text(30, 0.155, paste("LA_LM: ", bmi_mean_f[4]), cex = 0.7)

par(mfrow = c(1, 1))


###########################################################################################
#Scatter plot for the phenotype and asmi vs fmi

#raw fmi vs smi

x_limits <- c(2, 30)  
y_limits <- c(2, 15)  

# Plot the first scatter plot
#Y~X
svyplot(ASMI ~ FMI, design = cat_hahm, style = 'bubble',
        xlab = "FMI", ylab = 'ASMI', basecol = 'blue',
        xlim = x_limits, ylim = y_limits)

# Overlay the remaining scatter plots
par(new = TRUE)
svyplot(ASMI ~ FMI, design = cat_halm, style = 'bubble',
        xlab = "", ylab = '', basecol = 'red',
        xlim = x_limits, ylim = y_limits)
par(new = TRUE)
svyplot(ASMI ~ FMI, design = cat_lahm, style = 'bubble',
        xlab = "", ylab = '', basecol = 'green',
        xlim = x_limits, ylim = y_limits)
par(new = TRUE)
svyplot(ASMI ~ FMI, design = cat_lalm, style = 'bubble',
        xlab = "", ylab = '', basecol = 'orange',
        xlim = x_limits, ylim = y_limits)

legend("bottomright", col = c("blue", "red", "green", "orange"), pch = 19,
       legend = c("HA_HM", "HA_LM", "LA_HM", "LA_LM"))




############################################################################################
##DECILE_asmi vs. DECILE_fmi

x_limits <- c(0, 10)  
y_limits <- c(0, 10)  

# Plot the first scatter plot
#Y~X
svyplot(DECILE_asmi ~ DECILE_fmi, design = cat_hahm, style = 'bubble',
        xlab = "DECILE_FMI", ylab = 'DECILE_ASMI', basecol = 'blue',
        xlim = x_limits, ylim = y_limits)

par(new = TRUE)
svyplot(DECILE_asmi ~ DECILE_fmi, design = cat_halm, style = 'bubble',
        xlab = "", ylab = '', basecol = 'red',
        xlim = x_limits, ylim = y_limits)
par(new = TRUE)
svyplot(DECILE_asmi ~ DECILE_fmi, design = cat_lahm, style = 'bubble',
        xlab = "", ylab = '', basecol = 'green',
        xlim = x_limits, ylim = y_limits)
par(new = TRUE)
svyplot(DECILE_asmi ~ DECILE_fmi, design = cat_lalm, style = 'bubble',
        xlab = "", ylab = '', basecol = 'orange',
        xlim = x_limits, ylim = y_limits)

legend("topright", col = c("blue", "red", "green", "orange"), pch = 19,
       legend = c("HA_HM", "HA_LM", "LA_HM", "LA_LM"))


############################################################################################
#Stacked barchart

ggplot(m_circ1) + 
  geom_bar(aes(x = age_group2, y = Freq, fill = race_char),
           position = 'stack',
           stat = 'identity') + 
  scale_x_discrete(NULL, expand = c(0.2, 0.2)) +
  labs(x = "Race/ Ethnicity", y = "Frequency", fill = "Race/ Ethnicity") + 
  facet_grid(~ phenotype, switch = 'x',space = "free_x") + 
  scale_fill_manual(values = c("Hispanic" = "#0099cc", 
                               "White" = "#FFFF99", 
                               "Black" = "#9933FF", 
                               "Other" = "#00CCCC")) +
  theme(strip.placement = 'outside',
        strip.background = element_rect(fill = NA, color = 'white'),
        panel.spacing = unit(-.3, 'cm'),
        axis.text.x = element_text(size = 7),
        panel.background = element_rect(fill = "darkgrey")) +
  
  ggtitle("Age Group Distribution for each Race/ Ethnicity Based on Phenotype: Male")


##############################################################################################
#Circular stacked barchart
circ_m <- m_circ1 %>% 
  select(-age) %>% 
  mutate(race_char = factor(m_circ1$race_char, 
                            levels = c("Hispanic", "White", "Black", "Other")))


circ_m <- circ_m %>%
  group_by(phenotype, race_char, age_group2) %>%
  summarize(Freq = sum(Freq)) %>% 
  spread(race_char, Freq) %>% 
  ungroup()

circ_m <- circ_m %>% mutate(id = paste("Number", seq(1:nrow(circ_m))))
circ_m <- circ_m %>% gather(key = "observation", value = 'value', c(3:6))


empty_bar <- 5
n0bsType <- nlevels(as.factor(circ_m$observation))
to_add <- data.frame(matrix(NA, empty_bar*nlevels(circ_m$phenotype)*n0bsType,
                            ncol(circ_m)))
colnames(to_add) <- colnames(circ_m)  
to_add$phenotype <- rep(levels(circ_m$phenotype), each = empty_bar*n0bsType)
circ_m <- rbind(circ_m, to_add)
circ_m <- circ_m %>% arrange(phenotype, age_group2)
circ_m$id <- rep(seq(1, nrow(circ_m)/n0bsType), each = n0bsType)

label_data <- circ_m %>% group_by(id, age_group2) %>% summarize(tot = sum(value))
number_of_bar <- nrow(label_data)
angle <- 90-360 * (label_data$id-0.5)/number_of_bar

label_data$hjust <- ifelse(angle < -90, 1, 0) #0 is left, 1 is right
label_data$angle <- ifelse(angle < -90, angle + 180, angle)


base_data <- circ_m %>% group_by(phenotype) %>% 
  summarize(start = min(id), end = max(id) - empty_bar) %>%
  rowwise() %>% 
  mutate(title = mean(c(start, end)))


grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1: nrow(grid_data) - 1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1, ]


ggplot(circ_m) + 
  geom_bar(aes(x = as.factor(id), y = value, fill = observation), 
           stat = 'identity', alpha = 0.5) +
  scale_fill_viridis(discrete = T) + 
  geom_segment(data = grid_data, aes(x = end, y = 2500000, xend = start, yend = 2500000 ),
               colour = "grey", alpha = 1, size = 0.2,
               inherit.aes = F) + 
  geom_segment(data = grid_data, aes(x = end, y = 2000000, xend = start, yend = 2000000 ),
               colour = "grey", alpha = 1, size = 0.2,
               inherit.aes = F) + 
  geom_segment(data = grid_data, aes(x = end, y = 1500000, xend = start, yend = 1500000 ),
               colour = "grey", alpha = 1, size = 0.2,
               inherit.aes = F) +
  geom_segment(data = grid_data, aes(x = end, y = 1000000, xend = start, yend = 1000000 ),
               colour = "grey", alpha = 1, size = 0.2,
               inherit.aes = F) +
  geom_segment(data = grid_data, aes(x = end, y = 500000, xend = start, yend = 500000 ),
               colour = "grey", alpha = 1, size = 0.2,
               inherit.aes = F) +
  annotate("text", x = rep(max(circ_m$id), 5), y = c(500000, 1000000, 1500000, 2000000, 2500000), 
           label = c("0.5M", "1M", "1.5M", "2M", "2.5M"),
           colour = "grey", size = 3, angle = 0, fontface = 'bold',
           hjust = 1) + 
  
  ylim(-2000000, 5000000) + 
  theme_minimal() + 
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2, 4), "cm")) +  
  #plot.margin defines the plot area, minus value is expand the plotting boundaries
  coord_polar(start = 0) +
  
  geom_text(data = label_data, aes(x = id, y = tot + 10, label = age_group2, hjust = hjust),
            color = 'black', fontface = "bold",  alpha = 0.8,
            size = 3, angle = label_data$angle, inherit.aes = F) + 
  geom_segment(data = base_data, aes(x = start, y = -5, xend = end, yend = -5),
               colour = "black", alpha = 0.7, size = 0.6, inherit.aes = F) + 
  geom_text(data = base_data, aes(x = title, y = -500000, label = phenotype), hjust = 0.5,
            colour = "black", alpha = 0.8, size = 2.5, fontface = 'bold', inherit.aes = F) +
  labs(fill = "Race/ Ethnicity")  +
  ggtitle("Distribution of Race Depending on Age: Male") + 
  theme(plot.title = element_text(hjust = 0.5, margin = margin(2.1, 0, -1, 0, "cm") ))

#############################################################################################
#From here, we focus on the effect of weight

#Race/Ethnicity 
#Male
#Unweighted
raw_race_m <- as.data.frame(table(df_m$race1, df_m$phenotype))
colnames(raw_race_m) <- c("race1", "phenotype", 'Freq')
raw_race_m <- raw_race_m %>% 
  mutate(perc = Freq / ave(Freq, phenotype, FUN = sum) * 100,
         perc = round(perc, 1)) %>%
  ungroup() 

plot1 <- ggplot(raw_race_m, aes(x = "", y = perc, fill = race1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ phenotype) +
  geom_text(aes(label = perc),
            position = position_stack(vjust = 0.47)) +
  theme_void() +
  scale_fill_manual(values = c('lightblue', 'pink', 'lightgreen','yellow'),
                    breaks = c(1, 2, 3, 4),
                    labels = c('Hispanic', 'White', "Black" , "Other")) +
  theme(legend.position = "right") +
  labs(title = "Unweighted Distribution of Race/Ethnicity(%): Male",
       fill = "Race/ Ethnicity")


#Weighted
race_m <- as.data.frame(svytable(~ race1 + phenotype, m_addf)) %>% 
  mutate(perc = Freq / ave(Freq, phenotype, FUN = sum) * 100,
         perc = round(perc, 1)) %>%
  ungroup() 


plot2 <- ggplot(race_m, aes(x = "", y = perc, fill = race1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ phenotype) +
  geom_text(aes(label = perc),
            position = position_stack(vjust = 0.47)) +
  theme_void() +
  scale_fill_manual(values = c('lightblue', 'pink', 'lightgreen','yellow'),
                    breaks = c(1, 2, 3, 4),
                    labels = c('Hispanic', 'White', "Black" , "Other")) +
  theme(legend.position = "right") +
  labs(title = "Weighted Distribution of Race/Ethnicity(%): Male",
       fill = "Race/ Ethnicity")


plot1+plot2




##############################################################################################
## Marital status 

raw_maritalS_m <- as.data.frame(table(df_m$maritalS, df_m$phenotype))
colnames(raw_maritalS_m) <- c("maritalS", "phenotype", 'Freq')
raw_maritalS_m <- raw_maritalS_m %>% 
  mutate(perc = Freq / ave(Freq, phenotype, FUN = sum) * 100,
         perc = round(perc, 1)) %>%
  ungroup() 

plot1 <- ggplot(raw_maritalS_m, aes(x = "", y = perc, fill = maritalS)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ phenotype) +
  geom_text(aes(label = perc),
            position = position_stack(vjust = 0.47)) +
  theme_void() +
  scale_fill_manual(values = c('lightblue', 'pink', 'lightgreen' )) +
  theme(legend.position = "right") +
  labs(title = "Unweighted Distribution of Marital Status(%): Male",
       fill = "Marital Status")

#Weighted
maritals_m <- as.data.frame(svytable(~maritalS + phenotype,m_addf)) %>% 
  mutate(perc = Freq / ave(Freq, phenotype, FUN = sum) * 100,
         perc = round(perc, 1)) %>%
  ungroup() 

plot2 <- ggplot(maritals_m, aes(x = "", y = perc, fill = maritalS)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ phenotype) +
  geom_text(aes(label = perc),
            position = position_stack(vjust = 0.47)) +
  theme_void() +
  scale_fill_manual(values = c('lightblue', 'pink', 'lightgreen' )) +
  theme(legend.position = "right") +
  labs(title = "Weighted Distribution of Marital Status(%): Male",
       fill = "Marital Status")

plot1+plot2

