############################# Time-adjusted IMD score ############################

# Load requisite packages
library(rio)
library(dplyr)
library(plyr)
library(readxl)
library(lm.beta)
library(purrr)
library(mfp)
library(ggplot2)
library(ggsci)
library(ggtext)

# Import IMD raw indicators 
IMD2015_RAW <- import_list("Documents/Papers/Time Adjusted IMD/IMD2015_RAW.xlsx")
IMD2019_RAW <- import_list("Documents/Papers/Time Adjusted IMD/IMD2019_RAW.xlsx")
IMD2015_SCORE <- read_excel("Documents/Papers/Time Adjusted IMD/IMD2015_SCORE.xlsx", sheet="ID2015 Scores")
IMD2015_SCORE = select(IMD2015_SCORE, 1, 5)
IMD2019_SCORE <- read_excel("Documents/Papers/Time Adjusted IMD/IMD2019_SCORE.xlsx", sheet="IoD2019 Scores")
IMD2019_SCORE = select(IMD2019_SCORE, 1, 5)

# Create data frames frome xcel sheets
educ_raw_2015 <- IMD2015_RAW[['ID 2015 Education Domain']]
educ_raw_2019 <- IMD2019_RAW[["IoD2019 Education Domain"]]
health_raw_2015 <- IMD2015_RAW[['ID 2015 Health Domain']]
health_raw_2019 <- IMD2019_RAW[["IoD2019 Health Domain"]]
barr_raw_2015 <- IMD2015_RAW[['ID 2015 Barriers Domain']]
barr_raw_2019 <- IMD2019_RAW[["IoD2019 Barriers Domain"]]
barr_raw_2019 = select(barr_raw_2019,-11,-12) 
livenv_raw_2015 <- IMD2015_RAW[['ID 2015 Living Env Domain']]
livenv_raw_2019 <- IMD2019_RAW[["IoD2019 Living Env Domain"]]
livenv_raw_2019 = select(livenv_raw_2019,-8:-11) 



# Merge by year
all_raw_2015 <- join_all(list(IMD2015_SCORE, educ_raw_2015,health_raw_2015, barr_raw_2015,livenv_raw_2015), by = "LSOA code (2011)", type = 'full')
all_raw_2019 <- join_all(list(IMD2019_SCORE, educ_raw_2019, health_raw_2019, barr_raw_2019,livenv_raw_2019), by = "LSOA code (2011)", type = 'full')

# Vector for variable names
new_names <- c("LSOA_code", "IMD", "LSOA_name", "LA_code", "LA_name", "educ_1", "educ_2", "educ_3", "health_1" , "health_2", "health_3",
               "health_4", "barr_1", "barr_2", "barr_3", "barr_4", "barr_5", "barr_6", "barr_7","livenv_1","livenv_2","livenv_3","livenv_4")

# Rename variables
names(all_raw_2015) <- new_names
names(all_raw_2019) <- new_names

# Drop rows with mising vals
IMD_raw_2015 <- na.omit(all_raw_2015)
IMD_raw_2019 <- na.omit(all_raw_2019)

# Base regression 2015
lm.2015 <- lm(IMD ~ educ_1 + educ_2 + educ_3 + health_1 + health_2 + health_3 + health_4 + barr_1 + barr_2 + barr_3 + barr_4
   + barr_5 + barr_6 + livenv_1 + livenv_2 + livenv_3 + livenv_4, data = IMD_raw_2015)


# Base regression 2019
lm.2019 <- lm(IMD ~ educ_1 + educ_2 + educ_3 + health_1 + health_2 + health_3 + health_4 + barr_1 + barr_2 + barr_3 + barr_4
                + barr_5 + barr_6 + livenv_1 + livenv_2 + livenv_3 + livenv_4, data = IMD_raw_2019)



############## NOW residuals for full model, and then with respect to each variable#############

# Residuals to data frame
residuals(lm.2015)
IMD_raw_2015$residual <- residuals(lm.2015)

#vector for variable names
doms = names(IMD_raw_2015)[6:23]
doms= set_names(doms)

# Residual plot function
scatter_fun = function(x, y) {
  ggplot(IMD_raw_2015, aes(x = .data[[x]], y = .data[[y]]) ) +
    geom_point() +
    theme_bw()
}

# Inspect residuals by variable
domplots2015 = map(doms, ~scatter_fun(.x, "residual") )
domplots2015


# Multivariable fractional polynomial procedure
mfp2015 <- mfp(IMD ~ fp(educ_1) + fp(educ_2) + fp(educ_3) + fp(health_1) + fp(health_2)
           + fp(health_3) + fp(health_4) + fp(barr_1) + fp(barr_2) + fp(barr_3) + fp(barr_4)
           + fp(barr_5) + fp(barr_6) + fp(livenv_1) + fp(livenv_2) + fp(livenv_3) + fp(livenv_4)
           , family = gaussian, data = IMD_raw_2015, verbose = TRUE)


# Transform variables

mfp2015$formula
attach(IMD_raw_2015)
IMD_raw_2015$educ_1_FP1 <- (educ_1/0.1)^1
IMD_raw_2015$educ_1_FP2 <- log((educ_1/0.1))
IMD_raw_2015$educ_2_FP1 <- educ_2^3
IMD_raw_2015$educ_2_FP2 <- (educ_2^3)*log(educ_2)
IMD_raw_2015$educ_3_FP1 <- (educ_3/0.1)^-2
IMD_raw_2015$educ_3_FP2 <- (educ_3/0.1)^2
IMD_raw_2015$health_1_FP1 <- (health_1/100)^-1
IMD_raw_2015$health_1_FP2 <- (health_1^-1)*log((health_1/100))
IMD_raw_2015$health_2_FP1 <- (health_2/100)^3
IMD_raw_2015$health_2_FP2 <- ((health_2/100)^3)*log((health_2/100))
IMD_raw_2015$health_3_FP1 <- (health_3/100)^-1
IMD_raw_2015$health_3_FP2 <- ((health_3/100)^-1)*log((health_3/100))
IMD_raw_2015$health_4_FP1 <- log((health_4+2.9))
IMD_raw_2015$health_4_FP2 <- (health_4+2.9)^3
IMD_raw_2015$barr_1_FP1 <- barr_1^0.5
IMD_raw_2015$barr_2_FP1 <- barr_2^2
IMD_raw_2015$barr_2_FP2 <- (barr_2^2)*log(barr_2)
IMD_raw_2015$barr_3_FP1 <- barr_3^-0.5
IMD_raw_2015$barr_3_FP2 <- log(barr_3)
IMD_raw_2015$barr_4_FP1 <- barr_4^1
IMD_raw_2015$barr_4_FP2 <- barr_4^3
IMD_raw_2015$barr_5_FP1 <- (barr_5/0.1)^-0.5
IMD_raw_2015$barr_5_FP2 <- ((barr_5/0.1)^-0.5)*log((barr_5/0.1))
IMD_raw_2015$barr_6_FP1 <- (barr_6+0.1)/0.1
IMD_raw_2015$livenv_1_FP1 <- (livenv_1/0.1)^0.5
IMD_raw_2015$livenv_2_FP1 <- (livenv_2/0.01)^2
IMD_raw_2015$livenv_2_FP2 <- ((livenv_2/0.01)^2)*log((livenv_2/0.01))
IMD_raw_2015$livenv_3_FP1 <- livenv_3^0.5
IMD_raw_2015$livenv_3_FP2 <- livenv_3^1
IMD_raw_2015$livenv_4_FP1 <- livenv_4^-1
IMD_raw_2015$livenv_4_FP2 <- livenv_4^-0.5
detach(IMD_raw_2015)




# MFP regression 2015
lm.2015FP <- lm(IMD ~ educ_1_FP1 + educ_1_FP2 + educ_2_FP1 + educ_2_FP2 + educ_3_FP1 + educ_3_FP2 + health_1_FP1 + health_1_FP2 
                + health_2_FP1 + health_2_FP2 + health_3_FP1 + health_3_FP2 + health_4_FP1 + health_4_FP2 
                + barr_1_FP1 + barr_2_FP1 + barr_2_FP2 + barr_3_FP1 + barr_3_FP2 + barr_4_FP1 + barr_4_FP2
                + barr_5_FP1 + barr_5_FP2 + barr_6_FP1 + livenv_1_FP1 + livenv_2_FP1 + livenv_2_FP2 
                +livenv_3_FP1 + livenv_3_FP2 + livenv_4_FP1 + livenv_4_FP2, data = IMD_raw_2015)

# Store standard deviation of residuals
resSD2015 <- sigma(lm.2015FP)

######################################
# Multivariable fractional polynomial procedure
mfp2019 <- mfp(IMD ~ fp(educ_1) + fp(educ_2) + fp(educ_3) + fp(health_1) + fp(health_2)
               + fp(health_3) + fp(health_4) + fp(barr_1) + fp(barr_2) + fp(barr_3) + fp(barr_4)
               + fp(barr_5) + fp(barr_6) + fp(livenv_1) + fp(livenv_2) + fp(livenv_3) + fp(livenv_4)
               , family = gaussian, data = IMD_raw_2019, verbose = TRUE)




# Transform variables (new)
attach(IMD_raw_2019)
IMD_raw_2019$educ_1_FP1 <- (educ_1/0.1)^1
IMD_raw_2019$educ_1_FP2 <- log((educ_1/0.1))
IMD_raw_2019$educ_2_FP1 <- educ_2^3
IMD_raw_2019$educ_2_FP2 <- (educ_2^3)*log(educ_2)
IMD_raw_2019$educ_3_FP1 <- (educ_3/0.1)^-2
IMD_raw_2019$educ_3_FP2 <- (educ_3/0.1)^2
IMD_raw_2019$health_1_FP1 <- (health_1/100)^-1
IMD_raw_2019$health_1_FP2 <- (health_1^-1)*log((health_1/100))
IMD_raw_2019$health_2_FP1 <- (health_2/100)^3
IMD_raw_2019$health_2_FP2 <- ((health_2/100)^3)*log((health_2/100))
IMD_raw_2019$health_3_FP1 <- (health_3/100)^-1
IMD_raw_2019$health_3_FP2 <- ((health_3/100)^-1)*log((health_3/100))
IMD_raw_2019$health_4_FP1 <- log((health_4+2.9))
IMD_raw_2019$health_4_FP2 <- (health_4+2.9)^3
IMD_raw_2019$barr_1_FP1 <- barr_1^0.5
IMD_raw_2019$barr_2_FP1 <- barr_2^2
IMD_raw_2019$barr_2_FP2 <- (barr_2^2)*log(barr_2)
IMD_raw_2019$barr_3_FP1 <- barr_3^-0.5
IMD_raw_2019$barr_3_FP2 <- log(barr_3)
IMD_raw_2019$barr_4_FP1 <- barr_4^1
IMD_raw_2019$barr_4_FP2 <- barr_4^3
IMD_raw_2019$barr_5_FP1 <- (barr_5/0.1)^-0.5
IMD_raw_2019$barr_5_FP2 <- ((barr_5/0.1)^-0.5)*log((barr_5/0.1))
IMD_raw_2019$barr_6_FP1 <- (barr_6+0.1)/0.1
IMD_raw_2019$livenv_1_FP1 <- (livenv_1/0.1)^0.5
IMD_raw_2019$livenv_2_FP1 <- (livenv_2/0.01)^2
IMD_raw_2019$livenv_2_FP2 <- ((livenv_2/0.01)^2)*log((livenv_2/0.01))
IMD_raw_2019$livenv_3_FP1 <- livenv_3^0.5
IMD_raw_2019$livenv_3_FP2 <- livenv_3^1
IMD_raw_2019$livenv_4_FP1 <- livenv_4^-1
IMD_raw_2019$livenv_4_FP2 <- livenv_4^-0.5
detach(IMD_raw_2019)


# MFP regression 2019
lm.2019FP <- lm(IMD ~ educ_1_FP1 + educ_1_FP2 + educ_2_FP1 + educ_2_FP2 + educ_3_FP1 + educ_3_FP2 + health_1_FP1 + health_1_FP2 
                + health_2_FP1 + health_2_FP2 + health_3_FP1 + health_3_FP2 + health_4_FP1 + health_4_FP2 
                + barr_1_FP1 + barr_2_FP1 + barr_2_FP2 + barr_3_FP1 + barr_3_FP2 + barr_4_FP1 + barr_4_FP2
                + barr_5_FP1 + barr_5_FP2 + barr_6_FP1 + livenv_1_FP1 + livenv_2_FP1 + livenv_2_FP2 
                +livenv_3_FP1 + livenv_3_FP2 + livenv_4_FP1 + livenv_4_FP2
                , data = IMD_raw_2019)


# Store standard deviation of residuals
resSD2019 <- sigma(lm.2019FP)

# Matrix of coefficients
mat_coef2019 <- lm.2019FP$coefficients

# Create time-adjusted IMD score
#######################################

IMD_raw_2015$IMD_tadj <-
  # 2019 Constant
  (mat_coef2019[1] 
   # plus 2015 values scaled by 2019 FP coefficient (matrix cols)
  + (mat_coef2019[2]*IMD_raw_2015$educ_1_FP1)  + (mat_coef2019[3]*IMD_raw_2015$educ_1_FP2) + (mat_coef2019[4]*IMD_raw_2015$educ_2_FP1) 
  + (mat_coef2019[5]*IMD_raw_2015$educ_2_FP2) + (mat_coef2019[6]*IMD_raw_2015$educ_3_FP1) + (mat_coef2019[7]*IMD_raw_2015$educ_3_FP2) 
  + (mat_coef2019[8]*IMD_raw_2015$health_1_FP1) + (mat_coef2019[9]*IMD_raw_2015$health_1_FP2) + (mat_coef2019[10]*IMD_raw_2015$health_2_FP1) 
  + (mat_coef2019[11]*IMD_raw_2015$health_2_FP2) + (mat_coef2019[12]*IMD_raw_2015$health_3_FP1) + (mat_coef2019[13]*IMD_raw_2015$health_3_FP2) 
  + (mat_coef2019[14]*IMD_raw_2015$health_4_FP1) + (mat_coef2019[15]*IMD_raw_2015$health_4_FP2) + (mat_coef2019[16]*IMD_raw_2015$barr_1_FP1) 
  + (mat_coef2019[17]*IMD_raw_2015$barr_2_FP1) + (mat_coef2019[18]*IMD_raw_2015$barr_2_FP2) + (mat_coef2019[19]*IMD_raw_2015$barr_3_FP1) 
  + (mat_coef2019[20]*IMD_raw_2015$barr_3_FP2) + (mat_coef2019[21]*IMD_raw_2015$barr_4_FP1) + (mat_coef2019[22]*IMD_raw_2015$barr_4_FP2) 
  + (mat_coef2019[23]*IMD_raw_2015$barr_5_FP1) + (mat_coef2019[24]*IMD_raw_2015$barr_5_FP2) + (mat_coef2019[25]*IMD_raw_2015$barr_6_FP1) 
  + (mat_coef2019[26]*IMD_raw_2015$livenv_1_FP1) + (mat_coef2019[27]*IMD_raw_2015$livenv_2_FP1) + (mat_coef2019[28]*IMD_raw_2015$livenv_2_FP2) 
  + (mat_coef2019[29]*IMD_raw_2015$livenv_3_FP1) + (mat_coef2019[30]*IMD_raw_2015$livenv_3_FP2) + (mat_coef2019[31]*IMD_raw_2015$livenv_4_FP1)  
  + (mat_coef2019[32]*IMD_raw_2015$livenv_4_FP2) 
  # plus 2015 residual scaled (and standardized) by 2019 residual SD
  + ((IMD_raw_2015$residual*resSD2019)/resSD2015))


#       Mean difference plot         #
######################################

### Mean difference variables
IMD_raw_2015$IMD_dif <- IMD_raw_2015$IMD_tadj - IMD_raw_2015$IMD
IMD_raw_2015$IMD_mean <- (IMD_raw_2015$IMD + IMD_raw_2015$IMD_tadj)/2

### Mean + SD values 
meanline <- mean(IMD_raw_2015$IMD_dif)
difSD <- sd(IMD_raw_2015$IMD_dif)
lbciline <- meanline - (1.96*difSD)
ubciline <- meanline + (1.96*difSD)


### Plot
meandif <- ggplot(IMD_raw_2015,aes(x=IMD_mean, y=IMD_dif)) + 
            geom_point(color="#0073C2FF", size=1, shape=1, alpha=0.5, 
                    position = position_jitter(w = 0.5, h = 0.75)) + 
            geom_hline(yintercept=meanline, size=1.25, color="#EFC000FF") +
            xlim(0, 100) + theme(plot.margin = unit(c(.25,.25,.25,.25), "cm")) + 
            ylab("<i>t</i>-adjusted 2015 IMD score minus 2015 IMD score") +
            xlab("Mean of 2015 IMD score and <i>t</i>-adjusted 2015 IMD score")
            



            meandif +  geom_hline(yintercept = c(lbciline,ubciline), 
                                linetype="solid", color="#868686FF", size=0.7) +
                        annotate("text", x = 96.75, y = -0.01, 
                                label = "Mean = -0.78", size=4.25, fontface="bold") +
                        annotate("text", x = 95.25, y = -7.125, 
                                label = "-1.96 S.D. = -6.50", size=4.25, fontface="bold") +
                        annotate("text", x = 95.25, y = 5.6, 
                                label = "+1.96 S.D. = 4.94", size=4.25, fontface="bold") +
                     
              
              
              theme(axis.title.y=element_markdown(),
                    axis.title.x=element_markdown(),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_rect(colour = "black", size=.5, fill=NA))
                        
              
           
           

#     5-year difference bar chart      #
########################################
           
### Decile ranks
IMD_raw_2015 = mutate(IMD_raw_2015, IMD_decrk = ntile(IMD_raw_2015$IMD,10))
IMD_raw_2015 = mutate(IMD_raw_2015, IMD_tadj_decrk = ntile(IMD_raw_2015$IMD_tadj,10))
IMD_raw_2019 = mutate(IMD_raw_2019, IMD_decrk = ntile(IMD_raw_2019$IMD,10))
           
### Year identifiers
IMD_raw_2015$year <- 2015           
IMD_raw_2019$year <- 2019
           
###  Combine years 
IMD_1519 <- bind_rows(IMD_raw_2015, IMD_raw_2019)

### Fill missing t-adjusted IMD with standard IMD (for 2019 only)
IMD_1519$IMD_tadj_decrk = ifelse(!(is.na(IMD_1519$IMD_tadj_decrk)), IMD_1519$IMD_tadj_decrk,IMD_1519$IMD_decrk)

### 5-yr lag of decile rank
IMD_1519 <- IMD_1519 %>%                            
  group_by(LSOA_code) %>%
  dplyr::mutate(IMD_decrk_LAG = lag(IMD_decrk, n = 1, default = NA)) 
IMD_1519 <- IMD_1519 %>%                            
  group_by(LSOA_code) %>%
  dplyr::mutate(IMD_tadj_decrk_LAG = lag(IMD_tadj_decrk, n = 1, default = NA))

### Drop redundant 2015 obs
IMD_1519 <- IMD_1519[!(IMD_1519$year=="2015"),]

### Difference in decile rank (2019 minus 2015)
IMD_1519$IMD_decrk_DIF <- IMD_1519$IMD_decrk - IMD_1519$IMD_decrk_LAG
IMD_1519$IMD_tadj_decrk_DIF <- IMD_1519$IMD_tadj_decrk - IMD_1519$IMD_tadj_decrk_LAG

### Difference in difference 
IMD_1519$IMD_decrk_DIFinDIF <- IMD_1519$IMD_tadj_decrk_DIF - IMD_1519$IMD_decrk_DIF


### Tag obs. by difference
IMD_1519 <- IMD_1519 %>%
  mutate(direction = case_when(IMD_1519$IMD_decrk_DIFinDIF < 0 ~ 2,
                               IMD_1519$IMD_decrk_DIFinDIF > 0 ~ 1,
                               IMD_1519$IMD_decrk_DIFinDIF == 0 ~ 3))


### Sum of tagged obs. by decile rank/tag
IMD_1519 <- IMD_1519 %>%
  group_by(IMD_decrk,direction) %>%
  add_count(direction)

### Total no. of obs by decile rank
IMD_1519 <- IMD_1519 %>%                            
  group_by(IMD_decrk) %>%
  dplyr::mutate(N_bydecrk =n())

### Proportion of LSOAs with difference 5yr difference 
IMD_1519$DIF5ydif_perc <- (IMD_1519$n / IMD_1519$N_bydecrk)*100

### Declare factor variables 
IMD_1519$IMD_decrkf <- as.factor(IMD_1519$IMD_decrk)
IMD_1519$direction <- as.factor(IMD_1519$direction)

### Plot
difindif<-ggplot(data=IMD_1519, aes(x=factor(IMD_decrk), 
                                    y=DIF5ydif_perc, fill=direction, group=direction)) + # X/Y/by
  geom_bar(alpha = 0.85, stat="unique") + # Bars
  scale_fill_manual(labels = c("Positive ", "Negative", "Zero"), # Legend labels
                    values = c("1" = "#EDC000FF", # Yellow
                               "2" = "#CD534CFF", # Red
                               "3" = "#868686FF")) + # Grey
  scale_y_continuous(expand = c(.005,.005)) + # Reduce plot margin
  ylab('% of LSOAs') + # X-title
  xlab('2019 IMD decile rank')  + # Y-title
  guides(fill = guide_legend(title =
                               "5-year difference in <i>t</i>-adjusted IMD<br>"
                             ,
                             title.position = "top")) + # Legend title in ggtext markdown
  labs(tag = "relative to IMD") + # Second line of legend title as tag 
  coord_cartesian(xlim = c(1, 10), ylim = c(0, 100), clip = "off")+ # Allow tag to float
  theme(legend.position="top", legend.title.align=0.05, # Legend position
        panel.grid.major = element_blank(), # No grid lines
        panel.grid.minor = element_blank(), # As above
        panel.background = element_rect(
          colour = "black", size=.5, fill=NA), # Panel border
        legend.title=element_markdown(), # Declare markdown legend
        plot.tag.position = c(.53, .95),
        plot.tag=element_text(size=11)) # Position tag as second line in legend

difindif





### Beta coeffcients 
lm.IMD2015.beta <- lm.beta(lm.2015)
print(lm.IMD2015.beta)
lm.IMD2019.beta <- lm.beta(lm.2019)
print(lm.IMD2019.beta)





