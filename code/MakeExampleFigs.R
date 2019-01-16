#install packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse")
ipak(packages)

# set ggplot theme
theme_set(theme_classic() + 
            theme(axis.text=element_text(colour="black"), 
                  text=element_text(size=10)))

# set  directories
dataDir <- file.path(getwd(), "data")
figDir <- file.path(getwd(), "figures")

print(paste("last run ", Sys.time()))
print(R.version)


# learning curve
x <- seq(-3, 3, length.out = 100)
y <- 1/(1+ exp(-x))

df <- as.tbl(data.frame(x, y))

ggplot(df, aes(x = x , y= y)) + 
  geom_line(color = "#8856a7", size = 1.5) + 
  labs(x = "Experience", y = "Performance") + 
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank())


ggsave(file.path(figDir, "ExampleLearningCurve.svg"), width = 4, height = 3)


# make example frequency learning plot

x = seq(0,2, length.out = 2000)
y = rnorm(mean = 330, sd = 20, n = length(x))
plot(x, y)

x2 = seq(-1,3, length.out = 5000)
x3 <- x2 + 3
y3 = 1/(1+ exp(-3*x2)) * 20 + 330  + rnorm(length(x2), sd = seq(20, 10, length.out = length(x2)))

y4 <- 1/(1+ exp(3*x2)) * 20 + 330 - 20  + rnorm(length(x2), sd = seq(20, 10, length.out = length(x2)))


xc <- c(x, x3, x3)
yc <- c(y, y3, y4 )
plot(xc, yc)

plot(x3, y4)

plot(x = c(x, x3), y = c(y, y3))

ggplot(data = data.frame(x = c(x, x3), y = c(y, y3)), aes(x , y)) + 
  geom_point(alpha = 0, stroke = 0) + 
  stat_smooth(span = 0.9, se = FALSE, color = "#8856a7", size =2) + 
  labs(y = "Sonication frequency (Hz)", 
       x = "Time") + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) + 
  ylim(c(250, 400)) + 
  geom_hline(aes(yintercept = 330), lty = 2)

ggsave(file.path(figDir, "HypotheticalHighTrt.svg"), width = 4, height = 3)

ggsave(file.path(figDir, "HypotheticalHighTrt.png"), dpi = 400, width = 4, height = 3)


newDF <- as.tbl(data.frame(x1 = c(x, x3), y = c(y, y3))) %>%
  mutate(`Reward Status` = recode(as.character(x1 >= 2), 
                                  "TRUE" = "Rewarded for\nhigh frequency sonications", 
                                  "FALSE" = "Rewarded for\nany frequency"))


ggplot(newDF, aes(x = `Reward Status` , y)) + 
  geom_boxplot(aes(fill = `Reward Status`), alpha = 0.8, width = 0.5, outlier.colour = NA) +
  labs(y = "Sonication frequency (Hz)", 
       x = "") + 
  ylim(c(250, 400)) + 
  geom_hline(aes(yintercept = 330), lty = 2) + 
  scale_fill_manual(values = c("#4a1486", "#6a51a3")) + 
  theme(legend.position = "none")

ggsave(file.path(figDir, "HypotheticalHighTrt_boxplot.png"), dpi = 400, width = 4, height = 3)


newDF <- as.tbl(data.frame(x1 = c(x, x3), y = c(y, y4))) %>%
  mutate(`Reward Status` = recode(as.character(x1 >= 2), 
                                  "TRUE" = "Rewarded for\nlow frequency sonications", 
                                  "FALSE" = "Rewarded for\nany frequency"))

ggplot(newDF, aes(x = `Reward Status` , y)) + 
  geom_boxplot(aes(fill = `Reward Status`), alpha = 0.8, width = 0.5, outlier.colour = NA) +
  labs(y = "Sonication frequency (Hz)", 
       x = "") + 
  ylim(c(250, 400)) + 
  geom_hline(aes(yintercept = 330), lty = 2) + 
  #scale_fill_manual(values = c("#4a1486", "#6a51a3")) + 
  scale_fill_viridis_d(name = "Treatment", begin =0.1, end = 0.4, option = "A") + 
  theme(legend.position = "none")

ggsave(file.path(figDir, "HypotheticalLowTrt_boxplot.png"), dpi = 400, width = 4, height = 3)



ggplot(data = data.frame(x = c(x, x3), y = c(y, y4)), aes(x , y)) + 
  geom_point(alpha = 0, stroke = 0) + 
  stat_smooth(span = 0.9, se = FALSE, color = "#8856a7", size = 2) + 
  labs(y = "Sonication frequency (Hz)", 
       x = "Time") + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) + 
  ylim(c(250, 400))+ 
  geom_hline(aes(yintercept = 330), lty = 2)

ggsave(file.path(figDir, "HypotheticalLowTrt.svg"), width = 4, height = 3)
ggsave(file.path(figDir, "HypotheticalLowTrt.png"), dpi = 400, width = 4, height = 3)


## Sonication accel

ggplot(data = data.frame(x = c(x, x3), y = c(y, y4)), aes(x , y)) + 
  geom_point(alpha = 0.0, stroke = 0) + 
  stat_smooth(span = 0.9, se = FALSE, color = "#8856a7", size = 1.2) + 
  labs(y =  expression("Sonication acceleration " (m~s^{-2})), 
       x = "Time") + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) + 
  ylim(c(0, 50))+ 
  geom_hline(aes(yintercept = 330), lty = 2)

ggsave(file.path(figDir, "HypotheticalLowTrt_ACCEL.png"), dpi = 400, width = 4, height = 3)



## Sonication accel for predictable, innate response

ggplot(data = data.frame(x = c(x, x3), y = c(y, y3)), aes(x , (y-min(y))  / 7)) + 
  geom_point(alpha = 0, stroke = 0) + 
  stat_smooth(span = 0.9, se = FALSE, color = "#8856a7", size = 2) + 
  labs(y =  expression("Sonication acceleration " (m~s^{-2})), 
       x = "Time") + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) + 
  geom_hline(aes(yintercept = 9), lty = 2)

ggsave(file.path(figDir, "HypotheticalLowTrt_ACCEL2.png"), dpi = 400, width = 4, height = 3)



# Frequency and amplitude plot
x = seq(0, 0.1, length.out = 500)
y = 10 * sin(2 * pi* 50 * x)

ggplot(data.frame(x,y), aes(x , y )) + 
  geom_line(color = "#8856a7", size = 1.2) + 
  labs(x = "Time (s)", 
       y = expression("Sonication acceleration " (m~s^{-2}))) + 
  geom_hline(aes(yintercept = 0), lty = 2)

ggsave(file.path(figDir, "Hypotheticalwave.png"), width = 6, height = 3, dpi = 500)



# Physical constraint expectations

# Frequency and amplitude plot
factorVar = rep(c(0, 50), each = 500)
x = seq(0, 1, length.out = length(factorVar))
y = rnorm(mean = 0, sd = 20, n= length(x)) + 330 - factorVar


weightDF <- data.frame(x,y, factorVar) %>%
  mutate(`FlowerType` = recode(as.character(factorVar), 
                                  "0" = "Sham", 
                                  "50" = "IncreasedMass"))
ggplot(weightDF, aes(x , y, color = FlowerType )) + 
  geom_point(size = 1, alpha = 0) + 
  stat_smooth(span = 0.9, se = FALSE, size = 2) + 
  labs(x = "Visit Number", 
       y = expression("Sonication frequency     "        (Hz))) + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        legend.position = "none") + 
  geom_vline(aes(xintercept = 0.5), lty = 2) + 
  scale_color_manual(values = c("#4a1486", "#6a51a3"))

ggsave(file.path(figDir, "hypotheticalWeight_freq.png"), dpi = 500, width = 4, height = 3)


# Frequency and amplitude plot
factorVar = rep(c(0, 20), each = 500)
x = seq(0, 1, length.out = length(factorVar))
y = rnorm(mean = 0, sd = 20, n= length(x)) + 330 - factorVar


weightDF <- data.frame(x,y, factorVar) %>%
  mutate(`FlowerType` = recode(as.character(factorVar), 
                               "0" = "Sham", 
                               "50" = "IncreasedMass"))

ggplot(weightDF, aes(x , y/ 20, color = FlowerType )) + 
  geom_point(size = 1, alpha = 0) + 
  stat_smooth(span = 0.9, se = FALSE, size = 2) + 
  labs(x = "Visit Number", 
       y = expression("Sonication acceleration " (m~s^{-2}))) + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        legend.position = "none") + 
  geom_vline(aes(xintercept = 0.5), lty = 2) + 
  scale_color_manual(values = c("#4a1486", "#6a51a3"))

ggsave(file.path(figDir, "hypotheticalWeight_acc.png"), dpi = 500, width = 4, height = 3)
