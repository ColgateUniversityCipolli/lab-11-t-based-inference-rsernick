library(tidyverse)
library(patchwork)
library(effectsize)
library(pwr)

################################################################################
### PART 1
################################################################################

(n = pwr.t.test(d = 0.65, sig.level = 0.05, power = 0.80,
                    alternative='two.sided', type = 'one.sample')$n)

################################################################################
### PART 2
################################################################################

closer.dat = read_csv('Closer_vals.csv')
farther.dat = read_csv('Farther_vals.csv')

full.dat = closer.dat |>
  rename('Closer' = Closer_vals) |>
  mutate(Farther = farther.dat$Farther_vals,
         Difference = Closer - Farther)



################################################################################
### PART 3
################################################################################

ggplot(data = full.dat) +
  geom_boxplot(aes(x = Farther, color = 'farther')) +
  geom_boxplot(aes(x = Closer, color = 'closer')) +
  theme_bw() +
  scale_color_manual(name = '',
                     breaks = c("farther", "closer"),
                     values = c('blue', 'red')) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_text(face = "bold"),
        legend.key.size = ) +
  xlab('Change in Dopamine')

################################################################################
### PART 3 CHALLENGE
################################################################################

new.dat = full.dat |>
  mutate(Participant = 1:25)


ggplot(data = new.dat) +
  geom_point(aes(x = 'Closer', y = Closer), size = 3,
            shape = 1, color = 'blue', alpha = 0.4) +
  geom_point(aes(x = ' Further', y = Farther), size = 3,
             shape = 1, color = 'red', alpha = 0.4) +
  geom_errorbar(aes(x = 'Closer', ymin = mean(Closer)-sd(Closer),
                    ymax = mean(Closer)+sd(Closer)),
                linewidth = 1, linetype = 1) +
  geom_errorbar(aes(x = ' Further', ymin = mean(Farther)-sd(Farther),
                    ymax = mean(Farther)+sd(Farther)),
                linewidth = 1, linetype = 1) 


################################################################################
### PART 4
################################################################################


(closer.t = t.test(full.dat$Closer, conf.level = 0.05,
                   alternative = 'greater')$statistic)
(closer.p = t.test(full.dat$Closer, conf.level = 0.05,
                   alternative = 'greater')$p.value)
(closer.int = t.test(full.dat$Closer, conf.level = 0.05)$conf.int[1:2])

(closer.g = hedges_g(full.dat$Closer)$Hedges_g)


################################################################################

(farther.t = t.test(full.dat$Farther, conf.level = 0.05,
                   alternative = 'less')$statistic)
(farther.p = t.test(full.dat$Farther, conf.level = 0.05,
                   alternative = 'less')$p.value)
(farther.int = t.test(full.dat$Farther, conf.level = 0.05)$conf.int[1:2])

(farther.g = hedges_g(full.dat$Farther)$Hedges_g)

################################################################################

(difference.t = t.test(full.dat$Difference, conf.level = 0.05,
                    alternative = 'two.sided')$statistic)
(difference.p = t.test(full.dat$Difference, conf.level = 0.05,
                    alternative = 'two.sided')$p.value)
(difference.int = t.test(full.dat$Difference, conf.level = 0.05)$conf.int[1:2])

(difference.g = hedges_g(full.dat$Difference)$Hedges_g)

################################################################################

graph.dat = tibble(t = seq(-16, 16, length.out = 10000),
                   base = dt(t, 24),
                   closer = dt(t, 24, closer.t),
                   farther = dt(t, 24, farther.t),
                   diff = dt(t, 24, difference.t))

ggplot(data = graph.dat) + 
  geom_hline(yintercept = 0) +
  geom_line(aes(x = t, y = base)) +
  geom_ribbon(aes(x = t, ymax = ifelse(t>qt(0.95, 24),base, NA),
                  ymin = 0, fill = 'Rejection Region'), alpha = 0.3) +
  geom_point(aes(x = closer.t, y = 0)) +
  geom_line(aes(x = t, y = closer)) +
  scale_x_continuous(breaks = c(0, qt(0.95, 24), closer.t),
                     labels = round(c(0, qt(0.95, 24), closer.t), 2),
                     sec.axis = sec_axis(~., name = 'Change in Dopamine',
                                         breaks = c(0, qt(0.95, 24), closer.t),
                                         labels = round(c(0,
                                                    0+qt(0.95, 24)*sd(full.dat$Closer)
                                                    /sqrt(25),
                                                    mean(full.dat$Closer)),3))) +
  labs(y = 'Density', fill = '') +
  scale_fill_manual(values = 'red') +
  theme_bw()+
  theme(legend.position = 'bottom') 

ggplot(data = graph.dat) + 
  geom_hline(yintercept = 0) +
  geom_line(aes(x = t, y = base)) +
  geom_ribbon(aes(x = t, ymax = ifelse(t<qt(0.05, 24),base, NA),
                  ymin = 0, fill = 'Rejection Region'), alpha = 0.3) +
  geom_line(aes(x = t, y = farther)) + 
  scale_x_continuous(breaks = c(0, qt(0.05, 24), farther.t),
                     labels = round(c(0, qt(0.05, 24), farther.t), 2),
                     sec.axis = sec_axis(~., name = 'Change in Dopamine',
                                        breaks = c(0, qt(0.05, 24), farther.t),
                                        labels = round(c(0,
                                                         0+qt(0.05, 24)*sd(full.dat$Farther)
                                                         /sqrt(25),
                                                         mean(full.dat$Farther)),3))) +
  
  labs(y = 'Density', fill = '') +
  scale_fill_manual(values = 'red') +
  theme_bw()+
  theme(legend.position = 'bottom') 


ggplot(data = graph.dat) + 
  geom_hline(yintercept = 0) +
  geom_line(aes(x = t, y = base)) +
  geom_ribbon(aes(x = t, ymax = ifelse(t>qt(0.975, 24),base, NA),
                  ymin = 0, fill = 'Rejection Region'), alpha = 0.3) +
  geom_ribbon(aes(x = t, ymax = ifelse(t<qt(0.025, 24),base, NA),
                  ymin = 0, fill = 'Rejection Region'), alpha = 0.3) +
  geom_line(aes(x = t, y = diff)) +
  scale_x_continuous(breaks = c(0, qt(0.025, 24), qt(0.9755, 24), difference.t),
                     labels = round(c(0, qt(0.025, 24), qt(0.9755, 24), difference.t), 2),
                     sec.axis = sec_axis(~., name = 'Difference in Change in Dopamine',
                                         breaks = c(0, qt(0.025, 24), qt(0.975, 24),
                                                    difference.t),
                                         labels = round(c(0,
                                                          0+qt(0.025, 24)*sd(full.dat$Difference)
                                                          /sqrt(25), 
                                                          0+qt(0.975, 24)*sd(full.dat$Difference)
                                                          /sqrt(25),
                                                          mean(full.dat$Difference)),3))) +
  labs(y = 'Density', fill = '') +
  scale_fill_manual(values = 'red') +
  theme_bw()+
  theme(legend.position = 'bottom') 

t.dat = tibble(name = c('Closer', 'Farther', 'Difference'),
               t = c(closer.t, farther.t, difference.t),
               p = c(closer.p, farther.p, difference.p),
               g = c(closer.g, farther.g, difference.g),
               lower = c(closer.int[1], farther.int[1], difference.int[1]),
               upper = c(closer.int[2], farther.int[2], difference.int[2]))

