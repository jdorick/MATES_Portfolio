library(tidyverse)
library(readxl)
library(janitor)

philly <- read_excel('R Weather Graphic Input.xlsx', sheet = 'Philadelphia')

philly <- philly |>
  janitor::clean_names()

philly <- philly |>
  mutate(new_day = seq(1, length(day)),
         temp = ((high_f + low_f)/2)) 

dgr_fmt <- function(x, ...) {
  parse(text = paste(x, "*degree", sep = ""))
}

a <- dgr_fmt(seq(-20,100, by=10))

p <- ggplot(philly, aes(new_day, temp)) +
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        #axis.text = element_blank(),  
        axis.title = element_blank()) +
  geom_linerange(philly, mapping=aes(x=new_day, ymin=record_low_f, ymax=record_high_f), color = "wheat2", alpha=1)

p <- p +
  geom_linerange(philly, mapping=aes(x=new_day, ymin=average_low_f, ymax=average_high_f), colour = "wheat4")

p <- p + 
  geom_line(philly, mapping=aes(x=new_day, y=high_f, group=1, color = 'blue'), show.legend = FALSE) +
  geom_line(philly, mapping=aes(x=new_day, y=low_f, group=1, color = 'red'), show.legend = FALSE) +
  geom_vline(xintercept = 0, colour = "wheat4", linetype=1, size=1)

p <- p + 
  geom_vline(xintercept = 31, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 59, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 90, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 120, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 151, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 181, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 212, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 243, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 273, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 304, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 334, colour = "wheat4", linetype=3, size=.5) +
  geom_vline(xintercept = 365, colour = "wheat4", linetype=3, size=.5)

p <- p +
  coord_cartesian(ylim = c(-20,100)) +
  scale_y_continuous(breaks = seq(-20,100, by=10), labels = a) +
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                     labels = c("January", "February", "March", "April",
                                "May", "June", "July", "August", "September",
                                "October", "November", "December"))

high <- philly |>
  mutate(highs = ifelse(high_f >= record_high_f, "Y", "N")) |>
  filter(highs == 'Y')

low <- records |>
  mutate(lows = ifelse(low_f <= record_low_f, "Y", "N")) |>
  filter(lows == 'Y')

p <- p +
  geom_point(data=low, aes(x=new_day, y=low_f), colour="blue3") +
  geom_point(data=high, aes(x=new_day, y=high_f), colour="firebrick3")

p <- p +
  ggtitle("Philadelphia's Weather in 2022") +
  theme(plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="#3C3C3C",size=20)) +
  annotate("text", x = 30, y = 102, label = "Temperature", size=6,colour="#3C3C3C", fontface="bold")

p <- p +
  annotate("text", x = 58, y = 96, 
           label = "Data represents daily high and low temperature in the", size=3, colour="gray30") +
  annotate("text", x = 58, y = 92, 
           label = "city of Philadelphia in 2022. Record highs are noted", size=3, colour="gray30") +
  annotate("text", x = 58, y = 88, 
           label = "for Philly's five record high temperature days.", size=3, colour="gray30")

legend_data <- data.frame(x=seq(175,182),y=rnorm(8,15,2))

p <- p +
  annotate("segment", x = 181, xend = 181, y = 5, yend = 25, colour = "wheat2", size=3) +
  annotate("segment", x = 181, xend = 181, y = 12, yend = 18, colour = "wheat4", size=3) +
  geom_line(data=legend_data, aes(x=x,y=y)) +
  annotate("segment", x = 183, xend = 185, y = 17.7, yend = 17.7, colour = "wheat4", size=.5) +
  annotate("segment", x = 183, xend = 185, y = 12.2, yend = 12.2, colour = "wheat4", size=.5) +
  annotate("segment", x = 185, xend = 185, y = 12.2, yend = 17.7, colour = "wheat4", size=.5) +
  annotate("text", x = 199, y = 15, label = "NORMAL RANGE", size=2, colour="gray30") +
  annotate("text", x = 159, y = 15, label = "2022 TEMPERATURE", size=2, colour="gray30") +
  annotate("text", x = 194, y = 25, label = "RECORD HIGH", size=2, colour="gray30") +
  annotate("text", x = 194, y = 5, label = "RECORD LOW", size=2, colour="gray30")

p = p + theme(panel.background = element_rect(fill = 'snow', color = 'azure4'))

p <- p +
  annotate("segment", x = 312, xend = 313.5, y = 82, yend = 88, colour = "firebrick3") +
  annotate("text", x = 333, y = 95, label = "Philly had five matched", size=3, colour="firebrick3") +
  annotate("text", x = 335, y = 91.5, label = "high temperature records", size=3, colour="firebrick3")
