library(tidyverse)
library(readxl)
library(janitor)

crater = read_excel('R Weather Graphic Input.xlsx', sheet = 'Crater Lake')

crater <- crater |>
  janitor::clean_names()

crater <- crater |>
  mutate(new_day = seq(1, length(day)),
         temp = ((high_f + low_f)/2)) 

dgr_fmt <- function(x, ...) {
  parse(text = paste(x, "*degree", sep = ""))
}

a <- dgr_fmt(seq(-20,100, by=10))

p <- ggplot(crater, aes(new_day, temp)) +
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        #axis.text = element_blank(),  
        axis.title = element_blank()) +
  geom_linerange(crater, mapping=aes(x=new_day, ymin=record_low_f, ymax=record_high_f), color = "wheat2", alpha=1)

p <- p +
  geom_linerange(crater, mapping=aes(x=new_day, ymin=average_low_f, ymax=average_high_f), colour = "wheat4")

p <- p + 
  geom_line(crater, mapping=aes(x=new_day, y=high_f, group=1, color = 'blue'), show.legend = FALSE) +
  geom_line(crater, mapping=aes(x=new_day, y=low_f, group=1, color = 'red'), show.legend = FALSE) +
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

high <- crater |>
  mutate(highs = ifelse(high_f >= record_high_f, "Y", "N")) |>
  filter(highs == 'Y')

low <- records |>
  mutate(lows = ifelse(low_f <= record_low_f, "Y", "N")) |>
  filter(lows == 'Y')

p <- p +
  geom_point(data=low, aes(x=new_day, y=low_f), colour="blue3") +
  geom_point(data=high, aes(x=new_day, y=high_f), colour="firebrick3")

p <- p +
  ggtitle("Crater Lake's Weather in 2022") +
  theme(plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="#3C3C3C",size=20)) +
  annotate("text", x = 34, y = 101.75, label = "Temperature", size=6,colour="#3C3C3C", fontface="bold")

p <- p +
  annotate("text", x = 55, y = 96, 
           label = "Data represents daily high and low temperature", size=3, colour="gray30") +
  annotate("text", x = 55, y = 92, 
           label = "in Crater Lake, Oregon, in 2022. Record highs", size=3, colour="gray30") +
  annotate("text", x = 55, y = 88, 
           label = "Record highs are noted for Crater Lake's", size=3, colour="gray30") +
annotate("text", x = 55, y = 84, 
         label = "three record high temperature days.", size=3, colour="gray30")

legend_data <- data.frame(x=seq(175,182),y=rnorm(8,15,2))

y_offset = -20

p <- p +
  annotate("segment", x = 181, xend = 181, y = 5+y_offset, yend = 25+y_offset, colour = "wheat2", size=3) +
  annotate("segment", x = 181, xend = 181, y = 12+y_offset, yend = 18+y_offset, colour = "wheat4", size=3) +
  geom_line(data=legend_data, aes(x=x,y=y+y_offset)) +
  annotate("segment", x = 183, xend = 185, y = 17.7+y_offset, yend = 17.7+y_offset, colour = "wheat4", size=.5) +
  annotate("segment", x = 183, xend = 185, y = 12.2+y_offset, yend = 12.2+y_offset, colour = "wheat4", size=.5) +
  annotate("segment", x = 185, xend = 185, y = 12.2+y_offset, yend = 17.7+y_offset, colour = "wheat4", size=.5) +
  annotate("text", x = 199, y = 15+y_offset, label = "NORMAL RANGE", size=2, colour="gray30") +
  annotate("text", x = 159, y = 15+y_offset, label = "2022 TEMPERATURE", size=2, colour="gray30") +
  annotate("text", x = 194, y = 25+y_offset, label = "RECORD HIGH", size=2, colour="gray30") +
  annotate("text", x = 194, y = 5+y_offset, label = "RECORD LOW", size=2, colour="gray30")

p = p + theme(panel.background = element_rect(fill = 'snow', color = 'azure4'))

p <- p +
  annotate("segment", x = 250, xend = 260.5, y = 82, yend = 89, colour = "firebrick3") +
  annotate("text", x = 290, y = 92, label = "Crater Lake had three matched", size=3, colour="firebrick3") +
  annotate("text", x = 288, y = 88.5, label = "high temperature records", size=3, colour="firebrick3")