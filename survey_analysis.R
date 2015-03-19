library("ggplot2")
library("grid")
library("dplyr")

responses <- read.csv("survey_responses.csv", stringsAsFactors=F,
                      header=T, na.strings="")

responses$ans.time <- as.Date(responses$ans.time, format="%d/%m/%Y")

## Who's coming? 
# Experts and how often
attendance <- responses %>% group_by(expertise, attend) %>% 
  summarise(count=n())
attendance$attend <- factor(attendance$attend, ordered=T,
      levels=c("Likely every meeting", "Most meetings", 
               "If there's an interesting speaker or topic"))
attendance$expertise <- factor(attendance$expertise,
      levels=rev(c("No R knowledge", "Beginner", "Intermediate", "Expert")))

pdf("plots/attendance.pdf", 6, 3)
ggplot(attendance, aes(x=expertise, y=count, 
                       fill=attend, order=attend)) +
  geom_bar(stat="identity") +
  scale_fill_brewer() + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(x="", y="Responses", fill="") + coord_flip() +
  theme_minimal() +
  scale_y_continuous(expand=c(0,0)) +
  theme(legend.position=c(.7, .9), 
        legend.key.width=unit(.3,"cm"), 
        legend.key.height=unit(.4,"cm")) 
dev.off()

## Topics we want to hear about 
# Cleaning, tidying
topics <- table(unlist(lapply(responses$topic, strsplit, split=", ")))
topics <- data.frame(topic=names(topics), votes=as.vector(topics))
topics$topic <- factor(topics$topic, 
                       levels=topics[order(topics$votes, decreasing=F),"topic"])

pop_topics <- subset(topics, votes > 2)

# Ordered bar chart of popular topics
pdf("plots/popular_topics.pdf", 5, 4.5)
ggplot(pop_topics, aes(x=topic, y=votes)) + 
  geom_bar(stat="identity", fill=I("#3182BD")) +
  coord_flip() + theme_minimal() +
  scale_y_continuous(expand=c(0,0)) +
  labs(x="", y="Number of votes") 
dev.off()

pop_topics$speakers <- factor(c("N", "N", "N", "N", "Y", "Y", "N", 
                                "N", "N", "N", "Y", "N", "N", "N", 
                                "N", "N", "N"), levels=c("Y", "N"))

pdf("plots/missing_speakers.pdf", 5, 4.5)
ggplot(pop_topics, aes(x=topic, y=votes, fill=speakers)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#3182BD", "#dedede")) +
  coord_flip() + theme_minimal() +
  scale_y_continuous(expand=c(0,0)) +
  labs(x="", y="Number of votes") +
  theme(legend.position=c(.8,.15)) +
  labs(fill="Do we have\n a speaker?")
dev.off()

## Best time and date combination
# data tidying
best_time <- responses[,c("day", "time")]
dotw <- c("Mondays", "Tuesdays",
          "Wednesdays", "Thursdays",
          "Fridays", "Saturdays", "Sundays")

best_time$day <- factor(best_time$day, 
                        levels=rev(dotw))

day_table <- as.data.frame(table(best_time$day))
colnames(day_table) <- c("day", "votes")
day_table$day <- factor(day_table$day, levels=dotw)

pdf("plots/best_day.pdf", 6, 4)
ggplot(day_table, aes(x=day, y=votes)) +
  geom_bar(fill=I("#3182BD"), stat="identity") +
  theme_minimal() + labs(x="", y="Number of votes") +
  scale_y_continuous(expand=c(0,0)) +
  geom_text(inherit.aes=F, col=I("white"), face=I(2),
            aes(x=day, y=votes-1, label=gsub(0, "", votes)))
dev.off()

time_table <- as.data.frame(table(na.omit(best_time$time)))
colnames(time_table) <- c("time", "votes")

pdf("plots/best_time.pdf", 6, 4)
ggplot(time_table, aes(x=time, y=votes)) +
  geom_bar(fill=I("#3182BD"), stat="identity") +
  theme_minimal() + labs(x="", y="Number of votes") +
  scale_y_continuous(expand=c(0,0)) +
  geom_text(inherit.aes=F, col=I("white"), face=I(2),
            aes(x=time, y=votes-.7, label=gsub("^0$", "", votes))) 
dev.off()

best_time$time <- factor(gsub("pm", "", best_time$time), 
                         levels=c(12, 1:8))

pdf("plots/best_time_combo.pdf", 5, 3)
ggplot(best_time[complete.cases(best_time),],
       aes(x=time, y=day)) + 
  geom_bin2d() + theme_minimal() +
  labs(x="Best time (pm)", y="", fill="Votes") +
  scale_fill_continuous(limits=c(0.5, 10),
                        breaks=c(1, 10)) +
  guides(fill=guide_colourbar(
    barwidth=.5, barheight=2)) 
dev.off()

# which venue is best?
wrapper <- function(x, ...){
  paste(strwrap(x, ...), collapse = "\n")
}

responses$venue <- gsub("  ", " ", gsub("\\(.*?\\)", "", responses$venue))
responses$venue <- as.factor(responses$venue)
venue <- responses %>% group_by(venue) %>% 
  tally() %>% filter(n > 2)

venue$venue <- sapply(venue$venue, wrapper, width=15)
venue$y <- rep(1,3)

pdf("plots/venues.pdf", 7, 9)
ggplot(venue, aes(x=venue, y=y)) + 
  geom_point(aes(size=n,  col=venue)) +
  scale_size_continuous(range=c(50,120)) +
  geom_text(aes(label=paste0(venue, " (", n, ")"))) +
  theme_minimal() + 
  theme(legend.position="none",
        panel.grid=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank()) +
  coord_polar() + 
  scale_color_brewer(palette=3, type="qual")
dev.off()
