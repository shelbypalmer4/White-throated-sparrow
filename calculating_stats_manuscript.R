the_truth <- read.csv("/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/the_truth.csv")

three_note_rhythms <- the_truth[which(the_truth$min_max_dur_ratio > .5),]
mean(three_note_rhythms$mid_to_long_ratio)
sd(three_note_rhythms$mid_to_long_ratio/sqrt(length(three_note_rhythms$mid_to_long_ratio)))


min(the_truth$min_max_dur_ratio)
max(the_truth$min_max_dur_ratio)
mean(the_truth$min_max_dur_ratio)
sd(the_truth$min_max_dur_ratio/sqrt(length(the_truth$min_max_dur_ratio)))


min(the_truth$min_max_dur_ratio[which(the_truth$Terminal.Strophe.type == "Triplet")])
               
plot(the_truth$min_max_dur_ratio, the_truth$mid_to_long_ratio)

