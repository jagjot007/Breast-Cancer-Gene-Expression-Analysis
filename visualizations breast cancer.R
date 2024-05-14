setwd("~/Desktop/data science /breast cancer project")
getwd()

#loading libraries
library(tidyverse)
library(ggplot2)

data.long %>%
  filter(gene == "BRCA1") %>%
  head()

#visualizations of breast cancer dataset with ggplot2
#ggplot(data, aes(x = variable1, y = variable2)) + geom_visualization + addititonal info

#overview of BRCA1 expression in tumor and normal samples (barplot)
data.long %>%
  filter(gene == "BRCA1") %>%
  ggplot(., aes(x = samples, y = FPKM, fill = tissue)) +
  geom_col()

#comparison of expression of BRCA1 in tumor samples and normal samples via density plot
data.long %>%
  filter(gene == "BRCA1") %>%
  ggplot(., aes(x = FPKM, fill = tissue)) +
  geom_density(alpha = 0.3)    # alpha is opacity 

#expression of BRCA1 in samples showing metastasis, and samples with no metastasis using box plot
data.long %>%
  filter(gene == "BRCA1") %>%
  ggplot(., aes(x = metastasis, y = FPKM)) +
  geom_boxplot()

#analysing the relationship of BRCA1 and BRCA2 in normal samples and tumor samples via scatterplot
data.long %>%
  filter(gene == "BRCA1" | gene == "BRCA2") %>%
  spread(key = gene, value = FPKM) %>%     #making wide format
  head()

data.long %>%
  filter(gene == "BRCA1" | gene == "BRCA2") %>%
  spread(key = gene, value = FPKM) %>%
  ggplot(., aes(x = BRCA1, y = BRCA2, color = tissue)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)    #se is confi int

#making a heatmap to visualize BRCA1, BRCA2, p53 expression in all samples
genes.of.interest <- c("BRCA1", "BRCA2", "TP53", "ALK", "MYCN")

data.long %>%
  filter(gene %in% genes.of.interest) %>%
  head()

data.long %>%
  filter(gene %in% genes.of.interest) %>%
  ggplot(., aes(x = samples, y = gene, fill = FPKM)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red")

  
#saving the plot
pdf("heatmap of breast cancer genes.pdf", width = 10, height = 8)
data.long %>%
  filter(gene %in% genes.of.interest) %>%
  ggplot(., aes(x = samples, y = gene, fill = FPKM)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red")

dev.off()

#or
heatmap = data.long %>%
  filter(gene %in% genes.of.interest) %>%
  ggplot(., aes(x = samples, y = gene, fill = FPKM)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red")
ggsave(heatmap, filename = "heatmap_save.pdf", width = 10, height = 8)
