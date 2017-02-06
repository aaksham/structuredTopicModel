library(stm)
setwd("/Users/aaksha/Desktop/1CMU/C/Discourse Analysis/Assignment1")
data<-read.csv('poliblogs2008-short-mod-cor.csv')
data$documents<-as.character(data$documents)
data$docname<-as.character(data$docname)
processed <- textProcessor(data$documents, metadata = data)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta
#poliblogPrevFitSpec <- stm(out$documents, out$vocab, K = 20,prevalence =~ rating + s(day), max.em.its = 75, data = out$meta, init.type = "Spectral")
contentModel<-stm(out$documents,out$vocab,K=15,content=~rating,max.em.its = 75, data = out$meta, seed=8458159)
#contentModel <- selectModel(out$documents, out$vocab, K = 20,content =~rating, max.em.its = 75,data = out$meta, runs = 20, seed = 8458159)

#exp 1
poliblogSelect <- selectModel(out$documents, out$vocab, K = 10,prevalence =~ rating + s(day), max.em.its = 50, data = out$meta, runs = 10, seed = 8458159)
storage <- searchK(out$documents, out$vocab, K = c(7, 10),prevalence = ~rating + s(day), data = meta)
plotModels(poliblogSelect)
selectedmodel <- poliblogSelect$runout[[1]]
plot.STM(selectedmodel,type="summary",xlim=c(0,.3))
labelTopics(selectedmodel, c(7,10))
samples5<-findThoughts(selectedmodel,texts=data$documents,n=2,topics=5)$docs[[1]]
samples5
selmodprep <- estimateEffect(1:10 ~ rating + s(day), selectedmodel,
                       meta = out$meta, uncertainty = "Global")

plot.estimateEffect(selmodprep, covariate = "rating", topics = c(7, 10, 1,9),
                       model = selectedmodel, method = "difference",
                       cov.value1 = "Liberal", cov.value2 = "Conservative",
                        xlab = "More Conservative ... More Liberal",
                       main = "Effect of Liberal vs. Conservatistve",
                       xlim = c(-.1, .1), labeltype = "custom",
                       custom.labels = c('Financial', 'Terrorism','Campaign'))

plot.estimateEffect(selmodprep, "day", method = "continuous", topics = 9,
                    model = selectedmodel, printlegend = FALSE, xaxt = "n", xlab = "Time Frame", 
                    ylab="Expected Topic Proportion Trend-International War Policy")

monthseq <- seq(from = as.Date("2008-01-01"),to = as.Date("2008-12-01"), by = "month")
monthnames <- months(monthseq)
axis(1,at = as.numeric(monthseq) - min(as.numeric(monthseq)),labels = monthnames)

plot.STM(selectedmodel, type = "perspectives", topics = c(10, 9))

#exp 2
selK2<- searchK(out$documents, out$vocab, K = c(7, 10),prevalence = ~rating + blog, data = meta)
#choosing 10 due to higher heldout, lower residual and higher exclusivity albeit lower semcoh
modelSelect2 <- selectModel(out$documents, out$vocab, K = 15,prevalence =~ rating + blog, max.em.its = 50, data = out$meta, runs = 10, seed = 8458159)
plotModels(modelSelect2)
selectedmodel2<-modelSelect2$runout[[1]]
plot.STM(selectedmodel2,type="summary",xlim=c(0,.3))
selmodprep2 <- estimateEffect(1:15 ~ rating + blog, selectedmodel2,meta = out$meta, uncertainty = "Global")
plot.estimateEffect(selmodprep2, covariate = "rating", topics = c(7,2, 15, 8,13),
                    model = selectedmodel2, method = "difference",
                    cov.value1 = "Liberal", cov.value2 = "Conservative",
                    xlab = "More Conservative ... More Liberal",
                    main = "Effect of Liberal vs. Conservative",
                    xlim = c(-.1, .1), labeltype = "custom",
                    custom.labels = c('Democrats', 'Election','Iraq War','Financial Crisis',
                                      'International Policy'))

#exp 2a
modelSelect2a <- selectModel(out$documents, out$vocab, K = 15,prevalence =~ rating, max.em.its = 50, data = out$meta, runs = 10, seed = 8458159)
plotModels(modelSelect2a)
selectedmodel2a<-modelSelect2a$runout[[1]]
plot.STM(selectedmodel2a,type="summary",xlim=c(0,.3))
selmodprep2a <- estimateEffect(1:15 ~ rating, selectedmodel2a,meta = out$meta, uncertainty = "Global")
plot.estimateEffect(selmodprep2a, covariate = "rating", topics = c(4, 1),
                    model = selectedmodel2a, method = "difference",
                    cov.value1 = "Liberal", cov.value2 = "Conservative",
                    xlab = "More Conservative ... More Liberal",
                    main = "Effect of Liberal vs. Conservative",
                    xlim = c(-.1, .1), labeltype = "custom",
                    custom.labels = c('Obama', 'Mc Cain'))

modelSelect2b <- selectModel(out$documents, out$vocab, K = 15,prevalence =~ blog, max.em.its = 50, data = out$meta, runs = 10, seed = 8458159)
plotModels(modelSelect2b)
selectedmodel2b<-modelSelect2b$runout[[1]]
selmodprep2b <- estimateEffect(1:15 ~ blog, selectedmodel2b,meta = out$meta, uncertainty = "Global")
plot.estimateEffect(selmodprep2b, covariate = "blog", topics = c(15,8),
                    model = selectedmodel2b, method = "difference",
                    cov.value1 = "tp", cov.value2 = "at",
                    xlab = "More Republic(at) ... More Democratic(tp)",
                    main = "Effect of Republic vs. Democrat",
                    xlim = c(-.1, .1), labeltype = "custom",
                    custom.labels = c('Iraq War', 'Financial Crisis'))



