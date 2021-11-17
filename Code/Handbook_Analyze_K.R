# Analyze K

# setwd('M:\\Artikel\\Handbook CSS\\Data')

library(stm)
library(ggplot2)
library(doParallel)
library(igraph)
library(dfrtopics)


# Exclusivity and semantic coherence & Heldout Likelihood

# * Load STMs (cf. `Handbook_Preprocessing.R`)
STM.list <- list.files('STM_Model/')
length(STM.list) # should be 120
# * Extract model types
type <- gsub('\\...*', '', STM.list)
type <- gsub('Model_', '',type)
unique(type) # our 12 models
# * Set possible K
k.cand <- seq(10,100,10)
# * Run as cluster
ncores <- 12 
cl <- makeCluster(ncores)
registerDoParallel(cl)

## Start loop
start.0 <- Sys.time()
for(type.temp in unique(type)){
  ### load 'dtm.stm'
  f.out <- paste('STMProc_', type.temp,'.Rdata', sep = '')
  load(file = paste('STM_Proc/', f.out, sep = '' )) # dtm.stm
  ### extract relevant files for type
  STM.list.type <- grep(type.temp, STM.list, value = T)
  ### run for k=[10:100]; f == each file of model type
  ### make heldout for type
  heldout <- make.heldout(dtm.stm$documents, dtm.stm$vocab)
  
  start.1 <- Sys.time()
  for(f in STM.list.type){
    # extract K
    temp <- gregexpr("[0-9]+", f)  
    k <- as.numeric(unique(unlist(regmatches(f, temp))))
    # load model
    load(file = paste("STM_Model/",f, sep = ""))  # "model"
    #get metrics
    semcoh <- semanticCoherence(model, dtm.stm$documents)
    exclus <- exclusivity(model)
    # plot exclusivity and coherence (introduce upper/lower boundaries)
    df.temp <- data.frame(coherence = semcoh, exclusivity = exclus, k = k)
    df.plot.temp <- data.frame(coh = median(df.temp$coherence), exc = median(df.temp$exclusivity), lower.coh = min(df.temp$coherence), max.coh = max(df.temp$coherence), lower.ex = min(df.temp$exclusivity), max.ex = max(df.temp$exclusivity), k = k) # only median/ranges
    
    ev_temp <- data.frame(held = eval.heldout(model, heldout$missing)$expected.heldout, k = k)

    if(k == min(k.cand)){
      df.plot <- df.plot.temp
      ev <- ev_temp
      
    }
    else{
      df.plot <- rbind(df.plot, df.plot.temp)
      ev <- rbind(ev, ev_temp)
    }
  }
  if(type.temp == unique(type)[1]){
    df.plot$type <- type.temp
    df.plot.all <- df.plot
    ev$type <- type.temp
    ev.all <- ev
  }
  else{
    df.plot$type <- type.temp
    df.plot.all <- rbind(df.plot.all, df.plot)
    ev$type <- type.temp
    ev.all <- rbind(ev.all, ev)
  }
  cat('\n' ,type.temp, ': ', Sys.time()-start.1)
}
print(Sys.time() - start.0) 
stopCluster(cl)

## Make plots

### Exclusivity and semantic coherence
df.plot.all$SemCoherence <- df.plot.all$coh
df.plot.all$Exclusivity <- df.plot.all$exc
df.plot.long <- reshape2::melt(df.plot.all, id.vars = c('type', 'k'))
df.plot.long <- df.plot.long[df.plot.long$variable %in% c('Exclusivity', 'SemCoherence'),]

# * Split in three parts
half1 <- unique(df.plot.long$type)[1:4]
df.half1 <- df.plot.long[df.plot.long$type %in% half1,]

ggplot(data = df.half1, aes(x = k, y = value, group = variable, color = variable)) +
  geom_line() +
  facet_grid(df.half1$variable~df.half1$type, scales = 'free_y') +
  labs(y = '') +
  theme_bw() + 
  theme(legend.position="none")

half2 <- unique(df.plot.long$type)[5:8]
df.half2 <- df.plot.long[df.plot.long$type %in% half2,]
ggplot(data = df.half2, aes(x = k, y = value, group = variable, color = variable)) +
  geom_line() +
  facet_grid(df.half2$variable~df.half2$type, scales = 'free_y') +
  labs(y = '') +
  theme_bw() + 
  theme(legend.position="none")

half3 <- unique(df.plot.long$type)[9:12]
half3 <- df.plot.long[df.plot.long$type %in% half3,]
ggplot(data = half3, aes(x = k, y = value, group = variable, color = variable)) +
  geom_line() +
  facet_grid(half3$variable~half3$type, scales = 'free_y') +
  labs(y = '') +
  theme_bw() + 
  theme(legend.position="none")


### Heldout Likelihood

# * Line plot
ggplot(data=ev.all, aes(x=k, y=held, group =  type, color = type)) +
  geom_line() +
  geom_point(alpha = 0.5) +
  labs(y = 'Held-out Likelihood') +
  facet_wrap(.~type) +
  theme_bw() + 
  theme(legend.position="none")
dev.off()

# * Boxplot
ggplot(ev.all, aes(x = type, y = held)) + 
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun=mean, geom="point", shape=23, size=4, color = 'red', alpha = 0.5) +
  # geom_jitter(shape=16, position=position_jitter(0.2)) +
  labs(x = '', y = 'Held-out likelihood across K by PS') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # + coord_flip()


# Variation of Information 

## Start loop

start.0 <- Sys.time()
for(type.temp in unique(type)){
  STM.list.type <- grep(type.temp, STM.list, value = T)
  ### compare everything with model k=10
  f <- STM.list.type[1]
  # extract K
  temp <- gregexpr("[0-9]+", f)  
  k <- as.numeric(unique(unlist(regmatches(f, temp))))
  cat('\n', f, ' Start: ',k)
  # load model
  load(file = paste("STM_Model/",f, sep = ""))  # "model"
  theta.null <- data.frame(model$theta)
  theta.null$max <- apply(theta.null, 1, which.max)
  ### run from K=20; f == each file of model type
  start.1 <- Sys.time()
  for(f.temp in STM.list.type[2:10]){
    # extract K
    temp <- gregexpr("[0-9]+", f.temp)  
    k <- as.numeric(unique(unlist(regmatches(f.temp, temp))))
    # load model
    load(file = paste("STM_Model/",f.temp, sep = ""))  # "model"
    theta <- data.frame(model$theta)
    theta$max <- apply(theta, 1, which.max)
    df.temp <- data.frame(K = k, 
                          VI = igraph::compare(theta$max, theta.null$max, method = 'vi'))
    if(f.temp == STM.list.type[2]){
      df <- df.temp
    }
    else{
      df <- rbind(df, df.temp)
    }
  }
  
  if(type.temp == unique(type)[1]){
    df$type <- type.temp
    df.all <- df
  }
  else{
    df$type <- type.temp
    df.all <- rbind(df.all, df)
  }
  cat('\n' ,type.temp, ': ', Sys.time()-start.1)
}
print(Sys.time() - start.0)
stopCluster(cl)

## Plots
ggplot(data=df.all, aes(x=K, y=VI, group =  type, color = type)) +
  geom_line(alpha=0.5) +
  geom_point(alpha = 0.5) +
  facet_wrap(.~type) +
  theme_bw() 

### Compare preprocessing steps to each other
k.cand <- c(20, 60, 100)

start.0 <- Sys.time()
## start loop over types
for(k in k.cand){
  start.1 <- Sys.time()
  ### extract relevant files for type
  STM.list.K <- grep(k, STM.list, value = T)
  # load all models
  l <- foreach(i= STM.list.K) %do%{
    load(file = paste("STM_Model/",f.temp, sep = ""))  # "model"
    theta <- data.frame(model$theta)
    theta$max <- apply(theta, 1, which.max)
    theta
  }
  # possible combinations (66)
  combis <- combn(c(1:12), 2)
  # create edgelist
  for(i in 1:ncol(combis)){
    c1 <- combis[,i][1]
    c2 <- combis[,i][2]
    m1 <- l[[c1]]
    m2 <- l[[c2]]
    vi <- igraph::compare(m1$max, m2$max, method = 'vi')
    if(i == 1){
      edges <- data.frame(N1 = STM.list.K[c1], N2 = STM.list.K[c2], VI = vi)
    }
    else{
      temp <- data.frame(N1 = STM.list.K[c1], N2 = STM.list.K[c2], VI = vi)
      edges <- rbind(edges, temp)
    }
  }
  
  if(k == min(k.cand)){
    edges$K <- k
    edges.all <- edges
  }
  else{
    edges$K <- k
    edges.all <- rbind(edges.all, edges)
  }
  cat('\n' ,k, ': ', Sys.time()-start.1)
}
print(Sys.time() - start.0) 


### Heatmap 
for(k in unique(edges.all$K)){
  edges <- edges.all[edges.all$K ==k,]
  g <- graph_from_data_frame(edges, directed = F)
  V(g)$name <- gsub('Model_', '', V(g)$name)
  V(g)$name <- gsub('\\...*', '', V(g)$name)
  g.toplot <- data.frame(as_edgelist(g))
  g.toplot$VI <- E(g)$VI
  
  if(k == 20){
    g.toplot$K <- k
    g.all <- g.toplot
  }
  else{
    g.toplot$K <- k
    g.all <- rbind(g.all, g.toplot)
  }
}


ggplot(g.all, aes(X1, X2, fill = VI)) + 
  geom_tile() +
  labs(x = '', y = '') + 
  facet_wrap(.~K) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Redundancy
## Functions
create.net <- function(df_align){
  g <- graph.empty(directed = F)
  node.out <- unique(df_align$model)
  node.in <- unique(df_align$cluster)
  g <- add.vertices(g,nv=length(node.out),attr=list(name=node.out),type=rep(FALSE,length(node.out)))
  g <- add.vertices(g,nv=length(node.in),attr=list(name=node.in),type=rep(TRUE,length(node.in)))
  edge.list.vec <- as.vector(t(as.matrix(df_align[,c("model", "cluster")])))
  g <- add.edges(g,edge.list.vec)
  return(g)
}  

redundancy<-function(g){
  redundancy<-c()
  for(i in V(g)[which(V(g)$type==FALSE)]){
    overlap <- 0
    nei<-neighbors(g,i)
    if(length(nei)>1){
      comb<-combn(nei, 2)     
      for(c in seq(1:dim(comb)[2])){
        unei<-neighbors(g,comb[1,c])
        wnei<-neighbors(g,comb[2,c])
        redund<-setdiff(intersect(unei, wnei), i)
        if(length(redund)>0){
          overlap <- overlap + 1
        }
      }
    }
    if(overlap > 0){
      n <- length(nei)
      norm<-2.0/(n*(n-1))
    } else {
      norm <- 1
    }
    redundancy<-append(redundancy, overlap*norm)
  }
  return(redundancy)
}


## Start loop over types
start.0 <- Sys.time()
for(type.temp in unique(type)){
  start.1 <- Sys.time()
  ### load 'dtm.stm'
  f.out <- paste('STMProc_', type.temp,'.Rdata', sep = '')
  load(file = paste('STM_Proc/', f.out, sep = '' )) # dtm.stm
  ### extract relevant files for type
  STM.list.type <- grep(type.temp, STM.list, value = T)
  ### transform models to Mallet
  l <- foreach(i= STM.list.type) %do%{
    load(file = paste("STM_Model/",i, sep = ""))  # "model"
    m <- model
    m <- foreign_model(m, out$meta)
    m
  }
  # calculate distances and align topics
  dists <- model_distances(l, n_words=40)
  clusters <- align_topics(dists, threshold=0.1)
  # out as data frame 
  df_align <- alignment_frame(clusters)
  df_align <- as.data.frame(df_align)
  # rename models 
  df_align$model <- paste("K.", as.character((df_align$model * 10)),sep = "")
  df_align$cluster <- as.character(df_align$cluster)
  # build networks
  g <- create.net(df_align)
  redundancies <- redundancy(g)
  
  mods <- as.numeric(stringr::str_extract(names(V(g)[which(V(g)$type==FALSE)]),"\\d+$"))
  redun.df <- data.frame(redundancies = redundancies, K = mods)
  redun.df <- redun.df[order(redun.df$K),]
  
  if(type.temp == unique(type)[1]){
    redun.df$type <- type.temp
    redun.all <- redun.df
  }
  else{
    redun.df$type <- type.temp
    redun.all <- rbind(redun.all, redun.df)
  }
  cat('\n' ,type.temp, ': ', Sys.time()-start.1)
}
print(Sys.time() - start.0) 


## Plots
# * Drop $K=10$
redun.all <- redun.all[redun.all$K != 10,]

# * Line plots
ggplot(data = redun.all, aes(x = K, y = redundancies, group = type, color = type)) +
  geom_line(alpha=0.5) +
  geom_point(alpha = 0.5) +
  theme_bw() 

# * Boxplots
ggplot(redun.all, aes(x = type, y = redundancies)) + 
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y=mean, geom="point", shape=21, size=3, color = 'red', alpha = 0.5) +
  labs(x = '', y = 'Redundancy across K by PS') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))