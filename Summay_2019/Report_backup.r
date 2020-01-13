### 1. init environment
{
  library(xlsx) # Reading xlsx
  library(RColorBrewer) # Select color set
  library(scales) # adjust color transparent
  library(igraph) # network graph
  options(stringsAsFactors = FALSE)
  
  # set global variable
  fileName = "Data/2019_activities.xlsx"
  dataDir = "Data"
  networkDir = "Interaction"
  networkTableDir = "Interaction_table"
  graphDir = "Figure"
  # display.brewer.all()  # show all color panel
  colours = RColorBrewer::brewer.pal(12,"Set3")
  
  # create dir
  suppressWarnings(dir.create(networkDir))
  suppressWarnings(dir.create(networkTableDir))
  suppressWarnings(dir.create(graphDir))
}

### 2. Prepare Data
# 2.1 read data
{
  rawData = lapply(12:1, function(i){
    raw = read.xlsx2(file=fileName, sheetIndex=i, header=T)
    f1 = apply(raw, 2, function(s){mean(s=="")}) # remove invalid column
    raw = raw[1:60, f1!=1,drop=FALSE]
    # assign each month a unique color
    col = rep(colours[i],ncol(raw))
    list(raw=raw, col=col)
  })
  data = do.call(cbind, lapply(rawData, function(cl){cl$raw}))
  col = do.call(c, lapply(rawData, function(cl){cl$col}))
  # filter row
  f2 = apply(data, 1, function(s){mean(s=="")})
  data = data[f2 != 1, ]
  # correct column name
  colnames(data) = sapply(colnames(data), function(s){
    unlist(strsplit(s,split="2019"))[2]
  })
}

# 2.2 explore data
View(data)

# 2.3 corrected person name
{
  data[data=="Joy"] = "joy"
  data[data=="YanshuZhang"] = "yanshu"
  data[data=="YanshuzhangDF"] = "yanshu"
  data[data=="黄婉Ency"] = "黄婉"
  data[data=="木禾伊一"] = "尹一"
  data[data=="木禾尹一"] = "尹一"
  data[data=="Sherry"] = "sherry"
}

# save data
save(data, file="Data/data.rData")
write.xlsx(data, file="Data/data.xlsx")

### 3. Activities summary
v = unlist(data); v=v[v!=""]  # 总活动人数
Numbers = apply(data, 2, function(s){sum(s!="")})  #每月活动人数
# 3.1 summary
{
  jpeg(paste(graphDir,"Summary.jpeg",sep="/"), height=1680, width=960, quality=100)
  par(mai = c(0.42,3,0.42,0.42))
  a = barplot(Numbers, main=sprintf("1~12月\n%d次活动；%d人; %d人次",
                                    ncol(data),length(unique(v)),sum(Numbers)),
              xlab="人数", las=2, col=col, horiz=TRUE, cex.lab=0.2, xlim=c(0,40))
  text(y=a, x=Numbers, labels = Numbers, col="black", adj=c(1,0.5))
  text(y=a, x=Numbers+2, labels=data[1,])
  dev.off()
}

# 3.2 Region
{
  location = colnames(data)
  shenzhen = grep(pattern="[塘莲羊梅梧峦蛇娘湾明银迎看澜冲沙鹏]", location)
  shenzhen = c(shenzhen, grep("东湖",location))
  shenzhen = c(shenzhen, 7, 41,91, 17) #南山，罗湿5号绿避
  shenzhen = setdiff(shenzhen,39) # remove 西湾露营
  hongkong = c(grep(pattern="[蒲麦丫径涧元凤]", location),grep("大东屿",location))
  hongkong = c(hongkong,39)
  remote = setdiff(1:length(location),c(shenzhen,hongkong))
  x = c(深圳=length(shenzhen), 香港=length(hongkong), 远方=length(remote))
  a = barplot(x,horiz=FALSE, ylab="组织次数", las=1, ylim=c(0,75))
  text(y=x, x=a, labels=x, pos=3)
}

# 3.3 sub Region-shenzhen
{
  shenzhen = location[shenzhen]
  
  meihou = shenzhen[grep("梅.?后",shenzhen)]
  tanglang = shenzhen[grep("塘",shenzhen)]
  wutong_luohu = shenzhen[grep("[梧|罗]",shenzhen)]
  wutong_luohu_donghu = c(wutong_luohu, shenzhen[grep("东湖", shenzhen)])
  maluanshan = shenzhen[grep("马",shenzhen)]
  yangtai = shenzhen[grep("羊",shenzhen)]
  nanshan = shenzhen[grep("南",shenzhen)]
  yinhu = shenzhen[grep("银",shenzhen)]
  dongxichong = shenzhen[grep("东西",shenzhen)]
  shenzhenwan = shenzhen[grep("湾",shenzhen)]
  shenzhen.sub = list(梅林后山=meihou, 塘朗=tanglang, 梧桐_罗湖_东湖=wutong_luohu_donghu,
                          马峦山=maluanshan,羊台=yangtai, 南山=nanshan, 银湖=yinhu,
                          东西冲=dongxichong, 深圳湾=shenzhenwan)
  shenzhen.other = setdiff(shenzhen, unlist(shenzhen.sub))
  
  shenzhen.sub.len = sapply(shenzhen.sub, length)
  shenzhen.sub.len = sort(shenzhen.sub.len, decreasing = TRUE)
  
  shenzhen.sub.len = c(其他=length(shenzhen.other), shenzhen.sub.len[length(shenzhen.sub.len):1])
}
# 3.3 sub Region-hongkong
{
  hongkong = location[hongkong]
  maijing = hongkong[grep("[麦|湾|城]",hongkong)]
  fenghuang = hongkong[grep("[东|凤]",hongkong)]
  hongkong.sub = list(麦理浩径=maijing, 凤凰径=fenghuang)
  hongkong.other = setdiff(hongkong, unlist(hongkong.sub))
  
  hongkong.sub.len = sapply(hongkong.sub, length)
  hongkong.sub.len = sort(hongkong.sub.len, decreasing = TRUE)
  
  hongkong.sub.len = c(其他2=length(hongkong.other), hongkong.sub.len[length(hongkong.sub.len):1])
  
}

# 3.4 作图
{
  jpeg(paste(graphDir,"subregion.jpeg",sep="/"),quality=100)
  subregion = c(hongkong.sub.len, shenzhen.sub.len)
  par(mai=c(0.82,2,0.42,0.42))
  col = rep(c(alpha("blue",0.5), alpha("red",0.5)), c(length(hongkong.sub.len),length(shenzhen.sub.len)))
  a = barplot(subregion, horiz = TRUE,las=1, xlim=c(0,14), xlab="活动次数", col=col)
  text(y=a,x=subregion, pos=4, labels = subregion)
  dev.off()
}

# 3.5
{
  remote = location[remote]
  remote = sapply(remote, function(s){unlist(strsplit(s,split="_"))[2]})
  remote = as.data.frame(remote)
  remote = cbind(remote, class="")
  # remote.edit = edit(remote.edit)
  # save(remote.edit, file="remote.rData")
  load("Data/remote.rData")
  
  size = table(remote.edit[,"province"])
  x = rnorm(8, sd=4)
  y = rnorm(8, sd=4)
  col = sample(colours, size=10,replace = FALSE)
  plot(x,y,col=col,pch=16,cex=8, xlim=c(-10,10),ylim=c(-15,10), xant=FALSE, yant=FALSE)
  text(x,y,labels = sprintf("%s\n%s次?",names(size),size))
}

### 4. Person Summary
# 4.1 person's activities 统计
{
  freq = sort(table(v), decreasing=TRUE)
  result = lapply(names(freq), function(s){
    flag = apply(data, 2, function(x){
      s %in% x
    })
    act = colnames(data)[flag]
    act = paste(act, collapse = "; ")
    act
  })
  result = unlist(result)
  names(result) = names(freq)
}
# 4.1 person's activities 作图
{
  # 作图
  freq.v = freq[length(freq):1]
  names(freq.v) = paste(length(freq.v):1, names(freq.v), sep="  ")
  jpeg("Figure/personActivities.jpeg", height=8000, width=1024, quality=100)
  par(mai = c(0,2,0,3))
  b = barplot(freq.v, ylab="times", las=2,
              cex.lab=0.2, xlim=c(0,80), horiz=TRUE)
  text(y=b, x=freq.v, labels = freq.v, col="black",adj=c(1,0.5))
  text(y=b, x=freq.v+2, labels=result[length(result):1], pos=4, cex=0.9)
  dev.off()
}

### 5. Network
test_data = data
# 5.1 generate node and edges
{
  getEdge = function(cl,event=""){
    # cl = test_data[,1]
    cl = cl[cl != ""]
    edge = sapply(1:length(cl), function(i){
      a = cl[-i]
      paste(cl[i],a,event, sep="%")
    })
    edge = as.vector(edge)
    edge
  }
  
  edges = lapply(1:ncol(test_data),function(i){
    getEdge(test_data[,i], event=colnames(test_data)[i])
  })
  edges = unlist(edges)
  Graph = sapply(edges, function(s){
    unlist(strsplit(s, split="%"))
  })
  Graph = t(Graph)
  # index = which(sapply(Graph, length) != 3)
  # Graph = Graph[-index]
  # Graph = do.call(rbind, Graph)
  rownames(Graph)=1:nrow(Graph)
  colnames(Graph)=c("source","target","event")
}
write.xlsx(Graph, file="Data/Graph.xlsx")

# 5.2 compute degree of each pair

# 5.3 each person degree
person.freq = table(Graph[,1])
person.freq = sort(person.freq, decreasing = TRUE)
barplot(person.freq[1:30], horiz=TRUE, las=2)

# save data
stat.df = as.data.frame(result)
stat.df = cbind(name=rownames(stat.df), 活动=stat.df, 活动次数=freq[rownames(stat.df)],
                同行人数=person.freq[rownames(stat.df)])
stat.df = cbind(stat.df, 
  关系图=sprintf("https://github.com/HikingEveryWeek/Activities/tree/master/Summay_2019/Interaction/%s.jpeg",rownames(stat.df)),
  关系表=sprintf("https://github.com/HikingEveryWeek/Activities/tree/master/Summay_019/Interaction_table/%s.txt",rownames(stat.df))
  )
stat.df.1 = stat.df[,c(1,2,4,6,7,8)]
colnames(stat.df.1) = c("名字","活动","活动次数","同行人数","同行人图片","同行人表格")
write.xlsx(stat.df.1, file="Data/personStat.xlsx")

getSubset = function(name){
  subset = Graph[Graph[,1]==name,]
  freq = table(subset[,2])
  subset = data.frame(source=subset[1,1],target=names(freq), time=as.numeric(freq))
  subset = subset[order(subset[,"time"],decreasing = TRUE),]
  subset
}

# 5.4 plot network
for (i in 1:577){
  name = names(freq)[i]
  subset = getSubset(name)
  write.table(subset, file=sprintf("Interaction_table/%s.txt", name), sep="\t",row.names=FALSE)
  
  flag = ifelse(nrow(subset)>100,100,nrow(subset))
  g = graph_from_data_frame(subset[1:flag,], directed = FALSE)
  
  jpeg(file=sprintf("Interaction/%s.jpeg",name,nrow(subset),sum(subset[,"time"]>1)),
       width=1024,height=1024)
  main = sprintf("%s has hiked with %s person\nAnd %s person beyond 2 times",name,nrow(subset),
                 sum(subset[,"time"]>1))
  plot.igraph(g, vertex.size=1, edge.width=subset[1:100,3],edge.label=subset[,"time"],
              shape="circle", label.font=2, label.cex=15, layout=layout_as_star, label.dist=1,
              main=main)
  dev.off()
}

