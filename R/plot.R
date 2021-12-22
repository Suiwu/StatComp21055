library(bestsubset)
#' @title  plot function
#' @description  Plot the results for simulation
#' @param file  file list which contains simulation data
#' @param method.nums The indices of the methods.
#' @param method.names The names of the methods. 
#' @param what index for compare. It can be "error", "risk", "prop","F", or "nonzero".Default is "error".
#' @param fig.dir,file.name The figure directory and file name.
#' @param w,h The width and height. Defaults are 8 and 10
#' @param legend.pos location of legend
#' @param make.pdf save as pdf
#' @return figure or pdf
#' @export  

plot_index = function(file.list, method.nums=NULL, method.names=NULL,
                      what="error", main=NULL, ylim=NULL,legend.pos="right",
                      make.pdf=FALSE, fig.dir=".", file.name="sim",w=8, h=10) {
  
  options (warn = -1)
  # 设置要画图的方法
  sim.obj = readRDS(file.list[1])
  if (is.null(method.nums)) method.nums = 1:length(sim.obj$err.test)
  if (is.null(method.names)) method.names =
    names(sim.obj$err.test[method.nums])
  N = length(method.nums)
  
  # 设置单位元
  base.num = 0
  base.name = ifelse(what=="error","Bayes","null model")
  
  
  # y轴
  ylab = switch(what,
                error=paste0("Relative test error (to ",base.name,")"),
                risk=paste0("Relative risk (to ",base.name,")"),
                prop="Proportion of variance explained",
                F="F classification of nonzeros",
                nonzero="Number of nonzeros")
  
  # 找出所有y
  yvec = ybar= rho.vec = snr.vec = c()
  for (i in 1:length(file.list)) {
    sim.obj = readRDS(file.list[i])
    rho.vec = c(rho.vec,rep(sim.obj$rho,N))
    snr.vec = c(snr.vec,rep(sim.obj$snr,N))
    
    z = sim.obj[[switch(what,
                        error="err.test",
                        risk="risk",
                        prop="prop",
                        F="F1",
                        nonzero="nzs")]]
    res = tune.and.aggregate(sim.obj, z)
    
    # prop, F  and nonzero 没有相对指标
    if (what=="prop" || what=="F" || what=="nonzero") {
      yvec = c(yvec,res[[paste0("z.",substr("validation",1,3),".","ave")]][method.nums])
      ybar = c(ybar,res[[paste0("z.",substr("validation",1,3),".","std")]][method.nums])
    }
    
    # 画出相对error
    else {
      # First build the relative metric
      met = res[[paste0("z.",substr("validation",1,3))]]#[method.nums]
      if (base.num == 0 && what=="error") denom = sim.obj$sigma^2
      else if (base.num == 0 && what=="risk") denom = sim.obj$risk.null
      else denom = met[[base.num]]
      z.rel = lapply(met, function(v) v / denom)
      # Now aggregate the relative metric
      res2 = tune.and.aggregate(sim.obj, z.rel, tune=FALSE)
      yvec = c(yvec,unlist(res2[[paste0("z.","ave")]])[method.nums])
      ybar = c(ybar,unlist(res2[[paste0("z.","std")]])[method.nums])
    }
  }
  # x 轴标注
  xvec = snr.vec
  xlab = "Signal-to-noise ratio"
  
  # y轴范围
  if (is.null(ylim)) ylim = range(yvec-ybar, yvec+ybar)
  # Produce the plot
  rho.vec = factor(rho.vec)
  snr.vec = factor(snr.vec)
  levels(rho.vec) = paste("Correlation", levels(rho.vec))
  
  dat = data.frame(x=xvec, y=yvec, se=ybar, rho=rho.vec, snr=snr.vec,
                   Method=factor(rep(method.names, length=length(xvec))))
  
  gp = ggplot(dat, aes(x=x,y=y,color=Method)) +
    xlab(xlab) + ylab(ylab) + coord_cartesian(ylim=ylim) +
    geom_line(lwd=1) + geom_point(pch=19) +
    theme_bw() + theme(legend.pos=legend.pos)
  
  if (what=="error") gp = gp + geom_line(aes(x=x, y=1+x), lwd=0.5,
                                         linetype=3, color="black")
  if (what=="prop") gp = gp + geom_line(aes(x=x, y=x/(1+x)), lwd=0.5,
                                        linetype=3, color="black")
  if (what =="nonzero") gp = gp + geom_line(aes(x=x, y=sim.obj$s), lwd=0.5,
                                            linetype=3, color="black")
  if (!is.null(main)) gp = gp + ggtitle(main)
  if (!is.null(ylim)) gp = gp + coord_cartesian(ylim=ylim)
  if (make.pdf) {
    gp
    ggsave(sprintf("%s/%s.pdf",fig.dir,file.name),
           height=h, width=w, device="pdf")
  }
  else gp
}

get.stem = function(str.vec) {
  str.list = strsplit(str.vec, split="\\.")
  k = 0
  while (TRUE) {
    vec = c()
    for (i in 1:length(str.list)) {
      if (length(str.list[[i]]) < k+1) break
      vec = c(vec, str.list[[i]][k+1])
    }
    if (length(vec) < length(str.list)) break
    if (length(unique(vec)) > 1) break
    k = k+1
  }
  if (k == 0) stem = "foo"
  else stem = paste0(str.list[[1]][1:k], collapse=".")
  return(stem)
}
