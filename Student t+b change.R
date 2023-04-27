#### 我们假设时间为100，节点都是整数，时间尺度=t ####
#### 我们按λ=seq（1，100）进行仿真 ####
library(Runuran)
library(MASS)
library(PearsonDS) #这个包中Type VII的表达式有问题，但程序没问题
library(readxl)
library(pracma)
library(ggplot2)
library(ggpubr)


########################### 数据处理 #################################
#degradation_06 = Battery_06-rep(Battery_06[1],length(Battery_06))
#plot(seq(1,168),-degradation_06,type= 'l')
#points(seq(1,168),-degradation_06,pch=16,cex=0.5)
#plot(seq(1,168),degradation_06)

#par(mar=c(4,3,4,1),mgp=c(1.8,0.6,0))
#plot(seq(1,202,2),Bearing_data1,type='l',lwd=1,ylab = "Amplitude",xlab = "Minutes")
#plot(seq(1,202,2),Bearing_data1,type='l')
#lines(seq(1,202,2),miu*t)

#plot(seq(1,274,2),Bearing_data2,type='l')
#plot(seq(1,262,2),Bearing_data3,type='l',lty=2,xlab="Time (Minutes)",ylab='Amplitude (Vrms)')
#lines(seq(0,270,0.1),8.39e-06*seq(0,270,0.1)^1.447)
#lines(seq(0,270,0.1),5.38e-07*seq(0,270,0.1)^1.943,col='red')
#lines(seq(0,270,0.1),7.4e-07*seq(0,270,0.1)^1.886,col='blue')
#lines(seq(0,270,0.1),8.34e-06*seq(0,270,0.1)^1.449)
#lines(seq(0,270,0.1),7.63e-07*seq(0,270,0.1)^1.88,col='yellow')
#points(seq(1,262,2),Bearing_data3,pch=16,cex=0.6)
#lines(seq(1,262,2),5e-05*seq(1,262,2))

#trans_bearing = Bearing_data3[6:131]-Bearing_data3[5]
#trans_bearing = log(trans_bearing)
#trans_bearing = Bearing_data3[7:131]-Bearing_data3[6]
#trans_bearing = log(trans_bearing)
#plot(seq(2,2*125,2),trans_bearing,type='l',lty=2,xlab="Circles",ylab="Degradation")
#is.na(trans_bearing)
#trans_bearing = trans_bearing-trans_bearing[1]
#plot(seq(1,125),trans_bearing,type='l')

#Bearing_data3
#plot(seq(1,168),-degradation_06,type='l')
#lines(seq(1,168),0.005*seq(1,168))


################### t分布 ####################
t_function = function(para){
  ### 生成初始参数
  beta = -7#生成初始阶段
  miu=0.1 #生成μ
  m=0.1 #生成m
  sigma=0.1 #生成σ
  v=2 #生成v
  z = 2 #EM迭代计数
  b = c(0.5,0.01,2) #时间尺度参数
  N = 125 #总仿真个数
  max_iter = 4000 #最多迭代次数
  p=1
  weight = rbinom(N,1,p)
  #t=seq(2,2*N,2)^b #生成初始时间点
  t=b[1]*exp(b[2]*seq(2,2*N,2))+b[3]
  
  ### 生成仿真序列
  delta_x = rep(NA,N)
  for (i in 1:N){ 
    if (i == 1){
      delta_x[i] = rnorm(1,mean = miu*t[i],sd = m*sqrt(t[i]))
    }else{
      delta_x[i] = rnorm(1,mean = miu*(t[i]-t[i-1]),sd = m*sqrt(t[i]-t[i-1]))
    }
  }
  x = cumsum(delta_x)+beta
  Y = matrix(x+weight*rpearsonVII(n=N,df=v,location=0,scale=sigma)+(rep(1,N)-weight)*rnorm(n=N,mean=0,sd=sigma),nrow = 1,ncol = N)
  
  ### 引入实际数据
  #Y = -t(degradation_06)
  #Y = t(Bearing_data3)
  #Y = t(HDD_data)
  #Y = t(trans_bearing[1:N])
  Y = t(teacher_bearing_data)
  #plot(seq(2,2*N,2),Y,type='l')
  #lines(seq(2,2*N,2),miu*t+beta)
  
  
  
  ### 加入随机误差
  #outlier = round(runif(10,min=1,max=200))
  #outlier = seq(100,110)
  #Y[outlier] = Y[outlier]+seq(20,120,10)
  
  
  history_beta = rep(0,max_iter)
  history_beta[1] = beta
  history_miu = rep(0,max_iter)
  history_miu[1] = miu
  history_sigma = rep(0,max_iter)
  history_sigma[1] = sigma
  history_m = rep(0,max_iter)
  history_m[1] = m
  history_v = rep(0,max_iter)
  history_v[1] = v
  history_b = rep(0,max_iter)
  history_b[1] = b
  #change = sqrt(max((history_miu[z]-history_miu[z-1])^2,(history_sigma[z]-history_sigma[z-1])^2,(history_m[z]-history_m[z-1]))^2)
  
  
  ### 生成初始多元高斯
  q0_lambda_expect = 1 #rgamma(1,shape = (v+1)/2,scale = 2/(v+1))的期望
  D0 = diag(rep(1/q0_lambda_expect,N)) #生成对角阵D
  P = matrix(data = NA,ncol=N,nrow=N)
  for (i in 1:N){ #生成矩阵P0
    for (j in 1:N){
      if (i<j){
        P[i,j]=b[1]*exp(b[2]*2*i)+b[3]
      }
      else{
        P[i,j]=b[1]*exp(b[2]*2*j)+b[3]
      }
    }
  }
  #diag(diag(solve(D0))/(sigma^2))
  multi_var = solve(solve(D0)/rep((sigma^2),N)+(solve(P)/(m^2))) #计算多元高斯方差矩阵
  multi_miu = t(c(beta,rep(beta,N-1)))+miu*t+(Y-miu*t-t(c(beta,rep(beta,N-1))))%*%(solve(D0)/rep((sigma^2),N))%*%multi_var #计算多元高斯均值
  history_gamma_beta = matrix(data = 0, ncol = max_iter, nrow = N) #存放历史gamma的β
  history_multi_para = matrix(data = 0, ncol = max_iter, nrow = N) #存放历史多元高斯系数
  history_gamma_beta[,1] = rep((v+1)/2,N)
  history_multi_para[,1] = t(multi_miu)
  
  ### 计算初始完整似然
  ### 计算E(Inλi)
  log_expect = sum(digamma((v+1)/2)-log((v+1)/2)) 
  
  ### 计算E[(Δx-μΔ)^2]
  temp_three = sum(((multi_miu-c(beta,multi_miu[1:(N-1)]))-miu*(t-c(0,t[1:(N-1)])))^2/(t-c(0,t[1:(N-1)])))+sum(diag(multi_var%*%solve(P)))
 
  ### 生成E(ε^2λ)
  temp_four = matrix(data=rep(1,N),nrow=1)%*%(t(Y^2)-2*diag(as.vector(Y))%*%t(multi_miu)+diag(multi_var)+t(multi_miu)^2)
  
  complete_likelihood = -(N/2)*log(2*pi*m^2)-1/2*sum(log(t-c(0,t[1:(N-1)])))-(1/(2*m^2))*temp_three-0.5*N*log(2*pi*sigma^2)+((v+1)/2-1)*log_expect-(temp_four/(2*sigma^2))+(v*N/2)*log(v/2)-N*log(gamma(v/2))-(v/2)*N
  history_complete_likelihood = rep(0,max_iter)
  history_complete_likelihood[1] = complete_likelihood
  
  ### 计算初始对数似然
  ### 计算(E(ε^2)/2sigma^2+v/2)E[λ]
  temp_six = matrix(data=1,nrow=1,ncol=N)%*%((t(Y^2)-2*diag(as.vector(Y))%*%t(multi_miu)+diag(multi_var)+t(multi_miu)^2)/(2*rep(sigma^2,N))+rep(v/2,N))
  
  ### 计算det(var)所需的参数
  if (b < 1){
    c = -1/N*sum(log(t-c(0,t[1:(N-1)])))
  }else if (b>1){
    c = 1/N*sum(log(t-c(0,t[1:(N-1)])))
  }else{
    c = 1
  }
  log_likelihood = -(N/2)*log(2*pi*m^2)-1/2*sum(log(t-c(0,t[1:(N-1)])))-(1/(2*m^2))*temp_three-(N/2)*log(2*pi*sigma^2)-temp_six+(N*v/2)*log(v/2)-N*log(gamma(v/2))-N*((v+1)/2)*log((v+1)/2)+N*log(gamma((v+1)/2))+N*(v+1)/2+N/2*log(2*pi)+(1/2)*(log(det(solve(solve(P/c)+solve(D0/c)/rep((sigma^2/m^2),N))))+N*log(c*m^2))+N/2
  
  
  history_log_likelihood = rep(0,max_iter)
  history_log_likelihood[1] = log_likelihood
  
  
  # 核心算法循环
  while (z<=max_iter){
    if(max(abs(history_miu[z]-history_miu[z-1]),abs(history_sigma[z]-history_sigma[z-1])^2,abs(history_m[z]-history_m[z-1])^2,abs(history_v[z]-history_v[z-1])) < 1e-06){
    #if(max(abs(history_miu[z]-history_miu[z-1])/history_miu[z-1],abs(history_sigma[z]-history_sigma[z-1])^2/(history_sigma[z-1]^2),abs(history_m[z]-history_m[z-1])^2/(history_m[z-1]^2),abs(history_v[z]-history_v[z-1])/history_v[z-1])<=0.005){
      break
    }
    for (L in 1:2){
      if (L == 1){
        z = z+1
        
        
        ##########  Variational Bayesian Method  ###########
        k = 2 #Iteration counts
        gamma_para = matrix(data=NA, ncol=2, nrow=N)
        while (k<=(max_iter-1)){
          cond_VB = max(abs(history_multi_para[,k]-history_multi_para[,k-1]))<1e-06
          if (cond_VB){
            break
          }
          k = k+1
          ### New q(λ|Y)
          temp_one = 0
          for (i in 1:N){
            temp_one = Y[i]^2-2*Y[i]*multi_miu[i]+multi_miu[i]^2+multi_var[i,i]
            gamma_para[i,1] = (v+1)/2
            gamma_para[i,2] = (temp_one/(2*sigma^2))+v/2
          }
          history_gamma_beta[,k] = gamma_para[,2]
          
          ### New q(X|Y)
          q_lambda_expect = gamma_para[,1]/gamma_para[,2]
          #View(q_lambda_expect)
          D = diag(1/q_lambda_expect)
          multi_var = solve(solve(D)/rep((sigma^2),N)+(solve(P)/(m^2)))
          multi_miu = t(rep(beta,N))+miu*t+(Y-miu*t-t(rep(beta,N)))%*%(solve(D)/rep((sigma^2),N))%*%multi_var 
          
          history_multi_para[,k] = t(multi_miu)
          #View(gamma_para)
          #View(history_gamma_beta)
        }
        
        
        ##########  EM algorithm   ###########
        ### Update beta
        beta = multi_miu[1]-miu*t[1]
        
        ### Update miu
        miu = (multi_miu[N]-beta)/t[N]
        
        ### Update m
        ### calcultae E[(Δx-μΔ)^2]
        temp_three = sum(((multi_miu-c(beta,multi_miu[1:(N-1)]))-miu*(t-c(0,t[1:(N-1)])))^2/(t-c(0,t[1:(N-1)])))+sum(diag(multi_var%*%solve(P)))
        m = sqrt((1/N)*temp_three)
        #View(m^2)
        
        ### Update σ
        ### calculate E[ε^2λ]
        temp_four = (Y^2-2*Y%*%diag(array(multi_miu))+t(diag(multi_var))+multi_miu^2)%*%(gamma_para[,1]/gamma_para[,2])
        sigma = sqrt(temp_four/N)
        #View(sigma^2)
        
        
        ### 直接更新v,发现使用牛顿法的结果在2-3代之后与加速EM基本相同,初始间隔不能离零点过远
        #search_v = function(v_test){
        #log_expect_test = sum(rep(digamma((v_test+1)/2),100)-log(gamma_para[,2]))
        # return((100/2)*log(v_test/2)+50-(100/2)*digamma(v_test/2)+(1/2)*log_expect_test-(1/2)*sum(q_lambda_expect))
        # }
        # v_alter = uniroot(search_v,interval = c(1,2),extendInt="yes")$root
        
        ### 尝试直接用fminsearch而不是寻找导数0点
        #log_expect_test = sum(rep(digamma((v+1)/2),100)-log(gamma_para[,2]))
       # search_v = function(v_test){
       #   return((N/2)*v_test*log(v_test/2)+(v_test/2-1)*log_expect_test-(v_test/2)*sum(q_lambda_expect))-N*log(gamma(v_test/2))
       # }
       # v_alter = optimize(search_v,interval = c(0.01,5))
        #N/2*log(v_alter$minimum/2)+N/2+1/2*sum(rep(digamma((v_alter$minimum+1)/2),100)-log(gamma_para[,2]))-1/2*sum(q_lambda_expect)-N/2*digamma(v_alter$minimum/2)
        #N/2*log(v/2)+N/2+1/2*sum(rep(digamma((v+1)/2),100)-log(gamma_para[,2]))-1/2*sum(q_lambda_expect)-N/2*digamma(v/2)
        
        
        history_beta[z] = beta
        history_miu[z] = miu
        history_m[z] = m
        history_sigma[z] = sigma
      }
      else{
        ##########  变分阶段   ###########
        k = 2 #VBA迭代计数
        gamma_para = matrix(data=NA, ncol=2, nrow=N)
        while (k<=(max_iter-1)){
          cond_VB = max(abs(history_multi_para[,k]-history_multi_para[,k-1])/abs(history_multi_para[,k-1]))<1e-06
          if (cond_VB){
            break
          }
          k = k+1
          ### 生成新的q(λ|Y)
          temp_one = 0
          for (i in 1:N){ #生成q(λ)α和β
            temp_one = Y[i]^2-2*Y[i]*multi_miu[i]+multi_miu[i]^2+multi_var[i,i]
            gamma_para[i,1] = (v+1)/2
            gamma_para[i,2] = (temp_one/(2*sigma^2))+v/2
          }
          history_gamma_beta[,k] = gamma_para[,2]
          
          ### 生成新的q(X|Y)
          q_lambda_expect = gamma_para[,1]/gamma_para[,2]
          #View(q_lambda_expect)
          D = diag(1/q_lambda_expect)
          multi_var = solve(solve(D)/rep((sigma^2),N)+(solve(P)/(m^2)))#计算多元高斯方差矩阵
          multi_miu = t(rep(beta,N))+miu*t+(Y-miu*t-t(rep(beta,N)))%*%(solve(D)/rep((sigma^2),N))%*%multi_var #计算多元高斯均值
          history_multi_para[,k] = t(multi_miu)
          #View(gamma_para)
          #View(history_gamma_beta)
        }
        
        
        ##########  加速EM阶段   ###########
        a = 2
        iter_v = matrix(data=0,ncol = 100,nrow = 1)
        iter_v[1] = v
        vL = 0.01 #设置v的下界
        vU = 100 #设置v的上界
        temp_five = (Y^2-2*Y%*%diag(array(multi_miu))+t(diag(multi_var))+multi_miu^2)/(rep(sigma^2,N))
        while(a<100 & abs(iter_v[a]-iter_v[a-1])>1e-06){
          a=a+1
          H_first_deria = log(v/2)+1-digamma(v/2)+digamma((v+1)/2)-mean(log(temp_five/2+rep(v/2,N)))-((v+1))*mean(1/(temp_five+rep(v,N)))
          H_second_deria = 1/v-1/2*trigamma(v/2)+1/2*trigamma((v+1)/2)-mean(2/(temp_five+rep(v,N)))+((v+1))*mean(1/(temp_five+rep(v,N))^2)
          v=v-H_first_deria/H_second_deria
          v = min(max(v,vL),vU);
          iter_v[a] = v
        }
        history_v[z] = v
        
        ### 尝试直接用fminsearch而不是寻找导数0点
        #search_v = function(v_test){
          #return((N/2)*v_test*log(v_test/2)-N*log(gamma(v/2))+N*log(gamma((v+1)/2))-(v+1)/2*sum(log(temp_five/2+rep(v,N)/2)))
        #}
        #v_alter = optimize(search_v,interval = c(100,200),maximum = TRUE)$maximum
      }
    } #1：2的结尾
    
    
    ##########  计算完整似然   ###########
    ### 计算E(Inλi)
    log_expect = sum(digamma((v+1)/2)-log(gamma_para[,2])) 
    
    ### 计算E[(Δx-μΔ)^2]
    temp_three = sum(((multi_miu-c(beta,multi_miu[1:(N-1)]))-miu*(t-c(0,t[1:(N-1)])))^2/(t-c(0,t[1:(N-1)])))+sum(diag(multi_var%*%solve(P)))
    
    ### 生成E(ε^2λ)
    temp_four = matrix(data=(gamma_para[,1]/gamma_para[,2]),nrow=1)%*%(t(Y)^2-2*diag(as.vector(Y))%*%t(multi_miu)+diag(multi_var)+t(multi_miu)^2)
    
    complete_likelihood = -(N/2)*log(m^2)-1/2*sum(log(t-c(0,t[1:(N-1)])))-(1/(2*m^2))*temp_three-0.5*N*log(sigma^2)+((v+1)/2-1)*log_expect-(temp_four/(2*sigma^2))+(v*N/2)*log(v/2)-(v/2)*sum(gamma_para[,1]/gamma_para[,2])-N*log(gamma(v/2))
    history_complete_likelihood[z] = complete_likelihood
    
    
    ##########  计算对数似然   ###########
    ### 计算(E(ε^2)/2sigma^2+v/2)E[λ]
    temp_six = matrix(data=(gamma_para[,1]/gamma_para[,2]),nrow=1)%*%((t(Y)^2-2*diag(as.vector(Y))%*%t(multi_miu)+diag(multi_var)+t(multi_miu)^2)/(2*rep(sigma^2,N))+rep(v/2,N))

    ### 计算det(var)所需的参数
    if (b < 1){
      c = -1/N*sum(log(t-c(0,t[1:(N-1)])))
    }else if (b>1){
      c = 1/N*sum(log(t-c(0,t[1:(N-1)])))
    }else{
      c = 1
    }
    
    log_likelihood = -(N/2)*log(2*pi*m^2)-1/2*sum(log(t-c(0,t[1:(N-1)])))-(1/(2*m^2))*temp_three-(N/2)*log(2*pi*sigma^2)-temp_six+(N*v/2)*log(v/2)-N*log(gamma(v/2))+(v+1)/2*sum(log(gamma_para[,1]/gamma_para[,2]))-N*((v+1)/2)*log((v+1)/2)+N*log(gamma((v+1)/2))+N*(v+1)/2+N/2*log(2*pi)+(1/2)*(log(det(solve(solve(P/c)+solve(D/c)/rep((sigma^2/m^2),N))))+N*log(c*m^2))+N/2
    #View(log_likelihood)-(4*N)*log(10))
    #det(solve(solve(P/c)+solve(D/c)/rep((sigma^2/m^2),N)))
    ### 计算用v_alter的对数似然
    #log_likelihood_test = -(100/2)*log(2*pi*m^2)-1/2*sum(log(t-c(0,t[1:99])))-(1/(2*m^2))*temp_three-(100/2)*log(2*pi*sigma^2)-temp_six+(100*v_alter/2)*log(v_alter/2)-100*log(gamma(v_alter/2))+(v_alter+1)/2*sum(log(q_lambda_expect))-100*((v_alter+1)/2)*log((v_alter+1)/2)+100*log(gamma((v_alter+1)/2))+100*(v_alter+1)/2+100/2*log(2*pi)+(1/2)*log(det(multi_var))+(1/2*100)
    history_log_likelihood[z] = log_likelihood
    
    
    ##########  更新时间尺度参数b   ###########
    function(){
      solve_b = function(b_test){
        # 计算Δlambda`
        lambda_deriv = rep(NA,N)
        t_test = seq(1,N)^b_test
        for (i in 1:N){
          if (i == 1){
            lambda_deriv[i] = 0
          }else{
            lambda_deriv[i] = log(i)*(i^b_test)-log(i-1)*(i^b_test)
          }
        }
        
        # 计算P/b
        P_deriv = matrix(data = 0, nrow = N,ncol = N)
        for (i in 1:N){
          if (i == 1){
            P_deriv[i,i] = -(lambda_deriv[i+1]/((t_test[i+1]-t_test[i])^2))
            P_deriv[i,i+1] = lambda_deriv[i+1]/((t_test[i+1]-t_test[i])^2)
          }else if(i == N){
            P_deriv[i,i-1] = lambda_deriv[i]/((t_test[i]-t_test[i-1])^2)
            P_deriv[i,i] = -(lambda_deriv[i]/((t_test[i]-t_test[i-1])^2))
          }else{
            P_deriv[i,i-1] = lambda_deriv[i]/((t_test[i]-t_test[i-1])^2)
            P_deriv[i,i] = -(lambda_deriv[i]/((t_test[i]-t_test[i-1])^2))-(lambda_deriv[i+1]/((t_test[i+1]-t_test[i])^2))
            P_deriv[i,i+1] = lambda_deriv[i+1]/((t_test[i+1]-t_test[i])^2)
          }
        }
        
        return(-1/2*sum(lambda_deriv/(t_test-c(0,t_test[1:(N-1)])))-1/(2*m^2)*(sum(diag(multi_var%*%P_deriv))-((multi_miu-c(0,multi_miu[1:(N-1)]))^2/(t_test-c(0,t_test[1:(N-1)]))^2-rep(miu^2,N))%*%lambda_deriv))
      }
      b = uniroot(solve_b,interval = c(1,3),extendInt="yes")$root
      
      ### 更新时间点
      t=seq(1,N)^b
      
      ### 更新矩阵P
      P = matrix(data = NA,ncol=N,nrow=N)
      for (i in 1:N){ #生成矩阵P0
        for (j in 1:N){
          if (i<j){
            P[i,j]=i^b
          }
          else{
            P[i,j]=j^b
          }
        }
      }
      
      ### 更新multi_miu,multi_var
      multi_var = solve(solve(D)/rep((sigma^2),N)+(solve(P)/(m^2)))#计算多元高斯方差矩阵
      multi_miu = miu*t+(Y-miu*t)%*%(solve(D)/rep((sigma^2),N))%*%multi_var #计算多元高斯均值
      history_b[z] = b
    }
    
      
    
    
    #### 这里使用power-law time scale t^b
    function(){
      solve_b_test = function(b_test){
        t_test = (b_test[1]*b_test[2]*seq(2,2*N,2))^(b_test[2]-1)
        ### 计算第一项
        temp_five = multi_miu^2+c(beta,multi_miu[1:(N-1)])^2-2*multi_miu*c(beta,multi_miu[1:(N-1)])
        term_one = sum(temp_five/(t_test-c(0,t_test[1:(N-1)])))
        ### 计算第二项
        temp_six = diag(multi_var)+c(0,diag(multi_var)[1:(N-1)])-2*c(0,diag(multi_var[-nrow(multi_var),-1]))
        term_two = sum(temp_six/(t_test-c(0,t_test[1:(N-1)])))
        return(-(-1/2*sum(log(t_test-c(0,t_test[1:(N-1)])))-1/(2*m^2)*miu^2*t_test[N]-1/(2*m^2)*(term_one+term_two)))
      }
      b = optimize(solve_b_power,c(0.001,1),maximum = TRUE)$maximum
      b_odd = b
      b = optim(c(b_odd[1],b_odd[2]),solve_b_test)$par
      
      ### 更新时间点
      t=(b[1]*b[2]*seq(2,2*N,2))^(b[2]-1)
      
      ### 更新矩阵P
      P = matrix(data = NA,ncol=N,nrow=N)
      for (i in 1:N){ #生成矩阵P0
        for (j in 1:N){
          if (i<j){
            P[i,j]=(b[1]*b[2]*2*i)^(b[2]-1)
          }
          else{
            P[i,j]=(b[1]*b[2]*2*j)^(b[2]-1)
          }
        }
      }
      
      ### 更新multi_miu,multi_var
      multi_var = solve(solve(D)/rep(sigma^2,N)+(solve(P)/(m^2)))#计算多元高斯方差矩阵
      multi_miu = t(c(beta,rep(beta,N-1)))+miu*t+(Y-miu*t-t(c(beta,rep(beta,N-1))))%*%(solve(D)/rep((sigma^2),N))%*%multi_var #计算多元高斯均值
      history_b[z] = b
    }
    
      ### 这里使用log time scale b(exp(t)-1)
      solve_b_log = function(b_test){
        t_test = b_test[1]*exp(b_test[2]*seq(2,2*N,2))+b_test[3]
        ### 计算第一项
        temp_five = multi_miu^2+c(beta,multi_miu[1:(N-1)])^2-2*multi_miu*c(beta,multi_miu[1:(N-1)])
        term_one = sum(temp_five/(t_test-c(0,t_test[1:(N-1)])))
        ### 计算第二项
        temp_six = diag(multi_var)+c(0,diag(multi_var)[1:(N-1)])-2*c(0,diag(multi_var[-nrow(multi_var),-1]))
        term_two = sum(temp_six/(t_test-c(0,t_test[1:(N-1)])))
        return(-(-1/2*sum(log(t_test-c(0,t_test[1:(N-1)])))-1/(2*m^2)*miu^2*t_test[N]-1/(2*m^2)*(term_one+term_two)))
      }
      b = optimize(solve_b_log,c(0.001,1),maximum = TRUE)$maximum
      b_odd = b
      b = optim(c(b_odd[1],b_odd[2],b_odd[3]),solve_b_test)$par
      
      
      
      ### 更新时间点
      t=b[1]*exp(b[2]*seq(2,2*N,2))+b[3]
      
      ### 更新矩阵P
      P = matrix(data = NA,ncol=N,nrow=N)
      for (i in 1:N){ #生成矩阵P0
        for (j in 1:N){
          if (i<j){
            P[i,j]=b[1]*exp(b[2]*2*i)+b[3]
          }
          else{
            P[i,j]=b[1]*exp(b[2]*2*j)+b[3]
          }
        }
      }
      
      ### 更新multi_miu,multi_var
      multi_var = solve(solve(D)/rep(sigma^2,N)+(solve(P)/(m^2)))#计算多元高斯方差矩阵
      multi_miu = t(c(beta,rep(beta,N-1)))+miu*t+(Y-miu*t-t(c(beta,rep(beta,N-1))))%*%(solve(D)/rep((sigma^2),N))%*%multi_var #计算多元高斯均值
      history_b[z] = b
    
    
    
  }
  
  
  # 这里计算RUL
    N=1
    b=1
    S=-4
    f=function(x_test){
      A_test=(miu*(x_test^b-(2*N)^b)*multi_var[N,N]+(S-multi_miu[N])*(m^2)*(x_test^b-(2*N)^b))/((m^2)*(x_test^b-(2*N)^b)+multi_var[N,N])
      
      B_test=((m^2)*(x_test^b-(2*N)^b)*multi_var[N,N])/((m^2)*(x_test^b-(2*N)^b)+multi_var[N,N])
      
      C_test=(b*(x_test^(b-1)))/(2*pi*m*sqrt(multi_var[N,N])*((x_test^b-(2*N)^b)^(3/2)))*exp((A_test^2)/(2*B_test)-(((miu^2)*(x_test^b-(2*N)^b)/(m^2))+((S-multi_miu[N])^2/multi_var[N,N]))/2)
      
      phi_test = sum(dnorm(seq(-100,((A_test/sqrt(B_test))),0.01),mean=0,sd=1))*0.01
      
      return(C_test*(B_test*exp(-((A_test^2)/(2*B_test)))+sqrt(2*pi*B_test)*A_test*phi_test)*x_test)
    }
    
    RUL = integrate(Vectorize(f),lower=2*N,upper=Inf)
  
    
    
      
  
  if (z<=500){
    condition = "converge"
  }else{
    condition = 'Not converge'
  }
  return(list(t_multi_miu = multi_miu,t_miu = miu,t_sigma = sigma^2,t_m = m^2, t_v = v, t_b = b, x_test = x,converge = condition, iteration = z, loglike = history_log_likelihood, Y_t = Y,RUL_t = RUL))
}

result = t_function(1)
result1 = Pearson_function(1)
plot(seq(2,2*N,2),Y,type='l',lty=2,xlab="Circles",ylab="Degradation")
points(seq(1,N),Y,pch=16,cex=1)
lines(seq(2,2*N,2),multi_miu_t,col=mycolor[2],lwd=2)
lines(seq(2,2*N,2),miu*t+beta,col=mycolor[2],lwd=2)
par(mar=c(4,3,4,2),mgp=c(1.8,0.6,0))

lines(seq(1,N),multi_miu_t,col=mycolor[2],lty=3,lwd = 3)
lines(seq(1,N),x,lwd=1)
lines(seq(1,2*N,2),miu*seq(1,2*N,2)^b,col = 'red',lwd=2)
View(as.matrix(history_log_likelihood))
history_log_likelihood[266]
multi_miu_t = multi_miu
legend(x=1,y=0.13,legend=c("Observed data","Gaussian","Student-t","Laplace","Logistic","Pearson"),col=c('black',mycolor),lty = c(2,1,1,3,3,5),lwd = c(1,2,2,3,3,2),box.col = "grey",box.lwd=2,cex=1.1)
legend(x=1.6,y=0.1295,legend=" ",col="black",pch=16,bty='n',pt.cex=1.1)

########## 修改字体
library(showtext)
showtext_auto(enable=TRUE)
font_add(family="Times New Roman",regular="C:\\Windows\\Fonts\\times.ttf")
showtext_begin()
windows()
par(mar=c(4,3,4,1),mgp=c(1.8,0.6,0),family="Times New Roman")###这一步以上的代码即可
View(font.files())
font.families()
font_paths()
dev.off()
showtext_end()

############################# 画经验分布图 #########################
error = Y-x
error = sort(error)
min(error)
max(error)
error_t = error
v_t = v
sigma_t = sigma
plot(error_t,seq(1,200,1)/200,type = 'l',lwd=2,bty = 'l',ylab=NA)
lines(seq(-1377.50,94.62,0.01),ppearsonVII(seq(-1377.50,94.62,0.01),df=v_t,location=0,scale=sigma_t),col=mycolor[2],lwd = 2)
legend(x=200,y=0.4,legend=c("Empirical CDF","Student-t"),col=c("black",mycolor[2]),lty = 1,box.lwd = 2,box.col = "grey",lwd = 2,cex = 0.9)



############################# 画四张经验分布图 #########################
error = Y-x
error = sort(error)
min(error)
max(error)
plot(error,seq(1,200,1)/200,type = 'l',lwd=3,ylab=NA,bty = 'l')
lines(seq(-56.64,187.58,0.01),ppearsonVII(seq(-56.64,187.58,0.01),df=v,location=0,scale=sigma),col=mycolor[2],lwd = 2)

par(mfrow = c(2,2),mar=c(2,2,2,1))



############################# 画路径上误差分布图 #########################
mycolor = c("black",rgb(213, 93, 81, maxColorValue = 255),rgb(242, 197, 73, maxColorValue = 255),rgb(112, 159, 86, maxColorValue = 255),rgb(54, 129, 193, maxColorValue = 255))

sp1=spline(seq(1,200,1),abs(error_t),n=10000) 

error_t = multi_miu-x
plot(seq(1,200,1),abs(error_t),type = 'l',ylim = c(0,15),col = mycolor[2])
points(seq(1,200,1),abs(error_t),col = mycolor[2],pch=16,cex=0.5)
plot(sp1,type='l',ylim = c(0,15))



######################  并行计算尝试  ########################
library(foreach)
library(parallel)
library(doParallel)
library(snowfall)


sfInit(parallel = TRUE,cpus = detectCores()-2,slaveOutfile = "test.txt")
sfLibrary(snowfall)
sfLibrary(Runuran)
sfLibrary(MASS)
sfLibrary(PearsonDS)
sfLibrary(GeneralizedHyperbolic)
sfLibrary(rootSolve)
sfLibrary(ghyp)
sfLibrary(invgamma)
sfExport("Laplace_function")
sfExport("t_function","Pearson_function","logistic_function","Laplace_function","norm_function")

t_result = sfLapply(1:2000,function(x){
  sfCat(paste("t finished",x),sep="\n")
  return(t_function(x))})
Pearson_result = sfLapply(1:2000,function(x){
  sfCat(paste("Pearson finished",x),sep="\n")
  return(Pearson_function(x))})
Laplace_result = sfLapply(1:2000,function(x){
  sfCat(paste("Laplace finished",x),sep="\n")
  return(Laplace_function(x))})
logistic_result = sfLapply(1:2000,function(x){
  sfCat(paste("logistic finished",x),sep="\n")
  return(logistic_function(x))})
norm_result = sfLapply(1:2000,function(x){
  sfCat(paste("norm finished",x),sep="\n")
  return(norm_function(x))})

sfStop()




######################  模拟结果计算  ########################
### 创建记录，第一列是t分布，第二列是norm
MSE = matrix(data = NA,nrow = 2000,ncol = 1)
MAE = matrix(data = NA,nrow = 2000,ncol = 1)
miu_compare = matrix(data = NA,nrow = 2000,ncol = 1)
sigma_square_compare = matrix(data = NA,nrow = 2000,ncol = 1)
m_square_compare = matrix(data = NA,nrow = 2000,ncol = 1)
v_compare = matrix(data = NA,nrow = 2000,ncol = 1)
condition_compare = matrix(data = NA,nrow = 2000,ncol = 1)
b_compare = matrix(data = NA,nrow = 2000,ncol = 1)
iteration_compare = matrix(data=NA,nrow=2000,ncol=1)
rMAE = matrix(data=NA,nrow=2000,ncol=1)
Y_compare = matrix(data=NA,nrow=2000,ncol=200)
x_compare = matrix(data=NA,nrow=2000,ncol=200)
MAE_compare = matrix(data=NA,nrow=2000,ncol=200)
error_compare = matrix(data=NA,nrow=2000,ncol=200)


### 计算MAE和MSE
for (i in 1:2000){
  MAE[i] = sum(abs(t_result[[i]][[7]]-t_result[[i]][[1]]))/200
  MSE[i] = sum(abs(t_result[[i]][[7]]-t_result[[i]][[1]])^2)/200
  miu_compare[i] = t_result[[i]][[2]]
  sigma_square_compare[i] = t_result[[i]][[3]]
  m_square_compare[i] = t_result[[i]][[4]]
  v_compare[i] = t_result[[i]][[5]]
  b_compare[i] = t_result[[i]][[6]]
  iteration_compare[i] = t_result[[i]][[9]]
  Y_compare[i,] = t_result[[i]][[11]]
  x_compare[i,] = t_result[[i]][[7]]
  error_compare[i,] = abs(t_result[[i]][[11]]-t_result[[i]][[7]])
  MAE_compare[i,] = abs(t_result[[i]][[1]]-t_result[[i]][[7]])
  rMAE[i] = sum(1-(t_result[[i]][[1]]/t_result[[i]][[7]]))/200
}



plot(seq(1,2000),MAE[,1],ylim = c(0,5),pch = 16,col = 'red')
mean(MAE)
View(MAE)
points(seq(1,10),MAE[,2], pch = 16)
plot(seq(1,10),MSE[,1],ylim = c(0,10),pch = 16,col = 'red')
points(seq(1,10),MSE[,2], pch = 16)
plot(seq(1,1000),miu_compare[,1],ylim = c(0,10),pch = 16,col = 'red')
mean(miu_compare)
sd(miu_compare)
mean(rMAE)
sd(rMAE)
plot(seq(1,10),sigma_square_compare[,1],ylim = c(0,10),pch = 16,col = 'red')
mean(sqrt(sigma_square_compare))
sd(sqrt(sigma_square_compare))
mean(sqrt(m_square_compare))
sd(sqrt(m_square_compare))
plot(seq(1,200),colMeans(MAE_compare),col = mycolor[1],ylim = c(0,100),type = 'l')
points(seq(1,200),colMeans(error_compare),ylim = c(0,50))



View(v_compare)
miu_compare = miu_compare[-which(iteration>500)]
sigma_square_compare = sigma_square_compare[-which(iteration>500)]
mean(miu_compare)
sd(miu_compare)
mean(sigma_square_compare)
sd(sigma_square_compare)
mean(sqrt(sigma_square_compare))
sd(sqrt(sigma_square_compare))
mean(MAE)
sd(MAE)
which(iteration_compare>500)
mean(v_compare)




##########################  输出数据  ####################################
#第一列是Laplace分布，第二列是正态分布
MAE_t_5_50 = MAE
MSE_t_5_50 = MSE
miu_t_5_50 = miu_compare
sigma_square_t_5_50 = sigma_square_compare
m_square_t_5_50 = m_square_compare
v_t_5_50 = v_compare
b_t_5_50 = b_compare
iteration_t_5_50 = iteration_compare
write.csv(MAE_t_5_50,file = "MAE_t_5_50.csv")
write.csv(MSE_t_5_50,file = "MSE_t_5_50.csv")
write.csv(miu_t_5_50,file = "miu_t_5_50.csv")
write.csv(sigma_square_t_5_50,file = "sigma_square_t_5_50.csv")
write.csv(m_square_t_5_50,file = "m_square_t_5_50.csv")
write.csv(v_t_5_50,file = "v_t_5_50.csv")
write.csv(b_t_5_50,file = "b_t_5_50.csv")
write.csv(iteration_t_5_50,file = "iteration_t_5_50.csv")

write.csv(trans_bearing,file = "trans_bearing.csv")



######################## 画分布图 ##########################
v=1
sigma = 4
delta = 0.5

par(mar=c(4,3,4,1),mgp=c(1.8,0.6,0))
plot(seq(-20,20,0.01),dpearsonVII(seq(-20,20,0.01),df=v,location=0,scale=sigma),ylim=c(0,0.13),type="l",ylab="Probability",xlab="X",lty=3,col='red')
points(seq(-20,20,0.1),1/(sigma*sqrt(v)*beta(delta/2,1/2))*(1+seq(-20,20,0.1)^2/(v*sigma^2))^(-(delta+1)/2),col=rgb(119,136,153,100,maxColorValue = 255),cex=0.4,pch=16)
lines(seq(-20,20,0.01),dlogis(seq(-20,20,0.01),location=0,scale=sigma),col='green',lty=2)
lines(seq(-20,20,0.01),dnorm(seq(-20,20,0.01),mean=0,sd=sigma))
lines(seq(-20,20,0.01),dskewlap(seq(-20,20,0.01),mu=0,alpha=sigma,beta=sigma),col='blue',lty=4)
#lines(seq(-20,20,0.01),dvg(seq(-20,20,0.01),vgC=0,sigma=sigma,theta =0 ,nu=1),col='blue',lty=4)


legend(x=7,y=0.11,legend=c("Gaussian","Student-t","Laplace","Logistic","                    "),col=c('black','red','blue','green','white'),lty = c(1,3,4,2,7),box.lwd = 2,box.col = "grey",lwd = 2)
legend(x=8.2,y=0.0815,legend="    Pearson VII",col=rgb(112,128,170,100,maxColorValue = 255),pch = 16,box.lwd = 2,bty='n')


plot(seq(-20,20,0.01),dpearsonVII(seq(-20,20,0.01),df=v,location=0,scale=sigma),ylim=c(0,0.04),xlim=c(1,20),type="l",ylab="Probability",xlab="X",lwd=2)
lines(seq(-20,20,0.01),dlogis(seq(-20,20,0.01),location=0,scale=sigma),col='green',lty=2,lwd=2)
lines(seq(-20,20,0.01),dskewlap(seq(-20,20,0.01),mu=0,alpha=sigma,beta=sigma),col='blue',lty=4,lwd = 2)
lines(seq(-20,20,0.01),dnorm(seq(-20,20,0.01),mean=0,sd=sigma),col='red',lty=3,lwd=2)
#lines(seq(-20,20,0.01),dvg(seq(-20,20,0.01),vgC=0,sigma=sigma,theta =0 ,nu=1),col='blue',lty=4,lwd=2)
points(seq(-20,20,0.5),1/(sigma*sqrt(v)*beta(delta/2,1/2))*(1+seq(-20,20,0.5)^2/(v*sigma^2))^(-(delta+1)/2),col=rgb(112,128,170,100,maxColorValue = 255),cex=0.9,pch=16)
legend(x=12,y=0.04,legend=c("Student-t","                    ","Logistic","Laplace","Gaussian"),col=c('black','white','green','blue','red'),lty = c(1,7,2,4,3),box.lwd = 2,box.col = "grey",lwd = 2)
legend(x=12.6,y=0.0375,legend="    Pearson VII",col=rgb(112,128,170,100,maxColorValue = 255),pch = 16,bty='n')



################ 用回归粗略估计参数 #####################
regression_b = (t(trans_bearing-mean(trans_bearing))%*%(seq(2,2*126,2)-mean(seq(2,2*126,2))))/sum((seq(2,2*126,2)-mean(seq(2,2*126,2)))^2)

regression_a = mean(trans_bearing)-mean(seq(2,2*126,2))*regression_b
plot(seq(2,2*126,2),trans_bearing)
lines(seq(2,2*126,2),regression_b*seq(2,2*126,2)+regression_a,lwd=2)
regression_sigma = sqrt(sum((trans_bearing-(regression_b*seq(2,2*126,2)+regression_a))^2)/126)

regression_likelihood = 126*log(1/(regression_sigma*sqrt(2*pi)))-(1/(2*regression_sigma^2))*sum((trans_bearing-(regression_b*seq(2,2*126,2)+regression_a))^2)



regression_b = (t(log(teacher_bearing_data)-mean(log(teacher_bearing_data)))%*%(seq(2,2*125,2)-mean(seq(2,2*125,2))))/sum((seq(2,2*125,2)-mean(seq(2,2*125,2)))^2)

regression_a = mean(log(teacher_bearing_data))-mean(seq(2,2*125,2))*regression_b
plot(seq(2,2*125,2),teacher_bearing_data)
lines(seq(2,2*125,2),regression_b*seq(2,2*125,2)+regression_a,lwd=2)
regression_sigma = sqrt(sum((log(teacher_bearing_data)-(regression_b*seq(2,2*125,2)+regression_a))^2)/125)

regression_likelihood = 125*log(1/(regression_sigma*sqrt(2*pi)))-(1/(2*regression_sigma^2))*sum((log(teacher_bearing_data)-(regression_b*seq(2,2*125,2)+regression_a))^2)


