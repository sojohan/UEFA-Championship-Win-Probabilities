--
title: "UEFA Championship Win Probabilities"
author: "Soren Johansen"
date: "27 3 2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Understand the problem
The problem I want to model is a about European soccer. The hypothesis I want to test is if there is a home advantage during European UEFA Championship and also to see if decompose the teams into which is more attach versus more defense playing strategy. The data I have used is from Kaggle.
The model I have used is called the Poisson random effects model.

We indicate the number of points scored by the home and the away team in the g-th game of the season (175 games) as $y_{g1}$ and $y_{g2}$ respectively.

The vector of observed counts y=($y_{g1}$, $y_{g2}$) is modelled as independent Poisson: 
$y_{gj}|\theta_{gj}~Possion(\theta_{gj})$
playing at home (j=1) and away (j=2), respectively. We model these parameters according to a formulation that has been used widely in the statistical literature, assuming a log-linear random effect model:
$$ log \theta_{g1}=home+att_{h(g)}+def_{a(g)}$$
$$log \theta_{g2}=att_{a(g)}+def_{h(g)}$$
A more detailed study of the model can be found [here](https://discovery.ucl.ac.uk/id/eprint/16040/1/16040.pdf) with Italian football(Serie A championship 1991-1992) and an implementation in pymc3 is [here](https://docs.pymc.io/notebooks/rugby_analytics.html) on Rugby data. During the European Championship every team is assigned to specific football stadium and will play all their matches at the same stadium in the first round of the tournament. The notion of home advantage does make sense. 

## Plan and properly collect relevant data

The dataset includes 41,586 results of international football matches starting from the very first official match in 1972 up to 2019. The matches range from FIFA World Cup to FIFI World Cup to regular friendly matches. The data can be found [here](https://www.kaggle.com/martj42/international-football-results-from-1872-to-2017).
 I will only analyze the UEFA European Championship from year 2000:


```{r echo= FALSE,eval=TRUE,cache=TRUE}
library(ggplot2)
library(dplyr)
library(lubridate)
library(codatools)
library(coda)
```


```{r  eval=TRUE,cache=TRUE}
Soccer <- read.csv("~/Downloads/results.csv")
euro <- Soccer %>%
    filter(tournament=="UEFA Euro") %>%
    filter(year(date)>=2000)
```

This gives me:

* 175 matches (nrow(euro))
* 32 different European countries over 
* UEFA Championships in 2000(Belgium/Netherlands), 2004(Portugal), 2008(Austria/Switzerland), 2012(Poland/Ukraine) and 2016(France)


## Explore data
Some descriptive statistics:  

```{r eval=TRUE,cache=TRUE}
summary(euro$home_score)
```


```{r eval=TRUE,cache=TRUE}
summary(euro$away_score)
```

and standard deviation

```{r eval=TRUE,cache=TRUE}
sd(euro$home_score)
```

```{r eval=TRUE,cache=TRUE}
sd(euro$away_score)
```



I see that on average the home team scores more than the away team but that the standard deviation for home teams is higher than the away teams. The sum of all home scores and away scores can be visualized as this:

Total home and away scores

```{r eval=TRUE,cache=TRUE}

home_sum_scores<-euro %>%
  group_by(home_team) %>% summarise(total_home = sum(home_score))

away_sum_scores<-euro %>%
  group_by(away_team) %>% summarise(total_away = sum(away_score))
home_away_sum<-cbind(home_sum_scores,away_sum_scores)

```


```{r eval=TRUE,cache=TRUE}

f <- ggplot(home_away_sum, aes(total_home,total_away))
f + geom_text(aes(label=home_team))+geom_point()

```

Interesting to see the cluster of teams in upper right corner e.g. Spain, Portugal France, Germany. 

## Postulate a model


The model I use, has already been presented above. Here is the jags code: 

```{r eval=FALSE,cache=TRUE}
mod_string = " model {
for (i in 1:length(date)) {
  home_score[i] ~ dpois(theta_home[i])
  away_score[i] ~ dpois(theta_away[i])
  theta_home[i] = exp(home+att[home_ind[i]]+def[away_ind[i]]) 
  theta_away[i] = exp(att[away_ind[i]]+def[home_ind[i]])
  }

for (i in 1:max(home_ind)) {
  att_star[i] ~ dnorm(mu_att, tau_att)
  def_star[i] ~ dnorm(mu_def, tau_def)
  att[i]=att_star[i]-mean(att_star)
  def[i]=def_star[i]-mean(def_star)
}

mu_att ~ dnorm(0.0,1000)
tau_att ~ dgamma(0.01,0.01)
mu_def ~ dnorm(0.0,1000)
tau_def ~ dgamma(0.01, 0.01)
home ~ dnorm(0.0,1)
} "


```

You can find the full script [here](https://github.com/sojohan/mcmc_example/blob/master/project.R) 

## Fit the model

Empirical mean and standard deviation for the first ten variables att, def and home, mu and tau variables. Observe that the variable home is positive. Unfortunately, my model has many parameters that needs to be estimated and because this paper only can be max four pages long, I have made a spreadsheet with the full set of diagnostics here (you can download it). 


```{r eval=TRUE,cache=TRUE,echo=FALSE}
g<-read.csv(file='stat.csv',header = TRUE,sep=';')

attach<-g[1:10,]
defence<-g[33:42,]
# the last variables home, mu
data<-rbind(as.matrix(attach),as.matrix(defence))
knitr::kable(data, caption = "Fit Summary")

```

I can plot the pairs (def,att) in a graph. Interesting to see that the upper left quadrant is all the aggressive teams like Spain, France, England, Germany and Netherlands. 

```{r eval=TRUE,cache=TRUE,echo=FALSE}
g<-read.csv(file='stat.csv',header = TRUE,sep=';')

attach<-g[1:32,1]
defence<-g[33:64,1]


home<-euro$home_team
away<-euro$away_team

home_away<-unique(rbind(as.matrix(home),as.matrix(away)))
home_away<-data.frame(home_away)
home_away$ind<- seq.int(nrow(home_away))


attach_defence<-cbind(as.matrix(attach),as.matrix(defence))
attach_defence<-cbind(attach_defence,home_away)

attach_defence<-data.frame(attach_defence)
attach_defence<-select(attach_defence,-c(ind))

colnames(attach_defence)<-c('att','def','country')

# Plot of (att,def) variables
e <- ggplot(attach_defence, aes(def, att))
e + geom_text(aes(label=country))+geom_point()

```

## Check the model

Here is the first couple of traces:


![Traces for att ](traces.png) 


Here is the Gelman scale factor is 1.00 except tau_att which is 1.02. 

```{r eval=TRUE,cache=TRUE,echo=FALSE}
# the last variables home, mu

gelman<-data.frame(c("att[1]" ,"att[2]","att[3]","att[4]","att[5]","att[6]","att[7]","att[8]","att[9]","att[10]","home","mu_att","tau_att","tau_def"), c(1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.02,1.00),c(1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.02,1.00))
colnames(gelman)<-c("variable", "Point est.","Upper C.I.")

gelman

```




To view the full diagnostic data see this [link](https://github.com/sojohan/mcmc_example/blob/master/project_mcmc.xlsx)  for more. I conclude that the traces have converged. No autocorrelation was detected and here is subset of the correlation diagonal.

```{r eval=TRUE,cache=TRUE,echo=FALSE}
o<-read.csv('corr.csv', header = TRUE, sep=';')
corr_diag<-select(o,"home","mu_att","mu_def","tau_att","tau_def")

corr_diag

```


Note there is some autocorrelation in parameter tau_att.

## Use the model

I can simulate a tournament with our model and calculate the probability that Denmark will win. Let me take Denmark against (Spain, Germany, Sweden, Netherlands). Here is code Denmark against Spain: 


```{r eval=FALSE,cache=TRUE}
theta_home= exp(mod_csim[,"home"]+mod_csim[,"att[16]"]+mod_csim[,"def[8]"])
theta_away=exp(mod_csim[,"att[8]"]+mod_csim[,"def[16]"])
n_sim=length(theta_home)
den_spain=rpois(n=n_sim,theta_home)
spain_den=rpois(n=n_sim,theta_away)
mean(den_spain>spain_den)
```

Here are the probabilities that Denmark will win over Spain, Germany, Sweden, Netherlands:
```{r eval=TRUE,cache=TRUE, echo=FALSE}
 #24,91%	34,59%	31,51%	28,36%
results<-data.frame(index=c("Spain","Sweden","Germany","Netherlands"),c(24.91,34.59,31.51,28.36))
colnames(results)<-c("Team", " Denmark")
knitr::kable(results, caption = "Win Probabilities in%")
```
