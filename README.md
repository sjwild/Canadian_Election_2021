# Canandian_Election_2021

This is an attempt to model the upcoming 2021 (?) Canadian federal election using a state space model.  The first model is produced using R and Stan, while the second is a similar version produced using Julia and Turing.jl. Both models deliver similar results.

The model assumes that there is an underlying latent voting intention for each of the five main parties (plus others) that is measured by a bunch of noisy polls.  

The model is based on the model suggested by Simon Jackman in his book, _Bayesian Analysis for the Social Sciences_, and updated by [Peter Ellis]( http://freerangestats.info/elections/oz-2019/index.html). My Stan and Turing versions differ from their versions in the following ways:  

* I use the cholesky decomposition for the non-centered paramterization to improve sampling of the innovation sd (Stan)
* I anchor the election results, and use the polling error to predict voting intention for the period after the 2019 election 
* In addition to house effects, I decompose the polling error into four sources:  
  * the margin of error based on simple random sampling 
  * a party-specific error
  * a mode-by-party error
  * pollster-by-party error

The Julia model is run using Turing.jl. The Turing model is coded using a different way of creating a covariance matrix, so that I am able to use Distributions.jl and Turing.jl. I will (hopefully) recode the model when (if?) Distributions.jl gets a native cholesky decomposition of the LKJ distribution.

The predictions for voting intention (that is, for the period after 2019-10-21, the date of 2019 Canadian election) assume that the polling errors are the same after the election as before it. That may or may not be true. I suspect that polling errors are correlated between periods.

## Vote intention
As of August 12, 2021, these are the estimated vote shares based on Canadian polls up to August 11, 2021:

|**Party**    | **Vote share**  | **95% bounds**     |
|-------------|:---------------:|:------------------:|
|**LPC**      | 36.5%           | (34.6%, 38.2%)     |
|**CPC**      | 30.8%           | (29.3%, 32.3%)     |
|**NDP**      | 17.6%           | (16.0%, 19.1%)     |
|**BQ**       | 7.0%            | (6.3%, 7.7%)       |
|**GPC**      | 4.1%            | (3.1%, 5.1%)       |
|**Other**    | 4.4%            | (3.6%, 5.2%)       |

![alt text](https://github.com/sjwild/Canandian_Election_2021/raw/main/can_vote_intention_on_election_date.png "Density plot of estimated vote share per party.")

Overall, vote intention has been relatively stable since the last federal election. There was a bump in support for the Liberal Party of Canada at the beginning of the pandemic, but that increase in support has dropped. 

![alt text](https://github.com/sjwild/Canandian_Election_2021/raw/main/can_vote_intention_2019_2021.png "Vote share of Canadian parties from 2019 to 2021.")



![alt text](https://github.com/sjwild/Canandian_Election_2021/raw/main/can_vote_intention_2015_2021.png "Vote share of Canadian parties from 2015 to 2021.")


## House effects
House effects vary, but in general firms tended to overestimate NDP and Green Party vote share while underestimating LPC, CPC, and BQ vote share. 

![alt text](https://github.com/sjwild/Canandian_Election_2021/raw/main/house_effects_pollsters.png "House effects of Canadian polling firms from 2015 to 2021.")

# Future plans
I plan to eventually produce a second stage of the model to estimate seat distributions. If time permits, I also plan to try produce a third model to estimate riding-by-riding outcomes.



# References/inspirations

## Other state space models

Bailey, J. (2021). britpol v0.1.0: User Guide and Data Codebook. Retrieved from https://doi.org/10.17605/OSF.IO/2M9GB.  

Economist (2020.) Forecasting the US elections. Retrieved from https://projects.economist.com/us-2020-forecast/president. 

Ellis, P. (2019). ozfedelect R package. Retrieved from https://github.com/ellisp/ozfedelect.   

Savage, J.(2016). Trump for President? Aggregating National Polling Data. Retrieved from https://khakieconomics.github.io/2016/09/06/aggregating-polls-with-gaussian-Processes.html.  

INWT Statistics GmbH (2021). Election forecast. Retrieved from https://github.com/INWTlab/lsTerm-election-forecast.  

## Articles
Bélanger, É., & Godbout, J. F. (2010). Forecasting Canadian federal elections. _PS: Political Science & Politics_, _43_(4), 691-699.    

Heidemanns, M., Gelman, A., & Morris, G. E. (2020). An updated dynamic Bayesian forecasting model for the US presidential election. _Harvard Data Science Review_, _2_(4).  

Linzer, D. A. (2013). Dynamic Bayesian forecasting of presidential elections in the states. Journal of the American Statistical Association, 108(501), 124-134.   

MacInnis, B., Krosnick, J. A., Ho, A. S., & Cho, M. J. (2018). The accuracy of measurements with probability and nonprobability survey samples: Replication and extension. _Public Opinion Quarterly_, _82_(4), 707-744.  

Mongrain, P., Nadeau, R., & Jérôme, B. (2021). Playing the synthesizer with Canadian data: Adding polls to a structural forecasting model. _International Journal of Forecasting_, _37_(1), 289-301.   

Shirani-Mehr, H., Rothschild, D., Goel, S., & Gelman, A. (2018). Disentangling bias and variance in election polls. _Journal of the American Statistical Association_, _113_(522), 607-614.  

Walther, D. (2015). Picking the winner(s): Forecasting elections in multiparty systems. _Electoral Studies_, _40_, 1-13.  



