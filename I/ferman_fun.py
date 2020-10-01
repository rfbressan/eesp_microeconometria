# Ferman Assessment
    
import numpy as np
import pandas as pd
#import statsmodels.api as sm
import statsmodels.formula.api as smf
from scipy.stats import norm


#' Function to make the assessment. Input arguments are the data frame, 
#' unconstrained formula to regress, variable name to make the assessment, 
#' coefficient under H0, number of simulations to run, significance level, an optional 
#' argument to set whether the residuals from model fit should be sampled with 
#' replacement to carry out all simulations. If there are clusters in the 
#' model they may be passed on through "cluster".

def ferman_assessment(df, model, assess_on, H0=0.0, nsim=1000, alpha=0.05, res_samp=False, cluster=None):
    """Performs the inference assessment provided in Ferman (2019).

    Parameters
    ----------
    df : pandas data frame
        Your database
    model : character string
        A character string in R's formula style defining the unconstrained model. 
    assess_on : character string
        The variable the assessment will be taken on.
    H0 : float, optional
        Coefficient value under the null hypothesis, by default 0.0
    nsim : int, optional
        Number of simulations to run, by default 1000
    alpha : float, optional
        Significance level, by default 0.05
    res_samp : bool, optional
        Logical describing if residuals from null regression should be sampled
        with replacement to construct simulated residuals, by default False
    cluster : character string, optional
        Cluster variable name. Not yet implemented, by default None

    Returns
    -------
    float
        Assessment value for the given level of significance.
    """        
    # pass
    # Removes all whitespaces from model formula
    model = "".join(model.split())
    model_components = model.split("~")
    depvar = model_components[0]
    rhs_vars = model_components[1].split("+")
    
    # Rejections vector (of 0s and 1s)
    rejections = np.zeros(nsim)
    
    # Step 1: estimate H0 model
    rhs_null = [s for s in rhs_vars if s != assess_on]
    h0model = depvar+"~"+"+".join(rhs_null)
    h0fit = smf.glm(h0model, df, offset = H0*df[assess_on]).fit(cov_type = "HC0")
    # Step 2: store the predicted values
    y_pred = h0fit.predict()
    res = h0fit.resid_response
  
    # Step 3: nsim iterations
    # Step 3.1: draw simulation errors and generate y_sim
    for i in range(nsim):
        if res_samp:
            sim_error = np.random.choice(res, len(res), replace = True)
        else:
            sim_error = np.random.normal(size = len(res))
    
        y_sim = y_pred + sim_error
        df_sim = df.copy()
        df_sim[[depvar]] = y_sim

        # Step 3.2: Estimate the unrestricted model for each sim
        sim_reg = smf.ols(model, df_sim).fit(cov_type = "HC0")
        # Step 3.3: Test the null hypothesis for each sim
        # Extract all p-values from desired coefficient
        beta = sim_reg.params[assess_on]
        se_beta = sim_reg.bse[assess_on]
        tstat = abs((beta - H0)/se_beta)
        # Test whether tstat < tcritical and store in rejections
        rejections[i] = tstat > norm.ppf(1-alpha/2)
        
    # Return the mean of rejections
    return rejections.mean()
    
    
    
#' Testing it
if __name__ == "__main__":

    data = pd.read_stata("input/data_final.dta")

    formula = "resp_A~mailing+threat+appeal+info"
    var = "mailing"

    ferman_assessment(data, formula, var, nsim=100)

