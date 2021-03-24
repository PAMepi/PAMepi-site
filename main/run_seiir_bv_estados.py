import numpy as np
import pandas as pd
from JAS.models.gradient_optmization import seiir
import multiprocessing as mp

#read data
data = pd.read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv")


states = set(data["state"])


#State Populations estimated from IBGE (2019, updated in 20200622)
pops = {'RO':	1777225,
        'AC':	881935,
        'AM':	4144597,
        'RR': 	605761,
        'PA':	8602865,
        'AP':	845731,
        'TO':	1572866,
        'MA':	7075181,
        'PI':	3273227,
        'CE':	9132078,
        'RN':	3506853,
        'PB':   4018127,
        'PE':	9557071,
        'AL':   3337357,
        'SE':   2298696,
        'BA':   14873064,
        'MG':   21168791,
        'ES':	4018650,
        'RJ':   17264943,
        'SP':   45919049,
        'PR':   11433957,
        'SC':   7164788,
        'RS':	11377239,
        'MS':	2778986,
        'MT':   3484466,
        'GO':   7018354,
        'DF':	3015268,
        'TOTAL':210147125
       }

list_df = [data[data["state"] == state] for state in states]


def create_output(data):
    data["day"] = range(0,len(data["totalCases"]))
    
    pred_time = pd.date_range(start = pd.to_datetime(data["date"].iloc[0]),
                              end =  pd.to_datetime("2021-03-01"))
    population = pops[data["state"].iloc[0]]
    
    model = seiir.start_model(pop = population)
    model.fit(x = data["day"],
              y = data["totalCases"],
              n_tries = 10,
              n_betas = 3,
              fit_by = "cs",
              bounds = {"beta":  [0,2.0], "beta1": [0,2.0],"beta2": [0,2.0],
                        "delta": [0., 0.75],"rho": [0.13,0.5],"kappa": [1/6, 1/3],
                        "gammaA": [1/3.7,1/3.24], "gammaS": [1/5,1/3],"t1": [0.,45.],
                        "t2": [50.,100.]})
    
    
    results = model.predict(time = pred_time)
    
    data["totalCasesPred"] = results["Tt"][0:len(data["totalCases"])]
    
    
    output_data = pd.DataFrame({"day": pred_time,
                               "state": data["state"].iloc[0],
                               "suscetivel": model.S,
                               "exposto": model.E,
                               "infectadoA": model.Ia,
                               "infectadoS": model.Is,
                               "recuperado": model.R})
    
    output_par = pd.DataFrame({"beta1": [model.beta],
                               "beta2": [model.beta1],
                               "beta3": [model.beta2],
                               "gammaA": [model.gammaA],
                               "gammaS": [model.gammaS],
                               "pop": [population],
                               "state": [data["state"].iloc[0]]})

    
    return {"data": data, "output_data": output_data, "output_par": output_par}


pool = mp.Pool(6)
results = pool.map(create_output, list_df)
pool.close()
pool.join()


output_data = [results[i]["output_data"] for i in range(0,len(results))]
output_par = [results[i]["output_par"] for i in range(0,len(results))]
data = [results[i]["data"] for i in range(0,len(results))]


output_data = pd.concat(output_data)
output_par = pd.concat(output_par)
data = pd.concat(data)


data.to_csv('/srv/shiny-server/covidApp/data/model_data/data_seiir_bv_estados.csv', index = False)
output_par.to_csv('/srv/shiny-server/covidApp/data/model_par/par_seiir_bv_estados.csv', index = False)
output_data.to_csv('/srv/shiny-server/covidApp/data/model_comp/compartimentos_seiir_bv_estados.csv', index = False)


