
from numpy.lib.stride_tricks import as_strided



def py_add_operator(x, y):
    x += y

def model_comp_id(xspec_model, id):
    return(xspec_model(int(id)))
