import errno
import importlib
import json
import os
import sys

sys.path.append("src/math_expr")
from convert_math import parse_math

def convert_orders(orders):
    return map(lambda order: order.__dict__, orders)

def convert_particle(particle):
    d = particle.__dict__
    try:
        del(d["partial_widths"])
    except KeyError:
        pass
    d["mass"] = d["mass"].name
    d["width"] = d["width"].name
    try:
        if d["GoldstoneBoson"] != False:
            d["goldstoneboson"] = True
        del(d["GoldstoneBoson"])
    except KeyError:
        pass
    return d

def convert_vertex(vertex):
    d = vertex.__dict__
    d["particles"] = map(lambda p: p.pdg_code, d["particles"])
    d["lorentz"] = map(lambda l: l.name, d["lorentz"])
    d["color"] = map(lambda c: parse_math(c, mode="color", location="the color structure of vertex {}".format(vertex.name)), d["color"])
    d["couplings"] = map(lambda (pieces,v): {"color": pieces[0], "lorentz": pieces[1], "coupling": v.name}, d["couplings"].iteritems())
    return d

def convert_coupling(coupling):
    d = coupling.__dict__
    value = d["value"]
    if type(value) is dict:
        value = {"type": "orders", "value": {o: parse_math(c, location="the coupling {}".format(coupling.name)) for (o,c) in value.iteritems()}}
    else:
        value = {"type": "simple", "value": parse_math(d["value"], location="the coupling {}".format(coupling.name))}
    d["value"] = value
    return d

def convert_lorentz(lorentz):
    d = lorentz.__dict__
    d["structure"] = parse_math(d["structure"], mode="lorentz", location="the lorentz structure {}".format(lorentz.name))
    return d

def convert_parameter(parameter):
    d = parameter.__dict__
    if type(d["value"]) == str:
        d["value"] = parse_math(d["value"], location="the value of parameter {}".format(parameter.name))
    return d

def convert_decay(decay):
    d = decay.__dict__
    d["particle"] = d["particle"].pdg_code
    d["partial_widths"] = map(lambda (prods, width): {"decay_products": map(lambda p: p.name, prods), "width": parse_math(width, location="the width of particle {}".format(decay.particle))}, d["partial_widths"].iteritems())
    return d

def convert_propagator(propagator):
    d = propagator.__dict__
    d["numerator"] = parse_math(d["numerator"], location="the numerator of propagator {}".format(propagator.name))
    d["denominator"] = parse_math(d["denominator"], location="the denominator of propagator {}".format(propagator.name))
    return d

def convert_function(function):
    d = function.__dict__
    d["expr"] = parse_math(d["expr"], location="the definition of the function {}".format(function.name))
    if not isinstance(d["arguments"], tuple) and not isinstance(d["arguments"], list):
        d["arguments"] = (d["arguments"],)
    return d

def convert(model_path, model_name, output_path):
    sys.path.append(model_path)
    out_dir = os.path.join(output_path, model_name)
    try:
        os.makedirs(out_dir)
    except OSError as err:
        if err.errno == errno.EEXIST:
            pass
        else:
            raise err
    model = importlib.import_module(model_name)
    orders = convert_orders(model.all_orders)
    particles = map(convert_particle, model.all_particles)
    vertices = map(lambda v: convert_vertex(v), model.all_vertices)
    couplings = map(convert_coupling, model.all_couplings)
    lorentz = map(convert_lorentz, model.all_lorentz)
    parameters = map(convert_parameter, model.all_parameters)
    try:
        decays = map(convert_decay, model.all_decays)
        with open(os.path.join(out_dir, "decays.json"), "w") as jfile:
            json.dump(decays, jfile, indent=2)
    except:
        pass
    with open(os.path.join(out_dir, "coupling_orders.json"), "w") as jfile:
        json.dump(orders, jfile, indent=2)
    with open(os.path.join(out_dir, "particles.json"), "w") as jfile:
        json.dump(particles, jfile, indent=2)
    with open(os.path.join(out_dir, "vertices.json"), "w") as jfile:
        json.dump(vertices, jfile, indent=2)
    with open(os.path.join(out_dir, "couplings.json"), "w") as jfile:
        json.dump(couplings, jfile, indent=2)
    with open(os.path.join(out_dir, "lorentz.json"), "w") as jfile:
        json.dump(lorentz, jfile, indent=2)
    with open(os.path.join(out_dir, "parameters.json"), "w") as jfile:
        json.dump(parameters, jfile, indent=2)
    with open(os.path.join(out_dir, "function_library.json"), "w") as jfile:
        functions = map(convert_function, model.all_functions)
        json.dump(functions, jfile, indent=2)

def convert_relations(outfile):
    g5 = {
        "expr": parse_math("complex(0,1)*Gamma(1,11,-22)*Gamma(2,-22,-33)*Gamma(3,-33,-44)*Gamma(4,-44,55)", mode="lorentz"),
        "lorentz": [1,2,3,4],
        "spinor": [11,55],
    }
    pp = {
        "expr": parse_math("(Identity(1,2)+Gamma5(1,2))/2", mode="lorentz"),
        "spinor": [1,2],
    }
    pm = {
        "expr": parse_math("(Identity(1,2)-Gamma5(1,2))/2", mode="lorentz"),
        "spinor": [1,2],
    }
    sigma = {
        "expr": parse_math("complex(0,1)/2*(Gamma(1,11,-22)*Gamma(2,-22,33) - Gamma(2,11,-22)*Gamma(1,-22,11))", mode="lorentz"),
        "lorentz": [1,2],
        "spinor": [11,33],
    }
    relations = {
        "gamma5": g5,
        "proj_p": pp,
        "proj_m": pm,
        "sigma": sigma
    }
    with open(outfile, "w") as jfile:
        json.dump(relations, jfile, indent=2)

convert("tests/models", "SM_NLO", "tests/models_json")
convert("tests/models", "sm_mg5", "tests/models_json")
convert_relations("models/common/spin_relations.json")
