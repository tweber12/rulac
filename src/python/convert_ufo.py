"""
Convert a ufo model into a json format

This model contains functions to convert the components of a ufo
model into a json format that is easier to parse from other other
programming languages.
"""


import importlib
import json
import os
import sys

import util

def _convert_orders(orders):
    """Convert a list ufo CouplingOrder objects to a list of dictionaries"""
    return [order.__dict__ for order in orders]

def _convert_particle(particle):
    """Convert a ufo Particle object to a dictionary"""
    particle_dict = particle.__dict__
    try:
        # Remove the widths. If there are any, they will be contained in the decays anyway
        del particle_dict["partial_widths"]
    except KeyError:
        pass
    particle_dict["mass"] = particle_dict["mass"].name
    particle_dict["width"] = particle_dict["width"].name
    try:
        # Try to sort out the GoldstoneBoson situation
        # This is a bug in UFO, where all particles that are not a goldstone boson have
        # 'goldstoneboson' set to false, while if it is one, then 'GoldstoneBoson' is set
        # (notice the capitalisation). Additionally, it isn't always set to a bool, but
        # at least in one case also to -1 for no apparent reason.
        if particle_dict["GoldstoneBoson"] != False:
            particle_dict["goldstoneboson"] = True
        del particle_dict["GoldstoneBoson"]
    except KeyError:
        pass
    return particle_dict

def _convert_vertex(vertex):
    """Convert a ufo Vertex object to a dictionary"""
    vertex_dict = vertex.__dict__
    vertex_dict["particles"] = [p.pdg_code for p in vertex_dict["particles"]]
    vertex_dict["lorentz"] = [l.name for l in vertex_dict["lorentz"]]
    vertex_dict["couplings"] = [
        {"color": pieces[0], "lorentz": pieces[1], "coupling": v.name}
        for (pieces, v) in vertex_dict["couplings"].iteritems()
    ]
    return vertex_dict

def _convert_coupling(coupling):
    """Convert a ufo Coupling object to a dictionary"""
    coupling_dict = coupling.__dict__
    if isinstance(coupling_dict["value"], dict):
        # The coupling value is expanded in epsilon for counterterms, etc
        # In this case the coupling is not just a single expression but a dictionary
        # mapping orders to expressions
        kind = "orders"
    else:
        kind = "simple"
    coupling_dict["value"] = {
        "type": kind,
        "value": coupling_dict["value"]
    }
    print coupling_dict
    return coupling_dict

def _convert_lorentz(lorentz):
    """Convert a ufo Lorentz object to a dictionary"""
    return lorentz.__dict__

def _convert_parameter(parameter):
    """Convert a ufo Parameter object to a dictionary"""
    return parameter.__dict__

def _convert_decay(decay):
    """Convert a ufo Decay object to a dictionary"""
    decay_dict = decay.__dict__
    decay_dict["particle"] = decay_dict["particle"].pdg_code
    decay_dict["partial_widths"] = [
        { "decay_products": [p.name for p in prods], "width": width }
        for (prods, width) in decay_dict["partial_widths"].iteritems()
    ]
    return decay_dict

def _convert_function(function):
    """Convert a ufo Function object to a dictionary"""
    function_dict = function.__dict__
    args = function_dict["arguments"]
    if not isinstance(args, tuple) and not isinstance(args, list):
        function_dict["arguments"] = (args,)
    return function_dict

def convert(model_path, model_name, output_path):
    """
    Convert a ufo model to json

    The model that is located at 'model_path/model_name' will be converted
    and the json files will be placed in 'output_path/model_name'
    """
    out_dir = os.path.join(output_path, model_name)
    util.makedirs(out_dir)
    sys.path.append(model_path)
    model = importlib.import_module(model_name)
    try:
        # all_decays is not always present, only convert it if it is
        decays = map(_convert_decay, model.all_decays)
        with open(os.path.join(out_dir, "decays.json"), "w") as jfile:
            json.dump(decays, jfile, indent=2)
    except AttributeError:
        pass
    orders = _convert_orders(model.all_orders)
    with open(os.path.join(out_dir, "coupling_orders.json"), "w") as jfile:
        json.dump(orders, jfile, indent=2)
    particles = map(_convert_particle, model.all_particles)
    with open(os.path.join(out_dir, "particles.json"), "w") as jfile:
        json.dump(particles, jfile, indent=2)
    vertices = map(_convert_vertex, model.all_vertices)
    with open(os.path.join(out_dir, "vertices.json"), "w") as jfile:
        json.dump(vertices, jfile, indent=2)
    couplings = map(_convert_coupling, model.all_couplings)
    with open(os.path.join(out_dir, "couplings.json"), "w") as jfile:
        json.dump(couplings, jfile, indent=2)
    lorentz = map(_convert_lorentz, model.all_lorentz)
    with open(os.path.join(out_dir, "lorentz.json"), "w") as jfile:
        json.dump(lorentz, jfile, indent=2)
    parameters = map(_convert_parameter, model.all_parameters)
    with open(os.path.join(out_dir, "parameters.json"), "w") as jfile:
        json.dump(parameters, jfile, indent=2)
    functions = map(_convert_function, model.all_functions)
    with open(os.path.join(out_dir, "function_library.json"), "w") as jfile:
        json.dump(functions, jfile, indent=2)

def convert_relations(outfile):
    gamma5 = {
        "expr": "complex(0,1)*Gamma(1,11,-22)*Gamma(2,-22,-33)*Gamma(3,-33,-44)*Gamma(4,-44,55)",
        "lorentz": [1, 2, 3, 4],
        "spinor": [11, 55],
    }
    proj_p = {
        "expr": "(Identity(1,2)+Gamma5(1,2))/2",
        "spinor": [1, 2],
    }
    proj_m = {
        "expr": "(Identity(1,2)-Gamma5(1,2))/2",
        "spinor": [1, 2],
    }
    sigma = {
        "expr": "complex(0,1)/2*(Gamma(1,11,-22)*Gamma(2,-22,33) - Gamma(2,11,-22)*Gamma(1,-22,11))",
        "lorentz": [1, 2],
        "spinor": [11, 33],
    }
    relations = {
        "gamma5": gamma5,
        "proj_p": proj_p,
        "proj_m": proj_m,
        "sigma": sigma
    }
    with open(outfile, "w") as jfile:
        json.dump(relations, jfile, indent=2)

convert("tests/models", "SM_NLO", "tests/models_json")
convert("tests/models", "sm_mg5", "tests/models_json")
convert_relations("models/common/spin_relations.json")
