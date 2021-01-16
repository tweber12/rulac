import ast
import errno
import importlib
import json
import os
import sys

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
    d["couplings"] = map(lambda ((c,l),v): {"color": c, "lorentz": l, "coupling": v.name}, d["couplings"].iteritems())
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

def parse_math(expr, mode=None, location=None):
    try:
        parsed = ast.parse(expr).body
    except SyntaxError as err:
        if location == None:
            raise Exception("Syntax error while parsing a math expression:\n\t{}\n\t{}\t{}{}".format(err.msg, err.text, " "*(err.offset-2),"^"))
        else:
            raise Exception("Syntax error while parsing {}:\n\t{}\n\t{}\t{}{}".format(location, err.msg, err.text, " "*(err.offset-2),"^"))
    if len(parsed) > 1:
        raise Exception("Only one expression expected")
    try:
        return convert_math(parsed[0].value, mode=mode)
    except Exception as err:
        if location == None:
            raise Exception("Failed to convert a math expression:\n\t{}".format(err.message))
        else:
            raise Exception("Failed to convert {}:\n\t{}".format(location, err.message))

def convert_math(expr, mode=None):
    if type(expr) == ast.UnaryOp:
        return convert_unaryop(expr, mode=mode)
    elif type(expr) == ast.BinOp:
        return convert_binop(expr, mode=mode)
    elif type(expr) == ast.Name:
        return convert_variable(expr)
    elif type(expr) == ast.Call:
        return convert_call(expr, mode=mode)
    elif type(expr) == ast.Num:
        return convert_num(expr)
    elif type(expr) == ast.Attribute and type(expr.value) == ast.Name and expr.value.id == "cmath":
        return convert_constant(expr.attr)
    elif type(expr) == ast.Attribute and type(expr.value) == ast.Name and (expr.attr == "real" or expr.attr == "imag"):
        return {"type": "call", "function": "__{}__".format(expr.attr), "args": [convert_variable(expr.value)]}
    elif type(expr) == ast.IfExp:
        return convert_conditional(expr, mode=mode)
    elif type(expr) == ast.Str:
        return convert_str(expr)
    elif type(expr) == ast.Compare:
        return convert_comparison(expr)
    else:
        raise Exception("Unexpected expression: {}".format(ast.dump(expr)))

def convert_binop(expr, mode=None):
    d = dict()
    d["type"] = "binary_op"
    d["left"] = convert_math(expr.left, mode=mode)
    d["right"] = convert_math(expr.right, mode=mode)
    if type(expr.op) == ast.Add:
        d["operator"] = "add"
    elif type(expr.op) == ast.Sub:
        d["operator"] = "sub"
    elif type(expr.op) == ast.Mult:
        d["operator"] = "mul"
    elif type(expr.op) == ast.Div:
        d["operator"] = "div"
    elif type(expr.op) == ast.Pow:
        d["operator"] = "pow"
    else:
        raise Exception("Unexpected binary operator: {}".format(expr.op))
    return d

def convert_unaryop(expr, mode=None):
    d = dict()
    d["type"] = "unary_op"
    d["operand"] = convert_math(expr.operand, mode=mode)
    if type(expr.op) == ast.UAdd:
        d["operator"] = "plus"
    elif type(expr.op) == ast.USub:
        d["operator"] = "minus"
    else:
        raise Exception("Unexpected unary operator: {}".format(expr.op))
    return d

def convert_variable(expr):
    return {"type": "variable", "name": expr.id}

lorentz_structures = ["C", "Epsilon", "Gamma", "Gamma5", "Identity", "Metric", "P", "ProjP", "ProjM", "Sigma"]
color_structures = ["Identity", "T", "f", "d", "Epsilon", "EpsilonBar", "T6", "K6", "K6Bar"]
def convert_call(expr, mode=None):
    if expr.keywords != [] or expr.starargs is not None or expr.kwargs is not None:
        raise Exception("keyword or other special function arguments are not supported")
    if type(expr.func) == ast.Name:
        func = expr.func.id
    elif type(expr.func) == ast.Attribute and type(expr.func.value) == ast.Name and expr.func.value.id == "cmath":
        func = expr.func.attr
    elif type(expr.func) == ast.Attribute and type(expr.func.value) == ast.Name and expr.func.attr == "conjugate":
        return {"type": "call", "function": "__complex_conjugate__", "args": [convert_variable(expr.func.value)]}
    else:
        raise Exception("unexpected function call: {}".format(ast.dump(expr.func)))
    if mode=="color" and func in color_structures:
            return {"type": "color_tensor", "tensor": func, "indices": map(lambda arg: convert_math(arg, mode=mode), expr.args)}
    if mode=="lorentz" and func in lorentz_structures:
            return {"type": "lorentz_tensor", "tensor": func, "indices": map(lambda arg: convert_math(arg, mode=mode), expr.args)}
    return {"type": "call", "function": func, "args": map(lambda arg: convert_math(arg, mode=mode), expr.args)}

def convert_num(expr):
    return {"type": "number", "value": expr.n}

def convert_constant(expr):
    if expr == "pi":
        return {"type": "constant", "name": "pi"}
    else:
        raise Exception("Unexpected mathematical constant: {}".format(expr))

def convert_str(expr):
    return {"type": "string", "value": expr.s}

def convert_conditional(expr, mode=None):
    return {
        "type": "conditional",
        "condition": convert_math(expr.test, mode=mode),
        "if_true": convert_math(expr.body, mode=mode),
        "if_false": convert_math(expr.orelse, mode=mode)
        }

def comparison_operator(op):
    if type(op) == ast.Eq:
        return "equals"
    else:
        raise "Unexpected comparison operator: {}".format(op)

def convert_comparison(expr, mode=None):
    return {
        "type": "comparison",
        "operators": map(comparison_operator, expr.ops),
        "left": convert_math(expr.left, mode=mode),
        "comparators": map(lambda x: convert_math(x, mode=mode), expr.comparators)
    }

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
    try:
        propagators = map(convert_propagator, model.all_propagators)
        with open(os.path.join(out_dir, "propagators.json"), "w") as jfile:
            json.dump(propagators, jfile, indent=2)
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


convert("tests/models", "sm", "tests/models_json")