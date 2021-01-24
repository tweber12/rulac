import ast

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

def convert_math(expr, mode=None, summation_indices=None):
    if summation_indices is None:
        summation_indices = set()
    if type(expr) == ast.UnaryOp:
        return convert_unaryop(expr, summation_indices, mode=mode)
    elif type(expr) == ast.BinOp:
        return convert_binop(expr, summation_indices, mode=mode)
    elif type(expr) == ast.Name:
        return convert_variable(expr)
    elif type(expr) == ast.Call:
        return convert_call(expr, summation_indices, mode=mode)
    elif type(expr) == ast.Num:
        return convert_num(expr)
    elif type(expr) == ast.Attribute and type(expr.value) == ast.Name and expr.value.id == "cmath":
        return convert_constant(expr.attr)
    elif type(expr) == ast.Attribute and type(expr.value) == ast.Name and (expr.attr == "real" or expr.attr == "imag"):
        return {"type": "call", "function": "__{}__".format(expr.attr), "args": [convert_variable(expr.value)]}
    elif type(expr) == ast.IfExp:
        return convert_conditional(expr, summation_indices, mode=mode)
    elif type(expr) == ast.Str:
        return convert_str(expr)
    elif type(expr) == ast.Compare:
        return convert_comparison(expr)
    else:
        raise Exception("Unexpected expression: {}".format(ast.dump(expr)))

def convert_binop(expr, summation_indices, mode=None):
    il = set()
    ir = set()
    d = dict()
    d["type"] = "binary_op"
    d["left"] = convert_math(expr.left, summation_indices=il, mode=mode)
    d["right"] = convert_math(expr.right, summation_indices=ir, mode=mode)
    if type(expr.op) == ast.Add:
        d["operator"] = "add"
        assert(il == ir)
        for i in il:
            summation_indices.add(i)
    elif type(expr.op) == ast.Sub:
        d["operator"] = "sub"
        assert(il == ir)
        for i in il:
            summation_indices.add(i)
    elif type(expr.op) == ast.Mult:
        d["operator"] = "mul"
        for (i,t) in il.intersection(ir):
            d = {"type": "sum", "expr": d, "index": {"kind": t, "index": i}}
        for i in il.symmetric_difference(ir):
            summation_indices.add(i)
    elif type(expr.op) == ast.Div:
        d["operator"] = "div"
        assert(len(ir) == 0)
        for i in il:
            summation_indices.add(i)
    elif type(expr.op) == ast.Pow:
        d["operator"] = "pow"
        if len(il) == 1 and d["right"] == {"type": "number", "value": 2}:
            i,t = il.pop()
            return {"type": "sum", "expr": {"type": "binary_op", "operator": "mul", "left": d["left"], "right": d["left"]}, "index": {"kind": t, "index": i}}
        assert(len(il) == 0)
        assert(len(ir) == 0)
    else:
        raise Exception("Unexpected binary operator: {}".format(expr.op))
    return d

def convert_unaryop(expr, summation_indices, mode=None):
    d = dict()
    d["type"] = "unary_op"
    d["operand"] = convert_math(expr.operand, summation_indices=summation_indices, mode=mode)
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

def convert_lorentz_tensor(tensor, args, summation_indices):
    out = {"type": "lorentz_tensor", "tensor": tensor}
    if tensor == "Epsilon":
        assert(len(args)==4)
        out["mu1"] = convert_lorentz_index(args[0], summation_indices)
        out["mu2"] = convert_lorentz_index(args[1], summation_indices)
        out["mu3"] = convert_lorentz_index(args[2], summation_indices)
        out["mu4"] = convert_lorentz_index(args[3], summation_indices)
    elif tensor == "Metric":
        assert(len(args)==2)
        out["mu1"] = convert_lorentz_index(args[0], summation_indices)
        out["mu2"] = convert_lorentz_index(args[1], summation_indices)
    elif tensor == "Gamma":
        assert(len(args)==3)
        out["mu1"] = convert_lorentz_index(args[0], summation_indices)
        out["i2"] = convert_spinor_index(args[1], summation_indices)
        out["i3"] = convert_spinor_index(args[2], summation_indices)
    elif tensor == "C" or tensor == "Gamma5" or tensor == "Identity" or tensor == "ProjP" or tensor == "ProjM":
        assert(len(args)==2)
        out["i1"] = convert_spinor_index(args[0], summation_indices)
        out["i2"] = convert_spinor_index(args[1], summation_indices)
    elif tensor == "Sigma":
        assert(len(args)==4)
        out["mu1"] = convert_lorentz_index(args[0], summation_indices)
        out["mu2"] = convert_lorentz_index(args[1], summation_indices)
        out["i3"] = convert_spinor_index(args[2], summation_indices)
        out["i4"] = convert_spinor_index(args[3], summation_indices)
    elif tensor == "P":
        assert(len(args)==2)
        out["mu1"] = convert_lorentz_index(args[0], summation_indices)
        out["particle"] = convert_particle_number(args[1])
    return out

def convert_spinor_index(expr, indices):
    n = convert_index(expr)
    if n < 0:
        indices.add((n,"spinor"))
    return n

def convert_lorentz_index(expr, indices):
    n = convert_index(expr)
    if n < 0:
        indices.add((n,"lorentz"))
    return n

def convert_particle_number(expr):
    if type(expr) == ast.Num:
        return expr.n
    else:
        raise Exception("Unexpected expression for particle number: {}".format(expr))

color_structures = ["Identity", "T", "f", "d", "Epsilon", "EpsilonBar", "T6", "K6", "K6Bar"]

def convert_color_tensor(tensor, args, summation_indices):
    out = {"type": "color_tensor", "tensor": tensor}
    if tensor == "Identity":
        assert(len(args)==2)
        out["i1"] = convert_fundamental_index(args[0], summation_indices)
        out["jb2"] = convert_fundamental_index(args[1], summation_indices)
    elif tensor == "T":
        assert(len(args)==3)
        out["a1"] = convert_adjoint_index(args[0], summation_indices)
        out["i2"] = convert_fundamental_index(args[1], summation_indices)
        out["jb3"] = convert_fundamental_index(args[2], summation_indices)
    elif tensor == "f":
        assert(len(args)==3)
        out["a1"] = convert_adjoint_index(args[0], summation_indices)
        out["a2"] = convert_adjoint_index(args[1], summation_indices)
        out["a3"] = convert_adjoint_index(args[2], summation_indices)
    elif tensor == "d":
        assert(len(args)==3)
        out["a1"] = convert_adjoint_index(args[0], summation_indices)
        out["a2"] = convert_adjoint_index(args[1], summation_indices)
        out["a3"] = convert_adjoint_index(args[2], summation_indices)
    elif tensor == "Epsilon":
        assert(len(args)==3)
        out["i1"] = convert_fundamental_index(args[0], summation_indices)
        out["i2"] = convert_fundamental_index(args[1], summation_indices)
        out["i3"] = convert_fundamental_index(args[2], summation_indices)
    elif tensor == "EpsilonBar":
        assert(len(args)==3)
        out["ib1"] = convert_fundamental_index(args[0], summation_indices)
        out["ib2"] = convert_fundamental_index(args[1], summation_indices)
        out["ib3"] = convert_fundamental_index(args[2], summation_indices)
    elif tensor == "T6":
        assert(len(args)==3)
        out["a1"] = convert_adjoint_index(args[0], summation_indices)
        out["alpha2"] = convert_fundamental_index(args[1], summation_indices)
        out["betab3"] = convert_fundamental_index(args[2], summation_indices)
    elif tensor == "K6":
        assert(len(args)==3)
        out["alpha1"] = convert_sextet_index(args[0], summation_indices)
        out["ib2"] = convert_fundamental_index(args[1], summation_indices)
        out["jb3"] = convert_fundamental_index(args[2], summation_indices)
    elif tensor == "K6Bar":
        assert(len(args)==3)
        out["alphab1"] = convert_sextet_index(args[0], summation_indices)
        out["i2"] = convert_fundamental_index(args[1], summation_indices)
        out["j3"] = convert_fundamental_index(args[2], summation_indices)
    return out

def convert_fundamental_index(expr, indices):
    n = convert_index(expr)
    if n < 0:
        indices.add((n,"fundamental"))
    return n

def convert_adjoint_index(expr, indices):
    n = convert_index(expr)
    if n < 0:
        indices.add((n,"adjoint"))
    return n

def convert_sextet_index(expr, indices):
    n = convert_index(expr)
    if n < 0:
        indices.add((n,"sextet"))
    return n

def convert_index(expr):
    if type(expr) == ast.Num:
        return expr.n
    else:
        raise Exception("Unexpected expression for lorentz index: {}".format(expr))

def convert_call(expr, summation_indices, mode=None):
    if expr.keywords != [] or expr.starargs is not None or expr.kwargs is not None:
        raise Exception("keyword or other special function arguments are not supported")
    if type(expr.func) == ast.Name:
        func = expr.func.id
        if func == "complex":
            assert(len(expr.args) == 2)
            return {"type": "complex", "value": [convert_index(expr.args[0]), convert_index(expr.args[1])]}
    elif type(expr.func) == ast.Attribute and type(expr.func.value) == ast.Name and expr.func.value.id == "cmath":
        func = expr.func.attr
    elif type(expr.func) == ast.Attribute and type(expr.func.value) == ast.Name and expr.func.attr == "conjugate":
        return {"type": "call", "function": "__complex_conjugate__", "args": [convert_variable(expr.func.value)]}
    else:
        raise Exception("unexpected function call: {}".format(ast.dump(expr.func)))
    # The 'mode' argument is necessary, since the names of the tensors are not unique and it's not clear if
    # for example 'Epsilon' is a lorentz or a color tensor. In principle this is irrelevant, since they are
    # identical but at least for the types of the summation indices the color vs. lorentz distinction is
    # necessary.
    if mode=="lorentz" and func in lorentz_structures:
        return convert_lorentz_tensor(func, expr.args, summation_indices)
    if mode=="color" and func in color_structures:
        return convert_color_tensor(func, expr.args, summation_indices)
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

def convert_conditional(expr, summation_indices, mode=None):
    return {
        "type": "conditional",
        "condition": convert_math(expr.test, summation_indices=summation_indices, mode=mode),
        "if_true": convert_math(expr.body, summation_indices=summation_indices, mode=mode),
        "if_false": convert_math(expr.orelse, summation_indices=summation_indices, mode=mode)
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