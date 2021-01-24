"""
Convert python math expressions into a serialisable format
"""

import ast

def parse_math(expr, mode=None, location=None):
    """
    Convert a math expression

    A math expression in string format is parsed using the python parser and transformed
    into a dictionary which can then be used to write a json object for the original expression.

    To parse expressions containing either lorentz or color tensors, set the `mode` argument to
    either "lorentz" or "color" respectively.

    The `location` argument is only used in error messages to express where a parse error occured.
    """
    try:
        parsed = ast.parse(expr).body
    except SyntaxError as err:
        raise ParseException(err, location)
    if len(parsed) > 1:
        raise ConversionException("Only one expression expected", location)
    try:
        return _convert_math(parsed[0].value, mode=mode)
    except ConversionException as err:
        err.location = location
        raise err

class ParseException(Exception):
    """The expression could not be parsed as python code"""
    def __init__(self, error, location="a math expression"):
        initial = "Syntax error while parsing"
        pointer = " "*(error.offset-2) + "^"
        msg = "{} {}:\n\t{}\n\t{}\t{}".format(initial, location, error.msg, error.text, pointer)
        Exception.__init__(self, msg)

class ConversionException(Exception):
    """The expression could not be converted"""
    def __init__(self, msg, location="a math expression"):
        Exception.__init__(self, msg)
        self.location = location
    def __str__(self):
        return "Failed to convert {}:\n\t{}".format(self.location, self.message)


def _convert_math(expr, mode=None, summation_indices=None):
    """Convert math expression from a python ast to a dictionary"""
    if summation_indices is None:
        summation_indices = set()
    if isinstance(expr, ast.UnaryOp):
        return _convert_unaryop(expr, summation_indices, mode=mode)
    elif isinstance(expr, ast.BinOp):
        return _convert_binop(expr, summation_indices, mode=mode)
    elif isinstance(expr, ast.Name):
        return _convert_variable(expr)
    elif isinstance(expr, ast.Call):
        return _convert_call(expr, summation_indices, mode=mode)
    elif isinstance(expr, ast.Num):
        return _convert_num(expr)
    elif isinstance(expr, ast.Attribute) and isinstance(expr.value, ast.Name):
        # The expression is either an attribute of an object or a constant in a module
        if expr.value.id == "cmath":
            # It's a mathematical constant
            return _convert_constant(expr.attr)
        elif expr.attr == "real" or expr.attr == "imag":
            # It's the real or imaginary part of a complex number
            return {
                "type": "call",
                "function": "__{}__".format(expr.attr),
                "args": [_convert_variable(expr.value)]
            }
        else:
            raise ConversionException("Unexpected attribute: {}".format(ast.dump(expr)))
    elif isinstance(expr, ast.IfExp):
        return _convert_conditional(expr, summation_indices, mode=mode)
    elif isinstance(expr, ast.Compare):
        return _convert_comparison(expr)
    else:
        raise ConversionException("Unexpected expression: {}".format(ast.dump(expr)))

def _convert_binop(expr, summation_indices, mode=None):
    """
    Convert a binary operator from a python ast to a dictionary

    `summation_indices` contains the indices that should be summed over that
    at this point in the expression have only appeared once.
    `mode` indicates if a normal expression or one containing lorentz or color tensors is being
    parsed.
    """
    indices_left = set()
    indices_right = set()
    dict_binop = {
        "type": "binary_op",
        "left":  _convert_math(expr.left, summation_indices=indices_left, mode=mode),
        "right":  _convert_math(expr.right, summation_indices=indices_right, mode=mode)
    }
    # Add the operator and handle the summation indices
    if isinstance(expr.op, ast.Add):
        dict_binop["operator"] = "add"
        assert indices_left == indices_right
        for i in indices_left:
            summation_indices.add(i)
    elif isinstance(expr.op, ast.Sub):
        dict_binop["operator"] = "sub"
        assert indices_left == indices_right
        for i in indices_left:
            summation_indices.add(i)
    elif isinstance(expr.op, ast.Mult):
        dict_binop["operator"] = "mul"
        for (index, index_kind) in indices_left.intersection(indices_right):
            # Some indices are the same between left and right => insert sums
            dict_binop = {
                "type": "sum",
                "expr": dict_binop,
                "index": {"kind": index_kind, "index": index}
            }
        for i in indices_left.symmetric_difference(indices_right):
            summation_indices.add(i)
    elif isinstance(expr.op, ast.Div):
        dict_binop["operator"] = "div"
        assert len(indices_right) == 0
        for i in indices_left:
            summation_indices.add(i)
    elif isinstance(expr.op, ast.Pow):
        dict_binop["operator"] = "pow"
        if len(indices_left) == 1 and dict_binop["right"] == {"type": "number", "value": 2}:
            # This is a special case, where e.g. P^mu*P_mu is written as P(mu)^2
            # Make the sum and the product explicit
            index, index_kind = indices_left.pop()
            return {
                "type": "sum",
                "expr": {
                    "type": "binary_op",
                    "operator": "mul",
                    "left": dict_binop["left"],
                    "right": dict_binop["left"]
                },
                "index": {"kind": index_kind, "index": index}
            }
        assert len(indices_left) == 0
        assert len(indices_right) == 0
    else:
        raise ConversionException("Unexpected binary operator: {}".format(expr.op))
    return dict_binop

def _convert_unaryop(expr, summation_indices, mode=None):
    """
    Convert a unary operator from a python ast to a dictionary

    `summation_indices` contains the indices that should be summed over that
    at this point in the expression have only appeared once.
    `mode` indicates if a normal expression or one containing lorentz or color tensors is being
    parsed.
    """
    if isinstance(expr.op, ast.UAdd):
        operator = "plus"
    elif isinstance(expr.op, ast.USub):
        operator = "minus"
    else:
        raise ConversionException("Unexpected unary operator: {}".format(expr.op))
    return {
        "type": "unary_op",
        "operator": operator,
        "operand": _convert_math(expr.operand, summation_indices=summation_indices, mode=mode),
    }

def _convert_variable(expr):
    """
    Convert a variable from a python ast to a dictionary

    `summation_indices` contains the indices that should be summed over that
    at this point in the expression have only appeared once.
    `mode` indicates if a normal expression or one containing lorentz or color tensors is being
    parsed.
    """
    return {"type": "variable", "name": expr.id}

LORENTZ_STRUCTURES = [
    "C", "Epsilon", "Gamma", "Gamma5", "Identity",
    "Metric", "P", "ProjP", "ProjM", "Sigma"
]

def _convert_lorentz_tensor(tensor, args, summation_indices):
    """
    Convert a lorentz tensor from a python ast to a dictionary

    `tensor` is the name of the tensor.
    `args` are the indices of the tensor.
    `summation_indices` contains the indices that should be summed over that
    at this point in the expression have only appeared once.
    """
    out = {"type": "lorentz_tensor", "tensor": tensor}
    if tensor == "Epsilon":
        assert len(args) == 4
        out["mu1"] = _convert_lorentz_index(args[0], summation_indices)
        out["mu2"] = _convert_lorentz_index(args[1], summation_indices)
        out["mu3"] = _convert_lorentz_index(args[2], summation_indices)
        out["mu4"] = _convert_lorentz_index(args[3], summation_indices)
    elif tensor == "Metric":
        assert len(args) == 2
        out["mu1"] = _convert_lorentz_index(args[0], summation_indices)
        out["mu2"] = _convert_lorentz_index(args[1], summation_indices)
    elif tensor == "Gamma":
        assert len(args) == 3
        out["mu1"] = _convert_lorentz_index(args[0], summation_indices)
        out["i2"] = _convert_spinor_index(args[1], summation_indices)
        out["i3"] = _convert_spinor_index(args[2], summation_indices)
    elif tensor in ["C", "Gamma5", "Identity", "ProjP", "ProjM"]:
        assert len(args) == 2
        out["i1"] = _convert_spinor_index(args[0], summation_indices)
        out["i2"] = _convert_spinor_index(args[1], summation_indices)
    elif tensor == "Sigma":
        assert len(args) == 4
        out["mu1"] = _convert_lorentz_index(args[0], summation_indices)
        out["mu2"] = _convert_lorentz_index(args[1], summation_indices)
        out["i3"] = _convert_spinor_index(args[2], summation_indices)
        out["i4"] = _convert_spinor_index(args[3], summation_indices)
    elif tensor == "P":
        assert len(args) == 2
        out["mu1"] = _convert_lorentz_index(args[0], summation_indices)
        out["particle"] = _convert_particle_number(args[1])
    return out

def _convert_spinor_index(expr, indices):
    return _convert_index(expr, "spinor", indices)

def _convert_lorentz_index(expr, indices):
    return _convert_index(expr, "lorentz", indices)

def _convert_particle_number(expr):
    if isinstance(expr, ast.Num):
        return expr.n
    else:
        raise ConversionException("Unexpected expression for particle number: {}".format(expr))

COLOR_STRUCTURES = ["Identity", "T", "f", "d", "Epsilon", "EpsilonBar", "T6", "K6", "K6Bar"]

def _convert_color_tensor(tensor, args, summation_indices):
    """
    Convert a color tensor from a python ast to a dictionary

    `tensor` is the name of the tensor.
    `args` are the indices of the tensor.
    `summation_indices` contains the indices that should be summed over that
    at this point in the expression have only appeared once.
    """
    out = {"type": "color_tensor", "tensor": tensor}
    if tensor == "Identity":
        assert len(args) == 2
        out["i1"] = _convert_fundamental_index(args[0], summation_indices)
        out["jb2"] = _convert_fundamental_index(args[1], summation_indices)
    elif tensor == "T":
        assert len(args) == 3
        out["a1"] = _convert_adjoint_index(args[0], summation_indices)
        out["i2"] = _convert_fundamental_index(args[1], summation_indices)
        out["jb3"] = _convert_fundamental_index(args[2], summation_indices)
    elif tensor == "f":
        assert len(args) == 3
        out["a1"] = _convert_adjoint_index(args[0], summation_indices)
        out["a2"] = _convert_adjoint_index(args[1], summation_indices)
        out["a3"] = _convert_adjoint_index(args[2], summation_indices)
    elif tensor == "d":
        assert len(args) == 3
        out["a1"] = _convert_adjoint_index(args[0], summation_indices)
        out["a2"] = _convert_adjoint_index(args[1], summation_indices)
        out["a3"] = _convert_adjoint_index(args[2], summation_indices)
    elif tensor == "Epsilon":
        assert len(args) == 3
        out["i1"] = _convert_fundamental_index(args[0], summation_indices)
        out["i2"] = _convert_fundamental_index(args[1], summation_indices)
        out["i3"] = _convert_fundamental_index(args[2], summation_indices)
    elif tensor == "EpsilonBar":
        assert len(args) == 3
        out["ib1"] = _convert_fundamental_index(args[0], summation_indices)
        out["ib2"] = _convert_fundamental_index(args[1], summation_indices)
        out["ib3"] = _convert_fundamental_index(args[2], summation_indices)
    elif tensor == "T6":
        assert len(args) == 3
        out["a1"] = _convert_adjoint_index(args[0], summation_indices)
        out["alpha2"] = _convert_fundamental_index(args[1], summation_indices)
        out["betab3"] = _convert_fundamental_index(args[2], summation_indices)
    elif tensor == "K6":
        assert len(args) == 3
        out["alpha1"] = _convert_sextet_index(args[0], summation_indices)
        out["ib2"] = _convert_fundamental_index(args[1], summation_indices)
        out["jb3"] = _convert_fundamental_index(args[2], summation_indices)
    elif tensor == "K6Bar":
        assert len(args) == 3
        out["alphab1"] = _convert_sextet_index(args[0], summation_indices)
        out["i2"] = _convert_fundamental_index(args[1], summation_indices)
        out["j3"] = _convert_fundamental_index(args[2], summation_indices)
    return out

def _convert_fundamental_index(expr, indices):
    return _convert_index(expr, "fundamental", indices)

def _convert_adjoint_index(expr, indices):
    return _convert_index(expr, "adjoint", indices)

def _convert_sextet_index(expr, indices):
    return _convert_index(expr, "sextet", indices)

def _convert_index(expr, index_kind, indices):
    try:
        index = _convert_numeric_literal(expr)
        if index < 0:
            indices.add((index, index_kind))
        return index
    except:
        raise ConversionException("Unexpected expression for {} index: {}".format(index_kind, expr))

def _convert_numeric_literal(expr):
    if isinstance(expr, ast.Num):
        return expr.n
    else:
        raise ConversionException("Unexpected expression for numeric literal: {}".format(expr))

def _convert_call(expr, summation_indices, mode=None):
    """
    Convert a function call from a python ast to a dictionary

    `summation_indices` contains the indices that should be summed over that
    at this point in the expression have only appeared once.
    `mode` indicates if a normal expression or one containing lorentz or color tensors is being
    parsed.
    """
    if expr.keywords != [] or expr.starargs is not None or expr.kwargs is not None:
        raise ConversionException("keyword or other special function arguments are not supported")
    if isinstance(expr.func, ast.Name):
        # The function name is an unqualified name
        func = expr.func.id
        if func == "complex":
            # Special case, where the function call creates a complex number
            assert len(expr.args) == 2
            return {
                "type": "complex",
                "value": [
                    _convert_numeric_literal(expr.args[0]),
                    _convert_numeric_literal(expr.args[1])
                ]
            }
    elif isinstance(expr.func, ast.Attribute) and isinstance(expr.func.value, ast.Name):
        # The function is either in a different module or this is a method call
        if expr.func.value.id == "cmath":
            # The function that is being called is in the `cmath` module
            func = expr.func.attr
        elif expr.func.attr == "conjugate":
            # The call is a method call of the form `arg.conjugate()`
            return {
                "type": "call",
                "function": "__complex_conjugate__",
                "args": [_convert_variable(expr.func.value)]
            }
        else:
            msg = "Unexpected function or method: {}".format(ast.dump(expr.func))
            raise ConversionException(msg)
    else:
        raise ConversionException("Unexpected function call: {}".format(ast.dump(expr.func)))
    # Based on the `mode` argument parse the function as either a normal function, a lorentz tensor
    #  or a color tensor.
    # The 'mode' argument is necessary, since the names of the tensors are not unique and it's not
    # clear if for example 'Epsilon' is a lorentz or a color tensor. In principle this is
    # irrelevant, since they are identical but at least for the types of the summation indices the
    # color vs. lorentz distinction is necessary.
    if mode == "lorentz" and func in LORENTZ_STRUCTURES:
        return _convert_lorentz_tensor(func, expr.args, summation_indices)
    elif mode == "color" and func in COLOR_STRUCTURES:
        return _convert_color_tensor(func, expr.args, summation_indices)
    else:
        return {
            "type": "call",
            "function": func,
            "args": [_convert_math(arg, mode=mode) for arg in expr.args]
        }

def _convert_num(expr):
    """ Convert a number from a python ast to a dictionary """
    return {"type": "number", "value": expr.n}

def _convert_constant(expr):
    """
    Convert a constant from a python ast to a dictionary

    Currently supported are the following constants:
    * pi
    """
    if expr == "pi":
        return {"type": "constant", "name": "pi"}
    else:
        raise ConversionException("Unexpected mathematical constant: {}".format(expr))

def _convert_conditional(expr, summation_indices, mode=None):
    """
    Convert a conditional expression from a python ast to a dictionary

    `summation_indices` contains the indices that should be summed over that
    at this point in the expression have only appeared once.
    `mode` indicates if a normal expression or one containing lorentz or color tensors is being
    parsed.
    """
    return {
        "type": "conditional",
        "condition": _convert_math(expr.test, summation_indices=summation_indices, mode=mode),
        "if_true": _convert_math(expr.body, summation_indices=summation_indices, mode=mode),
        "if_false": _convert_math(expr.orelse, summation_indices=summation_indices, mode=mode)
        }

def comparison_operator(operator):
    """ Convert a comparison operator from a python ast to a dictionary """
    if isinstance(operator, ast.Eq):
        return "equals"
    else:
        raise "Unexpected comparison operator: {}".format(operator)

def _convert_comparison(expr):
    """ Convert a comparison expression from a python ast to a dictionary """
    return {
        "type": "comparison",
        "operators": map(comparison_operator, expr.ops),
        "left": _convert_math(expr.left),
        "comparators": [_convert_math(x) for x in expr.comparators]
    }
