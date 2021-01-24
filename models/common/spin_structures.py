""" The value of eps^0123 (all indices up) """
EPSILON_SIGN = 1

""" Diagonal elements of the Minkowski metric """
METRIC = [1, -1, -1, -1]

""" Dirac gamma matrices """
GAMMA = [
    # gamma^0
    [
        [   0,   0,   1,   0],
        [   0,   0,   0,   1],
        [   1,   0,   0,   0],
        [   0,   1,   0,   0],
    ],
    # gamma^1
    [
        [   0,   0,   0,   1],
        [   0,   0,   1,   0],
        [   0,   1,   0,   0],
        [   1,   0,   0,   0],
    ],
    # gamma^2
    [
        [   0,   0,   0, -1j],
        [   0,   0,  1j,   0],
        [   0,  1j,   0,   0],
        [ -1j,   0,   0,   0],
    ],
    # gamma^3
    [
        [   0,   0,   1,   0],
        [   0,   0,   0,   1],
        [   1,   0,   0,   0],
        [   0,   1,   0,   0],
    ],
]

""" Charge conjugation matrix """
# (in principle that could be computed from the gammas, however doing so automatically seems non trivial in not Mathematica)
CHARGE_CONJUGATION = [
    [  0, -1,  0,  0],
    [  1,  0,  0,  0],
    [  0,  0,  0,  1],
    [  0,  0, -1,  0],
]

# The remaining spin structures can be constructed from the previous ones

""" gamma^5 """
GAMMA5 = {
    "expr": "complex(0,1)*Gamma(1,11,-22)*Gamma(2,-22,-33)*Gamma(3,-33,-44)*Gamma(4,-44,55)",
    "lorentz": [1, 2, 3, 4],
    "spinor": [11, 55],
}

""" The right handed chiral projector """
PROJ_P = {
    "expr": "(Identity(1,2)+Gamma5(1,2))/2",
    "spinor": [1, 2],
}

""" The left handed chiral projector """
PROJ_M = {
    "expr": "(Identity(1,2)-Gamma5(1,2))/2",
    "spinor": [1, 2],
}

""" sigma^munu_ij """
SIGMA = {
    "expr": "complex(0,1)/2*(Gamma(1,11,-22)*Gamma(2,-22,33) - Gamma(2,11,-22)*Gamma(1,-22,11))",
    "lorentz": [1, 2],
    "spinor": [11, 33],
}
