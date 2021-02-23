use num_complex::Complex64;
use num_traits::{Pow, Zero};
use serde::{Deserialize, Serialize};
use std::ops;

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Number {
    Integer(i64),
    Real(f64),
    Complex(Complex64),
}

macro_rules! define_op {
    ($op:ident) => {
        pub fn $op(self) -> Number {
            match self {
                Number::Integer(i) => Number::Real((i as f64).$op()),
                Number::Real(f) => Number::Real(f.$op()),
                Number::Complex(c) => Number::Complex(c.$op()),
            }
        }
    };
}

impl Number {
    pub fn as_complex(self) -> Complex64 {
        match self {
            Number::Integer(i) => Complex64::new(i as f64, 0f64),
            Number::Real(f) => Complex64::new(f, 0f64),
            Number::Complex(c) => c,
        }
    }
    pub fn pow(self, exponent: Number) -> Number {
        match (self, exponent) {
            (Number::Integer(i), Number::Integer(j)) => {
                if j > 0 {
                    Number::Integer(i.pow(j as u32))
                } else {
                    Number::Real((i as f64).pow(j as i32))
                }
            }
            (Number::Integer(i), Number::Real(f)) => Number::Real((i as f64).pow(f as i32)),
            (Number::Integer(i), Number::Complex(c)) => {
                Number::Complex(Complex64::new(i as f64, 0f64).pow(c))
            }
            (Number::Real(f), Number::Integer(j)) => Number::Real(f.pow(j as i32)),
            (Number::Real(f), Number::Real(g)) => Number::Real(f.pow(g)),
            (Number::Real(f), Number::Complex(c)) => {
                Number::Complex(Complex64::new(f, 0f64).pow(c))
            }
            (Number::Complex(c), Number::Integer(j)) => Number::Complex((&c).pow(j)),
            (Number::Complex(c), Number::Real(f)) => Number::Complex(c.pow(f)),
            (Number::Complex(c), Number::Complex(d)) => Number::Complex(c.pow(d)),
        }
    }

    define_op!(cos);
    define_op!(sin);
    define_op!(tan);
    define_op!(acos);
    define_op!(asin);
    define_op!(atan);
    define_op!(sqrt);
    define_op!(ln);

    pub fn abs(self) -> Option<Number> {
        let number = match self {
            Number::Integer(i) => Number::Integer(i.abs()),
            Number::Real(f) => Number::Real(f.abs()),
            Number::Complex(c) => {
                if c.im.is_zero() {
                    Number::Real(c.re.abs())
                } else if c.re.is_zero() {
                    Number::Complex(Complex64::new(0f64, c.im.abs()))
                } else {
                    return None;
                }
            }
        };
        Some(number)
    }

    pub fn re(self) -> Number {
        match self {
            Number::Complex(c) => Number::Real(c.re),
            _ => self,
        }
    }

    pub fn im(self) -> Number {
        match self {
            Number::Complex(c) => Number::Real(c.im),
            _ => Number::Integer(0),
        }
    }

    pub fn complex_conjugate(self) -> Number {
        match self {
            Number::Integer(i) => Number::Complex(Complex64::new(0f64, i as f64)),
            Number::Real(f) => Number::Complex(Complex64::new(0f64, f)),
            Number::Complex(c) => Number::Complex(c.conj()),
        }
    }
}

macro_rules! impl_op {
    ($trait:ident, $op:ident) => {
        impl ops::$trait<Number> for Number {
            type Output = Number;
            fn $op(self, other: Number) -> Number {
                match (self, other) {
                    (Number::Integer(i), Number::Integer(j)) => Number::Integer(i.$op(j)),
                    (Number::Integer(i), Number::Real(f)) => Number::Real((i as f64).$op(f)),
                    (Number::Integer(i), Number::Complex(c)) => {
                        Number::Complex(Complex64::new(i as f64, 0f64).$op(c))
                    }
                    (Number::Real(f), Number::Integer(j)) => Number::Real(f.$op(j as f64)),
                    (Number::Real(f), Number::Real(g)) => Number::Real(f.$op(g)),
                    (Number::Real(f), Number::Complex(c)) => {
                        Number::Complex(Complex64::new(f, 0f64).$op(c))
                    }
                    (Number::Complex(c), Number::Integer(j)) => {
                        Number::Complex(c.$op(Complex64::new(j as f64, 0f64)))
                    }
                    (Number::Complex(c), Number::Real(f)) => {
                        Number::Complex(c.$op(Complex64::new(f, 0f64)))
                    }
                    (Number::Complex(c), Number::Complex(d)) => Number::Complex(c.$op(d)),
                }
            }
        }
    };
}
impl_op!(Add, add);
impl_op!(Mul, mul);
impl_op!(Sub, sub);
impl_op!(Div, div);

impl ops::Neg for Number {
    type Output = Number;
    fn neg(self) -> Number {
        match self {
            Number::Integer(i) => Number::Integer(-i),
            Number::Real(f) => Number::Real(-f),
            Number::Complex(c) => Number::Complex(-c),
        }
    }
}

impl Zero for Number {
    fn zero() -> Number {
        Number::Integer(0)
    }
    fn is_zero(&self) -> bool {
        match self {
            Number::Integer(i) => i.is_zero(),
            Number::Real(f) => f.is_zero(),
            Number::Complex(c) => c.is_zero(),
        }
    }
}

impl From<i64> for Number {
    fn from(other: i64) -> Number {
        Number::Integer(other)
    }
}
impl From<f64> for Number {
    fn from(other: f64) -> Number {
        Number::Real(other)
    }
}
impl From<Complex64> for Number {
    fn from(other: Complex64) -> Number {
        Number::Complex(other)
    }
}
