use std::ops::Rem;
use std::cmp::Ordering;
use ethers::types::U256;
use crate::i256::*;

use primitive_types::U512;

pub fn div(op1: U256, op2: U256) -> U256 {
    if op2.is_zero() {
        U256::zero()
    } else {
        //op1 / op2
        super::i256::div_u256::div_mod(op1, op2).0
    }
}

pub fn sdiv(op1: U256, op2: U256) -> U256 {
    i256_div(op1, op2)
}

pub fn rem(op1: U256, op2: U256) -> U256 {
    if op2.is_zero() {
        U256::zero()
    } else {
        op1.rem(op2)
    }
}

pub fn smod(op1: U256, op2: U256) -> U256 {
    if op2.is_zero() {
        U256::zero()
    } else {
        i256_mod(op1, op2)
    }
}

pub fn addmod(op1: U256, op2: U256, op3: U256) -> U256 {
    if op3.is_zero() {
        U256::zero()
    } else {
        let op1: U512 = op1.into();
        let op2: U512 = op2.into();
        let op3: U512 = op3.into();
        let v = (op1 + op2) % op3;
        v.try_into()
            .expect("op3 is less than U256::MAX, thus it never overflows; qed")
    }
}

pub fn mulmod(op1: U256, op2: U256, op3: U256) -> U256 {
    if op3.is_zero() {
        U256::zero()
    } else {
        let op1: U512 = op1.into();
        let op2: U512 = op2.into();
        let op3: U512 = op3.into();
        let v = (op1 * op2) % op3;
        v.try_into()
            .expect("op3 is less than U256::MAX, thus it never overflows; qed")
    }
}

pub fn exp(op1: U256, op2: U256) -> U256 {
    let mut op1 = op1;
    let mut op2 = op2;
    let mut r: U256 = 1.into();

    while op2 != 0.into() {
        if op2 & 1.into() != 0.into() {
            r = r.overflowing_mul(op1).0;
        }
        op2 >>= 1;
        op1 = op1.overflowing_mul(op1).0;
    }
    r
}

pub fn signextend(op1: U256, op2: U256) -> U256 {
    if op1 < U256::from(32) {
        // `low_u32` works since op1 < 32
        let bit_index = (8 * op1.low_u32() + 7) as usize;
        let bit = op2.bit(bit_index);
        let mask = (U256::one() << bit_index) - U256::one();
        if bit {
            op2 | !mask
        } else {
            op2 & mask
        }
    } else {
        op2
    }
}

pub fn slt(op1: U256, op2: U256) -> U256 {
    if i256_cmp(op1, op2) == Ordering::Less {
        U256::one()
    } else {
        U256::zero()
    }
}

pub fn sgt(op1: U256, op2: U256) -> U256 {
    if i256_cmp(op1, op2) == Ordering::Greater {
        U256::one()
    } else {
        U256::zero()
    }
}

pub fn iszero(op1: U256) -> U256 {
    if op1.is_zero() {
        U256::one()
    } else {
        U256::zero()
    }
}

pub fn not(op1: U256) -> U256 {
    !op1
}

pub fn byte(op1: U256, op2: U256) -> U256 {
    let mut ret = U256::zero();

    for i in 0..256 {
        if i < 8 && op1 < 32.into() {
            let o: usize = op1.as_usize();
            let t = 255 - (7 - i + 8 * o);
            let bit_mask = U256::one() << t;
            let value = (op2 & bit_mask) >> t;
            ret = ret.overflowing_add(value << i).0;
        }
    }

    ret
}

pub fn shl(shift: U256, value: U256) -> U256 {
    if value.is_zero() || shift >= U256::from(256) {
        U256::zero()
    } else {
        let shift: u64 = shift.as_u64();
        value << shift as usize
    }
}

pub fn shr(shift: U256, value: U256) -> U256 {
    if value.is_zero() || shift >= U256::from(256) {
        U256::zero()
    } else {
        let shift: u64 = shift.as_u64();
        value >> shift as usize
    }
}

pub fn sar(shift: U256, mut value: U256) -> U256 {
    let value_sign = i256_sign::<true>(&mut value);

    if value.is_zero() || shift >= U256::from(256) {
        match value_sign {
            // value is 0 or >=1, pushing 0
            Sign::Plus | Sign::Zero => U256::zero(),
            // value is <0, pushing -1
            Sign::Minus => two_compl(U256::one()),
        }
    } else {
        let shift: u64 = shift.as_u64();

        match value_sign {
            Sign::Plus | Sign::Zero => value >> shift as usize,
            Sign::Minus => {
                let shifted = ((value.overflowing_sub(U256::one()).0) >> shift as usize)
                    .overflowing_add(U256::one())
                    .0;
                two_compl(shifted)
            }
        }
    }
}