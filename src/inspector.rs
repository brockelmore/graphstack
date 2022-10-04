use std::collections::BTreeSet;
use std::ops::*;
use std::collections::VecDeque;
use ethers::types::*;
use petgraph::dot::Dot;
use petgraph::prelude::*;
use revm::{EVMData, Inspector, Interpreter, Return, opcode::*};
use core::ops::{BitAnd, BitOr, BitXor};
use crate::arithmetic::*;

#[derive(Debug, Clone)]
pub enum MemoryElem {
    FromStack {
        offset: NodeIndex,
        stack_elem: NodeIndex,
        one_byte: bool,
    },
    FromCalldata {
        offset: NodeIndex,
        dest_offset: NodeIndex,
        size: NodeIndex,
    },
    FromReturndata {
        offset: NodeIndex,
        dest_offset: NodeIndex,
        size: NodeIndex,
        from_call: bool,
    },
    FromCode {
        address: Option<NodeIndex>,
        offset: NodeIndex,
        dest_offset: NodeIndex,
        size: NodeIndex,
    },
}

impl MemoryElem {
    // fn offset_index(&self, graph: &Dag) -> NodeIndex {
    //     match &self {
    //         MemoryElem::FromStack { offset, .. } => {
    //             *offset
    //         }
    //         MemoryElem::FromCalldata { offset, .. } => {
    //             *offset
    //         }
    //         MemoryElem::FromReturndata { offset, .. } => {
    //             *offset
    //         }
    //         MemoryElem::FromCode { offset, .. } => {
    //             *offset
    //         }
    //     }
    // }

    // fn offset_symb<'a>(&'a self, graph: &'a Dag) -> &StackElem {
    //     match &self {
    //         MemoryElem::FromStack { offset, .. } => {
    //             let node = graph.node_weight(*offset).expect("No node for node index");
    //             match node {
    //                 GraphElem::Stack(stack_elem) => {
    //                     stack_elem
    //                 }
    //                 GraphElem::Memory(mem_elem) => {
    //                     mem_elem.offset_symb(graph)
    //                 }
    //                 _ => unreachable!()
    //             }
    //         }
    //         MemoryElem::FromCalldata { offset, .. } => {
    //             let node = graph.node_weight(*offset).expect("No node for node index");
    //             match node {
    //                 GraphElem::Stack(stack_elem) => {
    //                     stack_elem
    //                 }
    //                 GraphElem::Memory(mem_elem) => {
    //                     mem_elem.offset_symb(graph)
    //                 }
    //                 _ => unreachable!()
    //             }
    //         }
    //         MemoryElem::FromReturndata { offset, .. } => {
    //             let node = graph.node_weight(*offset).expect("No node for node index");
    //             match node {
    //                 GraphElem::Stack(stack_elem) => {
    //                     stack_elem
    //                 }
    //                 GraphElem::Memory(mem_elem) => {
    //                     mem_elem.offset_symb(graph)
    //                 }
    //                 _ => unreachable!()
    //             }
    //         }
    //         MemoryElem::FromCode { offset, .. } => {
    //             let node = graph.node_weight(*offset).expect("No node for node index");
    //             match node {
    //                 GraphElem::Stack(stack_elem) => {
    //                     stack_elem
    //                 }
    //                 GraphElem::Memory(mem_elem) => {
    //                     mem_elem.offset_symb(graph)
    //                 }
    //                 _ => unreachable!()
    //             }
    //         }
    //     }
    // }

    fn offset(&self, graph: &Dag) -> Option<U256> {
        match &self {
            MemoryElem::FromStack { offset, .. } => {
                let node = graph.node_weight(*offset).expect("No node for node index");
                match node {
                    GraphElem::Stack(stack_elem) => {
                        match stack_elem {
                            StackElem::Concrete(x) => {
                                Some(*x)
                            }
                            _ => None
                        }
                    }
                    GraphElem::Memory(mem_elem) => {
                        mem_elem.offset(graph)
                    }
                    _ => None
                }
            }
            MemoryElem::FromCalldata { offset, .. } => {
                let node = graph.node_weight(*offset).expect("No node for node index");
                match node {
                    GraphElem::Stack(stack_elem) => {
                        match stack_elem {
                            StackElem::Concrete(x) => {
                                Some(*x)
                            }
                            _ => None
                        }
                    }
                    GraphElem::Memory(mem_elem) => {
                        mem_elem.offset(graph)
                    }
                    _ => None
                }
            }
            MemoryElem::FromReturndata { offset, .. } => {
                let node = graph.node_weight(*offset).expect("No node for node index");
                match node {
                    GraphElem::Stack(stack_elem) => {
                        match stack_elem {
                            StackElem::Concrete(x) => {
                                Some(*x)
                            }
                            _ => None
                        }
                    }
                    GraphElem::Memory(mem_elem) => {
                        mem_elem.offset(graph)
                    }
                    _ => None
                }
            }
            MemoryElem::FromCode { offset, .. } => {
                let node = graph.node_weight(*offset).expect("No node for node index");
                match node {
                    GraphElem::Stack(stack_elem) => {
                        match stack_elem {
                            StackElem::Concrete(x) => {
                                Some(*x)
                            }
                            _ => None
                        }
                    }
                    GraphElem::Memory(mem_elem) => {
                        mem_elem.offset(graph)
                    }
                    _ => None
                }
            }
        }
    }

    fn eq(&self, other: &Self, graph: &Dag) -> bool {
        self.offset(&graph) == other.offset(&graph)
    }

    fn ne(&self, other: &Self, graph: &Dag) -> bool {
        self.offset(&graph) != other.offset(&graph)
    }

    fn lt(&self, other: &Self, graph: &Dag) -> bool {
        if let Some(self_off) = self.offset(graph) {
            if let Some(other_off) = other.offset(graph) {
                self_off < other_off
            } else {
                true
            }
        } else {
            false
        }
    }

    fn le(&self, other: &Self, graph: &Dag) -> bool {
        if let Some(self_off) = self.offset(&graph) {
            if let Some(other_off) = other.offset(&graph) {
                self_off <= other_off
            } else {
                true
            }
        } else {
            false
        }
    }

    fn gt(&self, other: &Self, graph: &Dag) -> bool {
        if let Some(self_off) = self.offset(&graph) {
            if let Some(other_off) = other.offset(&graph) {
                self_off > other_off
            } else {
                false
            }
        } else {
            true
        }
    }

    fn ge(&self, other: &Self, graph: &Dag) -> bool {
        if let Some(self_off) = self.offset(&graph) {
            if let Some(other_off) = other.offset(&graph) {
                self_off >= other_off
            } else {
                false
            }
        } else {
            true
        }
    }
}

#[derive(Clone)]
#[derive(PartialEq)]
pub enum StackElem {
    Symbolic(SymType),
    Concrete(U256),
}


impl std::fmt::Debug for StackElem {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &*self {
            StackElem::Symbolic(SymType::Intrinsic(x)) => write!(f, "Intrinsic::{:?}", x),
            StackElem::Symbolic(SymType::Calldata{offsets}) => {
                 write!(f, "Calldata{:?}", offsets)
            },
            StackElem::Symbolic(SymType::Storage{slots}) => {
                write!(f, "Storage Slots: {:?}", slots)
            },
            StackElem::Symbolic(SymType::Mixed {
                intrinsics,
                offsets,
                slots,
                mem_ops,
            }) => {
                write!(f, "(")?;
                let mut has_prev = false;
                if !intrinsics.is_empty() {
                    write!(f, "Intrinsics: {:?}", intrinsics)?;
                    has_prev = true;
                }
                if !slots.is_empty() {
                    if has_prev {
                        write!(f, ", ")?;
                    }
                    write!(f, "Slots: {:?}", intrinsics)?;
                }

                if !offsets.is_empty() {
                    if has_prev {
                        write!(f, ", ")?;
                    }
                    write!(f, "Calldata Offsets: {:?}", offsets)?;
                }

                if !mem_ops.is_empty() {
                    if has_prev {
                        write!(f, ", ")?;
                    }
                    write!(f, "MemOps: {:?}", mem_ops)?;
                }

                write!(f, ")")
            },
            StackElem::Symbolic(SymType::MemOp(x)) => {
                write!(f, "MemOps: {:?}", x)
            }
            StackElem::Concrete(x) => write!(f, "0x{:x}", x),
        }
    }
}

#[derive(Debug, Clone)]
pub enum GraphElem {
    Stack(StackElem),
    Memory(MemoryElem),
    Code(StackElem),
    Events { topics: Vec<NodeIndex>, offset: NodeIndex, size: NodeIndex },
    Exit(ExitType, StackElem, StackElem),
}

#[derive(Debug, Clone, Copy)]
pub enum ExitType {
    Return,
    Revert,
    Invalid
}

impl GraphElem {
    fn expect_stack_elem(&self) -> &StackElem {
        match &self {
            GraphElem::Stack(x) => {
                x
            }
            _ => panic!("Expected stack element, got other")
        }
    }
}

impl StackElem {
    fn maybe_concrete(&self) -> Option<U256> {
        match &self {
            StackElem::Concrete(x) => {
                Some(*x)
            }
            _ => None
        }
    }

    fn expect_concrete_val(&self) -> U256 {
        match &self {
            StackElem::Concrete(x) => {
                *x
            }
            _ => panic!("Expected concrete element, got symbolic")
        }
    }

    fn expect_zero_input_val(&self) -> &StackElem {
        match &self {
            StackElem::Concrete(_x) => {
                self
            }
            StackElem::Symbolic(SymType::Intrinsic(x)) => {
                match x {
                        SymIntrinsic::Balance(_)
                        | SymIntrinsic::ExtCodeSize(_)
                        | SymIntrinsic::ExtCodeCopy(_)
                        | SymIntrinsic::ExtCodeHash(_)
                        | SymIntrinsic::SelfDestruct(_) => panic!("Expected zero input element, got dynamic symbolic"),
                    _ => self
                }
            }
            _ => panic!("Expected concrete element, got symbolic")
        }
    }

    fn expect_zero_input_string(&self) -> String {
        match &self {
            StackElem::Concrete(x) => {
                format!("0x{:x}", x)
            }
            StackElem::Symbolic(SymType::Intrinsic(x)) => {
                match x {
                        SymIntrinsic::Balance(_)
                        | SymIntrinsic::ExtCodeSize(_)
                        | SymIntrinsic::ExtCodeCopy(_)
                        | SymIntrinsic::ExtCodeHash(_)
                        | SymIntrinsic::SelfDestruct(_) => panic!("Expected zero input element, got dynamic symbolic"),
                    _ => format!("{:?}", self)
                }
            }
            _ => panic!("Expected concrete element, got symbolic")
        }
    }

    fn eq(&self, other: &StackElem) -> Self {
        match (&self, &other) {
            (StackElem::Concrete(x), StackElem::Concrete(y)) => {
                StackElem::Concrete(U256::from((x == y) as usize))
            }
            _ => self.combine_symb(&other)
        }
    }

    fn ne(&self, other: &StackElem) -> Self {
        match (&self, &other) {
            (StackElem::Concrete(x), StackElem::Concrete(y)) => {
                StackElem::Concrete(U256::from((x != y) as usize))
            }
            _ => self.combine_symb(&other)
        }
    }

    fn lt(&self, other: &StackElem) -> Self {
        match (&self, &other) {
            (StackElem::Concrete(x), StackElem::Concrete(y)) => {
                StackElem::Concrete(U256::from((x < y) as usize))
            }
            _ => self.combine_symb(&other)
        }
    }

    fn slt(&self, other: &StackElem) -> Self {
        match (&self, &other) {
            (StackElem::Concrete(x), StackElem::Concrete(y)) => {
                StackElem::Concrete(slt(*x, *y))
            }
            _ => self.combine_symb(&other)
        }
    }

    fn sgt(&self, other: &StackElem) -> Self {
        match (&self, &other) {
            (StackElem::Concrete(x), StackElem::Concrete(y)) => {
                StackElem::Concrete(sgt(*x, *y))
            }
            _ => self.combine_symb(&other)
        }
    }

    fn le(&self, other: &StackElem) -> Self {
        match (&self, &other) {
            (StackElem::Concrete(x), StackElem::Concrete(y)) => {
                StackElem::Concrete(U256::from((x <= y) as usize))
            }
            _ => self.combine_symb(&other)
        }
    }
    fn gt(&self, other: &StackElem) -> Self {
        match (&self, &other) {
            (StackElem::Concrete(x), StackElem::Concrete(y)) => {
                StackElem::Concrete(U256::from((x > y) as usize))
            }
            _ => self.combine_symb(&other)
        }
    }
    fn ge(&self, other: &StackElem) -> Self {
        match (&self, &other) {
            (StackElem::Concrete(x), StackElem::Concrete(y)) => {
                StackElem::Concrete(U256::from((x >= y) as usize))
            }
            _ => self.combine_symb(&other)
        }
    }

    fn or(&self, other: &StackElem) -> Self {
        match (&self, &other) {
            (StackElem::Concrete(x), StackElem::Concrete(y)) => {
                StackElem::Concrete(x.bitor(*y))
            }
            _ => self.combine_symb(&other)
        }
    }

    fn and(&self, other: &StackElem) -> Self {
        match (&self, &other) {
            (StackElem::Concrete(x), StackElem::Concrete(y)) => {
                StackElem::Concrete(x.bitand(*y))
            }
            _ => self.combine_symb(&other)
        }
    }

    fn xor(&self, other: &StackElem) -> Self {
        match (&self, &other) {
            (StackElem::Concrete(x), StackElem::Concrete(y)) => {
                StackElem::Concrete(x.bitxor(*y))
            }
            _ => self.combine_symb(&other)
        }
    }

    fn not(&self) -> Self {
        match &self {
            StackElem::Concrete(x) => {
                StackElem::Concrete(!*x)
            }
            _ => self.clone()
        }
    }

    fn iszero(&self) -> Self {
        match &self {
            StackElem::Concrete(x) => {
                StackElem::Concrete(iszero(*x))
            }
            _ => self.clone()
        }
    }

    fn byte(&self, other: &StackElem) -> Self {
        match (&self, &other) {
            (StackElem::Concrete(x), StackElem::Concrete(y)) => {
                StackElem::Concrete(byte(*x, *y))
            }
            _ => self.combine_symb(&other)
        }
    }

    fn div(&self, other: &StackElem) -> Self {
        match (&self, &other) {
            (StackElem::Concrete(x), StackElem::Concrete(y)) => {
                StackElem::Concrete(div(*x, *y))
            }
            _ => self.combine_symb(&other)
        }
    }

    fn mul(&self, other: &StackElem) -> Self {
        match (&self, &other) {
            (StackElem::Concrete(x), StackElem::Concrete(y)) => {
                StackElem::Concrete(x.overflowing_mul(*y).0)
            }
            _ => self.combine_symb(&other)
        }
    }

    fn sdiv(&self, other: &StackElem) -> Self {
        match (&self, &other) {
            (StackElem::Concrete(x), StackElem::Concrete(y)) => {
                StackElem::Concrete(sdiv(*x, *y))
            }
            _ => self.combine_symb(&other)
        }
    }

    fn rem(&self, other: &StackElem) -> Self {
        match (&self, &other) {
            (StackElem::Concrete(x), StackElem::Concrete(y)) => {
                StackElem::Concrete(rem(*x, *y))
            }
            _ => self.combine_symb(&other)
        }
    }

    fn smod(&self, other: &StackElem) -> Self {
        match (&self, &other) {
            (StackElem::Concrete(x), StackElem::Concrete(y)) => {
                StackElem::Concrete(smod(*x, *y))
            }
            _ => self.combine_symb(&other)
        }
    }

    fn add(&self, other: &StackElem) -> Self {
        match (&self, &other) {
            (StackElem::Concrete(x), StackElem::Concrete(y)) => {
                StackElem::Concrete(x.overflowing_add(*y).0)
            }
            _ => self.combine_symb(&other)
        }
    }

    fn addmod(&self, other: &StackElem, other2: &StackElem) -> Self {
        match (&self, &other, &other2) {
            (StackElem::Concrete(x), StackElem::Concrete(y), StackElem::Concrete(z)) => {
                StackElem::Concrete(addmod(*x, *y, *z))
            }
            _ => self.combine_symb(&other).combine_symb(&other2)
        }
    }

    fn mulmod(&self, other: &StackElem, other2: &StackElem) -> Self {
        match (&self, &other, &other2) {
            (StackElem::Concrete(x), StackElem::Concrete(y), StackElem::Concrete(z)) => {
                StackElem::Concrete(mulmod(*x, *y, *z))
            }
            _ => self.combine_symb(&other).combine_symb(&other2)
        }
    }

    fn exp(&self, other: &StackElem) -> Self {
        match (&self, &other) {
            (StackElem::Concrete(x), StackElem::Concrete(y)) => {
                StackElem::Concrete(exp(*x, *y))
            }
            _ => self.combine_symb(&other)
        }
    }

    fn signextend(&self, other: &StackElem) -> Self {
        match (&self, &other) {
            (StackElem::Concrete(x), StackElem::Concrete(y)) => {
                StackElem::Concrete(signextend(*x, *y))
            }
            _ => self.combine_symb(&other)
        }
    }

    fn shl(&self, other: &StackElem) -> Self {
        match (&self, &other) {
            (StackElem::Concrete(x), StackElem::Concrete(y)) => {
                StackElem::Concrete(shl(*x, *y))
            }
            _ => self.combine_symb(&other)
        }
    }

    fn shr(&self, other: &StackElem) -> Self {
        match (&self, &other) {
            (StackElem::Concrete(x), StackElem::Concrete(y)) => {
                StackElem::Concrete(shr(*x, *y))
            }
            _ => self.combine_symb(&other)
        }
    }

    fn sar(&self, other: &StackElem) -> Self {
        match (&self, &other) {
            (StackElem::Concrete(x), StackElem::Concrete(y)) => {
                StackElem::Concrete(sar(*x, *y))
            }
            _ => self.combine_symb(&other)
        }
    }

    fn sub(&self, other: &StackElem) -> Self {
        match (&self, &other) {
            (StackElem::Concrete(x), StackElem::Concrete(y)) => {
                StackElem::Concrete(x.overflowing_sub(*y).0)
            }
            _ => self.combine_symb(&other)
        }
    }

    fn calldataload(&self, index: NodeIndex) -> Self {
        match &self {
            StackElem::Concrete(_) => {
                StackElem::Symbolic(SymType::Calldata { offsets: vec![index] })
            }
            _ => {
                let elem = StackElem::Symbolic(SymType::Calldata { offsets: vec![] });
                elem.combine_symb(&self)
            }
        }
    }

    pub fn combine_symb(&self, other: &Self) -> Self {
        match (self, other) {
            // panic when not symbolic. we should never reach here
            (StackElem::Concrete(_x), StackElem::Concrete(_y)) => {
                panic!("Not symbolic")
            }
            // calldata + concrete
            (StackElem::Concrete(_), StackElem::Symbolic(SymType::Calldata { offsets })) => {
                StackElem::Symbolic(SymType::Calldata { offsets: offsets.to_vec() })
            }
            (StackElem::Symbolic(SymType::Calldata{ offsets }), StackElem::Concrete(_)) => {
                StackElem::Symbolic(SymType::Calldata{ offsets: offsets.to_vec() })
            }

            // calldata + calldata
            (StackElem::Symbolic(SymType::Calldata{ offsets: offsets1 }), StackElem::Symbolic(SymType::Calldata{ offsets: offsets2 })) => {
                let mut offsets = offsets1.clone();
                offsets.extend(offsets2);
                StackElem::Symbolic(SymType::Calldata{ offsets })
            }

            // storage + storage
            (StackElem::Symbolic(SymType::Storage { slots: slots1 }), StackElem::Symbolic(SymType::Storage { slots: slots2 })) => {
                let mut slots = slots1.clone();
                slots.extend(slots2);
                StackElem::Symbolic(SymType::Storage { slots })
            }

            // Concrete + storage
            (StackElem::Concrete(_), StackElem::Symbolic(SymType::Storage { slots })) => {
                StackElem::Symbolic(SymType::Storage { slots: slots.to_vec() })
            }
            (StackElem::Symbolic(SymType::Storage { slots }), StackElem::Concrete(_)) => {
                StackElem::Symbolic(SymType::Storage { slots: slots.to_vec() })
            }

            // Concrete + intrinisic
            (StackElem::Concrete(_), StackElem::Symbolic(SymType::Intrinsic(x))) => {
                StackElem::Symbolic(SymType::Intrinsic(*x))
            }
            (StackElem::Symbolic(SymType::Intrinsic(x)), StackElem::Concrete(_)) => {
                StackElem::Symbolic(SymType::Intrinsic(*x))
            }

            // calldata + storage
            (StackElem::Symbolic(SymType::Storage { slots }), StackElem::Symbolic(SymType::Calldata{ offsets })) => {
                StackElem::Symbolic(SymType::Mixed { intrinsics: vec![],  slots: slots.to_vec(), offsets: offsets.to_vec(), mem_ops: vec![] })
            }
            (StackElem::Symbolic(SymType::Calldata{ offsets }), StackElem::Symbolic(SymType::Storage { slots })) => {
                StackElem::Symbolic(SymType::Mixed { intrinsics: vec![], slots: slots.to_vec(), offsets: offsets.to_vec(), mem_ops: vec![] })
            }

            // calldata + intrinsic
            (StackElem::Symbolic(SymType::Intrinsic(x)), StackElem::Symbolic(SymType::Calldata{ offsets })) => {
                StackElem::Symbolic(SymType::Mixed { intrinsics: vec![*x],  slots: vec![], offsets: offsets.to_vec(), mem_ops: vec![] })
            }
            (StackElem::Symbolic(SymType::Calldata{ offsets }), StackElem::Symbolic(SymType::Intrinsic(x))) => {
                StackElem::Symbolic(SymType::Mixed { intrinsics: vec![*x],  slots: vec![], offsets: offsets.to_vec(), mem_ops: vec![] })
            }

            // intrinsics + storage
            (StackElem::Symbolic(SymType::Storage { slots }), StackElem::Symbolic(SymType::Intrinsic(x))) => {
                StackElem::Symbolic(SymType::Mixed { intrinsics: vec![*x],  slots: slots.to_vec(), offsets: vec![], mem_ops: vec![] })
            }
            (StackElem::Symbolic(SymType::Intrinsic(x)), StackElem::Symbolic(SymType::Storage { slots })) => {
                StackElem::Symbolic(SymType::Mixed { intrinsics: vec![*x], slots: slots.to_vec(), offsets: vec![], mem_ops: vec![] })
            }

            // intrinsics + intrinsics
            (StackElem::Symbolic(SymType::Intrinsic(x)), StackElem::Symbolic(SymType::Intrinsic(y))) => {
                StackElem::Symbolic(SymType::Mixed { intrinsics: vec![*x, *y],  slots: vec![], offsets: vec![], mem_ops: vec![] })
            }

            // mixed + storage
            (StackElem::Symbolic(SymType::Mixed{ intrinsics, slots, offsets, mem_ops }), StackElem::Symbolic(SymType::Storage { slots: slots2 })) => {
                let mut slots1 = slots.clone();
                slots1.extend(slots2);
                StackElem::Symbolic(SymType::Mixed { intrinsics: intrinsics.to_vec(), slots: slots1, offsets: offsets.to_vec(), mem_ops: mem_ops.to_vec() })
            }
            (StackElem::Symbolic(SymType::Storage { slots: slots2 }), StackElem::Symbolic(SymType::Mixed{ intrinsics, slots, offsets, mem_ops })) => {
                let mut slots1 = slots.clone();
                slots1.extend(slots2);
                StackElem::Symbolic(SymType::Mixed { intrinsics: intrinsics.to_vec(), slots: slots1, offsets: offsets.to_vec(), mem_ops: mem_ops.to_vec() })
            }

            // mixed + calldata
            (StackElem::Symbolic(SymType::Mixed{ intrinsics, slots, offsets, mem_ops }), StackElem::Symbolic(SymType::Calldata { offsets: offsets2 })) => {
                let mut offsets1 = offsets.clone();
                offsets1.extend(offsets2);
                StackElem::Symbolic(SymType::Mixed { intrinsics: intrinsics.to_vec(), slots: slots.to_vec(), offsets: offsets1, mem_ops: mem_ops.to_vec() })
            }
            (StackElem::Symbolic(SymType::Calldata { offsets: offsets2 }), StackElem::Symbolic(SymType::Mixed{ intrinsics, slots, offsets, mem_ops })) => {
                let mut offsets1 = offsets.clone();
                offsets1.extend(offsets2);
                StackElem::Symbolic(SymType::Mixed { intrinsics: intrinsics.to_vec(), slots: slots.to_vec(), offsets: offsets1, mem_ops: mem_ops.to_vec() })
            }

            // mixed + mixed
            (StackElem::Symbolic(SymType::Mixed{ intrinsics, slots, offsets, mem_ops }), StackElem::Symbolic(SymType::Mixed{ intrinsics: intrinsics2, slots: slots2, offsets: offsets2, mem_ops: mem_ops2 })) => {
                let mut offsets1 = offsets.clone();
                offsets1.extend(offsets2);
                let mut slots1 = slots.clone();
                slots1.extend(slots2);
                let mut intrinsics1 = intrinsics.clone();
                intrinsics1.extend(intrinsics2);
                let mut mem_ops1 = mem_ops.clone();
                mem_ops1.extend(mem_ops2);
                StackElem::Symbolic(SymType::Mixed { intrinsics: intrinsics1, slots: slots1, offsets: offsets1, mem_ops: mem_ops1 })
            }

            // mixed + concrete
            (StackElem::Symbolic(SymType::Mixed{ intrinsics, slots, offsets, mem_ops }), StackElem::Concrete(_)) => {
                StackElem::Symbolic(SymType::Mixed { intrinsics: intrinsics.to_vec(), slots: slots.to_vec(), offsets: offsets.to_vec(), mem_ops: mem_ops.to_vec() })
            }
            (StackElem::Concrete(_), StackElem::Symbolic(SymType::Mixed{ intrinsics, slots, offsets, mem_ops })) => {
                StackElem::Symbolic(SymType::Mixed { intrinsics: intrinsics.to_vec(), slots: slots.to_vec(), offsets: offsets.to_vec(), mem_ops: mem_ops.to_vec() })
            }
            // mixed + intrinsic
            (StackElem::Symbolic(SymType::Intrinsic(x)),  StackElem::Symbolic(SymType::Mixed{ intrinsics, slots, offsets, mem_ops })) => {
                let mut intrinsics1 = intrinsics.clone();
                intrinsics1.push(*x);
                StackElem::Symbolic(SymType::Mixed { intrinsics: intrinsics1, slots: slots.to_vec(), offsets: offsets.to_vec(), mem_ops: mem_ops.to_vec() })
            }
            ( StackElem::Symbolic(SymType::Mixed{ intrinsics, slots, offsets, mem_ops }), StackElem::Symbolic(SymType::Intrinsic(y))) => {
                let mut intrinsics1 = intrinsics.clone();
                intrinsics1.push(*y);
                StackElem::Symbolic(SymType::Mixed { intrinsics: intrinsics1, slots: slots.to_vec(), offsets: offsets.to_vec(), mem_ops: mem_ops.to_vec() })
            }

            // Memop + concrete
            (StackElem::Symbolic(SymType::MemOp(x)), StackElem::Concrete(_)) => {
                StackElem::Symbolic(SymType::MemOp(*x))
            }
            (StackElem::Concrete(_), StackElem::Symbolic(SymType::MemOp(x))) => {
                StackElem::Symbolic(SymType::MemOp(*x))
            }

            // Memop + storage
            (StackElem::Symbolic(SymType::MemOp(x)), StackElem::Symbolic(SymType::Storage { slots })) => {
                StackElem::Symbolic(SymType::Mixed { intrinsics: vec![],  slots: slots.to_vec(), offsets: vec![], mem_ops: vec![*x] })
            }
            (StackElem::Symbolic(SymType::Storage { slots }), StackElem::Symbolic(SymType::MemOp(x))) => {
                StackElem::Symbolic(SymType::Mixed { intrinsics: vec![],  slots: slots.to_vec(), offsets: vec![], mem_ops: vec![*x] })
            }

            // Memop + calldata
            (StackElem::Symbolic(SymType::MemOp(x)), StackElem::Symbolic(SymType::Calldata { offsets })) => {
                StackElem::Symbolic(SymType::Mixed { intrinsics: vec![],  slots: vec![], offsets: offsets.to_vec(), mem_ops: vec![*x] })
            }
            (StackElem::Symbolic(SymType::Calldata { offsets }), StackElem::Symbolic(SymType::MemOp(x))) => {
                StackElem::Symbolic(SymType::Mixed { intrinsics: vec![],  slots: vec![], offsets: offsets.to_vec(), mem_ops: vec![*x] })
            }

            // Memop + mixed
            (StackElem::Symbolic(SymType::MemOp(x)), StackElem::Symbolic(SymType::Mixed{ intrinsics, slots, offsets, mem_ops })) => {
                let mut mem_ops1 = mem_ops.clone();
                mem_ops1.push(*x);
                StackElem::Symbolic(SymType::Mixed { intrinsics: intrinsics.to_vec(),  slots: slots.to_vec(), offsets: offsets.to_vec(), mem_ops: mem_ops1 })
            }
            (StackElem::Symbolic(SymType::Mixed{ intrinsics, slots, offsets, mem_ops }), StackElem::Symbolic(SymType::MemOp(x))) => {
                let mut mem_ops1 = mem_ops.clone();
                mem_ops1.push(*x);
                StackElem::Symbolic(SymType::Mixed { intrinsics: intrinsics.to_vec(),  slots: slots.to_vec(), offsets: offsets.to_vec(), mem_ops: mem_ops1 })
            }

            // memop + intrinsic
            (StackElem::Symbolic(SymType::Intrinsic(x)),  StackElem::Symbolic(SymType::MemOp(y))) => {
                StackElem::Symbolic(SymType::Mixed { intrinsics: vec![*x], slots: vec![], offsets: vec![], mem_ops: vec![*y] })
            }
            (StackElem::Symbolic(SymType::MemOp(y)), StackElem::Symbolic(SymType::Intrinsic(x))) => {
                StackElem::Symbolic(SymType::Mixed { intrinsics: vec![*x], slots: vec![], offsets: vec![], mem_ops: vec![*y] })
            }
            // memop + memop
            (StackElem::Symbolic(SymType::MemOp(x)),  StackElem::Symbolic(SymType::MemOp(y))) => {
                StackElem::Symbolic(SymType::Mixed { intrinsics: vec![], slots: vec![], offsets: vec![], mem_ops: vec![*x, *y] })
            }
            // _ => { println!("{:?} {:?}", self, other); panic!("not symbolic");}
        }
    }
}


#[derive(Debug, Clone)]
#[derive(PartialEq)]
pub enum SymType {
    Intrinsic(SymIntrinsic),
    Calldata {
        offsets: Vec<NodeIndex>,
    },
    Storage {
        slots: Vec<NodeIndex>,
    },
    Mixed {
        intrinsics: Vec<SymIntrinsic>,
        offsets: Vec<NodeIndex>,
        slots: Vec<NodeIndex>,
        mem_ops: Vec<SymMemOp>
    },
    MemOp(SymMemOp)
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SymMemOp {
    pub offset: NodeIndex,
    pub size: NodeIndex,
    pub mload: bool,
}

#[derive(Debug, Clone, Copy)]
#[derive(PartialEq)]
pub enum SymIntrinsic {
    SelfAddress,
    SelfBalance,
    Balance(NodeIndex),
    Origin,
    Caller,
    CallValue,
    CodeSize,
    CodeCopy,
    GasPrice,
    ExtCodeSize(NodeIndex),
    ExtCodeCopy(NodeIndex),
    ExtCodeHash(NodeIndex),
    Msize,
    Gas,
    Blockhash,
    Coinbase,
    Timestamp,
    Number,
    Difficulty,
    GasLimit,
    ChainId,
    BaseFee,
    SelfDestruct(NodeIndex),
    CallDataSize,
    CreateAddress,
    Create2Address(NodeIndex),
    ReturnDataSize,
    ReturnSuccess,
}


// Models the evm
type Dag = Graph<GraphElem, OpType>;

#[derive(Debug, Default, Clone)]
pub struct OpsGraph {
    pub stack_dag: Dag,
    pub stack: VecDeque<NodeIndex>,
    pub mem: Vec<MemoryElem>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum OpType {
    Addend,
    Minuend,
    Subtrahend,
    Factor,
    Dividend,
    Divisor,
    SDividend,
    SDivisor,
    Base,
    Exponent,
    Modulus,
    LoadSlot,
    StoreSlot,
    StoreValue,
    Duplicant,
    LtLhs,
    LtRhs,
    GtLhs,
    GtRhs,
    SltLhs,
    SltRhs,
    SgtLhs,
    SgtRhs,
    EqLhs,
    EqRhs,
    OrLhs,
    OrRhs,
    XorLhs,
    XorRhs,
    AndLhs,
    AndRhs,
    LShiftee,
    LShiftor,
    RShiftee,
    RShiftor,
    SARShiftee,
    SARShiftor,
    Not,
    IsZero,
    Byte,
    SignExtendee,
    ByteWord,
    CalldataOffset,
    JumpLoc,
    ConditionalJumpConditional,
    ConditionalJumpLoc,
    ConditionalJumpConstEval,
    Balance,
    MstoreLoc,
    MstoreVal,
    Mstore8Loc,
    Mstore8Val,
    ReturnMemLoc,
    ReturnMemSize,
    Sha3MemLoc,
    Sha3MemSize,
    MloadLoc,
    CallGas,
    CallAddress,
    CallValue,
    CallArgsMemLoc,
    CallArgsMemSize,
    CallReturnMemLoc,
    CallReturnMemSize,
    CallCodeGas,
    CallCodeAddress,
    CallCodeValue,
    CallCodeArgsMemLoc,
    CallCodeArgsMemSize,
    CallCodeReturnMemLoc,
    CallCodeReturnMemSize,
    DelegateCallGas,
    DelegateCallAddress,
    DelegateCallArgsMemLoc,
    DelegateCallArgsMemSize,
    DelegateCallReturnMemLoc,
    DelegateCallReturnMemSize,
    StaticCallGas,
    StaticCallAddress,
    StaticCallArgsMemLoc,
    StaticCallArgsMemSize,
    StaticCallReturnMemLoc,
    StaticCallReturnMemSize,
    Log0MemLoc,
    Log0MemSize,
    Log1MemLoc,
    Log1MemSize,
    Log1Topic0,
    Log2MemLoc,
    Log2MemSize,
    Log2Topic0,
    Log2Topic1,
    Log3MemLoc,
    Log3MemSize,
    Log3Topic0,
    Log3Topic1,
    Log3Topic2,
    Log4MemLoc,
    Log4MemSize,
    Log4Topic0,
    Log4Topic1,
    Log4Topic2,
    Log4Topic3,
    CreateValue,
    CreateMemLoc,
    CreateMemSize,
    Create2Value,
    Create2MemLoc,
    Create2MemSize,
    Create2Salt,
    CalldataLoadLoc,
    CalldataLoadSize,
    CalldataLoadDestLoc,
    ReturndataLoadLoc,
    ReturndataLoadSize,
    ReturndataLoadDestLoc,
    CodeCopyLoc,
    CodeCopySize,
    CodeCopyDestLoc,
    ExtCodeCopyAddress,
    ExtCodeCopyLoc,
    ExtCodeCopySize,
    ExtCodeCopyDestLoc,
    SelfDestructAddress,
    RevertMemLoc,
    RevertMemSize
}

impl OpsGraph {
    #[allow(non_snake_case)]
    pub fn push_to_u256(&mut self, N: usize, slice: &[u8]) -> U256 {
        let mut slot = U256::zero();
        let mut dangling = [0u8; 8];
        if N < 8 {
            dangling[8 - N..].copy_from_slice(slice);
            slot.0[0] = u64::from_be_bytes(dangling);
        } else if N < 16 {
            slot.0[0] = u64::from_be_bytes(*arrayref::array_ref!(slice, N - 8, 8));
            if N != 8 {
                dangling[8 * 2 - N..].copy_from_slice(&slice[..N - 8]);
                slot.0[1] = u64::from_be_bytes(dangling);
            }
        } else if N < 24 {
            slot.0[0] = u64::from_be_bytes(*arrayref::array_ref!(slice, N - 8, 8));
            slot.0[1] = u64::from_be_bytes(*arrayref::array_ref!(slice, N - 16, 8));
            if N != 16 {
                dangling[8 * 3 - N..].copy_from_slice(&slice[..N - 16]);
                slot.0[2] = u64::from_be_bytes(dangling);
            }
        } else {
            // M<32
            slot.0[0] = u64::from_be_bytes(*arrayref::array_ref!(slice, N - 8, 8));
            slot.0[1] = u64::from_be_bytes(*arrayref::array_ref!(slice, N - 16, 8));
            slot.0[2] = u64::from_be_bytes(*arrayref::array_ref!(slice, N - 24, 8));
            if N == 32 {
                slot.0[3] = u64::from_be_bytes(*arrayref::array_ref!(slice, 0, 8));
            } else if N != 24 {
                dangling[8 * 4 - N..].copy_from_slice(&slice[..N - 24]);
                slot.0[3] = u64::from_be_bytes(dangling);
            }
        }
        slot
    }

    pub fn add(&mut self) {
        let a_addend_index = self.stack.pop_front().expect("Stack underflow");
        let b_addend_index = self.stack.pop_front().expect("Stack underflow");
        let a_addend_elem = self.stack_dag.node_weight(a_addend_index).expect("No node for node index").expect_stack_elem();
        let b_addend_elem = self.stack_dag.node_weight(b_addend_index).expect("No node for node index").expect_stack_elem();
        let sum = a_addend_elem.add(b_addend_elem);
        let sum_index = self.stack_dag.add_node(GraphElem::Stack(sum));
        self.stack.push_front(sum_index);
        self.stack_dag.add_edge(a_addend_index, sum_index, OpType::Addend);
        self.stack_dag.add_edge(b_addend_index, sum_index, OpType::Addend);
    }

    pub fn mul(&mut self) {
        let a_factor_index = self.stack.pop_front().expect("Stack underflow");
        let b_factor_index = self.stack.pop_front().expect("Stack underflow");
        let a_factor_elem = self.stack_dag.node_weight(a_factor_index).expect("No node for node index").expect_stack_elem();
        let b_factor_elem = self.stack_dag.node_weight(b_factor_index).expect("No node for node index").expect_stack_elem();
        let prod = a_factor_elem.mul(b_factor_elem);
        let prod_index = self.stack_dag.add_node(GraphElem::Stack(prod));
        self.stack.push_front(prod_index);
        self.stack_dag.add_edge(a_factor_index, prod_index, OpType::Factor);
        self.stack_dag.add_edge(b_factor_index, prod_index, OpType::Factor);
    }

    pub fn sub(&mut self) {
        let a_minuend_index = self.stack.pop_front().expect("Stack underflow");
        let b_subtrahend_index = self.stack.pop_front().expect("Stack underflow");
        let a_minuend_elem = self.stack_dag.node_weight(a_minuend_index).expect("No node for node index").expect_stack_elem();
        let b_subtrahend_elem = self.stack_dag.node_weight(b_subtrahend_index).expect("No node for node index").expect_stack_elem();
        let diff = a_minuend_elem.sub(b_subtrahend_elem);
        let diff_index = self.stack_dag.add_node(GraphElem::Stack(diff));
        self.stack.push_front(diff_index);
        self.stack_dag.add_edge(a_minuend_index, diff_index, OpType::Minuend);
        self.stack_dag.add_edge(b_subtrahend_index, diff_index, OpType::Subtrahend);
    }

    pub fn div(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let b_index = self.stack.pop_front().expect("Stack underflow");
        let a_elem = self.stack_dag.node_weight(a_index).expect("No node for node index").expect_stack_elem();
        let b_elem = self.stack_dag.node_weight(b_index).expect("No node for node index").expect_stack_elem();
        let quot = a_elem.div(b_elem);
        let quot_index = self.stack_dag.add_node(GraphElem::Stack(quot));
        self.stack.push_front(quot_index);
        self.stack_dag.add_edge(a_index, quot_index, OpType::Dividend);
        self.stack_dag.add_edge(b_index, quot_index, OpType::Divisor);
    }

    pub fn sdiv(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let b_index = self.stack.pop_front().expect("Stack underflow");
        let a_elem = self.stack_dag.node_weight(a_index).expect("No node for node index").expect_stack_elem();
        let b_elem = self.stack_dag.node_weight(b_index).expect("No node for node index").expect_stack_elem();
        let quot = a_elem.sdiv(b_elem);
        let quot_index = self.stack_dag.add_node(GraphElem::Stack(quot));
        self.stack.push_front(quot_index);
        self.stack_dag.add_edge(a_index, quot_index, OpType::SDividend);
        self.stack_dag.add_edge(b_index, quot_index, OpType::SDivisor);
    }

    pub fn rem(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let b_index = self.stack.pop_front().expect("Stack underflow");
        let a_elem = self.stack_dag.node_weight(a_index).expect("No node for node index").expect_stack_elem();
        let b_elem = self.stack_dag.node_weight(b_index).expect("No node for node index").expect_stack_elem();
        let rem = a_elem.rem(b_elem);
        let rem_index = self.stack_dag.add_node(GraphElem::Stack(rem));
        self.stack.push_front(rem_index);
        self.stack_dag.add_edge(a_index, rem_index, OpType::Dividend);
        self.stack_dag.add_edge(b_index, rem_index, OpType::Modulus);
    }

    pub fn smod(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let b_index = self.stack.pop_front().expect("Stack underflow");
        let a_elem = self.stack_dag.node_weight(a_index).expect("No node for node index").expect_stack_elem();
        let b_elem = self.stack_dag.node_weight(b_index).expect("No node for node index").expect_stack_elem();
        let rem = a_elem.smod(b_elem);
        let rem_index = self.stack_dag.add_node(GraphElem::Stack(rem));
        self.stack.push_front(rem_index);
        self.stack_dag.add_edge(a_index, rem_index, OpType::SDividend);
        self.stack_dag.add_edge(b_index, rem_index, OpType::Modulus);
    }

    pub fn addmod(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let b_index = self.stack.pop_front().expect("Stack underflow");
        let c_index = self.stack.pop_front().expect("Stack underflow");
        let a_elem = self.stack_dag.node_weight(a_index).expect("No node for node index").expect_stack_elem();
        let b_elem = self.stack_dag.node_weight(b_index).expect("No node for node index").expect_stack_elem();
        let c_elem = self.stack_dag.node_weight(c_index).expect("No node for node index").expect_stack_elem();
        let summod = a_elem.addmod(b_elem, c_elem);
        let summod_index = self.stack_dag.add_node(GraphElem::Stack(summod));
        self.stack.push_front(summod_index);
        self.stack_dag.add_edge(a_index, summod_index, OpType::Addend);
        self.stack_dag.add_edge(b_index, summod_index, OpType::Addend);
        self.stack_dag.add_edge(c_index, summod_index, OpType::Modulus);
    }

    pub fn mulmod(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let b_index = self.stack.pop_front().expect("Stack underflow");
        let c_index = self.stack.pop_front().expect("Stack underflow");
        let a_elem = self.stack_dag.node_weight(a_index).expect("No node for node index").expect_stack_elem();
        let b_elem = self.stack_dag.node_weight(b_index).expect("No node for node index").expect_stack_elem();
        let c_elem = self.stack_dag.node_weight(c_index).expect("No node for node index").expect_stack_elem();
        let mulmod = a_elem.addmod(b_elem, c_elem);
        let mulmod_index = self.stack_dag.add_node(GraphElem::Stack(mulmod));
        self.stack.push_front(mulmod_index);
        self.stack_dag.add_edge(a_index, mulmod_index, OpType::Factor);
        self.stack_dag.add_edge(b_index, mulmod_index, OpType::Factor);
        self.stack_dag.add_edge(c_index, mulmod_index, OpType::Modulus);
    }

    pub fn exp(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let b_index = self.stack.pop_front().expect("Stack underflow");
        let a_elem = self.stack_dag.node_weight(a_index).expect("No node for node index").expect_stack_elem();
        let b_elem = self.stack_dag.node_weight(b_index).expect("No node for node index").expect_stack_elem();
        let ex = a_elem.exp(b_elem);
        let ex_index = self.stack_dag.add_node(GraphElem::Stack(ex));
        self.stack.push_front(ex_index);
        self.stack_dag.add_edge(a_index, ex_index, OpType::Base);
        self.stack_dag.add_edge(b_index, ex_index, OpType::Exponent);
    }

    pub fn signextend(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let b_index = self.stack.pop_front().expect("Stack underflow");
        let a_elem = self.stack_dag.node_weight(a_index).expect("No node for node index").expect_stack_elem();
        let b_elem = self.stack_dag.node_weight(b_index).expect("No node for node index").expect_stack_elem();
        let sig = a_elem.signextend(b_elem);
        let sig_index = self.stack_dag.add_node(GraphElem::Stack(sig));
        self.stack.push_front(sig_index);
        self.stack_dag.add_edge(a_index, sig_index, OpType::Byte);
        self.stack_dag.add_edge(b_index, sig_index, OpType::SignExtendee);
    }

    pub fn lt(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let b_index = self.stack.pop_front().expect("Stack underflow");
        let a_elem = self.stack_dag.node_weight(a_index).expect("No node for node index").expect_stack_elem();
        let b_elem = self.stack_dag.node_weight(b_index).expect("No node for node index").expect_stack_elem();
        let lt = a_elem.lt(b_elem);
        let lt_index = self.stack_dag.add_node(GraphElem::Stack(lt));
        self.stack.push_front(lt_index);
        self.stack_dag.add_edge(a_index, lt_index, OpType::LtLhs);
        self.stack_dag.add_edge(b_index, lt_index, OpType::LtRhs);
    }

    pub fn slt(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let b_index = self.stack.pop_front().expect("Stack underflow");
        let a_elem = self.stack_dag.node_weight(a_index).expect("No node for node index").expect_stack_elem();
        let b_elem = self.stack_dag.node_weight(b_index).expect("No node for node index").expect_stack_elem();
        let lt = a_elem.slt(b_elem);
        let lt_index = self.stack_dag.add_node(GraphElem::Stack(lt));
        self.stack.push_front(lt_index);
        self.stack_dag.add_edge(a_index, lt_index, OpType::SltLhs);
        self.stack_dag.add_edge(b_index, lt_index, OpType::SltRhs);
    }

    pub fn gt(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let b_index = self.stack.pop_front().expect("Stack underflow");
        let a_elem = self.stack_dag.node_weight(a_index).expect("No node for node index").expect_stack_elem();
        let b_elem = self.stack_dag.node_weight(b_index).expect("No node for node index").expect_stack_elem();
        let gt = a_elem.gt(b_elem);
        let gt_index = self.stack_dag.add_node(GraphElem::Stack(gt));
        self.stack.push_front(gt_index);
        self.stack_dag.add_edge(a_index, gt_index, OpType::GtLhs);
        self.stack_dag.add_edge(b_index, gt_index, OpType::GtRhs);
    }

    pub fn sgt(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let b_index = self.stack.pop_front().expect("Stack underflow");
        let a_elem = self.stack_dag.node_weight(a_index).expect("No node for node index").expect_stack_elem();
        let b_elem = self.stack_dag.node_weight(b_index).expect("No node for node index").expect_stack_elem();
        let gt = a_elem.sgt(b_elem);
        let gt_index = self.stack_dag.add_node(GraphElem::Stack(gt));
        self.stack.push_front(gt_index);
        self.stack_dag.add_edge(a_index, gt_index, OpType::SgtLhs);
        self.stack_dag.add_edge(b_index, gt_index, OpType::SgtRhs);
    }

    pub fn eq(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let b_index = self.stack.pop_front().expect("Stack underflow");
        let a_elem = self.stack_dag.node_weight(a_index).expect("No node for node index").expect_stack_elem();
        let b_elem = self.stack_dag.node_weight(b_index).expect("No node for node index").expect_stack_elem();
        let eq = a_elem.eq(b_elem);
        let eq_index = self.stack_dag.add_node(GraphElem::Stack(eq));
        self.stack.push_front(eq_index);
        self.stack_dag.add_edge(a_index, eq_index, OpType::EqLhs);
        self.stack_dag.add_edge(b_index, eq_index, OpType::EqRhs);
    }

    pub fn iszero(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let a_elem = self.stack_dag.node_weight(a_index).expect("No node for node index").expect_stack_elem();
        let iz = a_elem.iszero();
        let iz_index = self.stack_dag.add_node(GraphElem::Stack(iz));
        self.stack.push_front(iz_index);
        self.stack_dag.add_edge(a_index, iz_index, OpType::IsZero);
    }

    pub fn and(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let b_index = self.stack.pop_front().expect("Stack underflow");
        let a_elem = self.stack_dag.node_weight(a_index).expect("No node for node index").expect_stack_elem();
        let b_elem = self.stack_dag.node_weight(b_index).expect("No node for node index").expect_stack_elem();
        let an = a_elem.or(b_elem);
        let an_index = self.stack_dag.add_node(GraphElem::Stack(an));
        self.stack.push_front(an_index);
        self.stack_dag.add_edge(a_index, an_index, OpType::AndLhs);
        self.stack_dag.add_edge(b_index, an_index, OpType::AndRhs);
    }

    pub fn or(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let b_index = self.stack.pop_front().expect("Stack underflow");
        let a_elem = self.stack_dag.node_weight(a_index).expect("No node for node index").expect_stack_elem();
        let b_elem = self.stack_dag.node_weight(b_index).expect("No node for node index").expect_stack_elem();
        let or = a_elem.or(b_elem);
        let or_index = self.stack_dag.add_node(GraphElem::Stack(or));
        self.stack.push_front(or_index);
        self.stack_dag.add_edge(a_index, or_index, OpType::OrLhs);
        self.stack_dag.add_edge(b_index, or_index, OpType::OrRhs);
    }

    pub fn xor(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let b_index = self.stack.pop_front().expect("Stack underflow");
        let a_elem = self.stack_dag.node_weight(a_index).expect("No node for node index").expect_stack_elem();
        let b_elem = self.stack_dag.node_weight(b_index).expect("No node for node index").expect_stack_elem();
        let xor = a_elem.xor(b_elem);
        let xor_index = self.stack_dag.add_node(GraphElem::Stack(xor));
        self.stack.push_front(xor_index);
        self.stack_dag.add_edge(a_index, xor_index, OpType::XorLhs);
        self.stack_dag.add_edge(b_index, xor_index, OpType::XorRhs);
    }

    pub fn not(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let a_elem = self.stack_dag.node_weight(a_index).expect("No node for node index").expect_stack_elem();
        let not = a_elem.not();
        let not_index = self.stack_dag.add_node(GraphElem::Stack(not));
        self.stack.push_front(not_index);
        self.stack_dag.add_edge(a_index, not_index, OpType::Not);
    }

    pub fn byte(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let b_index = self.stack.pop_front().expect("Stack underflow");
        let a_elem = self.stack_dag.node_weight(a_index).expect("No node for node index").expect_stack_elem();
        let b_elem = self.stack_dag.node_weight(b_index).expect("No node for node index").expect_stack_elem();
        let byte = a_elem.byte(b_elem);
        let byte_index = self.stack_dag.add_node(GraphElem::Stack(byte));
        self.stack.push_front(byte_index);
        self.stack_dag.add_edge(a_index, byte_index, OpType::Byte);
        self.stack_dag.add_edge(b_index, byte_index, OpType::ByteWord);
    }

    pub fn shl(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let b_index = self.stack.pop_front().expect("Stack underflow");
        let a_elem = self.stack_dag.node_weight(a_index).expect("No node for node index").expect_stack_elem();
        let b_elem = self.stack_dag.node_weight(b_index).expect("No node for node index").expect_stack_elem();
        let shl = a_elem.shl(b_elem);
        let shl_index = self.stack_dag.add_node(GraphElem::Stack(shl));
        self.stack.push_front(shl_index);
        self.stack_dag.add_edge(a_index, shl_index, OpType::LShiftor);
        self.stack_dag.add_edge(b_index, shl_index, OpType::LShiftee);
    }

    pub fn shr(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let b_index = self.stack.pop_front().expect("Stack underflow");
        let a_elem = self.stack_dag.node_weight(a_index).expect("No node for node index").expect_stack_elem();
        let b_elem = self.stack_dag.node_weight(b_index).expect("No node for node index").expect_stack_elem();
        let shr = a_elem.shr(b_elem);
        let shr_index = self.stack_dag.add_node(GraphElem::Stack(shr));
        self.stack.push_front(shr_index);
        self.stack_dag.add_edge(a_index, shr_index, OpType::RShiftor);
        self.stack_dag.add_edge(b_index, shr_index, OpType::RShiftee);
    }

    pub fn sar(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let b_index = self.stack.pop_front().expect("Stack underflow");
        let a_elem = self.stack_dag.node_weight(a_index).expect("No node for node index").expect_stack_elem();
        let b_elem = self.stack_dag.node_weight(b_index).expect("No node for node index").expect_stack_elem();
        let sar = a_elem.byte(b_elem);
        let sar_index = self.stack_dag.add_node(GraphElem::Stack(sar));
        self.stack.push_front(sar_index);
        self.stack_dag.add_edge(a_index, sar_index, OpType::SARShiftor);
        self.stack_dag.add_edge(b_index, sar_index, OpType::SARShiftee);
    }

    pub fn address(&mut self) {
        let elem = GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::SelfAddress)));
        let elem_idx = self.stack_dag.add_node(elem);
        self.stack.push_front(elem_idx);
    }

    pub fn balance(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let elem = GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::Balance(a_index))));
        let elem_idx = self.stack_dag.add_node(elem);
        self.stack.push_front(elem_idx);
        self.stack_dag.add_edge(a_index, elem_idx, OpType::Balance);
    }

    pub fn origin(&mut self) {
        let elem = GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::Origin)));
        let elem_idx = self.stack_dag.add_node(elem);
        self.stack.push_front(elem_idx);
    }

    pub fn caller(&mut self) {
        let elem = GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::Caller)));
        let elem_idx = self.stack_dag.add_node(elem);
        self.stack.push_front(elem_idx);
    }

    pub fn callvalue(&mut self) {
        let elem = GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::CallValue)));
        let elem_idx = self.stack_dag.add_node(elem);
        self.stack.push_front(elem_idx);
    }

    pub fn calldataload(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let a_elem = self.stack_dag.node_weight(a_index).expect("No node for node index").expect_stack_elem();
        let cald = a_elem.calldataload(a_index);
        let cald_index = self.stack_dag.add_node(GraphElem::Stack(cald));
        self.stack.push_front(cald_index);
        self.stack_dag.add_edge(a_index, cald_index, OpType::CalldataOffset);
    }

    pub fn calldatasize(&mut self) {
        let elem = GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::CallDataSize)));
        let elem_idx = self.stack_dag.add_node(elem);
        self.stack.push_front(elem_idx);
    }

    pub fn codesize(&mut self) {
        let elem = GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::CodeSize)));
        let elem_idx = self.stack_dag.add_node(elem);
        self.stack.push_front(elem_idx);
    }

    pub fn gasprice(&mut self) {
        let elem = GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::GasPrice)));
        let elem_idx = self.stack_dag.add_node(elem);
        self.stack.push_front(elem_idx);
    }

    pub fn blockhash(&mut self) {
        let elem = GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::Blockhash)));
        let elem_idx = self.stack_dag.add_node(elem);
        self.stack.push_front(elem_idx);
    }

    pub fn coinbase(&mut self) {
        let elem = GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::Coinbase)));
        let elem_idx = self.stack_dag.add_node(elem);
        self.stack.push_front(elem_idx);
    }

    pub fn timestamp(&mut self) {
        let elem = GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::Timestamp)));
        let elem_idx = self.stack_dag.add_node(elem);
        self.stack.push_front(elem_idx);
    }

    pub fn number(&mut self) {
        let elem = GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::Number)));
        let elem_idx = self.stack_dag.add_node(elem);
        self.stack.push_front(elem_idx);
    }

    pub fn difficulty(&mut self) {
        let elem = GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::Difficulty)));
        let elem_idx = self.stack_dag.add_node(elem);
        self.stack.push_front(elem_idx);
    }

    pub fn gaslimit(&mut self) {
        let elem = GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::GasLimit)));
        let elem_idx = self.stack_dag.add_node(elem);
        self.stack.push_front(elem_idx);
    }

    pub fn chainid(&mut self) {
        let elem = GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::ChainId)));
        let elem_idx = self.stack_dag.add_node(elem);
        self.stack.push_front(elem_idx);
    }

    pub fn selfbalance(&mut self) {
        let elem = GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::SelfBalance)));
        let elem_idx = self.stack_dag.add_node(elem);
        self.stack.push_front(elem_idx);
    }

    pub fn basefee(&mut self) {
        let elem = GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::BaseFee)));
        let elem_idx = self.stack_dag.add_node(elem);
        self.stack.push_front(elem_idx);
    }

    pub fn extcodesize(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let elem = GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::ExtCodeSize(a_index))));
        let elem_idx = self.stack_dag.add_node(elem);
        self.stack.push_front(elem_idx);
        self.stack_dag.add_edge(a_index, elem_idx, OpType::Balance);
    }

    pub fn extcodehash(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let elem = GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::ExtCodeHash(a_index))));
        let elem_idx = self.stack_dag.add_node(elem);
        self.stack.push_front(elem_idx);
        self.stack_dag.add_edge(a_index, elem_idx, OpType::Balance);
    }

    pub fn returndatasize(&mut self) {
        let elem = GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::ReturnDataSize)));
        let elem_idx = self.stack_dag.add_node(elem);
        self.stack.push_front(elem_idx);
    }

    pub fn jump(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let a_elem = self.stack_dag.node_weight(a_index).expect("No node for node index").expect_stack_elem();
        let cald_index = self.stack_dag.add_node(GraphElem::Code(a_elem.clone()));
        self.stack_dag.add_edge(a_index, cald_index, OpType::JumpLoc);
    }

    pub fn pc(&mut self, pc: usize) {
        let pc_index = self.stack_dag.add_node(GraphElem::Stack(StackElem::Concrete(pc.into())));
        self.stack.push_front(pc_index);
    }

    pub fn msize(&mut self) {
        let msize_index = self.stack_dag.add_node(GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::Msize))));
        self.stack.push_front(msize_index);
    }

    pub fn gas(&mut self) {
        let msize_index = self.stack_dag.add_node(GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::Gas))));
        self.stack.push_front(msize_index);
    }

    pub fn jumpi(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let b_index = self.stack.pop_front().expect("Stack underflow");

        let a_elem = self.stack_dag.node_weight(a_index).expect("No node for node index").expect_stack_elem();
        let b_elem = self.stack_dag.node_weight(b_index).expect("No node for node index").expect_stack_elem();

        match b_elem {
            StackElem::Concrete(_x) => {
                let cald_index = self.stack_dag.add_node(GraphElem::Code(a_elem.clone()));
                self.stack_dag.add_edge(a_index, cald_index, OpType::ConditionalJumpConstEval);
                self.stack_dag.add_edge(b_index, cald_index, OpType::ConditionalJumpConditional);
            }
            _ => {
                let cald_index = self.stack_dag.add_node(GraphElem::Code(a_elem.clone()));
                self.stack_dag.add_edge(a_index, cald_index, OpType::ConditionalJumpLoc);
                self.stack_dag.add_edge(b_index, cald_index, OpType::ConditionalJumpConditional);
            }
        }
    }

    pub fn calldatacopy(&mut self) {
        let dest_offset = self.stack.pop_front().expect("Stack underflow");
        let offset = self.stack.pop_front().expect("Stack underflow");
        let size = self.stack.pop_front().expect("Stack underflow");
        let memory_elem = MemoryElem::FromCalldata { offset, dest_offset, size};
        self.mem_sorted_insert(memory_elem.clone());

        let node = GraphElem::Memory(memory_elem);
        let mem_index = self.stack_dag.add_node(node);
        self.stack_dag.add_edge(offset, mem_index, OpType::CalldataLoadLoc);
        self.stack_dag.add_edge(size, mem_index, OpType::CalldataLoadSize);
        self.stack_dag.add_edge(dest_offset, mem_index, OpType::CalldataLoadDestLoc);
    }

    pub fn returndatacopy(&mut self) {
        let dest_offset = self.stack.pop_front().expect("Stack underflow");
        let offset = self.stack.pop_front().expect("Stack underflow");
        let size = self.stack.pop_front().expect("Stack underflow");
        let memory_elem = MemoryElem::FromReturndata { offset, dest_offset, size, from_call: false};
        self.mem_sorted_insert(memory_elem.clone());

        let node = GraphElem::Memory(memory_elem);
        let mem_index = self.stack_dag.add_node(node);
        self.stack_dag.add_edge(offset, mem_index, OpType::ReturndataLoadLoc);
        self.stack_dag.add_edge(size, mem_index, OpType::ReturndataLoadSize);
        self.stack_dag.add_edge(dest_offset, mem_index, OpType::ReturndataLoadDestLoc);
    }

    pub fn codecopy(&mut self) {
        let dest_offset = self.stack.pop_front().expect("Stack underflow");
        let offset = self.stack.pop_front().expect("Stack underflow");
        let size = self.stack.pop_front().expect("Stack underflow");
        let memory_elem = MemoryElem::FromCode { address: None, offset, dest_offset, size};
        self.mem_sorted_insert(memory_elem.clone());

        let node = GraphElem::Memory(memory_elem);
        let mem_index = self.stack_dag.add_node(node);
        self.stack_dag.add_edge(offset, mem_index, OpType::CodeCopyLoc);
        self.stack_dag.add_edge(size, mem_index, OpType::CodeCopySize);
        self.stack_dag.add_edge(dest_offset, mem_index, OpType::CodeCopyDestLoc);
    }

    pub fn extcodecopy(&mut self) {
        let address = self.stack.pop_front().expect("Stack underflow");
        let dest_offset = self.stack.pop_front().expect("Stack underflow");
        let offset = self.stack.pop_front().expect("Stack underflow");
        let size = self.stack.pop_front().expect("Stack underflow");
        let memory_elem = MemoryElem::FromCode { address: Some(address), offset, dest_offset, size};
        self.mem_sorted_insert(memory_elem.clone());

        let node = GraphElem::Memory(memory_elem);
        let mem_index = self.stack_dag.add_node(node);
        self.stack_dag.add_edge(address, mem_index, OpType::ExtCodeCopyAddress);
        self.stack_dag.add_edge(offset, mem_index, OpType::ExtCodeCopyLoc);
        self.stack_dag.add_edge(size, mem_index, OpType::ExtCodeCopySize);
        self.stack_dag.add_edge(dest_offset, mem_index, OpType::ExtCodeCopyDestLoc);
    }

    pub fn mstore(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let b_index = self.stack.pop_front().expect("Stack underflow");
        let memory_elem = MemoryElem::FromStack { offset: a_index, stack_elem: b_index, one_byte: false};
        self.mem_sorted_insert(memory_elem.clone());

        let node = GraphElem::Memory(memory_elem);
        let mem_index = self.stack_dag.add_node(node);
        self.stack_dag.add_edge(a_index, mem_index, OpType::MstoreLoc);
        self.stack_dag.add_edge(b_index, mem_index, OpType::MstoreVal);
    }

    pub fn mstore8(&mut self) {
        let a_index = self.stack.pop_front().expect("Stack underflow");
        let b_index = self.stack.pop_front().expect("Stack underflow");

        let memory_elem = MemoryElem::FromStack { offset: a_index, stack_elem: b_index, one_byte: true};
        self.mem_sorted_insert(memory_elem.clone());

        let node = GraphElem::Memory(memory_elem);
        let mem_index = self.stack_dag.add_node(node);
        self.stack_dag.add_edge(a_index, mem_index, OpType::Mstore8Loc);
        self.stack_dag.add_edge(b_index, mem_index, OpType::Mstore8Val);
    }

    pub fn mem_sorted_insert(&mut self, mem_elem: MemoryElem) {
        let mut index_to_insert_to = self.mem.len();
        for (i, elem) in self.mem.iter().enumerate() {
            if mem_elem.lt(elem, &self.stack_dag) {
                index_to_insert_to = i;
                break;
            }
        }
        self.mem.insert(index_to_insert_to, mem_elem);
    }

    pub fn ret(&mut self) {
        let offset = self.stack.pop_front().expect("Stack underflow");
        let size = self.stack.pop_front().expect("Stack underflow");
        let offset_elem = self.stack_dag.node_weight(offset).expect("No node for node index").expect_stack_elem();
        let size_elem = self.stack_dag.node_weight(size).expect("No node for node index").expect_stack_elem();
        // TODO: evaluate memory by looking at size of writes. add implicit slice operation based on return size

        let return_elem = GraphElem::Exit(ExitType::Return, offset_elem.clone(), size_elem.clone());
        let ret_index = self.stack_dag.add_node(return_elem);
        self.stack_dag.add_edge(offset, ret_index, OpType::ReturnMemLoc);
        self.stack_dag.add_edge(size, ret_index, OpType::ReturnMemSize);
    }

    pub fn revert(&mut self) {
        let offset = self.stack.pop_front().expect("Stack underflow");
        let size = self.stack.pop_front().expect("Stack underflow");
        let offset_elem = self.stack_dag.node_weight(offset).expect("No node for node index").expect_stack_elem();
        let size_elem = self.stack_dag.node_weight(size).expect("No node for node index").expect_stack_elem();
        // TODO: evaluate memory by looking at size of writes. add implicit slice operation based on return size

        let rev_elem = GraphElem::Exit(ExitType::Revert, offset_elem.clone(), size_elem.clone());
        let rev_index = self.stack_dag.add_node(rev_elem);
        self.stack_dag.add_edge(offset, rev_index, OpType::RevertMemLoc);
        self.stack_dag.add_edge(size, rev_index, OpType::RevertMemSize);
    }

    pub fn invalid(&mut self) {
        let rev_elem = GraphElem::Exit(ExitType::Invalid, StackElem::Concrete(0.into()), StackElem::Concrete(0.into()));
        self.stack_dag.add_node(rev_elem);
    }

    pub fn sha3(&mut self) {
        let offset = self.stack.pop_front().expect("Stack underflow");
        let size = self.stack.pop_front().expect("Stack underflow");
        // TODO: evaluate memory by looking at size of writes. add implicit slice operation based on return size

        let sha3_elem = GraphElem::Stack(StackElem::Symbolic(SymType::MemOp(SymMemOp { offset, size, mload: false})));
        let sha3_index = self.stack_dag.add_node(sha3_elem);
        self.stack.push_front(sha3_index);
        self.stack_dag.add_edge(offset, sha3_index, OpType::Sha3MemLoc);
        self.stack_dag.add_edge(size, sha3_index, OpType::Sha3MemSize);
    }

    pub fn call(&mut self) {
        let gas = self.stack.pop_front().expect("Stack underflow");
        let address = self.stack.pop_front().expect("Stack underflow");
        let value = self.stack.pop_front().expect("Stack underflow");
        let args_offset = self.stack.pop_front().expect("Stack underflow");
        let args_size = self.stack.pop_front().expect("Stack underflow");
        let ret_offset = self.stack.pop_front().expect("Stack underflow");
        let ret_size = self.stack.pop_front().expect("Stack underflow");

        // TODO: evaluate memory by looking at size of writes. add implicit slice operation based on return size

        let memory_elem = MemoryElem::FromReturndata { offset: 0.into(), dest_offset: ret_offset, size: ret_size, from_call: true};
        self.mem_sorted_insert(memory_elem.clone());

        let node = GraphElem::Memory(memory_elem);
        let mem_index = self.stack_dag.add_node(node);

        let call_ret_elem = GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::ReturnSuccess)));
        let call_ret_index = self.stack_dag.add_node(call_ret_elem);
        self.stack.push_front(call_ret_index);
        self.stack_dag.add_edge(gas, call_ret_index, OpType::CallGas);
        self.stack_dag.add_edge(address, call_ret_index, OpType::CallAddress);
        self.stack_dag.add_edge(value, call_ret_index, OpType::CallValue);
        self.stack_dag.add_edge(args_offset, call_ret_index, OpType::CallArgsMemLoc);
        self.stack_dag.add_edge(args_size, call_ret_index, OpType::CallArgsMemSize);
        self.stack_dag.add_edge(ret_offset, call_ret_index, OpType::CallReturnMemLoc);
        self.stack_dag.add_edge(ret_size, call_ret_index, OpType::CallReturnMemSize);


        self.stack_dag.add_edge(ret_offset, mem_index, OpType::CallReturnMemLoc);
        self.stack_dag.add_edge(ret_size, mem_index, OpType::CallReturnMemSize);
    }

    pub fn callcode(&mut self) {
        let gas = self.stack.pop_front().expect("Stack underflow");
        let address = self.stack.pop_front().expect("Stack underflow");
        let value = self.stack.pop_front().expect("Stack underflow");
        let args_offset = self.stack.pop_front().expect("Stack underflow");
        let args_size = self.stack.pop_front().expect("Stack underflow");
        let ret_offset = self.stack.pop_front().expect("Stack underflow");
        let ret_size = self.stack.pop_front().expect("Stack underflow");

        // TODO: evaluate memory by looking at size of writes. add implicit slice operation based on return size

        let memory_elem = MemoryElem::FromReturndata { offset: 0.into(), dest_offset: ret_offset, size: ret_size, from_call: true};
        self.mem_sorted_insert(memory_elem.clone());

        let node = GraphElem::Memory(memory_elem);
        let mem_index = self.stack_dag.add_node(node);

        let call_ret_elem = GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::ReturnSuccess)));
        let call_ret_index = self.stack_dag.add_node(call_ret_elem);
        self.stack.push_front(call_ret_index);
        self.stack_dag.add_edge(gas, call_ret_index, OpType::CallCodeGas);
        self.stack_dag.add_edge(address, call_ret_index, OpType::CallCodeAddress);
        self.stack_dag.add_edge(value, call_ret_index, OpType::CallCodeValue);
        self.stack_dag.add_edge(args_offset, call_ret_index, OpType::CallCodeArgsMemLoc);
        self.stack_dag.add_edge(args_size, call_ret_index, OpType::CallCodeArgsMemSize);
        self.stack_dag.add_edge(ret_offset, call_ret_index, OpType::CallCodeReturnMemLoc);
        self.stack_dag.add_edge(ret_size, call_ret_index, OpType::CallCodeReturnMemSize);


        self.stack_dag.add_edge(ret_offset, mem_index, OpType::CallReturnMemLoc);
        self.stack_dag.add_edge(ret_size, mem_index, OpType::CallReturnMemSize);

        
    }

    pub fn delegatecall(&mut self) {
        let gas = self.stack.pop_front().expect("Stack underflow");
        let address = self.stack.pop_front().expect("Stack underflow");
        let args_offset = self.stack.pop_front().expect("Stack underflow");
        let args_size = self.stack.pop_front().expect("Stack underflow");
        let ret_offset = self.stack.pop_front().expect("Stack underflow");
        let ret_size = self.stack.pop_front().expect("Stack underflow");

        // TODO: evaluate memory by looking at size of writes. add implicit slice operation based on return size

        let memory_elem = MemoryElem::FromReturndata { offset: 0.into(), dest_offset: ret_offset, size: ret_size, from_call: true};
        self.mem_sorted_insert(memory_elem.clone());

        let node = GraphElem::Memory(memory_elem);
        let mem_index = self.stack_dag.add_node(node);

        let call_ret_elem = GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::ReturnSuccess)));
        let call_ret_index = self.stack_dag.add_node(call_ret_elem);
        self.stack.push_front(call_ret_index);
        self.stack_dag.add_edge(gas, call_ret_index, OpType::DelegateCallGas);
        self.stack_dag.add_edge(address, call_ret_index, OpType::DelegateCallAddress);
        self.stack_dag.add_edge(args_offset, call_ret_index, OpType::DelegateCallArgsMemLoc);
        self.stack_dag.add_edge(args_size, call_ret_index, OpType::DelegateCallArgsMemSize);
        self.stack_dag.add_edge(ret_offset, call_ret_index, OpType::DelegateCallReturnMemLoc);
        self.stack_dag.add_edge(ret_size, call_ret_index, OpType::DelegateCallReturnMemSize);


        self.stack_dag.add_edge(ret_offset, mem_index, OpType::CallReturnMemLoc);
        self.stack_dag.add_edge(ret_size, mem_index, OpType::CallReturnMemSize);
    }

    pub fn staticcall(&mut self) {
        let gas = self.stack.pop_front().expect("Stack underflow");
        let address = self.stack.pop_front().expect("Stack underflow");
        let args_offset = self.stack.pop_front().expect("Stack underflow");
        let args_size = self.stack.pop_front().expect("Stack underflow");
        let ret_offset = self.stack.pop_front().expect("Stack underflow");
        let ret_size = self.stack.pop_front().expect("Stack underflow");

        // TODO: evaluate memory by looking at size of writes. add implicit slice operation based on return size

        let memory_elem = MemoryElem::FromReturndata { offset: 0.into(), dest_offset: ret_offset, size: ret_size, from_call: true};
        self.mem_sorted_insert(memory_elem.clone());

        let node = GraphElem::Memory(memory_elem);
        let mem_index = self.stack_dag.add_node(node);

        let call_ret_elem = GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::ReturnSuccess)));
        let call_ret_index = self.stack_dag.add_node(call_ret_elem);
        self.stack.push_front(call_ret_index);
        self.stack_dag.add_edge(gas, call_ret_index, OpType::StaticCallGas);
        self.stack_dag.add_edge(address, call_ret_index, OpType::StaticCallAddress);
        self.stack_dag.add_edge(args_offset, call_ret_index, OpType::StaticCallArgsMemLoc);
        self.stack_dag.add_edge(args_size, call_ret_index, OpType::StaticCallArgsMemSize);
        self.stack_dag.add_edge(ret_offset, call_ret_index, OpType::StaticCallReturnMemLoc);
        self.stack_dag.add_edge(ret_size, call_ret_index, OpType::StaticCallReturnMemSize);


        self.stack_dag.add_edge(ret_offset, mem_index, OpType::CallReturnMemLoc);
        self.stack_dag.add_edge(ret_size, mem_index, OpType::CallReturnMemSize);
    }

    pub fn sym_create(&mut self) {
        let value = self.stack.pop_front().expect("Stack underflow");
        let offset = self.stack.pop_front().expect("Stack underflow");
        let size = self.stack.pop_front().expect("Stack underflow");
        // TODO: evaluate memory by looking at size of writes. add implicit slice operation based on return size

        let create_ret_elem = GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::CreateAddress)));
        let create_ret_index = self.stack_dag.add_node(create_ret_elem);
        self.stack.push_front(create_ret_index);
        self.stack_dag.add_edge(value, create_ret_index, OpType::CreateValue);
        self.stack_dag.add_edge(offset, create_ret_index, OpType::CreateMemLoc);
        self.stack_dag.add_edge(size, create_ret_index, OpType::CreateMemSize);
    }

    pub fn selfdestruct(&mut self) {
        let addr = self.stack.pop_front().expect("Stack underflow");
        // TODO: evaluate memory by looking at size of writes. add implicit slice operation based on return size

        let destruct_elem = GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::SelfDestruct(addr))));
        let destruct_ret_index = self.stack_dag.add_node(destruct_elem);
        self.stack_dag.add_edge(addr, destruct_ret_index, OpType::SelfDestructAddress);
    }

    pub fn create2(&mut self) {
        let value = self.stack.pop_front().expect("Stack underflow");
        let offset = self.stack.pop_front().expect("Stack underflow");
        let size = self.stack.pop_front().expect("Stack underflow");
        let salt = self.stack.pop_front().expect("Stack underflow");
        // TODO: evaluate memory by looking at size of writes. add implicit slice operation based on return size

        let create2_ret_elem = GraphElem::Stack(StackElem::Symbolic(SymType::Intrinsic(SymIntrinsic::Create2Address(salt))));
        let create2_ret_index = self.stack_dag.add_node(create2_ret_elem);
        self.stack.push_front(create2_ret_index);
        self.stack_dag.add_edge(value, create2_ret_index, OpType::Create2Value);
        self.stack_dag.add_edge(offset, create2_ret_index, OpType::Create2MemLoc);
        self.stack_dag.add_edge(size, create2_ret_index, OpType::Create2MemSize);
        self.stack_dag.add_edge(salt, create2_ret_index, OpType::Create2Salt);
    }

    pub fn log0(&mut self) {
        let offset = self.stack.pop_front().expect("Stack underflow");
        let size = self.stack.pop_front().expect("Stack underflow");
        let log_elem = GraphElem::Events { topics: vec![], offset, size };
        let log_elem_index = self.stack_dag.add_node(log_elem);
        self.stack_dag.add_edge(offset, log_elem_index, OpType::Log0MemLoc);
        self.stack_dag.add_edge(size, log_elem_index, OpType::Log0MemSize);
    }

    pub fn log1(&mut self) {
        let offset = self.stack.pop_front().expect("Stack underflow");
        let size = self.stack.pop_front().expect("Stack underflow");
        let topic0 = self.stack.pop_front().expect("Stack underflow");
        let log_elem = GraphElem::Events { topics: vec![topic0], offset, size };
        let log_elem_index = self.stack_dag.add_node(log_elem);
        self.stack_dag.add_edge(offset, log_elem_index, OpType::Log1MemLoc);
        self.stack_dag.add_edge(size, log_elem_index, OpType::Log1MemSize);
        self.stack_dag.add_edge(topic0, log_elem_index, OpType::Log1Topic0);
    }

    pub fn log2(&mut self) {
        let offset = self.stack.pop_front().expect("Stack underflow");
        let size = self.stack.pop_front().expect("Stack underflow");
        let topic0 = self.stack.pop_front().expect("Stack underflow");
        let topic1 = self.stack.pop_front().expect("Stack underflow");
        let log_elem = GraphElem::Events { topics: vec![topic0, topic1], offset, size };
        let log_elem_index = self.stack_dag.add_node(log_elem);
        self.stack_dag.add_edge(offset, log_elem_index, OpType::Log2MemLoc);
        self.stack_dag.add_edge(size, log_elem_index, OpType::Log2MemSize);
        self.stack_dag.add_edge(topic0, log_elem_index, OpType::Log2Topic0);
        self.stack_dag.add_edge(topic1, log_elem_index, OpType::Log2Topic1);
    }

    pub fn log3(&mut self) {
        let offset = self.stack.pop_front().expect("Stack underflow");
        let size = self.stack.pop_front().expect("Stack underflow");
        let topic0 = self.stack.pop_front().expect("Stack underflow");
        let topic1 = self.stack.pop_front().expect("Stack underflow");
        let topic2 = self.stack.pop_front().expect("Stack underflow");
        let log_elem = GraphElem::Events { topics: vec![topic0, topic1, topic2], offset, size };
        let log_elem_index = self.stack_dag.add_node(log_elem);
        self.stack_dag.add_edge(offset, log_elem_index, OpType::Log3MemLoc);
        self.stack_dag.add_edge(size, log_elem_index, OpType::Log3MemSize);
        self.stack_dag.add_edge(topic0, log_elem_index, OpType::Log3Topic0);
        self.stack_dag.add_edge(topic1, log_elem_index, OpType::Log3Topic1);
        self.stack_dag.add_edge(topic2, log_elem_index, OpType::Log3Topic2);
    }

    pub fn log4(&mut self) {
        let offset = self.stack.pop_front().expect("Stack underflow");
        let size = self.stack.pop_front().expect("Stack underflow");
        let topic0 = self.stack.pop_front().expect("Stack underflow");
        let topic1 = self.stack.pop_front().expect("Stack underflow");
        let topic2 = self.stack.pop_front().expect("Stack underflow");
        let topic3 = self.stack.pop_front().expect("Stack underflow");
        let log_elem = GraphElem::Events { topics: vec![topic0, topic1, topic2, topic3], offset, size };
        let log_elem_index = self.stack_dag.add_node(log_elem);
        self.stack_dag.add_edge(offset, log_elem_index, OpType::Log4MemLoc);
        self.stack_dag.add_edge(size, log_elem_index, OpType::Log4MemSize);
        self.stack_dag.add_edge(topic0, log_elem_index, OpType::Log4Topic0);
        self.stack_dag.add_edge(topic1, log_elem_index, OpType::Log4Topic1);
        self.stack_dag.add_edge(topic2, log_elem_index, OpType::Log4Topic2);
        self.stack_dag.add_edge(topic3, log_elem_index, OpType::Log4Topic3);
    }

    pub fn mload(&mut self) {
        let offset = self.stack.pop_front().expect("Stack underflow");

        // TODO: evaluate memory by looking at size of writes. add implicit slice operation based on return size
        let mload_elem = GraphElem::Stack(StackElem::Symbolic(SymType::MemOp(SymMemOp { offset, size: 0.into(), mload: true })));
        let mload_index = self.stack_dag.add_node(mload_elem);
        self.stack_dag.add_edge(offset, mload_index, OpType::MloadLoc);
        self.stack.push_front(mload_index);
    }



    pub fn stack_dot_str(&self, cumulative: bool) -> String {

        if cumulative {
            format!("{:?}", Dot::with_attr_getters(
                &self.stack_dag,
                &[petgraph::dot::Config::NodeNoLabel, petgraph::dot::Config::EdgeNoLabel],
                &|_graph, edge_ref| {
                    format!("label = {:?}", edge_ref.weight())
                },
                &|_graph, (id, node_ref)| {
                    let color = match node_ref {
                        GraphElem::Stack(_) => "blue",
                        GraphElem::Memory(_) => "green",
                        GraphElem::Code(_) => "orange",
                        GraphElem::Events{..} => "purple",
                        GraphElem::Exit(_, _, _) => "red",
                    };
                    format!("label = {:?} color = \"{}\"", self.backtrace_node(id), color)
                }
                )
            )
        } else {
            format!("{:?}", Dot::new(&self.stack_dag))
        }
    }


    pub fn recurse_build(&self, idx: NodeIndex, graph: &mut Dag, node_index_map: &mut [NodeIndex]) {
        self.stack_dag.edges_directed(idx, Direction::Incoming)
            .for_each(|edge| {
                let node = self.stack_dag.node_weight(edge.source()).expect("No node for node index");
                node_index_map[edge.source().index()] = graph.add_node(node.clone());
                self.recurse_build(edge.source(), graph, node_index_map);
            });
    }

    pub fn code_dot_str(&self) -> String {
        let mut g = Dag::with_capacity(0, 0);
        // mapping from old node index to new node index, end represents removed.
        let mut node_index_map = vec![NodeIndex::end(); self.stack_dag.node_count()];
        
        let mut code_nodes = BTreeSet::new();
        for (i, node) in self.stack_dag.raw_nodes().iter().enumerate() {
            match &node.weight {
                GraphElem::Code(_x) => {
                    code_nodes.insert(i);
                },
                _ => {}
            }
        }

        for i in code_nodes.iter() {
            let idx = NodeIndex::from(*i as u32);
            let node = self.stack_dag.node_weight(idx).expect("No node for node index");
            node_index_map[*i] = g.add_node(node.clone());
            self.recurse_build(idx, &mut g, &mut node_index_map);
        }

        for (_i, edge) in self.stack_dag.raw_edges().iter().enumerate() {
            // skip edge if any endpoint was removed
            let source = node_index_map[edge.source().index()];
            let target = node_index_map[edge.target().index()];
            if source != NodeIndex::end() && target != NodeIndex::end() {
                g.add_edge(source, target, edge.weight.clone());
            }
        }

        format!("{:?}", Dot::new(&g))
    }

    pub fn conditions_for_jumps(&self) -> Vec<(String, String)> {
        self.stack_dag.raw_nodes().iter().enumerate()
            .flat_map(|(i, node)| {
                let idx = NodeIndex::from(i as u32);
                match &node.weight {
                    GraphElem::Code(_x) => {
                        self.stack_dag.edges_directed(idx, Direction::Incoming)
                            .filter(|edge| *edge.weight() == OpType::ConditionalJumpConditional || *edge.weight() == OpType::ConditionalJumpConstEval)
                            .map(|edge| (format!("{:?}", node.weight), self.backtrace_node(edge.source())))
                            .collect()
                    },
                    _ => vec![]
                }
            })
            .collect()
    }

    pub fn optype(&self, op_type: &OpType, op_str: &mut String, current_op: &mut VecDeque<String>, special_fmt: &mut String, edge: petgraph::graph::EdgeReference<OpType>) {
        match op_type {
            OpType::CalldataOffset => {
                *special_fmt = format!("CalldataLoad {{ offset: {} }}", self.backtrace_node(edge.source()));
            }
            OpType::Addend => {
                *op_str = "Add".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            },
            OpType::Minuend => {
                *op_str = "Sub".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            },
            OpType::Subtrahend => {
                *op_str = "Sub".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            },
            OpType::Factor => {
                *op_str = "Mul".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            },
            OpType::Dividend => {
                *op_str = "Div".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            },
            OpType::Divisor => {
                *op_str = "Div".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            },
            OpType::SDividend => {
                *op_str = "Div".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            },
            OpType::SDivisor => {
                *op_str = "Div".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            },
            OpType::Base => {
                *op_str = "Exp".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            },
            OpType::Exponent => {
                *op_str = "Exp".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            },
            OpType::Modulus => {
                *op_str = "Mod".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            },
            OpType::LoadSlot => {
                *op_str = "Sload".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            },
            OpType::StoreSlot => {
                *op_str = "Sstore".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            },
            OpType::StoreValue => {
                *op_str = "Sstore".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            },
            OpType::LtLhs => {
                *op_str = "Lt".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            },
            OpType::LtRhs => {
                *op_str = "Lt".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            },
            OpType::GtLhs => {
                *op_str = "Gt".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            },
            OpType::GtRhs => {
                *op_str = "Gt".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            },
            OpType::SltLhs => {
                *op_str = "Slt".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            },
            OpType::SltRhs => {
                *op_str = "Slt".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            },
            OpType::SgtLhs => {
                *op_str = "Sgt".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            },
            OpType::SgtRhs => {
                *op_str = "Sgt".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            },
            OpType::EqRhs => {
                *op_str = "Eq".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            },
            OpType::EqLhs => {
                *op_str = "Eq".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            },
            OpType::OrLhs => {
                *op_str = "Or".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            },
            OpType::OrRhs => {
                *op_str = "Or".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            },
            OpType::XorLhs => {
                *op_str = "Xor".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            },
            OpType::XorRhs => {
                *op_str = "Xor".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            },
            OpType::AndLhs => {
                *op_str = "And".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            },
            OpType::AndRhs => {
                *op_str = "And".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            },
            OpType::LShiftee => {
                *op_str = "Shl".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            },
            OpType::LShiftor => {
                *op_str = "Shl".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            },
            OpType::RShiftee => {
                *op_str = "Shr".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            },
            OpType::RShiftor => {
                *op_str = "Shr".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            },
            OpType::SARShiftee => {
                *op_str = "Sar".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            },
            OpType::SARShiftor => {
                *op_str = "Sar".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            },
            OpType::Not => {
                *op_str = "Not".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            },
            OpType::IsZero => {
                *op_str = "IsZero".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            },
            OpType::Byte => {
                *op_str = "Byte".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            },
            OpType::SignExtendee => {
                *op_str = "SignExtend".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            },
            OpType::ByteWord => {
                *op_str = "Byte".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            },
            OpType::MstoreLoc => {
                *op_str = "Mstore".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::MstoreVal => {
                *op_str = "Mstore".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            }
            OpType::Mstore8Loc => {
                *op_str = "Mstore8".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::Mstore8Val => {
                *op_str = "Mstore8".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            }
            OpType::ConditionalJumpConditional => {
                *op_str = "Jumpi".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            }
            OpType::ConditionalJumpConstEval => {
                *op_str = "Jumpi".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            }
            OpType::ConditionalJumpLoc => {
                *op_str = "Jumpi".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::JumpLoc => {
                *op_str = "Jump".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::Balance => {
                *op_str = "Balance".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::ReturnMemLoc => {
                *op_str = "Return".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::ReturnMemSize => {
                *op_str = "Return".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            }
            OpType::RevertMemLoc => {
                *op_str = "Revert".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::RevertMemSize => {
                *op_str = "Revert".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            }
            OpType::Sha3MemLoc => {
                *op_str = "Sha3".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::Sha3MemSize => {
                *op_str = "Sha3".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            }
            OpType::Duplicant => {
                current_op.push_front(self.backtrace_node(edge.source()))
            }
            OpType::MloadLoc => {
                *op_str = "Mload".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::CallGas => {
                *op_str = "Call".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::CallAddress => {
                *op_str = "Call".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::CallValue => {
                *op_str = "Call".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::CallArgsMemLoc => {
                *op_str = "Call".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::CallArgsMemSize => {
                *op_str = "Call".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::CallReturnMemLoc => {
                *op_str = "Call".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::CallReturnMemSize => {
                *op_str = "Call".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            }
            OpType::CallCodeGas => {
                *op_str = "CallCode".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::CallCodeAddress => {
                *op_str = "CallCode".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::CallCodeValue => {
                *op_str = "CallCode".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::CallCodeArgsMemLoc => {
                *op_str = "CallCode".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::CallCodeArgsMemSize => {
                *op_str = "CallCode".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::CallCodeReturnMemLoc => {
                *op_str = "CallCode".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::CallCodeReturnMemSize => {
                *op_str = "CallCode".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            }

            OpType::DelegateCallGas => {
                *op_str = "DelegateCall".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::DelegateCallAddress => {
                *op_str = "DelegateCall".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::DelegateCallArgsMemLoc => {
                *op_str = "DelegateCall".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::DelegateCallArgsMemSize => {
                *op_str = "DelegateCall".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::DelegateCallReturnMemLoc => {
                *op_str = "DelegateCall".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::DelegateCallReturnMemSize => {
                *op_str = "DelegateCall".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            }

            OpType::StaticCallGas => {
                *op_str = "StaticCall".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::StaticCallAddress => {
                *op_str = "StaticCall".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::StaticCallArgsMemLoc => {
                *op_str = "StaticCall".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::StaticCallArgsMemSize => {
                *op_str = "StaticCall".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::StaticCallReturnMemLoc => {
                *op_str = "StaticCall".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::StaticCallReturnMemSize => {
                *op_str = "StaticCall".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            }
            OpType::Log0MemLoc => {
                *op_str = "Log0".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::Log0MemSize => {
                *op_str = "Log0".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::Log1MemLoc => {
                *op_str = "Log1".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::Log1MemSize => {
                *op_str = "Log1".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::Log1Topic0 => {
                *op_str = "Log1".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::Log2MemLoc => {
                *op_str = "Log2".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::Log2MemSize => {
                *op_str = "Log2".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::Log2Topic0 => {
                *op_str = "Log2".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::Log2Topic1 => {
                *op_str = "Log2".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::Log3MemLoc => {
                *op_str = "Log3".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::Log3MemSize => {
                *op_str = "Log3".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::Log3Topic0 => {
                *op_str = "Log3".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::Log3Topic1 => {
                *op_str = "Log3".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::Log3Topic2 => {
                *op_str = "Log3".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::Log4MemLoc => {
                *op_str = "Log4".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::Log4MemSize => {
                *op_str = "Log4".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::Log4Topic0 => {
                *op_str = "Log4".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::Log4Topic1 => {
                *op_str = "Log4".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::Log4Topic2 => {
                *op_str = "Log4".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::Log4Topic3 => {
                *op_str = "Log4".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::CreateValue => {
                *op_str = "Create".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::CreateMemLoc => {
                *op_str = "Create".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::CreateMemSize => {
                *op_str = "Create".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            }
            OpType::Create2Value => {
                *op_str = "Create2".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::Create2MemLoc => {
                *op_str = "Create2".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::Create2MemSize => {
                *op_str = "Create2".to_string();
                current_op.push_front(self.backtrace_node(edge.source()));
            }
            OpType::Create2Salt => {
                *op_str = "Create2".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            }
            OpType::CalldataLoadLoc => {
                *op_str = "CalldataLoad".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            }
            OpType::CalldataLoadSize => {
                *op_str = "CalldataLoad".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            }
            OpType::CalldataLoadDestLoc => {
                *op_str = "CalldataLoad".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            }
            OpType::ReturndataLoadLoc => {
                *op_str = "ReturndataLoad".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            }
            OpType::ReturndataLoadSize => {
                *op_str = "ReturndataLoad".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            }
            OpType::ReturndataLoadDestLoc => {
                *op_str = "ReturndataLoad".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            }
            OpType::CodeCopyLoc => {
                *op_str = "CodeCopy".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            }
            OpType::CodeCopySize => {
                *op_str = "CodeCopy".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            }
            OpType::CodeCopyDestLoc => {
                *op_str = "CodeCopy".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            }
            OpType::ExtCodeCopyAddress => {
                *op_str = "ExtCodeCopy".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            }
            OpType::ExtCodeCopyLoc => {
                *op_str = "ExtCodeCopy".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            }
            OpType::ExtCodeCopySize => {
                *op_str = "ExtCodeCopy".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            }
            OpType::ExtCodeCopyDestLoc => {
                *op_str = "ExtCodeCopy".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            }
            OpType::SelfDestructAddress => {
                *op_str = "SelfDestruct".to_string();
                current_op.push_back(self.backtrace_node(edge.source()));
            }
        }
    }

    pub fn backtrace_node(&self, node_idx: NodeIndex) -> String {
        // get parents
        let mut op_str = "".to_string();
        let mut current_op: VecDeque<String> = VecDeque::new();
        let edges: Vec<petgraph::graph::EdgeReference<OpType>> = self.stack_dag
            .edges_directed(node_idx, Direction::Incoming)
            .collect();

        if edges.is_empty() {
            let node = self.stack_dag.node_weight(node_idx).expect(&format!("No node for index {:?}. node count: {:?}", node_idx, self.stack_dag.node_count()));
            node.expect_stack_elem().expect_zero_input_string() 
        } else {
            let mut special_fmt = "".to_string();
            edges.iter()
                .for_each(|edge| {
                    let optype = edge.weight();
                    self.optype(optype, &mut op_str, &mut current_op, &mut special_fmt, *edge);
                });

            if special_fmt != "" {
                special_fmt.to_string()
            } else {
                let include_parens = op_str != "";
                let mut ops_str = op_str;
                let le = current_op.len();
                current_op.iter().enumerate().for_each(|(i, op)| {
                    if i == 0 && include_parens {
                        ops_str += "(";
                    }
                    ops_str += op;
                    if i == le - 1 {
                        if include_parens {
                            ops_str += ")";
                        }
                    } else {
                        ops_str += ", ";
                    }
                });
                ops_str
            }
        }
    }
}

impl<DB: revm::Database> Inspector<DB> for OpsGraph {
    fn initialize_interp(
        &mut self,
        _: &mut Interpreter,
        _data: &mut EVMData<'_, DB>,
        _: bool,
    ) -> Return {
        Return::Continue
    }

    fn step(&mut self, interpreter: &mut Interpreter, _: &mut EVMData<'_, DB>, _: bool) -> Return {
        // Record reads
        let op = interpreter.contract.bytecode.bytecode()[interpreter.program_counter()];
        // println!("op: {:?}", OPCODE_JUMPMAP[op as usize]);
        // println!("stack: {:?}", self.stack);
        match op {
            ADD => self.add(),
            MUL => self.mul(),
            SUB => self.sub(),
            DIV => self.div(),
            SDIV => self.sdiv(),
            MOD => self.rem(),
            SMOD => self.smod(),
            ADDMOD => self.addmod(),
            MULMOD => self.mulmod(),
            EXP => self.exp(),
            SIGNEXTEND => self.signextend(),
            LT => self.lt(),
            GT => self.gt(),
            SLT => self.slt(),
            SGT => self.sgt(),
            EQ => self.eq(),
            ISZERO => self.iszero(),
            AND => self.and(),
            OR => self.or(),
            XOR => self.xor(),
            NOT => self.not(),
            BYTE => self.byte(),
            SHL => self.shl(),
            SHR => self.shr(),
            SAR => self.sar(),
            SHA3 => self.sha3(),
            ADDRESS => self.address(),
            BALANCE => self.balance(),
            ORIGIN => self.origin(),
            CALLER => self.caller(),
            CALLVALUE => self.callvalue(),
            CALLDATALOAD => self.calldataload(),
            CALLDATASIZE => self.calldatasize(),
            CALLDATACOPY => self.calldatacopy(),
            CODESIZE => self.codesize(),
            CODECOPY => self.codecopy(),
            GASPRICE => self.gasprice(),
            EXTCODESIZE => self.extcodesize(),
            EXTCODECOPY => self.extcodecopy(),
            RETURNDATASIZE => self.returndatasize(),
            RETURNDATACOPY => self.returndatacopy(),
            BLOCKHASH => self.blockhash(),
            COINBASE => self.coinbase(),
            NUMBER => self.number(),
            DIFFICULTY => self.difficulty(),
            GASLIMIT => self.gaslimit(),
            BASEFEE => self.basefee(),
            POP =>  { self.stack.pop_front().expect("Stack underflow"); },
            MLOAD => self.mload(),
            MSTORE => self.mstore(),
            MSTORE8 => self.mstore8(),
            SLOAD => {
                let slot_index = self.stack.pop_front().expect("Stack underflow");
                let elem = StackElem::Symbolic(SymType::Storage { slots: vec![slot_index]});
                let elem_idx = self.stack_dag.add_node(GraphElem::Stack(elem));
                self.stack_dag.add_edge(slot_index, elem_idx, OpType::LoadSlot);
                self.stack.push_front(elem_idx);
            }
            SSTORE => {
                let slot_index = self.stack.pop_front().expect("Stack underflow");
                let value_index = self.stack.pop_front().expect("Stack underflow");
                let elem = StackElem::Symbolic(SymType::Storage { slots: vec![slot_index]});
                let elem_idx = self.stack_dag.add_node(GraphElem::Stack(elem));
                self.stack_dag.add_edge(slot_index, elem_idx, OpType::StoreSlot);
                self.stack_dag.add_edge(value_index, elem_idx, OpType::StoreValue);
                self.stack.push_front(elem_idx);
            }
            JUMP => self.jump(),
            JUMPI => self.jumpi(),
            PC => self.pc(interpreter.program_counter()),
            MSIZE => self.msize(),
            GAS => self.gas(),
            JUMPDEST => { /* do nothing */ },
            0x60..=0x7F => {
                // push
                let start = interpreter.program_counter() + 1;
                let push_len: usize = (op - 0x5f).into();
                let slice = &interpreter.contract.bytecode.bytecode()[start..start + push_len];
                let val = self.push_to_u256(push_len.into(), slice);

                let elem_idx = self.stack_dag.add_node(GraphElem::Stack(StackElem::Concrete(val)));
                self.stack.push_front(elem_idx);
            }
            0x80..=0x8f => {
                let dup_loc = (op - 0x80) as usize;
                let duplicant_idx = self.stack[dup_loc];
                let node = self.stack_dag.node_weight(duplicant_idx).expect("No node for index");
                let dupped_idx = self.stack_dag.add_node(node.clone());
                self.stack_dag.add_edge(duplicant_idx, dupped_idx, OpType::Duplicant);
                self.stack.push_front(dupped_idx);
            }
            0x90..=0x9f => {
                let swap_loc = (op - 0x8f) as usize;
                let swap_idx = self.stack[swap_loc];
                self.stack[swap_loc] = self.stack[0];
                self.stack[0] = swap_idx;
            }
            LOG0 => self.log0(),
            LOG1 => self.log1(),
            LOG2 => self.log2(),
            LOG3 => self.log3(),
            LOG4 => self.log4(),
            CREATE => self.sym_create(),
            CALL => self.call(),
            CALLCODE => self.callcode(),
            RETURN => self.ret(),
            DELEGATECALL => self.delegatecall(),
            CREATE2 => self.create2(),
            STATICCALL => self.staticcall(),
            REVERT => self.revert(),
            INVALID => self.invalid(),
            SELFDESTRUCT => self.selfdestruct(),
            _ => { println!("panicing: {:?}", op); self.invalid()},
        }

        // println!("stack:");
        // println!("    [");
        // self.stack.iter().for_each(|elem| {
        //     let st = self.backtrace_node(*elem);
        //     if &st[0..2] != "0x" {
        //         println!("        {},", self.backtrace_node(*elem));
        //     }
        // });
        // println!("    ]");

        Return::Continue
    }
}
