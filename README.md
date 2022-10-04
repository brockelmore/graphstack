# GraphStack

Generates a symbolic graph of what happens to various aspects of the EVM.

Tracks history of stack variables & memory* and lets you collapse the history into a single statement like so:


`IsZero(Eq(CalldataLoad { offset: Add(CalldataLoad { offset: 0x4 }, 0x4) }, 0x2a))`

What this means is effectively:


```solidity
let a := msg.data[0x4:0x4+32];
0 == (msg.data[a:a+32] == 0x2a)
```

Additionally integrates with graphviz:
![alt text](https://github.com/brockelmore/graphstack/blob/master/output.jpg?raw=true)

_Image from a run against https://github.com/paradigmxyz/paradigm-ctf-2022/blob/main/fun-reversing-challenge/public/contracts/Challenge.sol_


Graphviz generation has two modes, cumulative, and incremental. Incremental does no stack history collapsing, while cumulative does.

Very alfa. if you want to use it, currently you have to modify a couple lines:

1. Change the `inc_dec_reset_get` to the bytecode of your contract
2. Change `data` to be the calldata you want to send to your contract

This will be improved later but low priority while testing & building


## What this can be used for

1. A decompiler
2. Static analysis
3. Debugging
4. Pen testing
5. Improved Fuzzers