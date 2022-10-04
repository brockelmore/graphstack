# GraphStack

Generates a symbolic graph of what happens to various aspects of the EVM.

Tracks history of stack variables & memory* and lets you collapse the history into a single statement like so:


`IsZero(Eq(CalldataLoad { offset: Add(CalldataLoad { offset: 0x4 }, 0x4) }, 0x2a))`

What this means is effectively:


```solidity
let a := msg.data[0x4:0x4+32];
0 == (msg.data[a:a+32] == 0x2a)
```

Additionally integrates with graphviz