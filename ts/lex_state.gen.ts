/**
            * These are generated DFA states for the lexer.
            * Don't edit this file.
            */
            
            type TokenDef2 = {
                    kind: "(",
                    hasValue: true
                }
                
                type TokenDef5 = {
                    kind: ")",
                    hasValue: true
                }
                
                type TokenDef8 = {
                    kind: "add",
                    hasValue: true
                }
                
                type TokenDef13 = {
                    kind: "[1-9]+[0-9]*",
                    hasValue: true
                }
                
                export type Tokens = {
  2: TokenDef2;  5: TokenDef5;  8: TokenDef8;  13: TokenDef13;
}

type State0 = {
                    to: [1,2,3,4],
                    labels: [{ from: 40, to: 41 },{ from: 41, to: 42 },{ from: 49, to: 58 },{ from: 97, to: 98 }],
                    accepting: false
                }
                
                type State1 = {
                    to: [],
                    labels: [],
                    accepting: true
                }
                
                type State2 = {
                    to: [],
                    labels: [],
                    accepting: true
                }
                
                type State3 = {
                    to: [5,6],
                    labels: [{ from: 48, to: 49 },{ from: 49, to: 58 }],
                    accepting: true
                }
                
                type State4 = {
                    to: [7],
                    labels: [{ from: 100, to: 101 }],
                    accepting: false
                }
                
                type State5 = {
                    to: [5,5],
                    labels: [{ from: 48, to: 49 },{ from: 49, to: 58 }],
                    accepting: true
                }
                
                type State6 = {
                    to: [5,6],
                    labels: [{ from: 48, to: 49 },{ from: 49, to: 58 }],
                    accepting: true
                }
                
                type State7 = {
                    to: [8],
                    labels: [{ from: 100, to: 101 }],
                    accepting: false
                }
                
                type State8 = {
                    to: [],
                    labels: [],
                    accepting: true
                }
                
                export type States = [State0, State1, State2, State3, State4, State5, State6, State7, State8];

export type DFAtoNFA = {
  1: [ 2 ],
2: [ 5 ],
3: [ 13, 16, 17, 18, 19, 20, 21, 22 ],
4: [ 10 ],
5: [ 13, 14, 15, 16, 17 ],
6: [ 13, 14, 15, 16, 17, 18, 19, 20, 21, 22 ],
7: [ 9 ],
8: [ 8 ],
}