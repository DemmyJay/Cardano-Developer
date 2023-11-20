# PLUTUS PIONEER PROGRAM

## Plutus Smart Contract Implementation

Fundamental concepts for Plutus Smart Contract implementation:

* Simple and complex transaction creation and execution.
* Fungible NFT token minting.
* Multi-party smart contract.
* Transaction building with metadata.

Types of Plutus Contracts Implementations and use cases:

1. NFT Minting, transfer buying and selling.
2. Multi-party Smart contracts.
3. Oracles
4. Dex: Token Swap, Liquidity and Stake pools.
5. Lending and Borrowing, collateral escrow and flash loans.
6. DeFi Tools.

**Lecture 1:**

UTxO Model
Account Based Model
Plutus SC vs Ethereum SC

## A plutus smart contract is divided into

### On-Chain component

* Onchain code

### Off-chain component

* Off-chain code
* Serialization library
* Cli
* Cardano Transaction library
* LUCID

## On-Chain Code

A validator script basically is arbitrary logic limited to the size of the block making it part of the block data. 
To consume UTxOs sitting in a script address, the node would run the script whit is submitted with the transaction an depending on the result of the script,
determine whether that address is allowed to spend the UTxOs.There are 3 inputs expected by a validator script:

* Datum
* Redeemer
* Script Context

Datum
: On the UTxO output side, the datum sits in the UTxO of an address and is a very important part of a UTxO transaction, without it, a value can't be taken out or consumed of a contract cause the validator process is expecting a datum in order for that value to be spent.

Redeemer
: The redeemer is an arbitrary piece of data that verifies whether an address is allowed to spend a UtxO in a script address or not.

Script Context
: This contains the transaction being validated and it's inputs and outputs.

These inputs are of the type; `BuiltInData`, which is basically the the Data data-type but for off-chain code.
The Data data-type allows arbitrary piece of data to be represented is json like format and for typed validator, can be can be converted to `BuiltInData` data-typed using any one of the conversion functions such as `dataToBuiltInData`

Typed Validator
: This involves custom data-types which do not use compiler pipelines. In other to use type validators, the we define the type instance of the redeemer type and the datum type
as the types of the defined type signature which could be either custom data-types or predefined data-types types.

The Plutus SDK uses the byte-code directly, wrapped in a validator Type but most tools serialize the byte-code into Json encoded CBOR HEX to submit the byte-code to the Tx construction part.

## Off-Chain Code

The Off-chain code isn't always used for production and is not the only tool.
The plutus Off-chain code depends on the contract monad i.e "Contract w s e ()". The contract monad is partially an abstraction
of the event monad and the functions required for transaction construction just like analogies to "transaction build" and
"transaction build raw" and all it's parameters.

In a production level, off-chain code is not written in the same module as on-chain code especially if
we are making use of serialization libraries then there would be no need for off-chain code, we would just have an onchain code a serialized validator
and transactions would be created from the serialization library

For the contract monad `Contract w s e ()` to work it needs the types which are:

* w --> login state, behaves like the writer monad so you can store something which can be sent to another contract.
* s --> Schema; which contains all of the functions of all endpoints that provide functionality to the wallet and contract monad.
* e --> Type of errors you can output
* () --> Output of the monad itself, it behaves like the IO monad and is a wrapper of the event Monad

Schema
: Schema  defines the input, which is like the interface. This define the functionality that the PAB can expose to the wallet layer the contract can read it's interface
The Schema contains endpoints which are method in the contract monad. The contract monad is a specific functionality of the Plutus off-chain code in the Plutus SDK

tx = mustPayToScript valHash (Datum ) Amount (inputs and balance are done automatically in a plutus transaction)
The plutus transaction is the "script context"

PlutusTx
: This is defined as the on-chain code (validator script) and is compiled into Plutus core which is Plutus Byte code. The Plutus byte-code doesn't go into the blockchain,
what we have on the blockchain is the script address, during the process of a Tx execution, the script address is not executed as the validator
but the Tx is passed along with the validator script as byte-code i.e plutus core and the Datum is provided as well. The plutus code is compiled into byte code by the PlutusTx compiler extension for Haskell'

Reference script
: The reference script is a solution to reduce a "bloat" in memory consumption of byte code in every block, this is fundamentally a reference to an Instance of the same byte-code.

Tx Submission:
: This occurs at node level

Atomic Contract
: **to be defined**

  Questions:

  1. Datatype of the tx identifier
  2. How does Json encoded CBOR work?
  3. How does the constraint function work?
  4. What is the data-type of the "give" action?
  5. How does the playground separate the On-chain and Off-chain code
  6. Do you call or Validator script from a backend server or can your on-chain code function as a library definition,
  which you import and utilize your validator script from there.

### Study

  CIP 30, 31, 32 & 33
  What is emulator trace
  
## Child-Notes

### Notes

* Being aware of your limits is how you make a better transaction construction.

### Repl Imports & Commands

* PlutusTx, Ledger.Scripts, PlutusTx.prelude.error
* set -XOverloadedStrings" helps us set the compiler extension for working with byteStrings.

## Plutus Smart Contract Development

### Boilerplate

-> Outline project documentation
-> Identify and write out boiler-plates which include:
   • Imports
   • Pragmas
   • Onchain boilerplate
   • Off-chain boiler-plates

### On-chain code development process

#### UnTyped Validator Script

-> Illustrate the entire project process and Identify all necessary validator scripts make project as simple as possible
-> Identify all necessary checks on an atomic level for spending Script UTxOs
-> Identify  Interface endpoints
-> Use Typed-Plutus but with optimization features such as `unsafeToBuiltInData` function `Spooky` wrapper type.

#### Typed Validator Script

Typed validator scripts are more robust but come with a high cost of memory also the cost of execution of the script goes up as more computational resources are required to execute the script

### Off-chain code development process

#### Sending UTxO to a script

* First create transaction "tx" by specifying constraints which take in the validator hash, datum and amount as inputs
* Submit the transaction using "submitTx" function
* write code that awaits confirmation of the transaction
* Using validator parameter inputs when necessary, they change the script.
* Define schemas for UI and  define the functions that selects schema options

#### Spending UTxO from a script

* Get UTxOs at specified address using `utxosAt` function.
* Get all references to all UtxOs.
* create `lookups`, where `lookups :: ScriptLookups` which is basically telling the wallet where to find the UTxOs and the Validator Script
* Create the transaction "Tx" using Constraints.
* Submit the transaction.
* write code that awaits confirmation of the transaction.
