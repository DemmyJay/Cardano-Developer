# Cardano Solutions Arch

## Cardano Smart Contract Development Stack

### On-chain Validator Development Languages

- Plutus - Very high level, easy readability and writability, any inlinable Haskell function.

- Helios - JavaScript-based, 50% more efficient than Plutus, not type-safe, not very abstract.

- Plutarch - Very low level, 4-6x more efficient than Plutus, type-safe, not very abstract.

- Plu-ts - Typescript based language, 25% more efficient than Plutus.

- Aiken - Rust based incomplete.

### Off-chain Transaction Building Languages

- Lucid - JavaScript / TypeScript based, designed for simplicity, readability, writability, but not a lot of documentation.

- Martify Mesh - JavaScript / TypeScript based, most extensive documentation, designed for ease of use, readability, and writability, a little less robust than Lucid.

- Kuber - Haskell-based transaction builder, a robust Haskell solution.

- MLabs PAB - useable, but very complicated, basically Haskell wrapped Cardano-CLI.

- Atlas: Genius Yield's Open-source PAB - The most robust Haskell solution.  

- CTL - My personal favorite, PureScript based, painful to setup, very good readability / writability, and very robust.

https://github.com/Plutonomicon/cardano-transaction-lib/tree/develop/examples

Marginal optimization for validator script function:
 to optimize Datum, Redeemer and scriptContex by using the "unsafeToBuildInData" function.

## Topics

- Validator Script optimization using Typed Validator.
- Unit testing.
- Property-based testing.
- Benchmarking.
- Use case Analysis.


- 21st Jan 2023
Merkle trees
: Merkle trees allow you to commit large a amount of information on the blockchain with.
- On Chain Meta-Data using Reference NFTs. (21st Jan 2023)


- 28/1/23 & 29/1/23
Lucid.
: Install NPX using NPM package manager and integrate with Blockfrost.
  Lucid transaction building.
- [Transaction building Optimization using reference scripts (@ 5:42am & 29/1/23 last 1hr 30 minute).](https://github.com/colll78)
- Solving the concurrency issue using.
- Plutus Design Patterns.

- 4th Feb 2023
Minting tokens with Plutus scripts.

- 11th Feb 2023
  - [Transaction Token Patten: Using minting policies to spend a transaction rather than standard validators where it makes sense.](https://github.com/Plutonomicon/plutonomicon/blob/main/transaction-token-pattern.md)
  - [Solving the concurrency issues using on-chain linked list UTxOs.](https://mlabs.slab.com/public/posts/on-chain-association-list-with-constant-time-insert-removal-sh8z2xzy)
  - [Folding/Traversing over a list](https://mlabs.slab.com/public/posts/on-chain-association-list-with-constant-time-insert-removal-sh8z2xzy)
  The folding validator works by accumulating the values of all the UTxOs using some accumulator function.

Note
: The folding is done on-chain, each transaction takes the fold utxo (containing the datum with the accumulator/integer datum in this case)  locked at the fold validator and spends that utxo, reading over linked list nodes and adding their value to the datum of the fold UTxO.
: The idea of the transaction token pattern is that validation logic occurs at the transaction level instead of the UTxO level. Normally, when you use a normal spending validator, if you unlock 3 utxos at the smart contract address, the script will run 3 times, once for each input.
: A minting policy operates at the transaction level, in the sense that all the utxos can be validated by executing the minting policy once, instead of 3 times. Then for each utxo being unlocked, instead of running a ton of validation logic, all it does is check that a minting policy is executed successfully.

  **Use case of The Transaction Token pattern:**
    Auction
    Staking
  **Terms To Search**
    Forward minting policy: This is when a minting policy forwards its logic to a validator.

- 12th Feb 2023
 Middleware solutions for Cardano:
  - PAB (out of development): A centralized off-chain transaction builder that runs DBsync as an interface to the Blockchain
  - Frontend transaction building provider for Lucid: Blockfrost, Emulator provider, Kupmios.
  - Carp: Syncs data from the Cardano blockchain and stores it into a Postgres database as an alternative to the very heavy Blockfrost
    Other alternatives to CARP are; Oura, Kupo, Ogmios.
  - Scrolls:
  - Cardano Graph QL: This a backend infrastructure that offers direct integration with TypeScript and a Docker container that can be run easily

- 5th March 2023
  Improve your Haskell:
  :https://kowainik.github.io/posts/haskell-mini-patterns.html
  :https://www.fpcomplete.com/haskell/syllabus/
  :https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/
  
Solutions for Cardano:
: Latex: Documentation creator

- 12th March 2023:
  Oracle/Dao Integration:
  Agora On-chain governance.

- 18th March 2023:
  - Review of Algora
  - Advice from Philip: Master Plutus then learn Plutarch and Untyped Plutus Core also get familiar with Aiken, Helios and  Plu-Ts versatility.
  - Here is a breakdown regarding what projects are using (to the best of my knowledge) for on-chain and off-chain.

    SundaeSwap:
    Onchain: Plutus V1
    Off-chain: Go / Typescript-based PAB https://github.com/SundaeSwap-finance/toolkit-for-cardano

    WingRiders:
    Onchain Mainnet: Plutus V1 for DEX contracts, and Plutarch V1 for auxiliary contracts (vesting, farming, etc).
    Onchain (unreleased): Plutarch V2 for both DEX & auxiliary contracts.
    Off-chain: In-house typescript serialization / PAB

    Indigo:
    Onchain: No idea
    Offchain: Cardano Transaction Library (CTL)

    Minswap:
    Onchain: Hybrid of Plutus V1 & Plutarch V1
    Offchain: Open-source PAB implemented in Go
    https://github.com/minswap/pab-go

    Genius Yield:
    Onchain: Plutarch
    Offchain: In-house PAB implemented in Haskell, soon to be open-sourced.
    https://www.reddit.com/r/Genius_Yield/commentsyl77zs/genius_yields_latest_monthly_update_video_new/

    ErgoDex and TeddySwap:
    Onchain ErgoDex & TeddySwap: Plutarch V2 for core DEX contracts
    Onchain TeddySwap: Plu-ts for auxiliary DEX contracts like yield farming.
    Offchain ErgoDex: Open-source in-house PAB implemented in Haskell WIP
    https://github.com/spectrum-finance/cardano-dex-backend/
    Offchain TeddySwap: Lucid (uncertain)

    SpaceBudz / Nebula / Wormhole:
    Onchain: Used to use exclusively Plutus while Aiken was still unstable. Now has mostly updated to use Aiken.
    Offchain: Lucid

    Liqwid:
    Onchain: Plutarch V2
    Offchain: Cardano Transaction Library (CTL)

    Agora (Liqwid):
    Onchain: Plutarch V2 (open-source)
    Offchain: CTL (closed source currently)

    Ikigai Technologies:
    Onchain for Grabbit: Plutarch V2
    Offchain for Grabbit & Logosphere: Lucid

    Aada Finance:
    Onchain: Plutus V1 for lending contracts & Native scripts for liquidation oracles (currently being ported to Aiken).
    Offchain: Uncertain

    AnetaBTC:
    Onchain Bridge: Plutarch V2
    Onchain Staking: Plutarch lsV1
    Offchain: Lucid

- 25th of March

- 26th of March
  - setup for Plutarch

2nd of April

- Learning Plutarch:

  Setting up the Plutarch and Lucid environment.
  Building a Dapp that with a contract, requires people to mint an NFT, so if they pay they can mint an NFT.
  What is Liquid libs? It is an extension to Plutarch that introduces a lot of Plutarch instances.

- 15th of April
  
  A minting policy allows a user to pay funds to a DAO wallet instead of a specified address

- 23rd of April

Aiken & Deeper dive into Plutarch

30th of April

 Implementation of CIP68

6th of May

Continuation of CIP68

7th of May

CIP 57: Plutus Contract Blueprint.
 CIP very briefly. CIP-57 is a "Plutus Contract Blueprint". Meaning, you can take any Plutus script (that supports it) and get a blueprint of the contract. Meaning, it will tell you all the endpoints and constructors (options) you can interact with that script. So all the context of the script.

13th of May

Create a validator example with Plutarch and Lucid

14th of May

Formal Verification: Verification & Validation:

Dynamic Approaches vs Static Approaches

20th May 2023

Plutarch continuation(final class):
Write Lucid code to interact with script from our last class

- 21th May 2023
Reviewing projects:
Spacebudz: 
Wormhole: Wormhole is a project that allows you to port existing NFTs that were minted using the old metadata standard ie CIP 25 to using on-chain metadata.
Nebula: Nebula is a prebuilt open-source marketplace that you can launch in your project.

27th May 2023
Formal verification Mathematical using Isabella tool.

### Terms to research

Profiling tools.
What is folding a UTxO in a linked list?

### Questions

In the on-chain linked list, what do you mean by fold?
Reward UTxO
In the case of using a Transaction Token Pattern, why use a minting policies when we need to modify the datum of the UTxO that precedes the token we are trying to spend in the list? Minting policies can see the datums of the UTxOs.

### Webinar events

Mar 30, 2023
Genus-Yield: Plutus Application Backend & Atlas

Feb 7, 2023
Cardano tool-chain: 
Web3 made easy: "Lucid" by 

30th May 2023
"Jambhala" by Ian

### Need to study and practice

Atlas Course

Nix Course

Docker Course

Atala  Prism

GitHub

Lucid and Aiken Courses

Plutarch Course

## Link to CSA Curriculum

[CSA Curriculum](https://docs.google.com/spreadsheets/d/1Y6hv0ht0C8mOsklxaH0G5oUy1-7o3NJjuCLhHAlsJ-Q/edit?pli=1#gid=0)



