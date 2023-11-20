# NOTES

In an address with both the payment and staking components, the payment component hash has 53 characters.

## Topics

 1. [Setup Cardano node](link)
 2. [Setup Plutus Playground](link)
 3. [Install Bech32 tool](link)
 4. Building a single signature utxo
 5. Building a Multi-witness utxo
 6. Building a Multi-signature native script
 7. Building transaction Metadata
 8. Minting native fungible tokens using native scripts
 9. Minting native NFT using native scripts
 10. Plutus Smart contract: On-chain code vs off-chain code

## Bech32 Tool

The bech32 tool is used to disassemble / assemble Cardano addresses from the hex to bech32 encoded addresses or vice-versa:

### Installation Steps

> cd ~/cardano-src
> git clone https://github.com/input-output-hk/bech32.git
> cd bech32/
> cabal build all

 Just copy the file bech32 at the above path to the ~/cardano-node-1.35.3-linux directory

> cp ~/cardano-src/bech32/dist-newstyle/build/x86_64-linux/ghc-8.10.7/bech32-1.1.2/x/bech32/build/bech32/bech32 ~/cardano-node-1.35.3-linux/

------  Cryptographic algorithms used in Cardano's protocol  ------
 
curve25519 -> Public Key generation
Ed25519 -> Digital signatures
BECH32 -> Shelly Wallet Address -> Hashing algorithm used by Cardano for hashing transactions and public key addresses hashing
Advanced Encryption Standard (AES) -> Symmetric key encryption algorithm.
