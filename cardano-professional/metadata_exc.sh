 we start by creating a metadata.json file with the following content:

{
    "20220101": {
        "name": "hello world",
        "completed": 0
    }
}

cardano-cli Transaction build --alonzo-era --tx-in  --out $(cat receiver's address)+Amount  --change-address $(receiver's-address) $TESTNET --out-file (TxName.raw)

20220101

curl -H 'project_id: 20220101' https://cardano-testnet.blockfrost.io/api/v0/metadata/txs/labels/20220101 | jq




