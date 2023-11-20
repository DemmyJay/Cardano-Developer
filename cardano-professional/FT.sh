Asset Token:
tokenname1=$(echo -n "Testtoken" | xxd -ps | tr -d '\n')
tokenamount="10000000"
output="0"



UTXOIN="31a982034999ae30626f4bc1fbc5dbc79d85246710d23636056cc9f74b344c67#0"
funds="5000000"
policyid=$(cat policy/policyID)

--> Calculate the fees:
  fee=$(cardano-cli transaction calculate-min-fee --tx-body-file FT.raw --tx-in-count 1 --tx-out-count 1 --witness-count 2 $PRETESTNET --protocol-params-file protocol.json | cut -d " " -f1)


Note: The same transaction build syntax used to calculate the fee would is used to rebuild.

cardano-cli transaction build-raw \
 --fee $fee \
 --tx-in $UTXOIN \
 --tx-out $address+$output+"$tokenamount $policyid.$tokenname1" \
 --mint "$tokenamount $policyid.$tokenname1" \
 --minting-script-file policy/policy.script \
 --out-file FT.raw

 --> Sign Transaction:

  cardano-cli transaction sign  \
--signing-key-file addr1.skey  \
--signing-key-file policy/policy.skey  \
$PRETESTNET --tx-body-file FT.raw  \
--out-file FT.signed


cardano-cli transaction submit --tx-file FT.signed $PRETESTNET



--> Sending Fungible Tokens to another Address:

$ fee="0"
$ receiver="addr_test1qzamv0pg6x65ep78wxzvmhjstvs5rzzypxvhwqrt4vsmmkqjmu585ew2tyyh2pxwa5w73wf4tz6tfunmqhvnz8a2wutq2aztad"
$ receiver_output="10000000"
$ utxoin="d82e82776b3588c1a2c75245a20a9703f971145d1ca9fba4ad11f50803a43190#0"
$ funds="999824071"
 
 * Minted tokens be sent from base1.addr to base2.addr.



cardano-cli query utxo --address $(cat base1.addr) $PRETESTNET


Send token to another wallet











