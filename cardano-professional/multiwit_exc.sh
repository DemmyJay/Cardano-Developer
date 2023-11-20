
UTXO1="ce74b4d24b51dce76cb30a53870a7686c23693baf4232db76109c3ac4d4ce95a#0"
UTXO2="ce74b4d24b51dce76cb30a53870a7686c23693baf4232db76109c3ac4d4ce95a#1"

expr 5000000000 + 4999822751

Transaction Draft = Unsigned Transaction - Balance & Fee

cardano-cli transaction build-raw \
--tx-in $UTXO1 \
--tx-in $UTXO2 \
--tx-out $(cat base1.addr)+$BALANCE \
--fee $FEE \
--out-file tx.raw

VALIDTILL="expr 600 + $(cardano-cli query tip $PRETESTNET | jq -r .slot)"

cardano-cli transaction calculate-min-fee \
--tx-body-file tx.draft \
--tx-in-count 2 \
--tx-out-count 1 \
--witness-count 2 \
$PRETESTNET \
--protocol-params-file protparams.json

FEE="180461"

BALANCE="expr 9999822751 - $FEE"


| sed '1,2d' | awk '{print $3}' | xargs | sed 's/ / + /g' | xargs expr +
| sed '1,2d' | awk '{print $3}' | xargs | sed 's/ / + /g' | xargs expr + | sed 's/./&./6'

cardano-cli transaction sign \
 --tx-body-file tx.raw \
 --signing-key-file addr2.skey \
 --signing-key-file addr3.skey \
 $PRETESTNET \
 --out-file tx.signed




