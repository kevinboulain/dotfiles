#!/usr/bin/env bash

set -eu

csr() {
  local cn=$1
  local csr
  shift
  read -r -d '' csr << EOF || true
{
  "CN": "$cn",
  "key": { "algo": "ed25519" },
  "hosts": [ $(printf '"%s", ' "$@" | sed 's/, $//;s/""//') ]
}
EOF
  printf "%s\n" "$csr"
}

ca_config=$(mktemp)
> "$ca_config" echo '{
  "signing": {
    "profiles": {
      "client": {
        "expiry": "87600h",
        "usages": ["signing", "key encipherment", "client auth"]
      },
      "peer": {
        "expiry": "87600h",
        "usages": ["signing", "key encipherment", "client auth", "server auth"]
      },
      "server": {
        "expiry": "8760h",
        "usages": ["signing", "key encipherment", "client auth", "server auth"]
      }
    }
  }
}'

for kind in client peer; do
  mkdir -p certs/etcd/"$kind"
  <<< "$(csr etcd-"$kind"-ca)" cfssl gencert -initca - | cfssljson -bare certs/etcd/"$kind"/ca
  for k in kubernetes-0{1,2,3,4}; do
    <<< "$(csr "$k" "$k")" cfssl gencert -ca certs/etcd/"$kind"/ca.pem -ca-key certs/etcd/"$kind"/ca-key.pem -config "$ca_config" -profile server - | cfssljson -bare certs/etcd/"$kind"/"$k"
  done
done
