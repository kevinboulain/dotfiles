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
  if [ ! -e certs/etcd/"$kind"/ca-key.pem ]; then
    <<< "$(csr etcd-"$kind"-ca)" cfssl gencert -initca - | cfssljson -bare certs/etcd/"$kind"/ca
  else
    echo "SKIPPED CA"
  fi

  subkinds=()
  if [ "$kind" = "client" ]; then
    subkinds+=(client server)
  else
    subkinds+=("")
  fi

  for subkind in "${subkinds[@]}"; do
    for k in kubernetes-0{1,2,3,4}; do
      base=certs/etcd/"$kind${subkind:+"/$subkind"}"
      mkdir -p "$base"
      base="$base"/"$k"
      if [ ! -e "$base"-key.pem ]; then
        <<< "$(csr "$k" "$k")" cfssl gencert -ca certs/etcd/"$kind"/ca.pem -ca-key certs/etcd/"$kind"/ca-key.pem -config "$ca_config" -profile server - | cfssljson -bare "$base"
      else
        echo "SKIPPED $base"
      fi
    done
  done
done
