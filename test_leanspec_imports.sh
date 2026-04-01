#!/bin/bash
# leanSpec module import coverage test.
# Attempts to import each leanSpec source module through the LeanPython interpreter.
# Reports PASS / XFAIL (expected failure with issue#) / FAIL (unexpected).
#
# Usage: ./test_leanspec_imports.sh

set -euo pipefail

EXE=".lake/build/bin/leanPython"
SRC="/home/zksecurity/leanPython/references/leanSpec/src"

if [ ! -f "$EXE" ]; then
  echo "error: $EXE not found — run 'lake build' first"
  exit 1
fi

PASS=0
FAIL=0
XFAIL=0
FAILURES=""

# Expected failures: module_pattern -> "issue_number reason"
# Modules matching these patterns are XFAIL (expected to fail).
declare -A XFAIL_MAP

# #5: lean_multisig_py (pending pure Python impl)
XFAIL_MAP["lean_spec.subspecs.xmss.aggregation"]="#5 lean_multisig_py"
# Transitive: everything importing xmss.aggregation
XFAIL_MAP["lean_spec.subspecs.xmss.interface"]="#5 imports aggregation transitively"
XFAIL_MAP["lean_spec.subspecs.containers.attestation.attestation"]="#5 imports xmss.aggregation"
XFAIL_MAP["lean_spec.subspecs.containers.block.block"]="#5 imports xmss.aggregation"
XFAIL_MAP["lean_spec.subspecs.containers.block.types"]="#4,#5 dict subclass + xmss.aggregation"
XFAIL_MAP["lean_spec.subspecs.containers.state.state"]="#5 imports xmss.aggregation"
XFAIL_MAP["lean_spec.subspecs.containers.validator"]="#5 imports xmss.containers->config"
XFAIL_MAP["lean_spec.subspecs.containers.attestation.aggregation_bits"]="#5 imports validator->xmss"
XFAIL_MAP["lean_spec.subspecs.containers.state.types"]="#5 imports validator->xmss"

# #5: XMSS modules that need lean_spec.config (get_args / PEP 695 __value__)
XFAIL_MAP["lean_spec.config"]="#5 get_args + PEP695 __value__"
XFAIL_MAP["lean_spec.subspecs.xmss.constants"]="#5 imports lean_spec.config"
XFAIL_MAP["lean_spec.subspecs.xmss.types"]="#5 imports xmss.constants"
XFAIL_MAP["lean_spec.subspecs.xmss.containers"]="#5 imports xmss.constants"
XFAIL_MAP["lean_spec.subspecs.xmss._validation"]="#5 imports lean_spec.config"
XFAIL_MAP["lean_spec.subspecs.xmss.hypercube"]="#5 imports xmss.constants"
XFAIL_MAP["lean_spec.subspecs.xmss.message_hash"]="#5 imports xmss chain"
XFAIL_MAP["lean_spec.subspecs.xmss.poseidon"]="#5 imports xmss chain"
XFAIL_MAP["lean_spec.subspecs.xmss.prf"]="#5 imports xmss.constants"
XFAIL_MAP["lean_spec.subspecs.xmss.rand"]="#5 imports xmss.constants"
XFAIL_MAP["lean_spec.subspecs.xmss.subtree"]="#5 imports xmss.types"
XFAIL_MAP["lean_spec.subspecs.xmss.target_sum"]="#5 imports xmss.constants"
XFAIL_MAP["lean_spec.subspecs.xmss.tweak_hash"]="#5 imports xmss chain"
XFAIL_MAP["lean_spec.subspecs.xmss.utils"]="#5 imports xmss chain"

# #6: numpy/numba (pending pure Python impl)
XFAIL_MAP["lean_spec.subspecs.poseidon2.permutation"]="#6 numpy"
XFAIL_MAP["lean_spec.subspecs.poseidon2.constants"]="#6 imports permutation transitively"

# #7: asyncio
XFAIL_MAP["lean_spec.__main__"]="#7 asyncio"
XFAIL_MAP["lean_spec.subspecs.chain.clock"]="#7 asyncio"
XFAIL_MAP["lean_spec.subspecs.chain.service"]="#7 asyncio"
XFAIL_MAP["lean_spec.subspecs.api.server"]="#7 asyncio"
XFAIL_MAP["lean_spec.subspecs.api.routes"]="#7 imports api.server"
XFAIL_MAP["lean_spec.subspecs.api.endpoints.checkpoints"]="#7 imports api chain"
XFAIL_MAP["lean_spec.subspecs.api.endpoints.fork_choice"]="#7 imports api chain"
XFAIL_MAP["lean_spec.subspecs.api.endpoints.health"]="#7 imports api chain"
XFAIL_MAP["lean_spec.subspecs.api.endpoints.metrics"]="#7 imports api chain"
XFAIL_MAP["lean_spec.subspecs.api.endpoints.states"]="#7 asyncio"
XFAIL_MAP["lean_spec.subspecs.networking.client.event_source"]="#7 asyncio"
XFAIL_MAP["lean_spec.subspecs.networking.client.reqresp_client"]="#7 asyncio"
XFAIL_MAP["lean_spec.subspecs.networking.discovery.service"]="#7 asyncio"
XFAIL_MAP["lean_spec.subspecs.networking.discovery.transport"]="#7 asyncio + cryptography"
XFAIL_MAP["lean_spec.subspecs.networking.gossipsub.behavior"]="#7 asyncio"
XFAIL_MAP["lean_spec.subspecs.networking.service.service"]="#7 asyncio"
XFAIL_MAP["lean_spec.subspecs.networking.service.events"]="#7 imports service chain"
XFAIL_MAP["lean_spec.subspecs.networking.transport.quic.connection"]="#7 asyncio + aioquic"
XFAIL_MAP["lean_spec.subspecs.networking.transport.quic.stream_adapter"]="#7 asyncio"
XFAIL_MAP["lean_spec.subspecs.node.node"]="#7 asyncio"
XFAIL_MAP["lean_spec.subspecs.sync.service"]="#7 asyncio"
XFAIL_MAP["lean_spec.subspecs.sync.backfill_sync"]="#7 imports sync chain"
XFAIL_MAP["lean_spec.subspecs.sync.checkpoint_sync"]="#7 imports sync chain"
XFAIL_MAP["lean_spec.subspecs.sync.head_sync"]="#7 imports sync chain"
XFAIL_MAP["lean_spec.subspecs.validator.service"]="#7 asyncio"
XFAIL_MAP["lean_spec.subspecs.validator.registry"]="#5,#7 imports xmss + chain"

# chain/__init__.py imports clock.py which uses asyncio, blocking all chain.* imports
XFAIL_MAP["lean_spec.subspecs.chain.config"]="#7 chain/__init__.py imports clock->asyncio"
XFAIL_MAP["lean_spec.subspecs.containers.checkpoint"]="#7 imports chain->clock->asyncio"
XFAIL_MAP["lean_spec.subspecs.containers.config"]="#7 imports chain->clock->asyncio"
XFAIL_MAP["lean_spec.subspecs.containers.slot"]="#7 imports chain->clock->asyncio"

# #9: prometheus_client
XFAIL_MAP["lean_spec.subspecs.metrics.registry"]="#9 prometheus_client"
XFAIL_MAP["lean_spec.subspecs.forkchoice.store"]="#5,#9 xmss + prometheus_client"

# Other external deps (cryptography, aioquic, sqlite3)
XFAIL_MAP["lean_spec.subspecs.networking.discovery.crypto"]="cryptography lib"
XFAIL_MAP["lean_spec.subspecs.networking.discovery.keys"]="cryptography lib"
XFAIL_MAP["lean_spec.subspecs.networking.discovery.handshake"]="cryptography lib"
XFAIL_MAP["lean_spec.subspecs.networking.discovery.codec"]="imports discovery chain"
XFAIL_MAP["lean_spec.subspecs.networking.discovery.config"]="imports discovery chain"
XFAIL_MAP["lean_spec.subspecs.networking.discovery.messages"]="imports discovery chain"
XFAIL_MAP["lean_spec.subspecs.networking.discovery.packet"]="imports discovery chain"
XFAIL_MAP["lean_spec.subspecs.networking.discovery.routing"]="imports discovery chain"
XFAIL_MAP["lean_spec.subspecs.networking.discovery.session"]="imports discovery chain"
XFAIL_MAP["lean_spec.subspecs.networking.enr.enr"]="cryptography lib"
XFAIL_MAP["lean_spec.subspecs.networking.enr.eth2"]="imports enr chain"
XFAIL_MAP["lean_spec.subspecs.networking.enr.keys"]="cryptography lib"
XFAIL_MAP["lean_spec.subspecs.networking.transport.identity.keypair"]="cryptography lib"
XFAIL_MAP["lean_spec.subspecs.networking.transport.identity.signature"]="cryptography lib"
XFAIL_MAP["lean_spec.subspecs.networking.transport.peer_id"]="imports identity chain"
XFAIL_MAP["lean_spec.subspecs.networking.transport.protocols"]="imports transport chain"
XFAIL_MAP["lean_spec.subspecs.networking.transport.quic.tls"]="aioquic + cryptography"
XFAIL_MAP["lean_spec.subspecs.networking.peer"]="imports networking chain"
XFAIL_MAP["lean_spec.subspecs.networking.config"]="imports networking chain"
XFAIL_MAP["lean_spec.subspecs.networking.types"]="imports networking chain"
XFAIL_MAP["lean_spec.subspecs.networking.varint"]="imports networking chain"
XFAIL_MAP["lean_spec.subspecs.networking.reqresp.codec"]="imports networking chain"
XFAIL_MAP["lean_spec.subspecs.networking.reqresp.handler"]="imports networking chain"
XFAIL_MAP["lean_spec.subspecs.networking.reqresp.message"]="imports networking chain"
XFAIL_MAP["lean_spec.subspecs.networking.gossipsub.mcache"]="imports networking chain"
XFAIL_MAP["lean_spec.subspecs.networking.gossipsub.mesh"]="imports networking chain"
XFAIL_MAP["lean_spec.subspecs.networking.gossipsub.message"]="imports networking chain"
XFAIL_MAP["lean_spec.subspecs.networking.gossipsub.parameters"]="imports networking chain"
XFAIL_MAP["lean_spec.subspecs.networking.gossipsub.rpc"]="imports networking chain"
XFAIL_MAP["lean_spec.subspecs.networking.gossipsub.topic"]="imports networking chain"
XFAIL_MAP["lean_spec.subspecs.networking.gossipsub.types"]="imports networking chain"
XFAIL_MAP["lean_spec.subspecs.storage.sqlite"]="sqlite3"
XFAIL_MAP["lean_spec.subspecs.storage.database"]="imports storage chain"
XFAIL_MAP["lean_spec.subspecs.storage.exceptions"]="imports storage chain"
XFAIL_MAP["lean_spec.subspecs.storage.namespaces"]="imports storage chain"
XFAIL_MAP["lean_spec.subspecs.sync.block_cache"]="imports sync chain"
XFAIL_MAP["lean_spec.subspecs.sync.config"]="imports sync chain"
XFAIL_MAP["lean_spec.subspecs.sync.peer_manager"]="imports sync chain"
XFAIL_MAP["lean_spec.subspecs.sync.states"]="imports sync chain"
XFAIL_MAP["lean_spec.subspecs.genesis.config"]="#5 imports xmss chain"
XFAIL_MAP["lean_spec.subspecs.genesis.state"]="#5 imports genesis.config"

# --- Test runner ---

MAIN_PY="$SRC/_leanpython_import_test.py"
cleanup() { rm -f "$MAIN_PY"; }
trap cleanup EXIT

try_import() {
  local mod="$1"

  cat > "$MAIN_PY" << PYEOF
import $mod
PYEOF

  local output
  output=$(timeout 30 "$EXE" "$MAIN_PY" 2>&1) || true

  if [ -z "$output" ]; then
    return 0  # success (no output = no error)
  else
    echo "$output"
    return 1
  fi
}

echo "=== LeanPython leanSpec import coverage ==="
echo ""

# Collect all modules
MODULES=$(find "$SRC/lean_spec" -name "*.py" -not -name "__init__.py" -not -path "*__pycache__*" \
  | sed "s|$SRC/||; s|/|.|g; s|\.py$||" | sort)

for mod in $MODULES; do
  xfail_reason="${XFAIL_MAP[$mod]:-}"

  output=$(try_import "$mod" 2>&1) && ok=true || ok=false

  if $ok; then
    if [ -n "$xfail_reason" ]; then
      # Unexpectedly passed — still good, report as PASS
      printf "  PASS  %-60s (was xfail: %s)\n" "$mod" "$xfail_reason"
    else
      printf "  PASS  %s\n" "$mod"
    fi
    PASS=$((PASS + 1))
  else
    if [ -n "$xfail_reason" ]; then
      printf "  XFAIL %-60s [%s]\n" "$mod" "$xfail_reason"
      XFAIL=$((XFAIL + 1))
    else
      printf "  FAIL  %s\n" "$mod"
      FAILURES="$FAILURES\n  $mod: $(echo "$output" | head -1)"
      FAIL=$((FAIL + 1))
    fi
  fi
done

echo ""
echo "Results: $PASS pass, $FAIL fail, $XFAIL xfail (expected)"

if [ $FAIL -gt 0 ]; then
  echo ""
  echo "Unexpected failures:"
  printf "$FAILURES\n"
  exit 1
fi

exit 0
